library(dplyr)
library(tidyr)

# set paths and constants
gcamwrapper_path <- "C:/GCAM/gcamwrapper" # location of gcamwrapper package
config_filename <- "configuration_Reference_NZ_endogenous.xml" # filename of config to run
config_path <- "C:/GCAM/gcam7_climate_macro/exe" # location of config file above
db_location <- "./output" # where to write the database

CONV_KWH_EJ <- 3.6e-12
CONV_KWH_GJ <- 3.6e-3
hours_per_yr <- 8760

# read in input data and mappings
learning_rates <-
  as_tibble(read.csv("input/learning_rates.csv"))

initial_conditions <-
  as_tibble(read.csv("input/t0_cost_deployment.csv"))

cal_adders <-
  as_tibble(read.csv("params/cal_adders.csv"))

cooling_tech_map <-
  as_tibble(read.csv("input/mappings/cooling_tech_map.csv"))

learning_components_deployment_map <-
  as_tibble(read.csv("input/mappings/learning_components_deployment_map.csv"))

learning_components_learning_map <-
  as_tibble(read.csv("input/mappings/learning_components_learning_map.csv"))

tech_FCR <- read.csv("input/tech_FCR.csv")

non_learning_capital <-
  as_tibble(read.csv("params/non_learning_capital.csv"))

# load gcamwrapper
source(paste0(gcamwrapper_path, "/env.R"))
devtools::load_all(gcamwrapper_path)

# Create a GCAM instance by providing a configuration file
g <- create_and_initialize(config_filename, config_path)

# queries
elec_gen_tech_query <- "world/region{region@name}/sector[+NamedFilter,StringEquals,electricity]/subsector{subsector@name}/technology{tech@name}/period{vintage@year}/output{output@name}/physical-output{year@year}"
elec_gen_rootop_pv_query <- "world/region{region@name}/sector[+NamedFilter,StringEquals,elect_td_bld]/subsector[+NamedFilter,StringEquals,rooftop_pv]/technology{tech@name}/period{vintage@year}/output{output@name}/physical-output{year@year}"
elec_costs_tech_input_vintage_query <- "world/region{region@name}/sector[+NamedFilter,StringEquals,electricity]/subsector{subsector@name}/technology{tech@name}/period{vintage@year}/input{input@name}/adjusted-cost{year@year}"
rooftop_pv_costs_tech_input_vintage_query <- "world/region{region@name}/sector[+NamedFilter,StringEquals,elect_td_bld]/subsector[+NamedFilter,StringEquals,rooftop_pv]/technology{tech@name}/period{vintage@year}/input{input@name}/adjusted-cost{year@year}"
elec_cap_factors_query <- "world/region{region@name}/sector[+NamedFilter,StringEquals,electricity]/subsector{subsector@name}/technology{tech@name}/period{vintage@year}/capacity-factor"
rooftop_pv_cap_factors_query <- "world/region{region@name}/sector[+NamedFilter,StringEquals,elect_td_bld]/subsector[+NamedFilter,StringEquals,rooftop_pv]/technology{tech@name}/period{vintage@year}/capacity-factor"
generation_cooling_vintage <- "world/region{region@name}/sector[+NamedFilter,StringRegexMatches,^elec_]/subsector{subsector@name}/technology{tech@name}/period{vintage@year}/output{output@name}/physical-output{year@year}"


while (get_current_period(g) <= 10) {
  run_period(g)

  if (get_current_period(g) >= 4) {

    # 1. Query data

    # elec gen by tech (non cooling techs)
    elec_gen_tech_vintage <- get_data(g, elec_gen_tech_query) %>%
      filter(!technology %in% unique(cooling_tech_map$technology_passthru))

    elec_gen_rooftop_pv <- get_data(g, elec_gen_rootop_pv_query)

    # elec gen by cooling tech
    elec_gen_cooling_vintage <- get_data(g, generation_cooling_vintage) %>%
      filter(sector %in% unique(cooling_tech_map$sector))

    # combined elec gen
    elec_gen_all <- rbind(elec_gen_tech_vintage, elec_gen_cooling_vintage, elec_gen_rooftop_pv)

    # elec tech input costs
    elec_costs_tech_input_vintage <- get_data(g, elec_costs_tech_input_vintage_query)
    rooftop_pv_costs_tech_input_vintage <- get_data(g, rooftop_pv_costs_tech_input_vintage_query)
    elec_costs_tech_input_vintage_all <- rbind(elec_costs_tech_input_vintage, rooftop_pv_costs_tech_input_vintage)

    # elec tech capacity factors
    elec_cap_factors <- get_data(g, elec_cap_factors_query)
    rooftop_pv_cap_factors <- get_data(g, rooftop_pv_cap_factors_query)
    elec_cap_factors_all <- rbind(elec_cap_factors, rooftop_pv_cap_factors)


    # 2. Calculate cumulative capacity (sum of new capacity additions each period)
    # at the aggregate level (learning groups)
    cum_capacity <- elec_gen_all %>%
      filter(period >= 2015, period == year, region == "USA") %>%
      select(-year) %>%
      # aggregate cooling technologies
      left_join(cooling_tech_map, by = c("sector", "subsector", "technology")) %>%
      mutate(sector = if_else(!is.na(sector_passthru), sector_passthru, sector),
             subsector = if_else(!is.na(subsector_passthru), subsector_passthru, subsector),
             technology = if_else(!is.na(technology_passthru),
                                  technology_passthru, technology)) %>%
      select(-c(sector_passthru, subsector_passthru, technology_passthru)) %>%
      left_join(elec_cap_factors_all,
                by = c("region", "sector", "subsector", "technology", "period")) %>%
      mutate(capacity = `physical-output`/CONV_KWH_EJ/hours_per_yr/`capacity-factor`) %>%
      group_by(region, sector, subsector, technology) %>%
      summarise(value = sum(capacity)) %>%
      mutate(period = get_current_year(g)) %>%
      ungroup() %>%
      # aggregate learning groups (storage vs no storage)
      right_join(learning_components_deployment_map, by = c("sector", "subsector", "technology")) %>%
      group_by(region, learning_component, period, multiplier) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      filter(learning_component != "thermal_storage")

    # 3. Calculate learning
    learning <- learning_rates %>%
      left_join(cum_capacity,
                by = c("region", "learning_component")) %>%
      left_join(initial_conditions,
                by = c("region", "learning_component")) %>%
      # only calculate learning for technologies with nonzero cumulative deployment
      mutate(cost_adj = if_else(value == 0, 0, t0_cost*(value*multiplier/t0_deployment)^log2(1-learning_rate))) %>%
      select(region, learning_component, cost_adj)


    # 4. add exogenous components (calibration adders and nonlearning capital)
    next_year <- get_current_year(g) + 5

    # map from learning components to GCAM technologies and add
    # calibration adders and nonlearning capital
    learning_disagg <- learning %>%
      mutate(year = next_year) %>%
      left_join(learning_components_learning_map,
                by = c("learning_component")) %>%
      group_by(region, year, sector, subsector, technology) %>%
      summarize(cost_adj = sum(cost_adj)) %>%
      ungroup() %>%
      left_join(non_learning_capital, by = c("region", "year", "sector",
                                             "subsector", "technology")) %>%
      left_join(cal_adders,
                by = c("region", 'year', "sector", "subsector", "technology")) %>%
      replace_na(list(nonlearning_cap = 0, cal_adder = 0)) %>%
      filter(!(cost_adj == 0 & cal_adder == 0))


    # 5. Calculate and apply updated capital costs
    if (nrow(learning) > 0) {

      elec_cap_costs_adj <- learning_disagg %>%
        left_join(elec_cap_factors_all,
                  by = c("region", "sector", "subsector", "technology",
                         "year" = "period")) %>%
        left_join(tech_FCR, by = c("region", "sector", "subsector", "technology")) %>%
        mutate(adj_cost_new = (cost_adj + cal_adder + nonlearning_cap)*FCR/hours_per_yr/`capacity-factor`/CONV_KWH_GJ,
               input = "capital") %>%
        select(region, sector, subsector, technology, input, period = year, adj_cost_new)

      #print(as.matrix(elec_cap_costs_adj[,c(4,7)]))

      # add adjusted costs back into the table of all costs for updating
      # costs in the next period
      elec_cap_costs_adj_all <- elec_costs_tech_input_vintage_all %>%
        filter(period == next_year) %>%
        left_join(elec_cap_costs_adj,
                  by = c("region", "sector", "subsector", "technology", "period", 'input')) %>%
        mutate(`adjusted-cost` = if_else(is.na(adj_cost_new), `adjusted-cost`, adj_cost_new)) %>%
        select(colnames(elec_costs_tech_input_vintage))

      # update costs for the next period
      set_data(g, elec_cap_costs_adj_all, elec_costs_tech_input_vintage_query, list("region" = c("+", "="), "year" = c("+", "=")))

    }

  }

}

# write database of results (note: this may not work locally depending on
# issues with gcamwrapper setup)
#print_xmldb(g, xmldb_location = db_location)


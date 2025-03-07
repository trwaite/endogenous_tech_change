library(tidyverse)

learning_rates <- read_csv("input/learning_rates.csv")
tech_FCR <- read_csv("input/tech_FCR.csv")
cooling_tech_map <- read_csv("input/mappings/cooling_tech_map.csv")
learning_components_deployment_map <- read_csv("input/mappings/learning_components_deployment_map.csv")
learning_components_learning_map <- read_csv("input/mappings/learning_components_learning_map.csv")
t0_cost_deployment <- read_csv("input/t0_cost_deployment.csv")

CONV_KWH_EJ <- 3.6e-12
CONV_KWH_GJ <- 3.6e-3
hours_per_yr <- 8760

conv_USD_2015_1975 <- 0.285


# get costs and cumulative capacities from the reference run

db_dir <- "C:/GCAM/gcam7_climate_macro/output"
ref_db_name <- "db_Reference_exogenous"

query_file <- "endo_tech_change_queries.xml"


conn <- rgcam::localDBConn(db_dir, ref_db_name)
prj_etc <- rgcam::addScenario(conn, "prj_etc", queryFile = query_file,
                              clobber = T)


# new vintage generation
elec_gen_tech_new <- rgcam::getQuery(prj_etc,
                                     "elec gen by gen tech and cooling tech (new)") %>%
  rename(generation = value)

# levelized capital costs
elec_cap_costs <- rgcam::getQuery(prj_etc, "elec gen costs by tech") %>%
  filter(input == "capital") %>%
  separate(technology, into = c("technology", "vintage"), sep = "=") %>%
  mutate(technology = gsub(",year", "", technology),
         vintage = as.numeric(vintage))

# figure showing PV capital costs over time
elec_cap_costs %>%
  filter(technology == "PV", year >= 2015) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab("Levelized capital cost (1975$/GJ)")

ggsave("figures/exogneous_pv_capital_costs.png",
       width = 4, height = 3.5, units = "in")

# capacity factors
elec_cap_factors <- rgcam::getQuery(prj_etc, "elec capacity factors") %>%
  rename(cap_factor = value)

# calculate new capacity in each period
new_capacity <- elec_gen_tech_new %>%
  # start in 2015 because there is no vintaging before that
  filter(year >= 2015) %>%
  # add in capacity factors
  left_join(elec_cap_factors,
            by = c("year", "region", "sector", "subsector", "technology", "scenario")) %>%
  # calculate new capacity
  mutate(capacity = generation/CONV_KWH_EJ/hours_per_yr/cap_factor)

# aggregate new capacities to learning components
new_capacity_learning_components <- new_capacity %>%
  # map cooling techs to pass thru techs
  left_join(cooling_tech_map,
            by = c("sector", "subsector","technology")) %>%
  mutate(sector = if_else(!is.na(sector_passthru), sector_passthru, sector),
         subsector = if_else(!is.na(subsector_passthru), subsector_passthru, subsector),
         technology = if_else(!is.na(technology_passthru),
                              technology_passthru, technology)) %>%
  select(-c(sector_passthru, subsector_passthru, technology_passthru)) %>%
  # map technologies to their learning components
  right_join(learning_components_deployment_map,
             by = c("sector", "subsector", "technology")) %>%
  # add total new capacity per learning tech group
  group_by(scenario, region, learning_component, year) %>%
  summarize(capacity = sum(capacity*multiplier)) %>%
  ungroup() %>%
  # NAs occur when there's no deployment in the ref scenario (CCS)
  drop_na()

# calculate cumulative capacities
cum_capacity_learning_components <- new_capacity_learning_components %>%
  # fill in missing years with 0s
  complete(year, nesting(scenario, region, learning_component)) %>%
  replace_na(list(capacity = 0)) %>%
  # calculate cumulative capacity
  group_by(scenario, region, learning_component) %>%
  arrange(year) %>%
  mutate(cum_cap = cumsum(capacity)) %>%
  ungroup()


# convert capital inputs to overnight capital costs (1975$/kW)
# for all GCAM technologies
elec_cap_costs_overnight <- elec_cap_costs %>%
  left_join(elec_cap_factors,
            by = c("scenario", "region", "sector", "subsector", "technology", "year")) %>%
  right_join(tech_FCR, by = c("region", "sector", "subsector", "technology")) %>%
  mutate(cap_cost = value/FCR*hours_per_yr*cap_factor*CONV_KWH_GJ) %>%
  select(scenario, region, sector, subsector, technology, year, cap_cost)


# apply learning curves to cumulative deployment of learning components
learned_costs_learning_component <- cum_capacity_learning_components %>%
  left_join(learning_rates, by = c('region', 'learning_component')) %>%
  left_join(t0_cost_deployment, by = c('region', 'learning_component')) %>%
  mutate(learned_cost = if_else(cum_cap == 0, 0,
                                t0_cost*(cum_cap/t0_deployment)^(log2(1-learning_rate)))) %>%
  filter(learned_cost != 0) %>%
  group_by(learning_component) %>%
  arrange(year) %>%
  mutate(learned_cost = lag(learned_cost)) %>%
  filter(year > 2015) %>%
  select(scenario, region, year, learning_component, learned_cost)


# map back to GCAM technologies
learned_costs_GCAM_tech <- learned_costs_learning_component %>%
  left_join(learning_components_learning_map, by = c("learning_component")) %>%
  group_by(scenario, region, year, sector, subsector, technology) %>%
  summarize(learned_cost = sum(learned_cost, na.rm = T)) %>%
  ungroup()



# for technologies with non-learning components,
# calculate this component of capital cost.
# this consists of CSP storage capital and coal/ gas non-CCS capital

# CSP storage capital
non_learning_capital_storage <- elec_cap_costs %>%
  filter(grepl("CSP", technology)) %>%
  left_join(elec_cap_factors, by = c("scenario", "region", "year", "sector",
                                     "subsector", "technology")) %>%
  mutate(cap_cost = value/0.13*hours_per_yr*cap_factor*CONV_KWH_GJ) %>%
  select(scenario, region, year, sector, subsector, technology, cap_cost) %>%
  separate(technology, into = c("technology", "storage"), sep = "_") %>%
  replace_na(list(storage = "no_storage")) %>%
  pivot_wider(names_from = "storage", values_from = "cap_cost") %>%
  mutate(nonlearning_cap = storage - no_storage,
         technology = paste0(technology, "_storage")) %>%
  select(-c(storage, no_storage, scenario))

# coal and gas non-CCS capital
non_learning_capital_CCS <- elec_cap_costs %>%
  filter(grepl("coal|gas", technology),
         !grepl("steam/CT", technology)) %>%
  left_join(elec_cap_factors, by = c("scenario", "region", "year", "sector",
                                     "subsector", "technology")) %>%
  mutate(cap_cost = value/0.13*hours_per_yr*cap_factor*CONV_KWH_GJ,
         CCS = if_else(grepl("CCS", technology), "CCS", "no_CCS"),
         technology = gsub(" CCS", "", technology)) %>%
  select(scenario, region, year, sector, subsector, technology, CCS, cap_cost) %>%
  pivot_wider(names_from = "CCS", values_from = "cap_cost") %>%
  mutate(nonlearning_cap = no_CCS,
         CCS_cap = CCS - no_CCS,
         technology = gsub(")", " CCS)", technology)) %>%
  select(-c(CCS, no_CCS, CCS_cap, scenario))

# combine nonlearning capital components
non_learning_capital_all <- rbind(non_learning_capital_storage,
                                  non_learning_capital_CCS)

write_csv(non_learning_capital_all,
          "params/non_learning_capital.csv")


# calculate exogenous calibration adders

# compare with GCAM default costs
tech_costs_compare <- learned_costs_GCAM_tech %>%
  left_join(elec_cap_costs_overnight,
            by = c("scenario", "region", "year", "sector", "subsector", "technology")) %>%
  left_join(non_learning_capital_all,
            by = c("region", "year", "sector", "subsector", "technology")) %>%
  replace_na(list(nonlearning_cap = 0)) %>%
  mutate(learned_cost_total = learned_cost + nonlearning_cap) %>%
  select(-nonlearning_cap) %>%
  pivot_longer(c(learned_cost, cap_cost, learned_cost_total),
               names_to = "source", values_to = "value")

calibration_adders <- tech_costs_compare %>%
  filter(year >= 2020) %>%
  pivot_wider(names_from = "source", values_from = "value") %>%
  left_join(non_learning_capital_all,
            by = c("region", "year", "sector", "subsector", "technology")) %>%
  replace_na(list(learned_cost = 0, nonlearning_cap = 0)) %>%
  mutate(cal_adder = cap_cost - learned_cost - nonlearning_cap) %>%
  select(region, year, sector, subsector, technology, cal_adder)


# write out calibration params
write_csv(calibration_adders, "params/cal_adders.csv")



# plot calibration

plot_calib  <-
  tech_costs_compare %>%
  filter(source != "learned_cost", value > 0,
         !(year == 2020 & grepl("rooftop|storage", technology))) %>%
  pivot_wider(names_from = "source", values_from = "value") %>%
  drop_na()

ggplot() +
  geom_line(data = tech_costs_compare %>%
              filter(source != "learned_cost", value > 0,
                     !(year == 2020 & grepl("rooftop|storage", technology))) %>%
              mutate(source = if_else(source == "cap_cost", "GCAM default",
                                      "Learning curve output (Ref)")),
             aes(x = year, y = value, color = source)) +
  geom_segment(data = plot_calib,
               aes(x = year, xend = year, y = cap_cost, yend = learned_cost_total),
               lty = 2) +
  expand_limits(y = 0) +
  theme_bw() +
  facet_wrap(~technology, scales = "fixed", nrow = 2) +
  ylab("Capital cost (1975$/kW)")

ggsave("figures/tech_calibration.png",
       width = 10, height = 5, units = "in")


# subset of technologies for all hands slides
ggplot() +
  geom_line(data = tech_costs_compare %>%
              filter(source != "learned_cost", value > 0,
                     technology %in% c("wind", "PV", "wind_storage", "PV_storage"),
                     !(grepl("storage", technology) & year == 2020)) %>%
              mutate(source = if_else(source == "cap_cost", "GCAM default",
                                      "Learning curve\noutput (Ref)")),
            aes(x = year, y = value, color = source)) +
  geom_segment(data = plot_calib %>%
                 filter(technology %in% c("wind", "PV", "wind_storage", "PV_storage")),
               aes(x = year, xend = year, y = cap_cost, yend = learned_cost_total),
               lty = 3) +
  expand_limits(y = 0) +
  scale_color_discrete(name = "") +
  theme_bw() +
  facet_wrap(~technology, scales = "fixed", nrow = 2) +
  ylab("Capital cost (1975$/kW)")

ggsave("figures/tech_calibration_subset.png",
       width = 7, height = 5, units = "in")

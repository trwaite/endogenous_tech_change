# this script contains some code to analyze results of endogenous tech change
# experiments run using gcamwrapper.

library(tidyverse)

# constants
CONV_KWH_EJ <- 3.6e-12
CONV_KWH_GJ <- 3.6e-3
hours_per_yr <- 8760
CONV_KW_GW <- 1e-6

# FCR for converting between generation and capital
tech_FCR <- read_csv("input/tech_FCR.csv")

# filepaths
db_path <- "C:/GCAM/gcam7_climate_macro/output" # path to where databases are stored
db_names <- c("db_Reference_exogenous",
              "db_NZ_exogenous", "db_NZ_endogenous_v2") # database names
query_file <- "ETC_queries.xml" # queries

# read databases
for(db_name in db_names){
  conn <- rgcam::localDBConn(db_path, db_name)
  prj_ETC_results <- rgcam::addScenario(conn, "prj_ETC_results",
                                        queryFile = query_file,
                                        clobber = T)
}


# capital costs ----------------------------------------------------------------

capital_costs <- rgcam::getQuery(prj_ETC_results, "elec gen costs by tech") %>%
  filter(input == "capital",
         grepl("PV|CSP|wind|pv|Gen|CCS", technology),
         !grepl("liquids|biomass", technology)) %>%
  separate(technology, into = c("technology", "vintage"), sep = ",") %>%
  mutate(vintage = as.numeric(gsub("year=", "", vintage)))

capital_costs$technology <-
  factor(capital_costs$technology,
         levels = c("PV", "CSP", "wind", "PV_storage", "CSP_storage", "wind_storage",
                    "wind_offshore",
                    "rooftop_pv", "Gen_II_LWR", "Gen_III",
                    "coal (conv pul CCS)", "coal (IGCC CCS)", "gas (CC CCS)"))

capital_costs %>%
  filter(year >= 2015, year <= 2050,
         scenario %in% c("Reference_exogenous", "Reference_NZ_endogenous")) %>%
  mutate(scenario = if_else(scenario == "Reference_exogenous",
                            "GCAM default", "NZ endogenous")) %>%
  ggplot(aes(x = year, y = value, color = scenario)) +
  geom_line() +
  facet_wrap(~technology, nrow = 3, scales = "free_y") +
  expand_limits(y = 0) +
  theme_bw() +
  xlab("") + ylab("Capital cost (1975$/GJ)")

ggsave("figures/results_cap_cost.png", width = 10, height = 5, units = "in")


# deployment -------------------------------------------------------------------
elec_gen_tech_new <- rgcam::getQuery(prj_ETC_results,
                                     "elec gen by gen tech and cooling tech (new)") %>%
  rename(generation = value)


elec_cap_factors <- rgcam::getQuery(prj_ETC_results,
                                    "elec capacity factors") %>%
  rename(cap_factor = value)

# calculate new capacity in each period
new_capacity <- elec_gen_tech_new %>%
  filter(grepl("PV|CSP|wind|rooftop|Gen|CCS", technology),
         !grepl("biomass|liquids", technology)) %>%
  # right_join(learning_techs,
  #            by = c("region", "sector", "subsector", "technology")) %>%
  left_join(elec_cap_factors,
            by = c("year", "region", "technology", "scenario")) %>%
  mutate(capacity = generation/CONV_KWH_EJ/hours_per_yr/cap_factor*CONV_KW_GW,
         units = "GW") %>%
  select(scenario, region, sector = sector.x, subsector = subsector.x,
         technology, year, capacity, units) %>%
  mutate(technology = case_when(grepl("CSP_storage", technology) ~ "CSP_storage",
                                grepl("CSP", technology) ~ "CSP",
                                grepl("Gen", technology) ~ "nuclear",
                                grepl("conv pul CCS", technology) ~ "coal (conv pul CCS)",
                                grepl("IGCC CCS", technology) ~ "coal (IGCC CCS)",
                                grepl("CC CCS", technology) ~ "gas (CC CCS)",
                                T ~ technology)) %>%
  group_by(scenario, region, year, sector, subsector, technology, units) %>%
  summarize(capacity = sum(capacity)) %>%
  ungroup()

# calculate cumulative capacities
cum_capacity <- new_capacity %>%
  filter(year >= 2015) %>%
  arrange(year) %>%
  group_by(scenario, region, sector, subsector, technology, units) %>%
  mutate(cum_cap = cumsum(capacity)) %>%
  ungroup()

new_capacity$technology <-
  factor(new_capacity$technology,
         levels = c("PV", "CSP", "wind", "PV_storage", "CSP_storage",
                    "wind_storage", "wind_offshore", "rooftop_pv", "nuclear",
                    "coal (conv pul CCS)", "coal (IGCC CCS)", "gas (CC CCS)"))


# plot new capacity
new_capacity %>%
  filter(year >= 2015, year <= 2050) %>%
  # mutate(scenario = case_when(grepl("Ref", scenario) ~ "Ref exogenous only",
  #                             grepl("noLearn", scenario) ~ "NZ exogenous only",
  #                             T ~ "NZ endogenous + exogenous")) %>%
  ggplot(aes(x = year, y = capacity, color = scenario)) +
  geom_line() +
  facet_wrap(~technology, scales = "free_y") +
  theme_bw() +
  xlab("") + ylab("Capacity additions (GW)")

ggsave("figures/results_new_cap.png", width = 10, height = 5, units = "in")

cum_capacity$technology <-
  factor(cum_capacity$technology,
         levels = c("PV", "CSP", "wind", "PV_storage", "CSP_storage",
                    "wind_storage", "wind_offshore", "rooftop_pv", "nuclear",
                    "coal (conv pul CCS)", "coal (IGCC CCS)", "gas (CC CCS)"))


# plot cumulative capacity
cum_capacity %>%
  filter(year >= 2015, year <= 2050) %>%
  # mutate(scenario = case_when(grepl("Ref", scenario) ~ "Ref exogenous only",
  #                             grepl("noLearn", scenario) ~ "NZ exogenous only",
  #                             T ~ "NZ endogenous + exogenous")) %>%
  ggplot(aes(x = year, y = cum_cap, color = scenario)) +
  geom_line() +
  facet_wrap(~technology, scales = "free_y") +
  theme_bw() +
  xlab("") + ylab("Cumulative capacity additions (GW)")

ggsave("figures/results_cum_cap.png", width = 11, height = 5, units = "in")

# installed capacity
installed_cap <- rgcam::getQuery(prj_ETC_results,
                               "elec gen by gen tech and cooling tech and vintage") %>%

  separate(technology, into = c("technology", "vintage"), sep = ",") %>%
  filter(grepl("PV|CSP|wind|rooftop|Gen|CCS", technology),
         !grepl("biomass|liquids", technology)) %>%
  left_join(elec_cap_factors,
            by = c("year", "region", "technology", "scenario")) %>%
  mutate(capacity = value/CONV_KWH_EJ/hours_per_yr/cap_factor*CONV_KW_GW,
         units = "GW") %>%
  select(scenario, region, sector = sector.x, subsector = subsector.x,
         technology, year, capacity, units) %>%
  mutate(technology = case_when(grepl("CSP_storage", technology) ~ "CSP_storage",
                                grepl("CSP", technology) ~ "CSP",
                                grepl("Gen", technology) ~ "nuclear",
                                grepl("conv pul CCS", technology) ~ "coal (conv pul CCS)",
                                grepl("IGCC CCS", technology) ~ "coal (IGCC CCS)",
                                grepl("CC CCS", technology) ~ "gas (CC CCS)",
                                T ~ technology)) %>%
  group_by(scenario, region, year, sector, subsector, technology, units) %>%
  summarize(capacity = sum(capacity)) %>%
  ungroup()

installed_cap$technology <-
  factor(installed_cap$technology,
         levels = c("PV", "CSP", "wind", "PV_storage", "CSP_storage",
                    "wind_storage", "wind_offshore", "rooftop_pv", "nuclear",
                    "coal (conv pul CCS)", "coal (IGCC CCS)", "gas (CC CCS)"))

# plot installed capacity
installed_cap %>%
  filter(year >= 2015, year <= 2050) %>%
  ggplot(aes(x = year, y = capacity, color = scenario)) +
  geom_line() +
  facet_wrap(~technology, scales = "free_y") +
  theme_bw() +
  xlab("") + ylab("Installed capacity (GW)")

ggsave("figures/results_installed_cap.png", width = 10, height = 5, units = "in")


# elec gen by fuel/ tech -------------------------------------------------------

# get generation by tech
gen_by_tech <- rgcam::getQuery(prj_ETC_results,
                               "elec gen by gen tech and cooling tech and vintage") %>%

  separate(technology, into = c("technology", "vintage"), sep = ",") %>%
  mutate(CCS = if_else(grepl("CCS", technology), "CCS", "unabated"),
         technology = case_when(grepl("biomass", technology) ~ "biomass",
                                grepl("gas", technology) ~ "gas",
                                grepl("geothermal", technology) ~ "geothermal",
                                grepl("Gen", technology) ~ "nuclear",
                                grepl("coal", technology) ~ "coal",
                                #grepl("wind_storage", technology) ~ "wind_storage",
                                #grepl("wind_offshore", technology) ~ "wind_offshore",
                                grepl("wind", technology) ~ "wind",
                                grepl("PV", technology) ~ "PV",
                                grepl("refined", technology) ~ "refined liquids",
                                grepl("CSP", technology) ~ "CSP",
                                T ~ technology)) %>%
  group_by(scenario, region, technology, CCS,  year) %>%
  summarize(value = sum(value)) %>%
  ungroup()

gen_by_tech$scenario <-
  factor(gen_by_tech$scenario,
         levels = c("Reference_exogenous", "NZ_exogenous", "Reference_NZ_endogenous"))

gen_by_tech$technology <-
  factor(gen_by_tech$technology,
         levels = c("coal", "gas", "refined liquids", "nuclear", "geothermal",
                    "hydro", "biomass", "wind",
                    #"wind_storage", "wind_offshore",
                    "rooftop_pv", "CSP", "PV"))

# calculate diffs compared to reference
gen_by_tech_diffs <- gen_by_tech %>%
  pivot_wider(names_from = "scenario", values_from = "value") %>%
  replace_na(list(Reference_exogenous = 0, NZ_exogenous = 0, Reference_NZ_endogenous = 0)) %>%
  mutate(NZ_exogenous = NZ_exogenous - Reference_exogenous,
         Reference_NZ_endogenous = Reference_NZ_endogenous - Reference_exogenous) %>%
  pivot_longer(c(Reference_exogenous, NZ_exogenous, Reference_NZ_endogenous),
               names_to = "scenario", values_to = "value")

gen_by_tech_diffs$scenario <-
  factor(gen_by_tech_diffs$scenario,
         levels = c("Reference_exogenous", "NZ_exogenous", "Reference_NZ_endogenous"))

# calculate diffs of NZ with and without ETC
gen_by_tech_diffs2 <- gen_by_tech %>%
  filter(scenario != "Reference_exogenous") %>%
  pivot_wider(names_from = "scenario", values_from = "value") %>%
  replace_na(list(NZ_exogenous = 0, Reference_NZ_endogenous = 0)) %>%
  mutate(diff_NZ_endogenous_exogenous = Reference_NZ_endogenous - NZ_exogenous) %>%
  pivot_longer(c(NZ_exogenous, diff_NZ_endogenous_exogenous),
               names_to = "scenario", values_to = "value")

gen_by_tech_diffs2$scenario <-
  factor(gen_by_tech_diffs2$scenario,
         levels = c("NZ_exogenous", "diff_NZ_endogenous_exogenous"))

# plot generation by tech
gen_by_tech %>%
  filter(year >= 2015, year <= 2050) %>%
  ggplot(aes(x = year, y = value)) +
  ggpattern::geom_bar_pattern(aes(fill = technology, pattern = CCS),
                              stat = "identity", position = "stack",
                              color = "black", pattern_fill = "black",
                              pattern_spacing = 0.02, pattern_angle = 45) +
  ggpattern::scale_pattern_manual(values = c("CCS" = "stripe", "unabated" = "none"),
                                  guide = ggplot2::guide_legend(override.aes = list(fill = NA), order = 2),
                                  name = "") +
  facet_wrap(~scenario, nrow = 1) +
  scale_fill_manual(values = c("coal" = "lightgrey", "biomass" = "forestgreen",
                               "PV" = "#ffd500", "CSP" = "#ffe761", "rooftop_pv" = "#fff394",
                               wind = "#D2B48C",
                               jgcricolors::jgcricol()$pal_all),
                    guide = ggplot2::guide_legend(override.aes = list(pattern = "none"), order = 1)) +
  xlab("") + ylab("Electricity generation (EJ)") +
  theme_bw() +
  theme(legend.title = element_blank())

# plot diffs by tech
gen_by_tech_diffs2 %>%
  filter(year >= 2015, year <= 2050) %>%
  # mutate(scenario = if_else(grepl("NZ_exogenous", scenario), "NZ exogenous only",
  #                           "NZ exogenous z+ endogenous diff")) %>%
  ggplot(aes(x = year, y = value)) +
  ggpattern::geom_bar_pattern(aes(fill = technology, pattern = CCS),
                              stat = "identity", position = "stack",
                              color = "black", pattern_fill = "black",
                              pattern_spacing = 0.02, pattern_angle = 45) +
  ggpattern::scale_pattern_manual(values = c("CCS" = "stripe", "unabated" = "none"),
                                  guide = ggplot2::guide_legend(override.aes = list(fill = NA), order = 2),
                                  name = "") +
  facet_wrap(~scenario, nrow = 1, scales = "free") +
  scale_fill_manual(values = c("coal" = "lightgrey", "biomass" = "forestgreen",
                               "PV" = "#ffd500", "CSP" = "#ffe761", "rooftop_pv" = "#fff394",
                               "wind" = "#D2B48C", "wind_storage" = "brown",
                               "wind_offshore" = "brown4",
                               jgcricolors::jgcricol()$pal_all),
                    guide = ggplot2::guide_legend(override.aes = list(pattern = "none"), order = 1)) +
  xlab("") + ylab("Electricity generation (EJ)") +
  theme_bw() +
  theme(legend.title = element_blank())

ggsave("figures/results_elec_gen_tech_NZ_diff.png",
       width = 10, height = 5, units = "in")


# carbon prices ----------------------------------------------------------------

c_price <- rgcam::getQuery(prj_ETC_results, "CO2 prices") %>%
  filter(market == "USACO2")

c_price %>%
  filter(year >= 2020, year <= 2050) %>%
  # mutate(scenario = if_else(grepl("noLearning", scenario), "NZ exogenous only",
  #                           "NZ endogenous + exogenous")) %>%
  ggplot(aes(x = year, y = value, color = scenario)) +
  geom_line() +
  expand_limits(y = 0) +
  theme_bw() +
  xlab("") + ylab("Carbon price (1990$/tC)")

ggsave("figures/cPrice_NZ.png",
       width = 6, height = 4, units = "in")



# GHG emissions ----------------------------------------------------------------





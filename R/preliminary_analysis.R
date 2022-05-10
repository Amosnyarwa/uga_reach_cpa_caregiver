library(tidyverse)
library(srvyr)
library(janitor)
library(glue)
library(lubridate)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")
source("R/support_functions.R")

# load data ---------------------------------------------------------------

df_cleaned_with_kampala <- read_csv(file = "inputs/clean_data_caregiver.csv")
df_cleaned <- df_cleaned_with_kampala %>% 
  filter(district_name != "kampala")
df_children_perform_domestic_chores_info_with_kampala <- read_csv(file = "inputs/clean_children_perform_domestic_chores_info_data_caregiver.csv") 
df_children_perform_domestic_chores_info <- df_children_perform_domestic_chores_info_with_kampala %>% 
  filter(district_name != "kampala") 
df_protection_risky_places_with_kampala <- read_csv(file = "inputs/clean_protection_risky_places_data_caregiver.csv")
df_protection_risky_places <- df_protection_risky_places_with_kampala %>% 
  filter(district_name != "kampala")
df_children_perform_economic_labour_info_with_kampala <- read_csv(file = "inputs/clean_children_perform_economic_labour_info_data_caregiver.csv")
df_children_perform_economic_labour_info <- df_children_perform_economic_labour_info_with_kampala %>% 
  filter(district_name != "kampala")

dap <- read_csv("inputs/r_dap_caregiver.csv") %>% 
  janitor::clean_names()

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop_with_kampala <- read_csv("inputs/refugee_population_caregiver.csv")
df_ref_pop <- df_ref_pop_with_kampala %>% 
  filter(strata != "kampala")
df_host_pop <- read_csv("inputs/host_population_caregiver.csv")

# main: prepare data and create survey ------------------------------------------------

df_with_composites <- create_composite_indicators_cpa_caregiver(input_df = df_cleaned) %>% 
  mutate(strata = case_when(status == "refugee" ~ paste0(i.refugee_settlement, "_refugee"),
                            status == "host_community" ~ paste0(i.region,"_host"),
                            TRUE ~ status
  ))

# split data into host and refugee

df_ref <- df_with_composites %>% 
  filter(status == "refugee")

df_host <- df_with_composites %>% 
  filter(status == "host_community")

# create weights

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_ref, 
                                              input_refugee_pop = df_ref_pop)
df_ref_with_weights <- df_ref %>% 
  left_join(ref_weight_table, by = "strata")

# host weights
host_weight_table <- make_host_weight_table(input_df_host = df_host, 
                                            input_host_pop = df_host_pop)
df_host_with_weights <- df_host %>% 
  left_join(host_weight_table, by = "strata")

# set up design objects

ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights )
host_svy <- as_survey(.data = df_host_with_weights, strata = strata, weights = weights )

df_main_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy,
                                                           input_host_svy = host_svy,
                                                           input_dap = dap %>% filter(!variable %in% c("places_where_children_are_mostly_at_risk",
                                                                                                       "hrs_child_perfoms_domestic_chores",
                                                                                                       "hrs_child_perfoms_econ_labour")))

# # children_perform_domestic_chores: prepare data and create survey ------------------------------------------------
# 
# df_with_composites_children_perform_domestic_chores <- create_composite_indicators_cpa_caregiver_repeats(input_df = df_children_perform_domestic_chores_info)
# 
# # split data into host and refugee
# 
# df_ref_children_perform_domestic_chores <- df_with_composites_children_perform_domestic_chores %>% 
#   filter(status == "refugee")
# 
# df_host_children_perform_domestic_chores <- df_with_composites_children_perform_domestic_chores %>% 
#   filter(status == "host_community")
# 
# # attach weights
# 
# df_ref_with_weights_children_perform_domestic_chores <- df_ref_children_perform_domestic_chores %>% 
#   left_join(df_ref_with_weights %>% select(uuid, strata, weights), by = "uuid")
# 
# df_host_with_weights_children_perform_domestic_chores <- df_host_children_perform_domestic_chores %>% 
#   left_join(df_host_with_weights %>% select(uuid, strata, weights), by = "uuid")
# 
# # set up design objects
# 
# ref_svy_children_perform_domestic_chores <- as_survey(.data = df_ref_with_weights_children_perform_domestic_chores, strata = strata, weights = weights )
# host_svy_children_perform_domestic_chores <- as_survey(.data = df_host_with_weights_children_perform_domestic_chores, strata = strata, weights = weights )
# 
# 
# df_children_perform_domestic_chores_info_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy_children_perform_domestic_chores,
#                                                                                             input_host_svy = host_svy_children_perform_domestic_chores, 
#                                                                                             input_dap = dap %>% filter(variable %in% c("hrs_child_perfoms_domestic_chores")))

# protection_risky_places: prepare data and create survey ------------------------------------------------

df_with_composites_protection_risky_places <- create_composite_indicators_cpa_caregiver_repeats(input_df = df_protection_risky_places) 

# split data into host and refugee

df_ref_protection_risky_places <- df_with_composites_protection_risky_places %>% 
  filter(status == "refugee")

df_host_protection_risky_places <- df_with_composites_protection_risky_places %>% 
  filter(status == "host_community")

# set up design objects

ref_svy_protection_risky_places <- as_survey(.data = df_ref_protection_risky_places)
host_svy_protection_risky_places <- as_survey(.data = df_host_protection_risky_places)


df_protection_risky_places_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy_protection_risky_places,
                                                                              input_host_svy = host_svy_protection_risky_places,
                                                                              input_dap = dap %>% filter(variable %in% c("places_where_children_are_mostly_at_risk")))

# # children_perform_economic_labour_info: prepare data and create survey ------------------------------------------------
# 
# df_with_composites_children_perform_economic_labour_info <- create_composite_indicators_cpa_caregiver_repeats(input_df = df_children_perform_economic_labour_info)
# 
# # split data into host and refugee
# 
# df_ref_children_perform_economic_labour_info <- df_with_composites_children_perform_economic_labour_info %>% 
#   filter(status == "refugee")
# 
# df_host_children_perform_economic_labour_info <- df_with_composites_children_perform_economic_labour_info %>% 
#   filter(status == "host_community")
# 
# # attach weights
# 
# df_ref_with_weights_children_perform_economic_labour_info <- df_ref_children_perform_economic_labour_info %>% 
#   left_join(df_ref_with_weights %>% select(uuid, strata, weights), by = "uuid")
# 
# df_host_with_weights_children_perform_economic_labour_info <- df_host_children_perform_economic_labour_info %>% 
#   left_join(df_host_with_weights %>% select(uuid, strata, weights), by = "uuid")
# 
# # set up design objects
# 
# ref_svy_children_perform_economic_labour_info <- as_survey(.data = df_ref_with_weights_children_perform_economic_labour_info, strata = strata, weights = weights )
# host_svy_children_perform_economic_labour_info <- as_survey(.data = df_host_with_weights_children_perform_economic_labour_info, strata = strata, weights = weights )
# 
# 
# df_children_perform_economic_labour_info_analysis <- analysis_support_after_survey_creation(input_ref_svy = ref_svy_children_perform_economic_labour_info,
#                                                                                             input_host_svy = host_svy_children_perform_economic_labour_info, 
#                                                                                             input_dap = dap %>% filter(variable %in% c("hrs_child_perfoms_econ_labour")))
# 
# 

# analysis including kampala ----------------------------------------------
df_main_analysis_with_kampala <- analysis_support_mofification_kampala(input_df_cleaned = df_cleaned_with_kampala, 
                                                                    input_dap = dap %>% filter(!variable %in% c("places_where_children_are_mostly_at_risk",
                                                                                                                "hrs_child_perfoms_domestic_chores",
                                                                                                                "hrs_child_perfoms_econ_labour")))
df_protection_risky_places_analysis_with_kampala <- analysis_support_mofification_kampala(input_df_cleaned = df_protection_risky_places_analysis_with_kampala,
                                                                              input_dap = dap %>% filter(variable %in% c("places_where_children_are_mostly_at_risk")))



# merge analysis ----------------------------------------------------------

full_analysis_long <- bind_rows(df_main_analysis,
                                df_children_perform_domestic_chores_info_analysis,
                                df_protection_risky_places_analysis,
                                df_children_perform_economic_labour_info_analysis)
end <- Sys.time()

print(paste("Time taken to run the script: ", end - start))

full_analysis_long %>%
  write_csv(paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_caregiver.csv"), na="")

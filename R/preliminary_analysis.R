library(tidyverse)
library(srvyr)
library(janitor)
library(glue)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")
source("R/support_functions.R")

# load data ---------------------------------------------------------------

df_cleaned <- read_csv("inputs/clean_main_data_caregiver.csv")
df_children_perform_domestic_chores_info = read_csv(path = "inputs/clean_children_perform_domestic_chores_info_data_caregiver.csv") 
df_protection_risky_places = read_csv(path = "inputs/clean_protection_risky_places_data_caregiver.csv")
df_children_perform_economic_labour_info = read_csv(path = "inputs/clean_children_perform_economic_labour_info_data_caregiver.csv")

dap <- read_csv("inputs/r_dap_caregiver.csv") %>% 
  janitor::clean_names()

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop <- read_csv("inputs/refugee_population_caregiver.csv")
df_host_pop <- read_csv("inputs/host_population_caregiver.csv")

# analyse datasets --------------------------------------------------------

df_main_analysis <- analysis_support(input_df_cleaned = df_cleaned,
                                     input_df_ref_pop = df_ref_pop,
                                     input_df_host_pop = df_host_pop,
                                     input_dap = dap %>% filter(!variable %in% c("places_where_children_are_mostly_at_risk",
                                                                                 "hrs_child_perfoms_domestic_chores",
                                                                                 "hrs_child_perfoms_econ_labour")))

df_children_perform_domestic_chores_info_analysis <- analysis_support(input_df_cleaned = df_children_perform_domestic_chores_info,
                                                                      input_df_ref_pop = df_ref_pop,
                                                                      input_df_host_pop = df_host_pop,
                                                                      input_dap = dap %>% filter(variable %in% c("hrs_child_perfoms_domestic_chores")))

df_protection_risky_places_analysis <- analysis_support(input_df_cleaned = df_protection_risky_places,
                                                        input_df_ref_pop = df_ref_pop,
                                                        input_df_host_pop = df_host_pop,
                                                        input_dap = dap %>% filter(variable %in% c("places_where_children_are_mostly_at_risk")))

df_children_perform_economic_labour_info_analysis <- analysis_support(input_df_cleaned = df_children_perform_economic_labour_info,
                                                                      input_df_ref_pop = df_ref_pop,
                                                                      input_df_host_pop = df_host_pop,
                                                                      input_dap = dap %>% filter(variable %in% c("hrs_child_perfoms_econ_labour")))

# merge analysis ----------------------------------------------------------

full_analysis_long <- bind_rows(df_main_analysis,
                                df_children_perform_domestic_chores_info_analysis,
                                df_protection_risky_places_analysis,
                                df_children_perform_economic_labour_info_analysis)
end <- Sys.time()

print(paste("Time taken to run the script: ", end - start))

full_analysis_long %>%
  write_csv(paste0("outputs/", butteR::date_file_prefix(), "_full_analysis_lf_caregiver.csv"), na="")
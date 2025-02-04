# checks for data collection

library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")


# read data ---------------------------------------------------------------

df_tool_data <- readxl::read_excel("inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = district_name,
         i.check.point_number = point_number,
         start = as_datetime(start),
         end = as_datetime(end)) %>% 
  filter(consent_two == "yes", respondent_age >= 12, i.check.start_date > as_date("2022-01-30"), 
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE))
  ) %>% 
  select(-c(starts_with("...21")), -c("end_note":"children_category_facing_difficulty_accessing_social_activities_other"))
  

# repeats
children_perform_domestic_chores_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "children_perform_domestic_ch...") 

protection_risky_places = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "protection_risky_places")

children_perform_economic_labour_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "children_perform_economic_la...")

df_tool_data_children_perform_domestic_chores_info <- df_tool_data %>% 
  right_join(children_perform_domestic_chores_info, by = c("_uuid" = "_submission__uuid") ) %>% 
  filter(!is.na(`_uuid`)) 

df_tool_data_protection_risky_places <- df_tool_data %>% 
  right_join(protection_risky_places, by = c("_uuid" = "_submission__uuid") ) %>% 
  filter(!is.na(`_uuid`))

df_tool_data_children_perform_economic_labour_info <- df_tool_data %>% 
  right_join(children_perform_economic_labour_info, by = c("_uuid" = "_submission__uuid") ) %>% 
  filter(!is.na(`_uuid`))

# tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "choices")

# GIS layer
df_sample_data <- sf::st_read("inputs/cpa_caregiver_settlement_host_samples.gpkg", quiet = TRUE)

# output holder -----------------------------------------------------------

logic_output <- list()

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  check_duplicates_by_uuid(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_uuid")

# check outliers ---------------------------------------------------
# check respondent_age
df_c_outliers_respondent_age <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "respondent_age", 
                                                  input_lower_limit = quantile(df_tool_data$respondent_age, 0.01, na.rm = TRUE),
                                                  input_upper_limit = quantile(df_tool_data$respondent_age, 0.99, na.rm = TRUE))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_outliers_respondent_age")

# check hh_size
df_c_outliers_hh_size <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "hh_size", 
                                                  input_lower_limit = quantile(df_tool_data$hh_size, 0.01, na.rm = TRUE),
                                                  input_upper_limit = quantile(df_tool_data$hh_size, 0.99, na.rm = TRUE))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_outliers_hh_size")

# check household size children
df_c_outliers_hh_size_children <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "hh_size_children", 
                                                  input_lower_limit = quantile(df_tool_data$hh_size_children, 0.01, na.rm = TRUE),
                                                  input_upper_limit = quantile(df_tool_data$hh_size_children, 0.99, na.rm = TRUE))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_outliers_hh_size_children")

# check children_provide_kinship_care
df_c_outliers_children_provide_kinship_care <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "children_provide_kinship_care",
                                                  input_lower_limit = quantile(df_tool_data$children_provide_kinship_care, 0.01, na.rm = TRUE),
                                                  input_upper_limit = quantile(df_tool_data$children_provide_kinship_care, 0.99, na.rm = TRUE))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_outliers_children_provide_kinship_care")

# check children_provide_foster
df_c_outliers_children_provide_foster <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "children_provide_foster",
                                                  input_lower_limit = quantile(df_tool_data$children_provide_foster, 0.01, na.rm = TRUE),
                                                  input_upper_limit = quantile(df_tool_data$children_provide_foster, 0.99, na.rm = TRUE))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_outliers_children_provide_foster")

# check hrs_child_perfoms_domestic_chores
df_c_outliers_hrs_child_perfoms_domestic_chores <-  check_outliers_repeats(input_tool_data = df_tool_data_children_perform_domestic_chores_info,
                                                  input_column = "hrs_child_perfoms_domestic_chores",
                                                  input_lower_limit = quantile(df_tool_data_children_perform_domestic_chores_info$hrs_child_perfoms_domestic_chores, 0.025, na.rm = TRUE),
                                                  input_upper_limit = quantile(df_tool_data_children_perform_domestic_chores_info$hrs_child_perfoms_domestic_chores, 0.975, na.rm = TRUE),
                                                  input_sheet_name = "children_perform_domestic_chores_info")

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_outliers_hrs_child_perfoms_domestic_chores")

# check hrs_child_perfoms_econ_labour
df_c_outliers_hrs_child_perfoms_econ_labour <-  check_outliers_repeats(input_tool_data = df_tool_data_children_perform_economic_labour_info,
                                                  input_column = "hrs_child_perfoms_econ_labour",
                                                  input_lower_limit = quantile(df_tool_data_children_perform_economic_labour_info$hrs_child_perfoms_econ_labour, 0.025, na.rm = TRUE),
                                                  input_upper_limit = quantile(df_tool_data_children_perform_economic_labour_info$hrs_child_perfoms_econ_labour, 0.975, na.rm = TRUE),
                                                  input_sheet_name = "children_perform_economic_labour_info")

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_outliers_hrs_child_perfoms_econ_labour")

# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 25
max_time_of_survey <- 120

df_c_survey_time <-  check_survey_time(input_tool_data = df_tool_data, 
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_survey_time")

# check the time between surveys
min_time_btn_surveys <- 5

df_c_time_btn_survey <- check_time_interval_btn_surveys(input_tool_data = df_tool_data,
                                                        input_min_time = min_time_btn_surveys)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_time_btn_survey")

# Logical checks ----------------------------------------------------------
# check_shortest_path
df_c_short_path <- check_shortest_path(input_tool_data = df_tool_data)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_short_path")


# spatial checks ----------------------------------------------------------

sample_pt_nos <- df_sample_data %>% 
  mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
  pull(unique_pt_number) %>% 
  unique()

# duplicate point numbers
df_c_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data %>% filter(district_name != "kampala"),
                                                    input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_duplicate_pt_nos")

# pt id does not exist in sample
df_c_pt_not_in_sample <- check_pt_number_not_in_samples(input_tool_data = df_tool_data %>% filter(district_name != "kampala"), 
                                                        input_sample_pt_nos_list = sample_pt_nos)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_pt_not_in_sample")

# check for exceeded threshold distance

threshold_dist <- 150

df_c_greater_thresh_distance <- check_threshold_distance(input_sample_data = df_sample_data,
                                                         input_tool_data = df_tool_data %>% filter(!district_name %in% c("kampala")),
                                                         input_threshold_dist = threshold_dist)

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_c_greater_thresh_distance")

# combined logical checks ----------------------------------------------------------

df_logic_checks <- bind_rows(logic_output)

# others checks

df_others_data <- extract_other_data(input_tool_data = df_tool_data, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)

df_others_data_repeats <- extract_other_data_repeats(input_repeat_data = df_tool_data_protection_risky_places, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices, 
                                     input_sheet_name = "protection_risky_places",
                                     input_repeat_cols = c("places_where_children_are_mostly_at_risk"))

# combine logic and others checks
df_combined_checks <- bind_rows(df_logic_checks, df_others_data, df_others_data_repeats)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_caregiver.csv"), na = "")

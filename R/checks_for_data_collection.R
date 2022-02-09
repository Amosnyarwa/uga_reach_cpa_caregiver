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
  )

df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/cpa_caregiver_settlement_host_samples.gpkg", quiet = TRUE)

# output holder -----------------------------------------------------------

logic_output <- list()

# check duplicate uuids ---------------------------------------------------

df_c_duplicate_uuid <-  check_duplicates_by_uuid(input_tool_data = df_tool_data)

if(exists("df_c_duplicate_uuid")){
  if(nrow(df_c_duplicate_uuid) > 0){
    logic_output$df_c_duplicate_uuid <- df_c_duplicate_uuid
  }
}



# check outliers ---------------------------------------------------
# check respondent_age
df_c_outliers_respondent_age <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "respondent_age", 
                                                  input_lower_limit = quantile(df_tool_data$respondent_age, 0.025),
                                                  input_upper_limit = quantile(df_tool_data$respondent_age, 0.97))
if(exists("df_c_outliers_respondent_age")){
  if(nrow(df_c_outliers_respondent_age) > 0){
    logic_output$df_c_outliers_respondent_age <- df_c_outliers_respondent_age
  }
}
# check hh_size
df_c_outliers_hh_size <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "hh_size", 
                                                  input_lower_limit = quantile(df_tool_data$hh_size, 0.025),
                                                  input_upper_limit = quantile(df_tool_data$hh_size, 0.97))
if(exists("df_c_outliers_hh_size")){
  if(nrow(df_c_outliers_hh_size) > 0){
    logic_output$df_c_outliers_hh_size <- df_c_outliers_hh_size
  }
}
# check household size children
df_c_outliers_hh_size_children <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "hh_size_children", 
                                                  input_lower_limit = quantile(df_tool_data$hh_size_children, 0.025),
                                                  input_upper_limit = quantile(df_tool_data$hh_size_children, 0.97))
if(exists("df_c_outliers_hh_size_children")){
  if(nrow(df_c_outliers_hh_size_children) > 0){
    logic_output$df_c_outliers_hh_size_children <- df_c_outliers_hh_size_children
  }
}
# check children_provide_kinship_care
df_c_outliers_children_provide_kinship_care <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "children_provide_kinship_care", 
                                                  input_lower_limit = quantile(df_tool_data$children_provide_kinship_care, 0.025),
                                                  input_upper_limit = quantile(df_tool_data$children_provide_kinship_care, 0.97))
if(exists("df_c_outliers_children_provide_kinship_care")){
  if(nrow(df_c_outliers_children_provide_kinship_care) > 0){
    logic_output$df_c_outliers_children_provide_kinship_care <- df_c_outliers_children_provide_kinship_care
  }
}
# check children_provide_foster
df_c_outliers_children_provide_foster <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "children_provide_foster", 
                                                  input_lower_limit = quantile(df_tool_data$children_provide_foster, 0.025),
                                                  input_upper_limit = quantile(df_tool_data$children_provide_foster, 0.97))
if(exists("df_c_outliers_children_provide_foster")){
  if(nrow(df_c_outliers_children_provide_foster) > 0){
    logic_output$df_c_outliers_children_provide_foster <- df_c_outliers_children_provide_foster
  }
}
# check hrs_child_perfoms_domestic_chores
df_c_outliers_hrs_child_perfoms_domestic_chores <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "hrs_child_perfoms_domestic_chores", 
                                                  input_lower_limit = quantile(df_tool_data$hrs_child_perfoms_domestic_chores, 0.025),
                                                  input_upper_limit = quantile(df_tool_data$hrs_child_perfoms_domestic_chores, 0.97))
if(exists("df_c_outliers_hrs_child_perfoms_domestic_chores")){
  if(nrow(df_c_outliers_hrs_child_perfoms_domestic_chores) > 0){
    logic_output$df_c_outliers_hrs_child_perfoms_domestic_chores <- df_c_outliers_hrs_child_perfoms_domestic_chores
  }
}
# check hrs_child_perfoms_econ_labour
df_c_outliers_hrs_child_perfoms_econ_labour <-  check_outliers(input_tool_data = df_tool_data,
                                                  input_column = "hrs_child_perfoms_econ_labour", 
                                                  input_lower_limit = quantile(df_tool_data$hrs_child_perfoms_econ_labour, 0.025),
                                                  input_upper_limit = quantile(df_tool_data$hrs_child_perfoms_econ_labour, 0.97))
if(exists("df_c_outliers_hrs_child_perfoms_econ_labour")){
  if(nrow(df_c_outliers_hrs_child_perfoms_econ_labour) > 0){
    logic_output$df_c_outliers_hrs_child_perfoms_econ_labour <- df_c_outliers_hrs_child_perfoms_econ_labour
  }
}
# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 20
max_time_of_survey <- 120

df_c_survey_time <-  check_survey_time(input_tool_data = df_tool_data, 
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

if(exists("df_c_survey_time")){
  if(nrow(df_c_survey_time) > 0){
    logic_output$df_c_survey_time <- df_c_survey_time
  }
}

# check the time between surveys
min_time_btn_surveys <- 5

df_c_time_btn_survey <- check_time_interval_btn_surveys(input_tool_data = df_tool_data,
                                                        input_min_time = min_time_btn_surveys)

if(exists("df_c_time_btn_survey")){
  if(nrow(df_c_time_btn_survey) > 0){
    logic_output$df_c_time_btn_survey <- df_c_time_btn_survey
  }
}

# Logical checks ----------------------------------------------------------


# spatial checks ----------------------------------------------------------

sample_pt_nos <- df_sample_data %>% 
  mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
  pull(unique_pt_number) %>% 
  unique()

# duplicate point numbers
df_c_duplicate_pt_nos <- check_duplicate_pt_numbers(input_tool_data = df_tool_data %>% filter(district_name != "kampala"),
                                                    input_sample_pt_nos_list = sample_pt_nos)

if(exists("df_c_duplicate_pt_nos")){
  if(nrow(df_c_duplicate_pt_nos) > 0){
    logic_output$df_c_duplicate_pt_nos <- df_c_duplicate_pt_nos
  }
}

# pt id does not exist in sample
df_c_pt_not_in_sample <- check_pt_number_not_in_samples(input_tool_data = df_tool_data %>% filter(district_name != "kampala"), 
                                                        input_sample_pt_nos_list = sample_pt_nos)

if(exists("df_c_pt_not_in_sample")){
  if(nrow(df_c_pt_not_in_sample) > 0){
    logic_output$df_c_pt_not_in_sample <- df_c_pt_not_in_sample
  }
}

# check for exceeded threshold distance

threshold_dist <- 150

df_c_greater_thresh_distance <- check_threshold_distance(input_sample_data = df_sample_data,
                                                         input_tool_data = df_tool_data %>% filter(!district_name %in% c("kampala")),
                                                         input_threshold_dist = threshold_dist)

if(exists("df_c_greater_thresh_distance")){
  if(nrow(df_c_greater_thresh_distance) > 0){
    logic_output$df_c_greater_thresh_distance <- df_c_greater_thresh_distance
  }
}

# combined logical checks ----------------------------------------------------------

df_logic_checks <- bind_rows(logic_output)

# others checks

df_others_data <- extract_other_data(input_tool_data = df_tool_data, 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)

# combine logic and others checks
df_combined_checks <- bind_rows(df_logic_checks, df_others_data)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_checks_caregiver.csv"), na = "")

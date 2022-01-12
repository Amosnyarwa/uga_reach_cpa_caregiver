# checks for data collection

library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# read data 
df_tool_data <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool_Jan2022.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = district_name,
         i.check.point_number = point_number,
         start = as_datetime(start),
         end = as_datetime(end)) %>% 
  filter(consent == "yes", respondent_age >= 18, i.check.start_date > as_date("2022-01-17"), 
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE))
  )

df_survey <- readxl::read_excel("inputs/Child_Protection_Caregiver_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Caregiver_Tool.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/cpa_caregiver_settlement_host_samples.gpkg", quiet = TRUE)

# output holder -----------------------------------------------------------

logic_output <- list()

# Time checks -------------------------------------------------------------

# Time interval for the survey
min_time_of_survey <- 30
max_time_of_survey <- 120

df_c_survey_time <-  check_survey_time(input_df = df_tool_data, 
                                       input_min_time = min_time_of_survey, 
                                       input_max_time = max_time_of_survey)

if(exists("df_c_survey_time")){
  if(nrow(df_c_survey_time) > 0){
    logic_output$df_c_survey_time <- df_c_survey_time
  }
}

# check the time between surveys
min_time_btn_surveys <- 5

df_c_time_btn_survey <- check_time_interval_btn_surveys(input_df = df_tool_data,
                                                        input_min_time = min_time_btn_surveys)

if(exists("df_c_time_btn_survey")){
  if(nrow(df_c_time_btn_survey) > 0){
    logic_output$df_c_time_btn_survey <- df_c_time_btn_survey
  }
}

# Logical checks ----------------------------------------------------------


# spatial checks ----------------------------------------------------------




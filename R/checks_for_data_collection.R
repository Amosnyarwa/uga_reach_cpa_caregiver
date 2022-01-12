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
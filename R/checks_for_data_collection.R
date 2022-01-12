# checks for data collection

library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# read data 
df_tool_data <- readxl::read_excel("inputs/Child_Protection_Caregiver_Assessment_HH_Tool_Jan2022.xlsx") %>% 
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
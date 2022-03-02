library(tidyverse)
library(lubridate)
library(glue)


# read data ---------------------------------------------------------------

# tool data
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
# cleaning log
df_cleaning_log <- read_csv("inputs/combined_checks_caregiver.csv") %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log)) %>%
  filter(adjust_log != "delete_log", !is.na(value), !is.na(uuid)) %>% 
  mutate(sheet = NA, index = NA, relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)
# survey tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "choices")

# find all new choices to add to choices sheet ----------------------------

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))
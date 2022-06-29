library(tidyverse)
library(janitor)
library(lubridate)

# read data ---------------------------------------------------------------

# clean data
data_file <- "inputs/clean_data_caregiver.xlsx"

data_nms <- names(readxl::read_excel(path = data_file, sheet = "UGA2109_Cross-Sectoral Child...", n_max = 200))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_clean_data <- readxl::read_excel(path = data_file, sheet = "UGA2109_Cross-Sectoral Child...", col_types = c_types)
df_clean_data_protection_risky_places <- readxl::read_excel(path = data_file, sheet = "protection_risky_places")

# read analysis output 
df_analysis_output <- readr::read_csv("inputs/full_analysis_lf_caregiver.csv")

# tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "choices")


# feedback ----------------------------------------------------------------

df_feedback_data <- df_clean_data %>% 
  filter(interview_feedback %in% c("yes")) %>% 
  select(interview_feedback:call_back_note)

openxlsx::write.xlsx(x = df_feedback_data,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_ACTED_feedback_caregiver.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

# prepare data for publishing ---------------------------------------------

# main

df_prepared_data_main <- df_clean_data %>% 
  select(-c("interview_feedback":"call_back_note"))

df_prepared_data_protection_risky_places <- df_clean_data_protection_risky_places %>% 
  select(-c("start":"nationality_other"))

# writing output to excel

list_of_prepared_datasets <- list("UGA2109_Cross-Sectoral Child..." = df_prepared_data_main,
                               "protection_risky_places" = df_prepared_data_protection_risky_places)

openxlsx::write.xlsx(x = list_of_prepared_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_UGA2109 - Cross-sectoral child protection assessment_caregiver_data.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")


# prepare analysis for publishing -----------------------------------------

# - questions, indicators, results in percent form, disaggregations

df_prepare_survey <- df_survey %>% 
  select(type, name, label) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
  filter(select_type %in% c("integer", "date", "text", "calculate", "select_one", "select_multiple"))

df_prepare_analysis_output <- df_analysis_output %>% 
  mutate(variable = ifelse(is.na(variable), variable_val, variable),
         variable = ifelse(variable == "i.education_level", "hh_member_education", variable),
         int.variable = ifelse(str_detect(string = variable, pattern = "^i\\."), str_replace(string = variable, pattern = "^i\\.", replacement = ""), variable)) %>% 
  left_join(df_prepare_survey, by = c("int.variable" = "name")) %>% 
  relocate(label, .after = variable) %>% 
  mutate(label = ifelse(is.na(label), variable, label),
         `mean/pct` = ifelse(select_type %in% c("integer"), `mean/pct`, `mean/pct`*100),
         `mean/pct` = round(`mean/pct`, digits = 2)) %>% 
  select(`Question`= label, `choices/options` = variable_val, `Results(mean/percentage)` = `mean/pct`, population, subset_1_name, subset_1_val)


openxlsx::write.xlsx(x = df_prepare_analysis_output,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_UGA2109 - Cross-sectoral child protection assessment_caregiver_analysis.xlsx"), 
                     overwrite = TRUE, keepNA = TRUE, na.string = "NA")

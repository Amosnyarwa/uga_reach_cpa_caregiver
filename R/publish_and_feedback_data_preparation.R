library(tidyverse)
library(janitor)
library(lubridate)

# read data ---------------------------------------------------------------

data_file <- "inputs/clean_data_caregiver.xlsx"

data_nms <- names(readxl::read_excel(path = data_file, sheet = "UGA2109_Cross-Sectoral Child...", n_max = 200))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_clean_data <- readxl::read_excel(path = data_file, sheet = "UGA2109_Cross-Sectoral Child...", col_types = c_types)
df_clean_data_protection_risky_places <- readxl::read_excel(path = data_file, sheet = "protection_risky_places")


# feedback ----------------------------------------------------------------

df_feedback_data <- df_clean_data %>% 
  filter(interview_feedback %in% c("yes")) %>% 
  select(interview_feedback:call_back_note)

openxlsx::write.xlsx(x = df_feedback_data,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_ACTED_feedback.xlsx"), 
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



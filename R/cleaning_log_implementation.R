library(tidyverse)
library(lubridate)
library(glue)

source("R/support_functions.R")

# read data ---------------------------------------------------------------

# tool data
cols_from_main_dataset <- c("start",   "end", "today", "instruction_note", "consent_one", "consent_two", "district_name", "enumerator_id", "point_number", "status", 
                            "refugee_settlement", "refugee_settlement_zone", "sub_county_div", "hoh_equivalent", "primary_caregiver", "responent_sex", "respondent_age", "nationality", "nationality_other",    "_id",   "uuid",  "index")

# sheets
data_cols_to_remove <- c("_parent_table_name",	"_submission__id",	"_submission__submission_time",	"_submission__validation_status", 
                         "_submission__notes",	"_submission__status",	"_submission__submitted_by",	"_submission__tags")

children_perform_domestic_chores_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "children_perform_domestic_ch...") %>% 
  # filter(!is.na(age_gender_breakdown_domestic_chores_post)) %>% 
  select(-all_of(data_cols_to_remove)) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

protection_risky_places = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "protection_risky_places") %>% 
  # filter(!is.na(risky_protection_places_post)) %>%
  select(-all_of(data_cols_to_remove)) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

children_perform_economic_labour_info = readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "children_perform_economic_la...") %>% 
  # filter(!is.na(age_gender_breakdown_econ_labour_post)) %>%
  select(-all_of(data_cols_to_remove)) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .)))

# main dataset
data_nms <- names(readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "UGA2109_Cross-Sectoral Child...", n_max = 100))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")

df_raw_data <- readxl::read_excel(path = "inputs/UGA2109_Cross_Sectoral_Child_Protection_Assessment_Caregiver_Data.xlsx", sheet = "UGA2109_Cross-Sectoral Child...", col_types = c_types) %>%
  filter(consent_two == "yes", respondent_age >= 12, as_date(as_datetime(start)) > as_date("2022-01-30"), 
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE))  ) %>% 
  rename(`if_selected_ngo_or_un_agency/medecins_sans_frontieres` = `if_selected_ngo_or_un_agency/médecins_sans_frontières`,
         `causes_of_stress_among_caregivers/childrens_safety` = `causes_of_stress_among_caregivers/children’s_safety`,
         `action_child_takes_when_told_to_do_harsh_work/i_tell_the_person_i_wont_do_it` = `action_child_takes_when_told_to_do_harsh_work/i_tell_the_person_i_won't_do_it`  ) %>% 
  mutate(if_selected_ngo_or_un_agency = str_replace(string = if_selected_ngo_or_un_agency, pattern = "médecins_sans_frontières", replacement = "medecins_sans_frontieres"),
         causes_of_stress_among_caregivers = str_replace(string = causes_of_stress_among_caregivers, pattern = "children’s_safety", replacement = "childrens_safety"),
         action_child_takes_when_told_to_do_harsh_work = str_replace(string = action_child_takes_when_told_to_do_harsh_work, pattern = "i_tell_the_person_i_won't_do_it", replacement = "i_tell_the_person_i_wont_do_it")  ) %>% 
  mutate(refugee_settlement = ifelse(district_name == "kampala" & status == "refugee", district_name, refugee_settlement),
         refugee_settlement_zone = ifelse(district_name == "kampala" & status == "refugee", sub_county_div, refugee_settlement_zone)  ) %>% 
  select(-c(starts_with("...21")), -c("end_note":"children_category_facing_difficulty_accessing_social_activities_other")) %>% 
  mutate(across(.cols = everything(), .fns = ~ifelse(str_detect(string = ., pattern = fixed(pattern = "N/A", ignore_case = TRUE)), "NA", .))) %>% 
  mutate(start = as_datetime(start), end = as_datetime(end), today = as_date(as_datetime(today)), date_arrival = as_date(as_datetime(date_arrival)))

df_raw_data_children_perform_domestic_chores_info <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(children_perform_domestic_chores_info, by = c("_uuid" = "_submission__uuid") ) 

df_raw_data_protection_risky_places <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(protection_risky_places, by = c("_uuid" = "_submission__uuid") ) 

df_raw_data_children_perform_economic_labour_info <- df_raw_data %>% 
  select(-`_index`) %>% 
  inner_join(children_perform_economic_labour_info, by = c("_uuid" = "_submission__uuid") )

# cleaning log
df_cleaning_log <- read_csv("inputs/combined_checks_caregiver.csv", col_types = cols(sheet = "c", index = "i")) %>% 
  mutate(adjust_log = ifelse(is.na(adjust_log), "apply_suggested_change", adjust_log),
         value = ifelse(is.na(value) & comment == "implement_logical_change", "blank", value),
         value = ifelse(is.na(value) & issue_id %in% c("logic_c_outlier"), "blank", value),
         value = ifelse(is.na(value) & type == "remove_survey", "blank", value)) %>%
  filter(adjust_log != "delete_log", !is.na(value), !is.na(uuid)) %>% 
  mutate(value = ifelse(value == "blank" & comment == "implement_logical_change", NA, value),
         relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)

# survey tool
df_survey <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/Child_Protection_Assessment_Caregiver_Tool.xlsx", sheet = "choices") %>% 
  mutate( name = str_replace(string = name, pattern = "médecins_sans_frontières", replacement = "medecins_sans_frontieres"),
          name = str_replace(string = name, pattern = "children’s_safety", replacement = "childrens_safety"),
          name = str_replace(string = name, pattern = "i_tell_the_person_i_won't_do_it", replacement = "i_tell_the_person_i_wont_do_it") )

# handle datasets ---------------------------------------------------------

# main dataset
df_cleaned_data <- implement_cleaning_support(input_df_raw_data = df_raw_data, 
                                              input_df_survey = df_survey, 
                                              input_df_choices = df_choices, 
                                              input_df_cleaning_log = df_cleaning_log %>% filter(name %in% colnames(df_raw_data)))

write_csv(df_cleaned_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_data_caregiver.csv"))

# children_perform_domestic_chores_info
df_cleaning_log_children_perform_domestic_chores_info <- df_cleaning_log %>% 
  filter(uuid %in% df_raw_data_children_perform_domestic_chores_info$`_uuid`, name %in% colnames(df_raw_data_children_perform_domestic_chores_info))

df_cleaned_children_perform_domestic_chores_info_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_children_perform_domestic_chores_info, 
                                                                                    input_df_survey = df_survey, 
                                                                                    input_df_choices = df_choices, 
                                                                                    input_df_cleaning_log = df_cleaning_log_children_perform_domestic_chores_info) %>% 
  select(cols_from_main_dataset, any_of(colnames(children_perform_domestic_chores_info)))

write_csv(df_cleaned_children_perform_domestic_chores_info_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_children_perform_domestic_chores_info_data_caregiver.csv"))

# protection_risky_places
df_cleaning_log_protection_risky_places <- df_cleaning_log %>% 
  filter(uuid %in% df_raw_data_protection_risky_places$`_uuid`, name %in% colnames(df_raw_data_protection_risky_places))

df_cleaned_protection_risky_places_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_protection_risky_places, 
                                                                      input_df_survey = df_survey, 
                                                                      input_df_choices = df_choices, 
                                                                      input_df_cleaning_log = df_cleaning_log_protection_risky_places) %>% 
  select(cols_from_main_dataset, any_of(colnames(protection_risky_places)))

write_csv(df_cleaned_protection_risky_places_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_protection_risky_places_data_caregiver.csv"))

# children_perform_economic_labour_info
df_cleaning_log_children_perform_economic_labour_info <- df_cleaning_log %>% 
  filter(uuid %in% df_raw_data_children_perform_economic_labour_info$`_uuid`, name %in% colnames(df_raw_data_children_perform_economic_labour_info))

df_cleaned_children_perform_economic_labour_info_data <- implement_cleaning_support(input_df_raw_data = df_raw_data_children_perform_economic_labour_info, 
                                                                                    input_df_survey = df_survey, 
                                                                                    input_df_choices = df_choices, 
                                                                                    input_df_cleaning_log = df_cleaning_log_children_perform_economic_labour_info) %>% 
  select(cols_from_main_dataset, any_of(colnames(children_perform_economic_labour_info)))

write_csv(df_cleaned_children_perform_economic_labour_info_data, file = paste0("outputs/", butteR::date_file_prefix(), "_clean_children_perform_economic_labour_info_data_caregiver.csv"))


list_of_clean_datasets <- list("UGA2109_Cross-Sectoral Child..." = df_cleaned_data,
                               "children_perform_domestic_ch..." = df_cleaned_children_perform_domestic_chores_info_data,
                               "protection_risky_places" = df_cleaned_protection_risky_places_data,
                               "children_perform_economic_la..." = df_cleaned_children_perform_economic_labour_info_data
)

openxlsx::write.xlsx(x = list_of_clean_datasets,
                     file = paste0("outputs/", butteR::date_file_prefix(), 
                                   "_clean_data_caregiver.xlsx"), 
                     overwrite = TRUE)
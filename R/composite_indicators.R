# function for creating composite indicators

create_composite_indicators_cpa_caregiver <- function(input_df) {
  input_df %>% 
    mutate(
      i.refugee_settlement = case_when(district_name == "adjumani" & status == "refugee" ~ "adjumani", 
                                       district_name == "kampala" & status == "refugee" ~ "kampala",
                                       refugee_settlement == "rhino" ~ "rhino_camp",
                                       TRUE ~ refugee_settlement),
      i.nationality = case_when(nationality %in% c("south_sudan", "drc", "rwanda", "burundi") & status == "refugee" ~ nationality,
                                nationality %in% c("other", "somalia", "sudan", "eritrea") & status == "refugee" ~ "other",
                           TRUE ~ nationality),
      i.region = case_when(district_name %in% c("kampala") ~ "central",
                           district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa") ~ "south_west",
                           TRUE ~ "west_nile"),
      i.location_type = case_when(district_name %in% c("kampala") ~ "urban",
                                  TRUE ~ "rural"),
      i.respondent_age = case_when(respondent_age <= 18 ~ "age_12_18",
                                   respondent_age <= 59 ~ "age_19_59",
                                   respondent_age > 59 ~ "age_greater_59",
                                   TRUE ~ "NA"),
      i.education_level = case_when(hh_member_education %in% c("no_formal_education") ~ "none",
                                    hh_member_education %in% c("completed_primary", "incomplete_primary", "incomplete_secondary") ~ "low",
                                    hh_member_education %in% c("completed_secondary", "incomplete_university", "incomplete_prof_degree", 
                                                               "incomplete_voc_training", "completed_voc_training", "incomplete_tertiary") ~ "middle",
                                    hh_member_education %in% c("completed_tertiary", "completed_university", "completed_prof_degree") ~ "higher",
                                    hh_member_education %in% c("other") ~ "other",
                                    TRUE ~ hh_member_education
                                    ),
      int.date_arrival_interval = interval(as_date(date_arrival), today),
      int.length_since_date_arrival = time_length(int.date_arrival_interval, "year"),
      i.date_arrival = case_when(int.length_since_date_arrival <= 02.5 ~ "last_3_months",
                                 int.length_since_date_arrival <= 0.5 ~ "3_and_6_month_ago",
                                 int.length_since_date_arrival <= 1 ~ "6_month_1_yr_ago",
                                 int.length_since_date_arrival <= 5 ~ "1_and_5_yrs_ago",
                                 int.length_since_date_arrival <= 10 ~ "5_and_10_yrs_ago",
                                 int.length_since_date_arrival > 10 ~ "greater_10_yrs_ago",
                                 TRUE ~ "NA"
                                 )
    )
}

create_composite_indicators_cpa_caregiver_repeats <- function(input_df) {
  input_df %>% 
    mutate(
      i.refugee_settlement = case_when(district_name == "adjumani" & status == "refugee" ~ "adjumani", 
                                       district_name == "kampala" & status == "refugee" ~ "kampala",
                                       refugee_settlement == "rhino" ~ "rhino_camp",
                                       TRUE ~ refugee_settlement),
      i.nationality = case_when(nationality %in% c("south_sudan", "drc", "rwanda", "burundi") & status == "refugee" ~ nationality,
                                nationality %in% c("other", "somalia", "sudan", "eritrea") & status == "refugee" ~ "other",
                                TRUE ~ nationality),
      i.region = case_when(district_name %in% c("kampala") ~ "central",
                           district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa") ~ "south_west",
                           TRUE ~ "west_nile"),
      i.location_type = case_when(district_name %in% c("kampala") ~ "urban",
                                  TRUE ~ "rural"),
      i.respondent_age = case_when(respondent_age <= 18 ~ "age_12_18",
                                   respondent_age <= 59 ~ "age_19_59",
                                   respondent_age > 59 ~ "age_greater_59",
                                   TRUE ~ "NA")
    )
}
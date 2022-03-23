# function for creating composite indicators

create_composite_indicators_cpa_caregiver <- function(input_df) {
  input_df %>% 
    mutate(
      i.refugee_settlement = case_when(district_name == "adjumani" & status == "refugee" ~ "adjumani", 
                                       district_name == "kampala" & status == "refugee" ~ "kampala",
                                       refugee_settlement == "rhino" ~ "rhino_camp",
                                       TRUE ~ refugee_settlement),
      i.region = case_when(district_name %in% c("kampala") ~ "central",
                           district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa") ~ "south_west",
                           TRUE ~ "west_nile"),
      i.location_type = case_when(district_name %in% c("kampala") ~ "urban",
                                  TRUE ~ "rural"),
      i.respondent_age = case_when(respondent_age <= 18 ~ "age_12_18",
                                   respondent_age <= 59 ~ "age_19_59",
                                   TRUE ~ "age_greater_59"),
      i.education_level = case_when(hh_member_education %in% c("no_formal_education") ~ "none",
                                    hh_member_education %in% c("completed_primary", "incomplete_primary", "incomplete_secondary") ~ "low",
                                    hh_member_education %in% c("completed_secondary", "incomplete_university", "incomplete_prof_degree", 
                                                               "incomplete_voc_training", "completed_voc_training", "incomplete_tertiary") ~ "middle",
                                    hh_member_education %in% c("completed_tertiary", "completed_university", "completed_prof_degree") ~ "higher",
                                    hh_member_education %in% c("other") ~ "other",
                                    TRUE ~ hh_member_education
                                    )
    )
}
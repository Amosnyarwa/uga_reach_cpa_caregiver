# function for creating composite indicators

create_composite_indicators_dfa <- function(input_df) {
  input_df %>% 
    mutate(
      i.refugee_settlement = case_when(district_name == "adjumani" & status == "refugee" ~ "adjumani", 
                                       settlement_name == "rhino" ~ "rhino_camp",
                                       TRUE ~ settlement_name),
      i.region = case_when(district_name %in% c("kampala") ~ "central",
                           district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa") ~ "south_west"
                           TRUE ~ "west_nile")
    ) %>% 
    select(-starts_with("int."))
}
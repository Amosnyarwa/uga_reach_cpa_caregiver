# output holder -----------------------------------------------------------

logic_seperate_output <- list()

# okay_parents_arrange_child_marriage_not_agree_1 -------------------------
df_c_logic_okay_parents_arrange_child_marriage_not_agree <- df_tool_data %>% 
  filter(okay_parents_arrange_child_marriage %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") &
           (okay_parents_arrange_child_marriage_for_money %in% c("agree", "strongly_agree") | 
              okay_parents_arrange_child_marriage_for_her_safety %in% c("agree", "strongly_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_not_agree_1",
         i.check.issue = glue("okay_parents_arrange_child_marriage_for_money: {okay_parents_arrange_child_marriage_for_money}, okay_parents_arrange_child_marriage_for_her_safety: {okay_parents_arrange_child_marriage_for_her_safety}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_parents_arrange_child_marriage_not_agree")){
  if(nrow(df_c_logic_okay_parents_arrange_child_marriage_not_agree) > 0){
    logic_seperate_output$df_c_logic_okay_parents_arrange_child_marriage_not_agree <- df_c_logic_okay_parents_arrange_child_marriage_not_agree
  }
}
# okay_parents_arrange_child_marriage_agree_2 -----------------------------
df_c_logic_okay_parents_arrange_child_marriage_agree <- df_tool_data %>% 
  filter(okay_parents_arrange_child_marriage %in% c("agree", "strongly_agree") &
           (okay_parents_arrange_child_marriage_for_money %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") | 
              okay_parents_arrange_child_marriage_for_her_safety %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_agree_2",
         i.check.issue = glue("okay_parents_arrange_child_marriage_for_money: {okay_parents_arrange_child_marriage_for_money}, okay_parents_arrange_child_marriage_for_her_safety: {okay_parents_arrange_child_marriage_for_her_safety}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_parents_arrange_child_marriage_agree")){
  if(nrow(df_c_logic_okay_parents_arrange_child_marriage_agree) > 0){
    logic_seperate_output$df_c_logic_okay_parents_arrange_child_marriage_agree <- df_c_logic_okay_parents_arrange_child_marriage_agree
  }
}
# okay_girl_less_18_years_get_married_not_agree_3 -------------------------
df_c_logic_okay_girl_get_married_stop_school_once_married <- df_tool_data %>% 
  filter(okay_girl_less_18_years_get_married %in% c("disagree", "strongly_disagree") & 
           okay_girl_stay_home_and_stop_school_once_married %in% c("agree", "strongly_agree", "neither_agree_nor_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_girl_less_18_years_get_married",
         i.check.current_value = okay_girl_less_18_years_get_married,
         i.check.value = "",
         i.check.issue_id = "okay_girl_less_18_years_get_married_not_agree_3",
         i.check.issue = glue("okay_girl_stay_home_and_stop_school_once_married: {okay_girl_stay_home_and_stop_school_once_married}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_girl_get_married_stop_school_once_married")){
  if(nrow(df_c_logic_okay_girl_get_married_stop_school_once_married) > 0){
    logic_seperate_output$df_c_logic_okay_girl_get_married_stop_school_once_married <- df_c_logic_okay_girl_get_married_stop_school_once_married
  }
}
# okay_girl_less_18_years_get_married_not_agree_4 -------------------------
df_c_logic_okay_girl_get_married_reach_puberty <- df_tool_data %>% 
  filter(okay_girl_less_18_years_get_married %in% c("disagree", "strongly_disagree") & 
           okay_girl_get_married_once_reach_puberty %in% c("agree", "strongly_agree", "neither_agree_nor_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_girl_less_18_years_get_married",
         i.check.current_value = okay_girl_less_18_years_get_married,
         i.check.value = "",
         i.check.issue_id = "okay_girl_less_18_years_get_married_not_agree_4",
         i.check.issue = glue("okay_girl_get_married_once_reach_puberty: {okay_girl_get_married_once_reach_puberty}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_girl_get_married_reach_puberty")){
  if(nrow(df_c_logic_okay_girl_get_married_reach_puberty) > 0){
    logic_seperate_output$df_c_logic_okay_girl_get_married_reach_puberty <- df_c_logic_okay_girl_get_married_reach_puberty
  }
}
# okay_girl_less_18_years_get_married_agree_5 -----------------------------
df_c_logic_okay_girl_get_married_agree_reach_puberty <- df_tool_data %>% 
  filter(okay_girl_less_18_years_get_married %in% c("agree", "strongly_agree") & 
           okay_girl_get_married_once_reach_puberty %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_girl_less_18_years_get_married",
         i.check.current_value = okay_girl_less_18_years_get_married,
         i.check.value = "",
         i.check.issue_id = "okay_girl_less_18_years_get_married_agree_5",
         i.check.issue = glue("okay_girl_get_married_once_reach_puberty: {okay_girl_get_married_once_reach_puberty}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_girl_get_married_agree_reach_puberty")){
  if(nrow(df_c_logic_okay_girl_get_married_agree_reach_puberty) > 0){
    logic_seperate_output$df_c_logic_okay_girl_get_married_agree_reach_puberty <- df_c_logic_okay_girl_get_married_agree_reach_puberty
  }
}
# okay_father_mother_to_hit_his_child_agree_but_disagree_reasons_6 --------
# correction: (okay_parents_hit_child_to_discipline:okay_parents_hit_child_to_displine)
df_c_logic_hit_child_agree_but_disagree_reasons <- df_tool_data %>%
  filter((okay_father_to_hit_his_child %in% c("agree", "strongly_agrees") |
            okay_mother_to_hit_her_child %in% c("agree", "strongly_agrees")) &
           (okay_parents_hit_child_to_displine %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") &
okay_parents_hit_child_to_set_example %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree"))) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "okay_father_to_hit_his_child",
         i.check.current_value = okay_father_to_hit_his_child,
         i.check.value = "",
         i.check.issue_id = "okay_father_mother_to_hit_his_child_agree_but_disagree_reasons_6",
         i.check.issue = glue("okay_father_to_hit_his_child: {okay_father_to_hit_his_child}, okay_mother_to_hit_her_child : {okay_mother_to_hit_her_child}, okay_parents_hit_child_to_displine : {okay_parents_hit_child_to_displine}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_hit_child_agree_but_disagree_reasons")){
  if(nrow(df_c_logic_hit_child_agree_but_disagree_reasons) > 0){
    logic_seperate_output$df_c_logic_hit_child_agree_but_disagree_reasons <- df_c_logic_hit_child_agree_but_disagree_reasons
  }
}
# okay_father_mother_to_hit_his_child_disagree_but_agree_reasons_7 --------
# correction: (okay_parents_hit_child_to_discipline:okay_parents_hit_child_to_displine)
df_c_logic_hit_child_disagree_but_agree_reasons <- df_tool_data %>%
  filter((okay_father_to_hit_his_child %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") |
            okay_mother_to_hit_her_child %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree")) &
           (okay_parents_hit_child_to_displine %in% c("agree", "strongly_agrees") |
              okay_parents_hit_child_to_set_example %in% c("agree", "strongly_agrees"))) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "okay_father_to_hit_his_child",
         i.check.current_value = okay_father_to_hit_his_child,
         i.check.value = "",
         i.check.issue_id = "okay_father_mother_to_hit_his_child_disagree_but_agree_reasons_7",
         i.check.issue = glue("okay_father_to_hit_his_child: {okay_father_to_hit_his_child}, okay_mother_to_hit_her_child : {okay_mother_to_hit_her_child}, okay_parents_hit_child_to_displine : {okay_parents_hit_child_to_displine}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_hit_child_disagree_but_agree_reasons")){
  if(nrow(df_c_logic_hit_child_disagree_but_agree_reasons) > 0){
    logic_seperate_output$df_c_logic_hit_child_disagree_but_agree_reasons <- df_c_logic_hit_child_disagree_but_agree_reasons
  }
}
# parents_responsible_to_provide_child_8 ----------------------------------
df_c_logic_parents_responsible_to_provide_child_contradict <- df_tool_data %>% 
  filter(parents_responsible_to_provide_child_enough_food %in% c("strongly_disagree", "disagree"),
              parents_responsible_to_provide_all_child_needs %in% c("strongly_agree", "agree")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "parents_responsible_to_provide_child_enough_food",
         i.check.current_value = parents_responsible_to_provide_child_enough_food,
         i.check.value = "NA",
         i.check.issue_id = "parents_responsible_to_provide_child_8",
         i.check.issue = glue("parents_responsible_to_provide_child_enough_food: {parents_responsible_to_provide_child_enough_food}, parents_responsible_to_provide_all_child_needs: {parents_responsible_to_provide_all_child_needs}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  slice(rep(1:n(), each = 2)) %>% 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.district_name, 
           i.check.point_number, i.check.type,  i.check.name,  i.check.current_value) %>% 
  mutate(rank = row_number(),
         i.check.name = ifelse(rank == 2, "parents_responsible_to_provide_all_child_needs", i.check.name),
         i.check.current_value = ifelse(rank == 2, parents_responsible_to_provide_all_child_needs, i.check.current_value)) %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_parents_responsible_to_provide_child_contradict")){
  if(nrow(df_c_logic_parents_responsible_to_provide_child_contradict) > 0){
    logic_seperate_output$df_c_logic_parents_responsible_to_provide_child_contradict <- df_c_logic_parents_responsible_to_provide_child_contradict
  }
}
# child_labour_reponse_contradiction_13 -----------------------------------
df_c_logic_child_labour_reponse_contradiction <- df_tool_data %>% 
  filter((child_labour_economic_types %in% c("bonded_labour_or_slavery", "construction", "charcoal_burning", "handling_of_heavy_loads", 
                                             "mining", "sand_mining", "producing_and_or_trafficking_or_selling_drugs", 
                                             "sale_or_trafficking_of_children_for_labour_purposes", "sexual_exploitation", 
                                             "stone_quarrying", "working_with_armed_groups")|
            work_type_children_involved_in_community %in% c("bonded_labour_or_slavery", "construction", "charcoal_burning", 
                                                            "handling_of_heavy_loads", "mining", "sand_mining", 
                                                            "producing_and_or_trafficking_or_selling_drugs", 
                                                            "sale_or_trafficking_of_children_for_labour_purposes", "sexual_exploitation", 
                                                            "stone_quarrying", "working_with_armed_groups")), 
         frequency_child_involved_in_harsh_work == "never"
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_child_involved_in_harsh_work",
         i.check.current_value = frequency_child_involved_in_harsh_work,
         i.check.value = "NA",
         i.check.issue_id = "child_labour_reponse_contradiction_13",
         i.check.issue = glue("child_labour_economic_types: {child_labour_economic_types}, work_type_children_involved_in_community: {work_type_children_involved_in_community}, frequency_child_involved_in_harsh_work: {frequency_child_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_child_labour_reponse_contradiction")){
  if(nrow(df_c_logic_child_labour_reponse_contradiction) > 0){
    logic_seperate_output$df_c_logic_child_labour_reponse_contradiction <- df_c_logic_child_labour_reponse_contradiction
  }
}
# work_type_children_involved_14 ------------------------------------------
df_c_logic_work_type_children_involved <- df_tool_data %>% 
  filter(!work_type_children_involved_in_community %in% c("bonded_labour_or_slavery", "construction", "charcoal_burning", "handling_of_heavy_loads", 
                                                          "mining", "sand_mining", "producing_and_or_trafficking_or_selling_drugs", 
                                                          "sale_or_trafficking_of_children_for_labour_purposes", "sexual_exploitation", 
                                                          "stone_quarrying", "working_with_armed_groups"), 
         frequency_child_involved_in_harsh_work != "never"
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_child_involved_in_harsh_work",
         i.check.current_value = frequency_child_involved_in_harsh_work,
         i.check.value = "",
         i.check.issue_id = "work_type_children_involved_14",
         i.check.issue = glue("work_type_children_involved_in_community: {work_type_children_involved_in_community}, frequency_child_involved_in_harsh_work: {frequency_child_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_work_type_children_involved")){
  if(nrow(df_c_logic_work_type_children_involved) > 0){
    logic_seperate_output$df_c_logic_work_type_children_involved <- df_c_logic_work_type_children_involved
  }
}
# action_taken_by_caretaker_for_child_harsh_work_15 -----------------------
# correction: (services_available_to_protect_child_from_harsh_labour:services_availiable_to_protect_child_from_harsh_labour)
df_c_logic_action_taken_by_caretaker_for_child_harsh_work <- df_tool_data %>%
  filter(action_taken_by_caretaker_when_sees_child_doing_harsh_work %in% c("i_engage_the_child_protection_committees", "I_report_it_to_ngo_staff",
                                                                           "i_report_it_to_rwc", "i_report_it_to_the_police"),
         (child_labour_protection_services_sought == "no" | services_availiable_to_protect_child_from_harsh_labour == "no")
  ) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "action_taken_by_caretaker_when_sees_child_doing_harsh_work",
         i.check.current_value = action_taken_by_caretaker_when_sees_child_doing_harsh_work,
         i.check.value = "",
         i.check.issue_id = "action_taken_by_caretaker_for_child_harsh_work_15",
         i.check.issue = glue("action_taken_by_caretaker_when_sees_child_doing_harsh_work: {action_taken_by_caretaker_when_sees_child_doing_harsh_work}, child_labour_protection_services_sought: {child_labour_protection_services_sought}, services_availiable_to_protect_child_from_harsh_labour: {services_availiable_to_protect_child_from_harsh_labour}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept",
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>%
  dplyr::select(starts_with("i.check")) %>%
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_action_taken_by_caretaker_for_child_harsh_work")){
  if(nrow(df_c_logic_action_taken_by_caretaker_for_child_harsh_work) > 0){
    logic_seperate_output$df_c_logic_action_taken_by_caretaker_for_child_harsh_work <- df_c_logic_action_taken_by_caretaker_for_child_harsh_work
  }
}
# protection_services_for_child_16 ----------------------------------------
df_c_logic_protection_services_for_child_a <- df_tool_data %>% 
  filter(protection_services_for_child_violence == "no", protection_services_for_child_physical_harm == "yes"
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "protection_services_for_child_violence",
         i.check.current_value = protection_services_for_child_violence,
         i.check.value = "",
         i.check.issue_id = "protection_services_for_child_16",
         i.check.issue = glue("protection_services_for_child_violence: {protection_services_for_child_violence}, protection_services_for_child_physical_harm: {protection_services_for_child_physical_harm}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "accept", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_protection_services_for_child_a")){
  if(nrow(df_c_logic_protection_services_for_child_a) > 0){
    logic_seperate_output$df_c_logic_protection_services_for_child_a <- df_c_logic_protection_services_for_child_a
  }
}
# protection_services_for_child_17 ----------------------------------------
df_c_logic_protection_services_for_child <- df_tool_data %>% 
  filter(protection_services_for_child_violence == "yes", protection_services_for_child_physical_harm == "no"
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "protection_services_for_child_violence",
         i.check.current_value = protection_services_for_child_violence,
         i.check.value = "",
         i.check.issue_id = "protection_services_for_child_17",
         i.check.issue = glue("protection_services_for_child_violence: {protection_services_for_child_violence}, protection_services_for_child_physical_harm: {protection_services_for_child_physical_harm}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_protection_services_for_child")){
  if(nrow(df_c_logic_protection_services_for_child) > 0){
    logic_seperate_output$df_c_logic_protection_services_for_child <- df_c_logic_protection_services_for_child
  }
}
# demographics_and_seperated_children_18 ----------------------------------
df_c_logic_demographics_and_seperated_children <- df_tool_data %>% 
  filter(
    (children_provide_kinship_care > 0 & current_giving_care_to_separated_children == "no") |
      ((children_provide_kinship_care == 0 | children_biological_parent == "yes") &
         current_giving_care_to_separated_children == "yes")
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "current_giving_care_to_separated_children",
         i.check.current_value = current_giving_care_to_separated_children,
         i.check.value = "",
         i.check.issue_id = "demographics_and_seperated_children_18",
         i.check.issue = glue("children_provide_kinship_care: {children_provide_kinship_care}, current_giving_care_to_separated_children: {current_giving_care_to_separated_children}, children_biological_parent: {children_biological_parent}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_demographics_and_seperated_children")){
  if(nrow(df_c_logic_demographics_and_seperated_children) > 0){
    logic_seperate_output$df_c_logic_demographics_and_seperated_children <- df_c_logic_demographics_and_seperated_children
  }
}
# demographics_and_unaccompanied_children_19 ------------------------------
df_c_logic_demographics_and_unaccompanied_children <- df_tool_data %>% 
  filter(
    ((children_provide_foster > 0 & current_giving_care_to_unaccompanied_children == "no") |
       (((children_provide_foster == 0 | children_biological_parent == "yes") &
           current_giving_care_to_unaccompanied_children == "yes")))
       ) %>% 
       mutate(i.check.type = "change_response",
              i.check.name = "current_giving_care_to_unaccompanied_children",
              i.check.current_value = current_giving_care_to_unaccompanied_children,
              i.check.value = "",
              i.check.issue_id = "demographics_and_unaccompanied_children_19",
              i.check.issue = glue("children_provide_foster: {children_provide_foster}, current_giving_care_to_unaccompanied_children: {current_giving_care_to_unaccompanied_children}, children_biological_parent: {children_biological_parent}"),
              i.check.other_text = "",
              i.check.checked_by = "",
              i.check.checked_date = as_date(today()),
              i.check.comment = "", 
              i.check.reviewed = "",
              i.check.adjust_log = "",
              i.check.uuid_cl = "",
              i.check.so_sm_choices = "") %>% 
       dplyr::select(starts_with("i.check")) %>% 
       rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
     
     if(exists("df_c_logic_demographics_and_unaccompanied_children")){
       if(nrow(df_c_logic_demographics_and_unaccompanied_children) > 0){
         logic_seperate_output$df_c_logic_demographics_and_unaccompanied_children <- df_c_logic_demographics_and_unaccompanied_children
       }
     }
# frequency_children_separate_from_parents_20 -----------------------------
df_c_logic_frequency_children_separate_from_parents <- df_tool_data %>% 
  filter(current_giving_care_to_separated_children == "yes" &
           frequency_children_separate_from_parents == "none") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_separate_from_parents",
         i.check.current_value = frequency_children_separate_from_parents,
         i.check.value = "",
         i.check.issue_id = "frequency_children_separate_from_parents_20",
         i.check.issue = glue("current_giving_care_to_separated_children: {current_giving_care_to_separated_children}, frequency_children_separate_from_parents: {frequency_children_separate_from_parents}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_children_separate_from_parents")){
  if(nrow(df_c_logic_frequency_children_separate_from_parents) > 0){
    logic_seperate_output$df_c_logic_frequency_children_separate_from_parents <- df_c_logic_frequency_children_separate_from_parents
  }
}
# frequency_unaccompanied_children_occurrence_21 ---------------------------
# correction: (frequency_unaccompanied_children_occurence:frequency_unaccompanied_children_occurrence)
df_c_logic_frequency_unaccompanied_children_occurrence <- df_tool_data %>% 
  filter(current_giving_care_to_unaccompanied_children == "yes" &
           frequency_unaccompanied_children_occurrence == "none") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_unaccompanied_children_occurrence",
         i.check.current_value = frequency_unaccompanied_children_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_unaccompanied_children_occurrence_21",
         i.check.issue = glue("current_giving_care_to_unaccompanied_children: {current_giving_care_to_unaccompanied_children}, frequency_unaccompanied_children_occurrence: {frequency_unaccompanied_children_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_unaccompanied_children_occurrence")){
  if(nrow(df_c_logic_frequency_unaccompanied_children_occurrence) > 0){
    logic_seperate_output$df_c_logic_frequency_unaccompanied_children_occurrence <- df_c_logic_frequency_unaccompanied_children_occurrence
  }
}
# frequency_of_child_violence_occurrence_22 -------------------------------
df_c_logic_frequency_of_child_violence_occurrence <- df_tool_data %>% 
  filter(hh_child_experienced_violence == "yes" &
           frequency_of_child_violence_occurrence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_of_child_violence_occurrence",
         i.check.current_value = frequency_of_child_violence_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_of_child_violence_occurrence_22",
         i.check.issue = glue("hh_child_experienced_violence: {hh_child_experienced_violence}, frequency_of_child_violence_occurrence: {frequency_of_child_violence_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_of_child_violence_occurrence")){
  if(nrow(df_c_logic_frequency_of_child_violence_occurrence) > 0){
    logic_seperate_output$df_c_logic_frequency_of_child_violence_occurrence <- df_c_logic_frequency_of_child_violence_occurrence
  }
}
# frequency_children_experience_sexual_violence_23 ------------------------
df_c_logic_frequency_children_experience_sexual_violence <- df_tool_data %>% 
  filter(children_involved_with_armed_groups == "yes" &
           frequency_children_experience_sexual_violence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_experience_sexual_violence",
         i.check.current_value = frequency_children_experience_sexual_violence,
         i.check.value = "",
         i.check.issue_id = "frequency_children_experience_sexual_violence_23",
         i.check.issue = glue("children_involved_with_armed_groups: {children_involved_with_armed_groups}, frequency_children_experience_sexual_violence: {frequency_children_experience_sexual_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_children_experience_sexual_violence")){
  if(nrow(df_c_logic_frequency_children_experience_sexual_violence) > 0){
    logic_seperate_output$df_c_logic_frequency_children_experience_sexual_violence <- df_c_logic_frequency_children_experience_sexual_violence
  }
}
# frequency_child_involved_in_harsh_work_24 -------------------------------
df_c_logic_frequency_child_involved_in_harsh_work <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("child_labour"), 
         frequency_child_involved_in_harsh_work == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_child_involved_in_harsh_work",
         i.check.current_value = frequency_child_involved_in_harsh_work,
         i.check.value = "",
         i.check.issue_id = "frequency_child_involved_in_harsh_work_24",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, frequency_child_involved_in_harsh_work: {frequency_child_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_child_involved_in_harsh_work")){
  if(nrow(df_c_logic_frequency_child_involved_in_harsh_work) > 0){
    logic_seperate_output$df_c_logic_frequency_child_involved_in_harsh_work <- df_c_logic_frequency_child_involved_in_harsh_work
  }
}
# frequency_child_involved_in_harsh_work_25 -------------------------------
df_c_logic_frequency_child_involved_in_harsh_work_b <- df_tool_data %>% 
  filter(!child_protection_risks_witnessed %in% c("child_labour"), 
         frequency_child_involved_in_harsh_work != "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_child_involved_in_harsh_work",
         i.check.current_value = frequency_child_involved_in_harsh_work,
         i.check.value = "",
         i.check.issue_id = "frequency_child_involved_in_harsh_work_25",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, frequency_child_involved_in_harsh_work: {frequency_child_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_child_involved_in_harsh_work_b")){
  if(nrow(df_c_logic_frequency_child_involved_in_harsh_work_b) > 0){
    logic_seperate_output$df_c_logic_frequency_child_involved_in_harsh_work_b <- df_c_logic_frequency_child_involved_in_harsh_work_b
  }
}
# frequency_children_experience_sexual_violence_26 ------------------------
df_c_logic_frequency_children_experience_sexual_violence <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("sexual_violence_and_exploitation"), 
         frequency_children_experience_sexual_violence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_experience_sexual_violence",
         i.check.current_value = frequency_children_experience_sexual_violence,
         i.check.value = "",
         i.check.issue_id = "frequency_children_experience_sexual_violence_26",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, frequency_children_experience_sexual_violence: {frequency_children_experience_sexual_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_children_experience_sexual_violence")){
  if(nrow(df_c_logic_frequency_children_experience_sexual_violence) > 0){
    logic_seperate_output$df_c_logic_frequency_children_experience_sexual_violence <- df_c_logic_frequency_children_experience_sexual_violence
  }
}
# frequency_children_experience_sexual_violence_27 ------------------------
df_c_logic_frequency_children_experience_sexual_violence_b <- df_tool_data %>% 
  filter(!child_protection_risks_witnessed %in% c("sexual_violence_and_exploitation"), 
         frequency_children_experience_sexual_violence != "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_experience_sexual_violence",
         i.check.current_value = frequency_children_experience_sexual_violence,
         i.check.value = "",
         i.check.issue_id = "frequency_children_experience_sexual_violence_27",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, frequency_children_experience_sexual_violence: {frequency_children_experience_sexual_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_children_experience_sexual_violence_b")){
  if(nrow(df_c_logic_frequency_children_experience_sexual_violence_b) > 0){
    logic_seperate_output$df_c_logic_frequency_children_experience_sexual_violence_b <- df_c_logic_frequency_children_experience_sexual_violence_b
  }
}
# frequency_unaccompanied_children_occurrence_28 --------------------------
df_c_logic_frequency_unaccompanied_children_occurrence_28 <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("sexual_violence_and_exploitation"), 
         frequency_children_separate_from_parents == "none",
         frequency_unaccompanied_children_occurrence != "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_unaccompanied_children_occurrence",
         i.check.current_value = frequency_unaccompanied_children_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_unaccompanied_children_occurrence_28",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, frequency_children_separate_from_parents: {frequency_children_separate_from_parents}, frequency_unaccompanied_children_occurrence: {frequency_unaccompanied_children_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_unaccompanied_children_occurrence_28")){
  if(nrow(df_c_logic_frequency_unaccompanied_children_occurrence_28) > 0){
    logic_seperate_output$df_c_logic_frequency_unaccompanied_children_occurrence_28 <- df_c_logic_frequency_unaccompanied_children_occurrence_28
  }
}
# frequency_unaccompanied_children_occurrence_29 --------------------------
df_c_logic_frequency_unaccompanied_children_occurrence_29 <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("separation_from_family_members"), 
         (frequency_children_separate_from_parents != "none"|
            frequency_unaccompanied_children_occurrence != "none")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_unaccompanied_children_occurrence",
         i.check.current_value = frequency_unaccompanied_children_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_unaccompanied_children_occurrence_29",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, frequency_children_separate_from_parents: {frequency_children_separate_from_parents}, frequency_unaccompanied_children_occurrence: {frequency_unaccompanied_children_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_unaccompanied_children_occurrence_29")){
  if(nrow(df_c_logic_frequency_unaccompanied_children_occurrence_29) > 0){
    logic_seperate_output$df_c_logic_frequency_unaccompanied_children_occurrence_29 <- df_c_logic_frequency_unaccompanied_children_occurrence_29
  }
}
# frequency_of_child_violence_occurrence_risk_witnessed_30 ----------------
df_c_logic_frequency_of_child_violence_occurrence_risk <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("physical_violence"),
         frequency_of_child_violence_occurrence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_of_child_violence_occurrence",
         i.check.current_value = frequency_of_child_violence_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_of_child_violence_occurrence_risk_witnessed_30",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, frequency_of_child_violence_occurrence: {frequency_of_child_violence_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_of_child_violence_occurrence_risk")){
  if(nrow(df_c_logic_frequency_of_child_violence_occurrence_risk) > 0){
    logic_seperate_output$df_c_logic_frequency_of_child_violence_occurrence_risk <- df_c_logic_frequency_of_child_violence_occurrence_risk
  }
}
# frequency_of_child_violence_occurrence_risk_not_selected_31 -------------
df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected <- df_tool_data %>% 
  filter(!child_protection_risks_witnessed %in% c("physical_violence"),
         frequency_of_child_violence_occurrence != "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_of_child_violence_occurrence",
         i.check.current_value = frequency_of_child_violence_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_of_child_violence_occurrence_risk_not_selected_31",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, frequency_of_child_violence_occurrence: {frequency_of_child_violence_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected")){
  if(nrow(df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected) > 0){
    logic_seperate_output$df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected <- df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected
  }
}
# okay_parents_arrange_child_marriage_37 ----------------------------------
df_c_logic_okay_parents_arrange_child_marriage_mismatch <- df_tool_data %>% 
  filter(
    okay_parents_arrange_child_marriage %in% c("agree", "strongly_agree"),
    okay_girl_less_18_years_get_married %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree")
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_37",
         i.check.issue = glue("okay_parents_arrange_child_marriage: {okay_parents_arrange_child_marriage}, okay_girl_less_18_years_get_married: {okay_girl_less_18_years_get_married}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_parents_arrange_child_marriage_mismatch")){
  if(nrow(df_c_logic_okay_parents_arrange_child_marriage_mismatch) > 0){
    logic_seperate_output$df_c_logic_okay_parents_arrange_child_marriage_mismatch <- df_c_logic_okay_parents_arrange_child_marriage_mismatch
  }
}
# okay_parents_arrange_child_marriage_38 ----------------------------------
df_c_logic_okay_parents_arrange_child_marriage_mismatch_38 <- df_tool_data %>% 
  filter(
    okay_parents_arrange_child_marriage %in% c("agree", "strongly_agree"),
    okay_girl_stay_home_and_stop_school_once_become_mother %in% c("strongly_disagree", "disagree")
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_38",
         i.check.issue = glue("okay_parents_arrange_child_marriage: {okay_parents_arrange_child_marriage}, okay_girl_stay_home_and_stop_school_once_become_mother: {okay_girl_stay_home_and_stop_school_once_become_mother}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_parents_arrange_child_marriage_mismatch_38")){
  if(nrow(df_c_logic_okay_parents_arrange_child_marriage_mismatch_38) > 0){
    logic_seperate_output$df_c_logic_okay_parents_arrange_child_marriage_mismatch_38 <- df_c_logic_okay_parents_arrange_child_marriage_mismatch_38
  }
}
# child_protection_risks_witnessed_reported_in_particular_39 --------------
df_c_logic_child_protection_risks_witnessed_reported_in_particular <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("no_particular_risk") & 
           (!boys_protection_risks %in%("no_particular_risk")&!girls_protection_risks %in%("no_particular_risk")) ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "child_protection_risks_witnessed",
         i.check.current_value = child_protection_risks_witnessed,
         i.check.value = "",
         i.check.issue_id = "child_protection_risks_witnessed_reported_in_particular_39",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed}, boys_protection_risks: {boys_protection_risks}, girls_protection_risks: {girls_protection_risks}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_child_protection_risks_witnessed_reported_in_particular")){
  if(nrow(df_c_logic_child_protection_risks_witnessed_reported_in_particular) > 0){
    logic_seperate_output$df_c_logic_child_protection_risks_witnessed_reported_in_particular <- df_c_logic_child_protection_risks_witnessed_reported_in_particular
  }
}
# children_experienced_sexual_violence_freq_mismatch_40 -------------------
df_c_logic_children_experienced_sexual_violence <- df_tool_data %>% 
  filter(children_experienced_sexual_violence == "yes", 
         frequency_children_experience_sexual_violence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "child_protection_risks_witnessed",
         i.check.current_value = child_protection_risks_witnessed,
         i.check.value = "",
         i.check.issue_id = "children_experienced_sexual_violence_freq_mismatch_40",
         i.check.issue = glue("children_experienced_sexual_violence: {children_experienced_sexual_violence}, frequency_children_experience_sexual_violence: {frequency_children_experience_sexual_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_children_experienced_sexual_violence")){
  if(nrow(df_c_logic_children_experienced_sexual_violence) > 0){
    logic_seperate_output$df_c_logic_children_experienced_sexual_violence <- df_c_logic_children_experienced_sexual_violence
  }
}

# combined seperate logical checks ----------------------------------------------------------

df_logic_seperate_checks <- bind_rows(logic_seperate_output)

# output the resulting data frame
write_csv(x = df_logic_seperate_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_logical_checks_caregiver.csv"), na = "")

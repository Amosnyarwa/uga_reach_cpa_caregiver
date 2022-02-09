# okay_parents_arrange_child_marriage_not_agree_1
df_c_logic_okay_parents_arrange_child_marriage_not_agree <- df_tool_data %>% 
  filter(okay_parents_arrange_child_marriage %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") &
           (okay_parents_arrange_child_marriage_for_money %in% c("agree", "strongly_agree") | 
              okay_parents_arrange_child_marriage_for_her_safety %in% c("agree", "strongly_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_not_agree_1",
         i.check.issue = glue("okay_parents_arrange_child_marriage_for_money: {okay_parents_arrange_child_marriage_for_money},  
                              okay_parents_arrange_child_marriage_for_her_safety: {okay_parents_arrange_child_marriage_for_her_safety}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_parents_arrange_child_marriage_not_agree")){
  if(nrow(df_c_logic_okay_parents_arrange_child_marriage_not_agree) > 0){
    logic_output$df_c_logic_okay_parents_arrange_child_marriage_not_agree <- df_c_logic_okay_parents_arrange_child_marriage_not_agree
  }
}
# okay_parents_arrange_child_marriage_agree_2
df_c_logic_okay_parents_arrange_child_marriage_agree <- df_tool_data %>% 
  filter(okay_parents_arrange_child_marriage %in% c("agree", "strongly_agree") &
           (okay_parents_arrange_child_marriage_for_money %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree") | 
              okay_parents_arrange_child_marriage_for_her_safety %in% c("disagree", "strongly_disagree", "neither_agree_nor_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_parents_arrange_child_marriage",
         i.check.current_value = okay_parents_arrange_child_marriage,
         i.check.value = "",
         i.check.issue_id = "okay_parents_arrange_child_marriage_agree_2",
         i.check.issue = glue("okay_parents_arrange_child_marriage_for_money: {okay_parents_arrange_child_marriage_for_money},  
                              okay_parents_arrange_child_marriage_for_her_safety: {okay_parents_arrange_child_marriage_for_her_safety}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_parents_arrange_child_marriage_agree")){
  if(nrow(df_c_logic_okay_parents_arrange_child_marriage_agree) > 0){
    logic_output$df_c_logic_okay_parents_arrange_child_marriage_agree <- df_c_logic_okay_parents_arrange_child_marriage_agree
  }
}
# okay_girl_less_18_years_get_married_not_agree_3
girl_less_18_years_get_married_not_agree_stop_school_once_married <- df_tool_data %>% 
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
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "")

girl_less_18_years_get_married_not_agree_stop_school_once_married_b <- girl_less_18_years_get_married_not_agree_stop_school_once_married %>% 
  mutate(i.check.name = "okay_girl_stay_home_and_stop_school_once_married",
         i.check.current_value = okay_girl_stay_home_and_stop_school_once_married
  )

df_c_logic_okay_girl_get_married_stop_school_once_married <- bind_rows(girl_less_18_years_get_married_not_agree_stop_school_once_married, 
                                                                       girl_less_18_years_get_married_not_agree_stop_school_once_married_b) %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_girl_get_married_stop_school_once_married")){
  if(nrow(df_c_logic_okay_girl_get_married_stop_school_once_married) > 0){
    logic_output$df_c_logic_okay_girl_get_married_stop_school_once_married <- df_c_logic_okay_girl_get_married_stop_school_once_married
  }
}
# okay_girl_less_18_years_get_married_not_agree_4
girl_less_18_years_get_married_not_agree_reach_puberty <- df_tool_data %>% 
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
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "")

girl_less_18_years_get_married_not_agree_reach_puberty_b <- girl_less_18_years_get_married_not_agree_reach_puberty %>% 
  mutate(i.check.name = "okay_girl_get_married_once_reach_puberty",
         i.check.current_value = okay_girl_get_married_once_reach_puberty
  )

df_c_logic_okay_girl_get_married_reach_puberty <- bind_rows(girl_less_18_years_get_married_not_agree_reach_puberty, 
                                                            girl_less_18_years_get_married_not_agree_reach_puberty_b) %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_girl_get_married_reach_puberty")){
  if(nrow(df_c_logic_okay_girl_get_married_reach_puberty) > 0){
    logic_output$df_c_logic_okay_girl_get_married_reach_puberty <- df_c_logic_okay_girl_get_married_reach_puberty
  }
}
# okay_girl_less_18_years_get_married_agree_5
girl_less_18_years_get_married_agree_reach_puberty <- df_tool_data %>% 
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
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "")

girl_less_18_years_get_married_agree_reach_puberty_b <- girl_less_18_years_get_married_agree_reach_puberty %>% 
  mutate(i.check.name = "okay_girl_get_married_once_reach_puberty",
         i.check.current_value = okay_girl_get_married_once_reach_puberty
  )

df_c_logic_okay_girl_get_married_agree_reach_puberty <- bind_rows(girl_less_18_years_get_married_agree_reach_puberty, 
                                                                  girl_less_18_years_get_married_agree_reach_puberty_b) %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_girl_get_married_agree_reach_puberty")){
  if(nrow(df_c_logic_okay_girl_get_married_agree_reach_puberty) > 0){
    logic_output$df_c_logic_okay_girl_get_married_agree_reach_puberty <- df_c_logic_okay_girl_get_married_agree_reach_puberty
  }
}

# okay_father_mother_to_hit_his_child_agree_but_disagree_reasons_6
father_to_hit_his_child_agree <- df_tool_data %>% 
  filter((okay_father_to_hit_his_child %in% c("agrees", "strongly_agrees") | 
            okay_mother_to_hit_her_child %in% c("agrees", "strongly_agrees")) &
           (okay_parents_hit_child_to_discipline %in% c("disagrees", "strongly_disagrees", "neither_agree_nor_agree") &
okay_parents_hit_child_to_set_example %in% c("disagrees", "strongly_disagrees", "neither_agree_nor_agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_father_to_hit_his_child",
         i.check.current_value = okay_father_to_hit_his_child,
         i.check.value = "",
         i.check.issue_id = "okay_father_mother_to_hit_his_child_agree_but_disagree_reasons_6",
         i.check.issue = glue("okay_father_to_hit_his_child: {okay_father_to_hit_his_child},
                             okay_mother_to_hit_her_child : {okay_mother_to_hit_her_child},
                             okay_parents_hit_child_to_discipline : {okay_parents_hit_child_to_discipline}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "")

mother_to_hit_his_child_agree <- father_to_hit_his_child_agree %>% 
  mutate(i.check.name = "okay_mother_to_hit_her_child",
         i.check.current_value = okay_mother_to_hit_her_child
  )

df_c_logic_hit_child_agree_but_disagree_reasons <- bind_rows(father_to_hit_his_child_agree, 
                                                             mother_to_hit_his_child_agree) %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))


if(exists("df_c_logic_hit_child_agree_but_disagree_reasons")){
  if(nrow(df_c_logic_hit_child_agree_but_disagree_reasons) > 0){
    logic_output$df_c_logic_hit_child_agree_but_disagree_reasons <- df_c_logic_hit_child_agree_but_disagree_reasons
  }
}

# okay_father_mother_to_hit_his_child_disagree_but_agree_reasons_7
father_to_hit_his_child_disagree <- df_tool_data %>% 
  filter((okay_father_to_hit_his_child %in% c("disagrees", "strongly_disagrees", "neither_agree_nor_agree") |
            okay_mother_to_hit_her_child %in% c("disagrees", "strongly_disagrees", "neither_agree_nor_agree")) &
           (okay_parents_hit_child_to_discipline %in% c("agrees", "strongly_agrees") |
              okay_parents_hit_child_to_set_example %in% c("agrees", "strongly_agrees"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "okay_father_to_hit_his_child",
         i.check.current_value = okay_father_to_hit_his_child,
         i.check.value = "",
         i.check.issue_id = "okay_father_mother_to_hit_his_child_disagree_but_agree_reasons_7",
         i.check.issue = glue("okay_father_to_hit_his_child: {okay_father_to_hit_his_child},
                             okay_mother_to_hit_her_child : {okay_mother_to_hit_her_child},
                             okay_parents_hit_child_to_discipline : {okay_parents_hit_child_to_discipline}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "")

mother_to_hit_his_child_disagree <- father_to_hit_his_child_disagree %>% 
  mutate(i.check.name = "okay_mother_to_hit_her_child",
         i.check.current_value = okay_mother_to_hit_her_child
  )

df_c_logic_hit_child_disagree_but_agree_reasons <- bind_rows(father_to_hit_his_child_disagree, 
                                                             mother_to_hit_his_child_disagree) %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_hit_child_disagree_but_agree_reasons")){
  if(nrow(df_c_logic_hit_child_disagree_but_agree_reasons) > 0){
    logic_output$df_c_logic_hit_child_disagree_but_agree_reasons <- df_c_logic_hit_child_disagree_but_agree_reasons
  }
}
# parents_responsible_to_provide_child_8
df_c_logic_parents_responsible_to_provide_child_contradict <- df_tool_data %>% 
  filter((parents_responsible_to_provide_child_enough_food %in% c("strongly_agree", "agree") &
            parents_responsible_to_provide_all_child_needs %in% c("strongly_disagree", "disagree")) |
           (parents_responsible_to_provide_child_enough_food %in% c("strongly_disagree", "disagree") &
              parents_responsible_to_provide_all_child_needs %in% c("strongly_agree", "agree"))) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "parents_responsible_to_provide_child_enough_food",
         i.check.current_value = parents_responsible_to_provide_child_enough_food,
         i.check.value = "",
         i.check.issue_id = "parents_responsible_to_provide_child_8",
         i.check.issue = glue("parents_responsible_to_provide_child_enough_food: {parents_responsible_to_provide_child_enough_food},  
                              parents_responsible_to_provide_all_child_needs: {parents_responsible_to_provide_all_child_needs}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_parents_responsible_to_provide_child_contradict")){
  if(nrow(df_c_logic_parents_responsible_to_provide_child_contradict) > 0){
    logic_output$df_c_logic_parents_responsible_to_provide_child_contradict <- df_c_logic_parents_responsible_to_provide_child_contradict
  }
}

# child_labour_reponse_contradiction_13
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
         i.check.value = "",
         i.check.issue_id = "child_labour_reponse_contradiction_13",
         i.check.issue = glue("child_labour_economic_types: {child_labour_economic_types},
         work_type_children_involved_in_community: {work_type_children_involved_in_community}, 
                              frequency_child_involved_in_harsh_work: {frequency_child_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_child_labour_reponse_contradiction")){
  if(nrow(df_c_logic_child_labour_reponse_contradiction) > 0){
    logic_output$df_c_logic_child_labour_reponse_contradiction <- df_c_logic_child_labour_reponse_contradiction
  }
}
# work_type_children_involved_14
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
         i.check.issue = glue("work_type_children_involved_in_community: {work_type_children_involved_in_community},
                              frequency_child_involved_in_harsh_work: {frequency_child_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_work_type_children_involved")){
  if(nrow(df_c_logic_work_type_children_involved) > 0){
    logic_output$df_c_logic_work_type_children_involved <- df_c_logic_work_type_children_involved
  }
}
# action_taken_by_caretaker_for_child_harsh_work_15
df_c_logic_action_taken_by_caretaker_for_child_harsh_work <- df_tool_data %>% 
  filter(action_taken_by_caretaker_when_sees_child_doing_harsh_work %in% c("i_engage_the_child_protection_committees", "I_report_it_to_ngo_staff", 
                                                                           "i_report_it_to_rwc", "i_report_it_to_the_police"), 
         (child_labour_protection_services_sought == "no" | services_available_to_protect_child_from_harsh_labour == "no")
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "action_taken_by_caretaker_when_sees_child_doing_harsh_work",
         i.check.current_value = action_taken_by_caretaker_when_sees_child_doing_harsh_work,
         i.check.value = "",
         i.check.issue_id = "action_taken_by_caretaker_for_child_harsh_work_15",
         i.check.issue = glue("action_taken_by_caretaker_when_sees_child_doing_harsh_work: {action_taken_by_caretaker_when_sees_child_doing_harsh_work},
                              child_labour_protection_services_sought: {child_labour_protection_services_sought}, 
                              services_available_to_protect_child_from_harsh_labour: {services_available_to_protect_child_from_harsh_labour}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_action_taken_by_caretaker_for_child_harsh_work")){
  if(nrow(df_c_logic_action_taken_by_caretaker_for_child_harsh_work) > 0){
    logic_output$df_c_logic_action_taken_by_caretaker_for_child_harsh_work <- df_c_logic_action_taken_by_caretaker_for_child_harsh_work
  }
}
# protection_services_for_child_16
df_c_logic_protection_services_for_child_a <- df_tool_data %>% 
  filter(protection_services_for_child_violence == "no", protection_services_for_child_physical_harm == "yes"
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "protection_services_for_child_violence",
         i.check.current_value = protection_services_for_child_violence,
         i.check.value = "",
         i.check.issue_id = "protection_services_for_child_16",
         i.check.issue = glue("protection_services_for_child_violence: {protection_services_for_child_violence},
                              protection_services_for_child_physical_harm: {protection_services_for_child_physical_harm}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_protection_services_for_child_a")){
  if(nrow(df_c_logic_protection_services_for_child_a) > 0){
    logic_output$df_c_logic_protection_services_for_child_a <- df_c_logic_protection_services_for_child_a
  }
}
# protection_services_for_child_17
df_c_logic_protection_services_for_child <- df_tool_data %>% 
  filter(protection_services_for_child_violence == "yes", protection_services_for_child_physical_harm == "no"
  ) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "protection_services_for_child_violence",
         i.check.current_value = protection_services_for_child_violence,
         i.check.value = "",
         i.check.issue_id = "protection_services_for_child_17",
         i.check.issue = glue("protection_services_for_child_violence: {protection_services_for_child_violence},
                              protection_services_for_child_physical_harm: {protection_services_for_child_physical_harm}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_protection_services_for_child")){
  if(nrow(df_c_logic_protection_services_for_child) > 0){
    logic_output$df_c_logic_protection_services_for_child <- df_c_logic_protection_services_for_child
  }
}


# frequency_children_separate_from_parents_20
df_c_logic_frequency_children_separate_from_parents <- df_tool_data %>% 
  filter(current_giving_care_to_separated_children == "yes" &
           frequency_children_separate_from_parents == "none") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_separate_from_parents",
         i.check.current_value = frequency_children_separate_from_parents,
         i.check.value = "",
         i.check.issue_id = "frequency_children_separate_from_parents_20",
         i.check.issue = glue("current_giving_care_to_separated_children: {current_giving_care_to_separated_children},  
                              frequency_children_separate_from_parents: {frequency_children_separate_from_parents}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_children_separate_from_parents")){
  if(nrow(df_c_logic_frequency_children_separate_from_parents) > 0){
    logic_output$df_c_logic_frequency_children_separate_from_parents <- df_c_logic_frequency_children_separate_from_parents
  }
}
# frequency_unaccompanied_children_occurence_21
df_c_logic_frequency_unaccompanied_children_occurence <- df_tool_data %>% 
  filter(current_giving_care_to_unaccompanied_children == "yes" &
           frequency_unaccompanied_children_occurence == "none") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_unaccompanied_children_occurence",
         i.check.current_value = frequency_unaccompanied_children_occurence,
         i.check.value = "",
         i.check.issue_id = "frequency_unaccompanied_children_occurence_21",
         i.check.issue = glue("current_giving_care_to_unaccompanied_children: {current_giving_care_to_unaccompanied_children},  
                              frequency_unaccompanied_children_occurence: {frequency_unaccompanied_children_occurence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_unaccompanied_children_occurence")){
  if(nrow(df_c_logic_frequency_unaccompanied_children_occurence) > 0){
    logic_output$df_c_logic_frequency_unaccompanied_children_occurence <- df_c_logic_frequency_unaccompanied_children_occurence
  }
}
# frequency_of_child_violence_occurrence_22
df_c_logic_frequency_of_child_violence_occurrence <- df_tool_data %>% 
  filter(hh_child_experienced_violence == "yes" &
           frequency_of_child_violence_occurrence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_of_child_violence_occurrence",
         i.check.current_value = frequency_of_child_violence_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_of_child_violence_occurrence_22",
         i.check.issue = glue("hh_child_experienced_violence: {hh_child_experienced_violence},  
                              frequency_of_child_violence_occurrence: {frequency_of_child_violence_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_of_child_violence_occurrence")){
  if(nrow(df_c_logic_frequency_of_child_violence_occurrence) > 0){
    logic_output$df_c_logic_frequency_of_child_violence_occurrence <- df_c_logic_frequency_of_child_violence_occurrence
  }
}
# frequency_children_experience_sexual_violence_23
df_c_logic_frequency_children_experience_sexual_violence <- df_tool_data %>% 
  filter(children_involved_with_armed_groups == "yes" &
           frequency_children_experience_sexual_violence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_experience_sexual_violence",
         i.check.current_value = frequency_children_experience_sexual_violence,
         i.check.value = "",
         i.check.issue_id = "frequency_children_experience_sexual_violence_23",
         i.check.issue = glue("children_involved_with_armed_groups: {children_involved_with_armed_groups},  
                              frequency_children_experience_sexual_violence: {frequency_children_experience_sexual_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_children_experience_sexual_violence")){
  if(nrow(df_c_logic_frequency_children_experience_sexual_violence) > 0){
    logic_output$df_c_logic_frequency_children_experience_sexual_violence <- df_c_logic_frequency_children_experience_sexual_violence
  }
}
# frequency_child_involved_in_harsh_work_24
df_c_logic_frequency_child_involved_in_harsh_work <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("child_labour"), 
         frequency_child_involved_in_harsh_work == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_child_involved_in_harsh_work",
         i.check.current_value = frequency_child_involved_in_harsh_work,
         i.check.value = "",
         i.check.issue_id = "frequency_child_involved_in_harsh_work_24",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed},  
                              frequency_child_involved_in_harsh_work: {frequency_child_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_child_involved_in_harsh_work")){
  if(nrow(df_c_logic_frequency_child_involved_in_harsh_work) > 0){
    logic_output$df_c_logic_frequency_child_involved_in_harsh_work <- df_c_logic_frequency_child_involved_in_harsh_work
  }
}
# frequency_child_involved_in_harsh_work_25
df_c_logic_frequency_child_involved_in_harsh_work_b <- df_tool_data %>% 
  filter(!child_protection_risks_witnessed %in% c("child_labour"), 
         frequency_child_involved_in_harsh_work != "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_child_involved_in_harsh_work",
         i.check.current_value = frequency_child_involved_in_harsh_work,
         i.check.value = "",
         i.check.issue_id = "frequency_child_involved_in_harsh_work_25",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed},  
                              frequency_child_involved_in_harsh_work: {frequency_child_involved_in_harsh_work}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_child_involved_in_harsh_work_b")){
  if(nrow(df_c_logic_frequency_child_involved_in_harsh_work_b) > 0){
    logic_output$df_c_logic_frequency_child_involved_in_harsh_work_b <- df_c_logic_frequency_child_involved_in_harsh_work_b
  }
}
# frequency_children_experience_sexual_violence_26
df_c_logic_frequency_children_experience_sexual_violence <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("sexual_violence_and_exploitation"), 
         frequency_children_experience_sexual_violence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_experience_sexual_violence",
         i.check.current_value = frequency_children_experience_sexual_violence,
         i.check.value = "",
         i.check.issue_id = "frequency_children_experience_sexual_violence_26",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed},  
                              frequency_children_experience_sexual_violence: {frequency_children_experience_sexual_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_children_experience_sexual_violence")){
  if(nrow(df_c_logic_frequency_children_experience_sexual_violence) > 0){
    logic_output$df_c_logic_frequency_children_experience_sexual_violence <- df_c_logic_frequency_children_experience_sexual_violence
  }
}
# frequency_children_experience_sexual_violence_27
df_c_logic_frequency_children_experience_sexual_violence_b <- df_tool_data %>% 
  filter(!child_protection_risks_witnessed %in% c("sexual_violence_and_exploitation"), 
         frequency_children_experience_sexual_violence != "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_children_experience_sexual_violence",
         i.check.current_value = frequency_children_experience_sexual_violence,
         i.check.value = "",
         i.check.issue_id = "frequency_children_experience_sexual_violence_27",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed},  
                              frequency_children_experience_sexual_violence: {frequency_children_experience_sexual_violence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_children_experience_sexual_violence_b")){
  if(nrow(df_c_logic_frequency_children_experience_sexual_violence_b) > 0){
    logic_output$df_c_logic_frequency_children_experience_sexual_violence_b <- df_c_logic_frequency_children_experience_sexual_violence_b
  }
}
# frequency_unaccompanied_children_occurrence_28
df_c_logic_frequency_unaccompanied_children_occurrence <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("sexual_violence_and_exploitation"), 
         frequency_children_separate_from_parents == "none",
         frequency_unaccompanied_children_occurrence != "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_unaccompanied_children_occurrence",
         i.check.current_value = frequency_unaccompanied_children_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_unaccompanied_children_occurrence_28",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed},
                              frequency_children_separate_from_parents: {frequency_children_separate_from_parents},
                              frequency_unaccompanied_children_occurrence: {frequency_unaccompanied_children_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_unaccompanied_children_occurrence")){
  if(nrow(df_c_logic_frequency_unaccompanied_children_occurrence) > 0){
    logic_output$df_c_logic_frequency_unaccompanied_children_occurrence <- df_c_logic_frequency_unaccompanied_children_occurrence
  }
}
# frequency_unaccompanied_children_occurrence_29
df_c_logic_frequency_unaccompanied_children_occurrence <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("separation_from_family_members"), 
         (frequency_children_separate_from_parents != "none"|
            frequency_unaccompanied_children_occurrence != "none")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_unaccompanied_children_occurrence",
         i.check.current_value = frequency_unaccompanied_children_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_unaccompanied_children_occurrence_29",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed},
                              frequency_children_separate_from_parents: {frequency_children_separate_from_parents},
                              frequency_unaccompanied_children_occurrence: {frequency_unaccompanied_children_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_unaccompanied_children_occurrence")){
  if(nrow(df_c_logic_frequency_unaccompanied_children_occurrence) > 0){
    logic_output$df_c_logic_frequency_unaccompanied_children_occurrence <- df_c_logic_frequency_unaccompanied_children_occurrence
  }
}
# frequency_of_child_violence_occurrence_risk_witnessed_30
df_c_logic_frequency_of_child_violence_occurrence_risk <- df_tool_data %>% 
  filter(child_protection_risks_witnessed %in% c("physical_violence"),
         frequency_of_child_violence_occurrence == "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_of_child_violence_occurrence",
         i.check.current_value = frequency_of_child_violence_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_of_child_violence_occurrence_risk_witnessed_30",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed},  
                              frequency_of_child_violence_occurrence: {frequency_of_child_violence_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_of_child_violence_occurrence_risk")){
  if(nrow(df_c_logic_frequency_of_child_violence_occurrence_risk) > 0){
    logic_output$df_c_logic_frequency_of_child_violence_occurrence_risk <- df_c_logic_frequency_of_child_violence_occurrence_risk
  }
}
# frequency_of_child_violence_occurrence_risk_not_selected_31
df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected <- df_tool_data %>% 
  filter(!child_protection_risks_witnessed %in% c("physical_violence"),
         frequency_of_child_violence_occurrence != "never") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "frequency_of_child_violence_occurrence",
         i.check.current_value = frequency_of_child_violence_occurrence,
         i.check.value = "",
         i.check.issue_id = "frequency_of_child_violence_occurrence_risk_not_selected_31",
         i.check.issue = glue("child_protection_risks_witnessed: {child_protection_risks_witnessed},  
                              frequency_of_child_violence_occurrence: {frequency_of_child_violence_occurrence}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected")){
  if(nrow(df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected) > 0){
    logic_output$df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected <- df_c_logic_frequency_of_child_violence_occurrence_risk_not_selected
  }
}
# okay_parents_arrange_child_marriage_37
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
         i.check.issue = glue("okay_parents_arrange_child_marriage: {okay_parents_arrange_child_marriage},
                              okay_girl_less_18_years_get_married: {okay_girl_less_18_years_get_married}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_logic_okay_parents_arrange_child_marriage_mismatch")){
  if(nrow(df_c_logic_okay_parents_arrange_child_marriage_mismatch) > 0){
    logic_output$df_c_logic_okay_parents_arrange_child_marriage_mismatch <- df_c_logic_okay_parents_arrange_child_marriage_mismatch
  }
}
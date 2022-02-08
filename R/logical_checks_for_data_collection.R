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


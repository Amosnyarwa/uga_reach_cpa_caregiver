
# extract others checks ---------------------------------------------------

extract_other_data <- function(input_tool_data, input_survey, input_choices) {
  
  # add and rename some columns
  df_data <- input_tool_data %>% 
    rename(uuid = `_uuid`) %>% 
    mutate(start_date = as_date(start))
  
  # get questions with other
  others_colnames <-  df_data %>% 
    select(ends_with("_other"), -contains("/"), -c("okay_children_fight_each_other")) %>% 
    colnames()
  
  # data.frame for holding _other response data
  df_other_response_data <- data.frame()
  
  for (cln in others_colnames) {
    
    current_parent_qn = str_replace_all(string = cln, pattern = "_other", replacement = "")
    
    df_filtered_data <- df_data %>% 
      select(-contains("/")) %>% 
      select(uuid, start_date, enumerator_id, district_name, point_number, other_text = cln, current_value = current_parent_qn) %>% 
      filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>% 
      mutate( other_name = cln, 
              int.my_current_val_extract = ifelse(str_detect(current_value, "other\\b"), str_extract_all(string = current_value, pattern = "other\\b|[a-z]+._other\\b"), current_value),
              value = "",
              parent_qn = current_parent_qn)
    df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
  }
  
  # arrange the data
  df_data_arranged <- df_other_response_data %>% 
    arrange(start_date, uuid)
  
  # get choices to add to the _other responses extracted
  df_grouped_choices <- input_choices %>% 
    group_by(list_name) %>% 
    summarise(choice_options = paste(name, collapse = " : ")) %>% 
    arrange(list_name)
  
  # extract parent question and join survey for extracting list_name
  df_data_parent_qns <- df_data_arranged %>% 
    left_join(input_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
    rename(name = parent_qn)
  
  # join other responses with choice options based on list_name
  df_join_other_response_with_choices <- df_data_parent_qns %>% 
    left_join(df_grouped_choices, by = "list_name") %>% 
    mutate(issue_id = "other_checks",
           issue = "",
           checked_by = "",
           checked_date = as_date(today()),
           comment = "",
           reviewed = "",
           adjust_log = ""
    ) %>% 
    filter(str_detect(string = current_value, pattern = "other\\b|[a-z]+._other\\b"))
  
  # care for select_one and select_multiple (change_response, add_option, remove_option)
  output <- list()
  # select_one checks
  output$select_one <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_one|select one"))) %>% 
    mutate(type = "change_response")
  
  # select_multiple checks
  select_mu_data <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_multiple|select multiple")))
  
  select_mu_add_option <- select_mu_data %>% 
    mutate(type = "add_option")
  select_mu_remove_option <- select_mu_data %>% 
    mutate(type = "remove_option",
           value = as.character(int.my_current_val_extract))
  
  output$select_multiple <- bind_rows(select_mu_add_option, select_mu_remove_option) %>% 
    arrange(uuid, start_date, enumerator_id, name)
  
  # merge other checks
  merged_other_checks <- bind_rows(output) %>% 
    mutate(uuid_cl = "",
           so_sm_choices = choice_options) %>% 
    select(uuid,
           start_date,
           enumerator_id,
           district_name,
           point_number,
           type,
           name,
           current_value,
           value,
           issue_id,
           issue,
           other_text,
           checked_by,
           checked_date,
           comment,
           reviewed,
           adjust_log,
           uuid_cl,
           so_sm_choices)
}

extract_other_data_repeats <- function(input_repeat_data, input_survey, input_choices, input_sheet_name, input_repeat_cols) {
  
  # add and rename some columns
  df_data <- input_repeat_data %>% 
    rename(uuid = `_uuid`) %>% 
    filter(!is.na(start)) %>% 
    mutate(start_date = as_date(start))
  
  
  # get questions with other
  others_colnames <-  df_data %>% 
    select(starts_with(input_repeat_cols)) %>% 
    select(ends_with("_other"), -contains("/")) %>% 
    colnames()
  
  # data.frame for holding _other response data
  df_other_response_data <- data.frame()
  
  for (cln in others_colnames) {
    
    current_parent_qn = str_replace_all(string = cln, pattern = "_other", replacement = "")
    
    df_filtered_data <- df_data %>% 
      select(-contains("/")) %>% 
      select(uuid, start_date, enumerator_id, district_name, point_number, 
             other_text = cln, current_value = current_parent_qn, index = `_index.y`) %>% 
      filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>% 
      mutate( other_name = cln, 
              int.my_current_val_extract = ifelse(str_detect(current_value, "other\\b"), str_extract_all(string = current_value, pattern = "other\\b|[a-z]+._other\\b"), current_value),
              value = "",
              parent_qn = current_parent_qn)
    df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
  }
  
  # arrange the data
  df_data_arranged <- df_other_response_data %>% 
    arrange(start_date, uuid)
  
  # get choices to add to the _other responses extracted
  df_grouped_choices <- input_choices %>% 
    group_by(list_name) %>% 
    summarise(choice_options = paste(name, collapse = " : ")) %>% 
    arrange(list_name)
  
  # extract parent question and join survey for extracting list_name
  df_data_parent_qns <- df_data_arranged %>% 
    left_join(input_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
    rename(name = parent_qn)
  
  # join other responses with choice options based on list_name
  df_join_other_response_with_choices <- df_data_parent_qns %>% 
    left_join(df_grouped_choices, by = "list_name") %>% 
    mutate(issue_id = "other_checks",
           issue = "",
           sheet = input_sheet_name,
           checked_by = "",
           checked_date = as_date(today()),
           comment = "",
           reviewed = "",
           adjust_log = ""
    ) %>% 
    filter(str_detect(string = current_value, pattern = "other\\b|[a-z]+._other\\b"))
  
  # care for select_one and select_multiple (change_response, add_option, remove_option)
  output <- list()
  # select_one checks
  output$select_one <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_one|select one"))) %>% 
    mutate(type = "change_response")
  
  # select_multiple checks
  select_mu_data <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_multiple|select multiple")))
  
  select_mu_add_option <- select_mu_data %>% 
    mutate(type = "add_option")
  select_mu_remove_option <- select_mu_data %>% 
    mutate(type = "remove_option",
           value = as.character(int.my_current_val_extract))
  
  output$select_multiple <- bind_rows(select_mu_add_option, select_mu_remove_option) %>% 
    arrange(uuid, start_date, enumerator_id, name)
  
  # merge other checks
  merged_other_checks <- bind_rows(output) %>% 
    mutate(uuid_cl = "",
           so_sm_choices = choice_options) %>% 
    select(sheet,
           uuid,
           start_date,
           enumerator_id,
           district_name,
           point_number,
           type,
           name,
           current_value,
           value,
           index,
           issue_id,
           issue,
           other_text,
           checked_by,
           checked_date,
           comment,
           reviewed,
           adjust_log,
           uuid_cl,
           so_sm_choices)
}


# duplicate uuid ----------------------------------------------------------

# check uuids of surveys for duplicates
check_duplicates_by_uuid <- function(input_tool_data) {
  input_tool_data %>% 
    group_by(i.check.uuid) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank > 1) %>%  
    mutate(
      i.check.type = "remove_survey",
      i.check.name = "point_number",
      i.check.current_value = "",
      i.check.value = "",
      i.check.issue_id = "duplicate_uuid",
      i.check.issue = "The uuid: {i.check.uuid} is duplicate in the data",
      i.check.other_text = "",
      i.check.checked_by = "",
      i.check.checked_date = as_date(today()),
      i.check.comment = "", 
      i.check.reviewed = "",
      i.check.adjust_log = "",
      i.check.uuid_cl = "",
      i.check.so_sm_choices = "")%>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# survey time check -------------------------------------------------------

# check survey time against expected minimum time and maximum time of the survey
check_survey_time <- function(input_tool_data, input_min_time, input_max_time) {
  input_tool_data %>% 
    mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
           int.survey_time_interval = ceiling(int.survey_time_interval),
           i.check.type = "remove_survey",
           i.check.name = "point_number",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = case_when(
             int.survey_time_interval < input_min_time ~ "less_survey_time",
             int.survey_time_interval > input_max_time ~ "more_survey_time",
             TRUE ~ "normal_survey_time" ),
           i.check.issue = glue("{int.survey_time_interval} min taken to do the survey"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "")%>% 
    filter(i.check.issue_id %in% c("less_survey_time", "more_survey_time")) %>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# check interval between surveys by the same enumerator
check_time_interval_btn_surveys <- function(input_tool_data, input_min_time) {
  input_tool_data %>% 
    group_by(i.check.start_date, i.check.enumerator_id) %>%
    filter(n()>1) %>% 
    arrange(start, .by_group = TRUE) %>%
    mutate(int.time_between_survey = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
           int.time_between_survey = ceiling(int.time_between_survey)) %>%
    filter(int.time_between_survey != 0 & int.time_between_survey < input_min_time) %>%
    mutate(i.check.type = "remove_survey",
           i.check.name = "point_number",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "less_time_btn_surveys",
           i.check.issue = glue("{int.time_between_survey} min taken between surveys"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}


# Outliers ----------------------------------------------------------------
check_outliers <- function(input_tool_data, input_column, input_lower_limit, input_upper_limit) {
  input_tool_data %>% 
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = input_column,
           i.check.current_value = as.character(!!sym({{input_column}})),
           i.check.value = "",
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(input_column,": ",!!sym({{input_column}}), "seems to be an outlier, needs engagement with enumerator"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}
check_outliers_repeats <- function(input_tool_data, input_column, input_lower_limit, input_upper_limit, input_sheet_name) {
  input_tool_data %>% 
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>% 
    mutate(i.check.sheet = input_sheet_name,
           i.check.type = "change_response",
           i.check.name = input_column,
           i.check.current_value = as.character(!!sym({{input_column}})),
           i.check.value = "",
           i.check.index = `_index.y`,
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(input_column,": ",!!sym({{input_column}}), "seems to be an outlier, needs engagement with enumerator"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# short path checks -------------------------------------------------------

check_shortest_path <- function(input_tool_data) {
  input_tool_data %>% 
    mutate(
      int.test_path_children_biological_parent = ifelse(children_biological_parent %in% c("no"), 1, 0),
      int.test_path_suffering_chronic_or_disability = ifelse(suffering_chronic_or_disability %in% c("no"), 1, 0),
      int.test_path_child_protection_risks_witnessed = ifelse(child_protection_risks_witnessed %in% c("no_particular_risk", "no_answer"), 1, 0),
      int.test_path_children_perform_domestic_chores = ifelse(children_perform_domestic_chores %in% c("no", "no_answer"), 1, 0),
      int.test_path_children_perform_econ_labour = ifelse(children_perform_econ_labour %in% c("no", "no_answer"), 1, 0),
      int.test_path_frequency_child_involved_in_harsh_work = ifelse(frequency_child_involved_in_harsh_work %in% c("never"), 1, 0),
      int.test_path_services_availiable_to_protect_child_from_harsh_labour = ifelse(services_availiable_to_protect_child_from_harsh_labour %in% c("no", "no_answer"), 1, 0),
      int.test_path_frequency_children_experience_sexual_violence = ifelse(frequency_children_experience_sexual_violence %in% c("never"), 1, 0),
      int.test_path_services_for_proctecting_child_against_sexual_violence = ifelse(services_for_proctecting_child_against_sexual_violence %in% c("no", "no_answer"), 1, 0),
      int.test_path_frequency_children_separate_from_parents = ifelse(frequency_children_separate_from_parents %in% c("none"), 1, 0),
      int.test_path_frequency_unaccompanied_children_occurrence = ifelse(frequency_unaccompanied_children_occurrence %in% c("none"), 1, 0),
      int.test_path_protection_services_for_separated_and_unaccompanied_children = ifelse(protection_services_for_separated_and_unaccompanied_children %in% c("no", "no_answer"), 1, 0),
      int.test_path_protection_services_for_child_violence = ifelse(protection_services_for_child_violence %in% c("no", "no_answer"), 1, 0),
      int.test_path_other_risks_children_face_in_community = ifelse(other_risks_children_face_in_community %in% c("none"), 1, 0),
      int.test_path_protection_services_for_child_physical_harm = ifelse(protection_services_for_child_physical_harm %in% c("no", "no_answer"), 1, 0),
      int.test_path_protection_services_for_caregivers = ifelse(protection_services_for_caregivers %in% c("no", "no_answer"), 1, 0)
    ) %>% 
    rowwise() %>% 
    mutate(i.total_path_checks = sum(across(starts_with("int.test_path")), na.rm = TRUE)) %>%
    ungroup() %>%
    filter(i.total_path_checks >= 10) %>%
    mutate(
      i.check.type = "remove_survey",
      i.check.name = "",
      i.check.current_value = "",
      i.check.value = "",
      i.check.issue_id = "logic_c_short_path",
      i.check.issue = glue("Detected : {i.total_path_checks} tests, out of 15 tests checked."),
      i.check.other_text = "",
      i.check.checked_by = "",
      i.check.checked_date = as_date(today()),
      i.check.comment = "", 
      i.check.reviewed = "",
      i.check.adjust_log = "",
      i.check.uuid_cl = "",
      i.check.so_sm_choices = "") %>%
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# spatial checks ----------------------------------------------------------

# check for duplicate point numbers
check_duplicate_pt_numbers <- function(input_tool_data, input_sample_pt_nos_list) {
  input_tool_data %>% 
    mutate(unique_pt_number = paste0(status, "_", point_number )) %>% 
    group_by(i.check.district_name, status, i.check.point_number) %>% 
    filter(n() > 1, unique_pt_number %in% input_sample_pt_nos_list) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "point_number",
           i.check.current_value = point_number,
           i.check.value = "",
           i.check.issue_id = "spatial_c_duplicate_pt_no",
           i.check.issue = glue("point_number: {point_number} is duplicated: check that its not a repeated survey"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# check for point number not being in samples
check_pt_number_not_in_samples <- function(input_tool_data, input_sample_pt_nos_list) {
  input_tool_data %>% 
    mutate(unique_pt_number = paste0(status, "_", point_number )) %>% 
    filter(!unique_pt_number %in% input_sample_pt_nos_list) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "point_number",
           i.check.current_value = point_number,
           i.check.value = "",
           i.check.issue_id = "spatial_c_pt_no_not_in_sample",
           i.check.issue = glue("point_number: {point_number} not in samples"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# check that collected point is not at a distance greater than the threshold 
check_threshold_distance <- function(input_sample_data, input_tool_data, input_threshold_dist) {
  df_sample_data_thresh <- input_sample_data %>% 
    mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
    sf::st_transform(4326)
  
  df_tool_data_thresh <- input_tool_data %>% 
    mutate(unique_pt_number = paste0(status, "_", point_number)) %>% 
    sf::st_as_sf(coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326)
  
  # sample_data_unique_pts
  sample_data_unique_pts <- df_sample_data_thresh %>%  
    pull(unique_pt_number) %>% 
    unique()
  # tool_data_unique_pts
  tool_data_unique_pts <- df_tool_data_thresh %>% 
    pull(unique_pt_number) %>% 
    unique()
  
  sample_pt_nos_thresh <- sample_data_unique_pts[sample_data_unique_pts %in% tool_data_unique_pts]
  
  if(length(sample_pt_nos_thresh) > 0){
    
    # tibble to hold the data
    df_data_with_distance <- tibble()
    
    for (pt_number in sample_pt_nos_thresh){
      current_sample <- df_sample_data_thresh %>% 
        filter(unique_pt_number == pt_number)
      current_tool_data <- df_tool_data_thresh %>% 
        filter(unique_pt_number == pt_number) 
      
      if(nrow(current_tool_data) > 0){
        current_sample_target_dist <- sf::st_distance(x = current_sample, y = current_tool_data, by_element = TRUE)
        
        current_data_with_dist <- current_tool_data %>% 
          sf::st_drop_geometry() %>% 
          mutate(distance = round(x = current_sample_target_dist, digits = 0))
        
        df_data_with_distance <- bind_rows(df_data_with_distance, current_data_with_dist)
      }
    }
    
    # format the required data
    df_data_with_distance %>% 
      filter(as.numeric(distance) >= input_threshold_dist) %>% 
      mutate(i.check.type = "remove_survey",
             i.check.name = "point_number",
             i.check.current_value = point_number,
             i.check.value = "",
             i.check.issue_id = "spatial_c_dist_to_sample_greater_than_threshold",
             i.check.issue = glue("{distance} m greater_than_threshold:{input_threshold_dist} m"),
             i.check.other_text = "",
             i.check.checked_by = "",
             i.check.checked_date = as_date(today()),
             i.check.comment = "", 
             i.check.reviewed = "",
             i.check.adjust_log = "",
             i.check.uuid_cl = "",
             i.check.so_sm_choices = "") %>% 
      dplyr::select(starts_with("i.check"))%>% 
      rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
  }
}


# other support functions -------------------------------------------------

# get date to prefix file names

last_date_in_tool_data <- function(input_tool_data) {
  if("start" %in% colnames(input_tool_data)){
    input_tool_data %>%
      mutate(start_date = as_date(start)) %>% 
      select(start_date) %>% 
      unique() %>% 
      arrange(start_date) %>% 
      last() %>% 
      str_replace_all(pattern = "-", replacement = "_")
  } else{
    message(str_glue("no column of 'start' in the dataset, used the system date: ", format(Sys.Date(), '%Y_%m_%d')))
    
    format(Sys.Date(), '%Y_%m_%d')
  }
  
}

# add checks data to a list of checks  ------------------------------------

add_checks_data_to_list <- function(input_list_name, input_df_name) {
  if(exists(input_list_name) & exists(input_df_name)){
    # get the current values of these objects
    global_list_data <- get(input_list_name, envir = .GlobalEnv)
    global_df_data <- get(input_df_name, envir = .GlobalEnv)
    # check if the dataframe of interest has data
    if(nrow(global_df_data) > 0){
      # append the data frame to the list
      global_list_data[[input_df_name]] <-  global_df_data
      # assign the data to the global environment
      assign(x = input_list_name, value = global_list_data, envir = .GlobalEnv)
    }
  } else{
    message("given objects not in the global environment.")
  }
}

# reuse cleaning steps to handle different datasets -----------------------

implement_cleaning_support <- function(input_df_raw_data, input_df_survey, input_df_choices, input_df_cleaning_log, input_post_fix) {
  
  # find all new choices to add to choices sheet
  
  # gather choice options based on unique choices list
  df_grouped_choices<- input_df_choices %>% 
    group_by(list_name) %>% 
    summarise(choice_options = paste(name, collapse = " : "))
  
  # get new name and choice pairs to add to the choices sheet
  new_vars <- input_df_cleaning_log %>% 
    filter(type %in% c("change_response", "add_option")) %>% 
    left_join(input_df_survey, by = "name") %>% 
    filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
    separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
    left_join(df_grouped_choices, by = "list_name") %>%
    filter(!str_detect(string = choice_options, pattern = value ) ) %>%
    rename(choice = value ) %>%
    select(name, choice) %>%
    distinct() %>% # to make sure there are no duplicates
    arrange(name)
  
  # create kobold object
  
  kbo <- kobold::kobold(survey = input_df_survey, 
                        choices = input_df_choices, 
                        data = input_df_raw_data, 
                        cleaning = input_df_cleaning_log)
  
  # modified choices for the survey tool
  df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)
  
  # special treat for variables for select_multiple, we need to add the columns to the data itself
  df_survey_sm <- input_df_survey %>% 
    mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                              str_detect(string = type, pattern = "select_one|select one") ~ "so",
                              TRUE ~ type)) %>% 
    select(name, q_type)
  
  # construct new columns for select multiple
  new_vars_sm <- new_vars %>% 
    left_join(df_survey_sm, by = "name") %>% 
    filter(q_type == "sm") %>% 
    mutate(new_cols = paste0(name,"/",choice))
  
  # add new columns to the raw data
  df_raw_data_modified <- input_df_raw_data %>% 
    butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )
  
  # make some cleanup
  kbo_modified <- kobold::kobold(survey = input_df_survey %>% filter(name %in% colnames(df_raw_data_modified)), 
                                 choices = df_choises_modified, 
                                 data = df_raw_data_modified, 
                                 cleaning = input_df_cleaning_log)
  kbo_cleaned <- kobold::kobold_cleaner(kbo_modified)
  
  # handling Personally Identifiable Information(PII)
  input_vars_to_remove_from_data <- c("complainant_name",
                                      "complainant_id",
                                      "respondent_telephone",
                                      "name_pers_recording",
                                      "geopoint",
                                      "_geopoint_latitude",
                                      "_geopoint_longitude",
                                      "_geopoint_altitude",
                                      "_geopoint_precision")
  
  df_handle_pii <- kbo_cleaned$data %>% 
    mutate(across(any_of(input_vars_to_remove_from_data), .fns = ~na_if(., .)))
  
  # handling added responses after starting data collection and added responses in the cleaning process
  
  sm_colnames <-  df_handle_pii %>% 
    select(contains("/")) %>% 
    colnames() %>% 
    str_replace_all(pattern = "/.+", replacement = "") %>% 
    unique()
  
  df_handle_sm_data <- df_handle_pii
  
  for (cur_sm_col in sm_colnames) {
    df_updated_data <- df_handle_sm_data %>% 
      mutate(
        across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , FALSE, .)),
        across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA, .))
      )
    df_handle_sm_data <- df_updated_data
  }
  
  df_final_cleaned_data <- df_handle_sm_data
}

# analysis support --------------------------------------------------------

analysis_support <- function(input_df_cleaned, input_df_ref_pop, input_df_host_pop, input_dap) {
  
  # make composite indicator ------------------------------------------------
  
  df_with_composites <- create_composite_indicators_cpa_caregiver(input_df = input_df_cleaned) %>% 
    mutate(strata = case_when(status == "refugee" ~ paste0(i.refugee_settlement, "_refugee"),
                              status == "host_community" ~ paste0(i.region,"_host"),
                              TRUE ~ status
    ))
  
  # split data into host and refugee
  
  df_ref <- df_with_composites %>% 
    filter(status == "refugee")
  
  df_host <- df_with_composites %>% 
    filter(status == "host_community")
  
  # create weights
  
  # refugee weights
  ref_weight_table <- make_refugee_weight_table(input_df_ref = df_ref, 
                                                input_refugee_pop = input_df_ref_pop)
  df_ref_with_weights <- df_ref %>% 
    left_join(ref_weight_table, by = "strata")
  
  # host weights
  host_weight_table <- make_host_weight_table(input_df_host = df_host, 
                                              input_host_pop = input_df_host_pop)
  df_host_with_weights <- df_host %>% 
    left_join(host_weight_table, by = "strata")
  
  # set up design objects
  
  ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights )
  host_svy <- as_survey(.data = df_host_with_weights, strata = strata, weights = weights )
  
  # store analyses
  outputs <-list()
  
  # refugee -----------------------------------------------------------------
  
  dap_refugee <- input_dap %>% 
    filter(split %in% c("all", "refugee_only"))
  
  # no subsets
  refugee_variables_no_subsets <- dap_refugee %>% 
    pull(variable) %>% unique()
  
  # refugee overall, no additional subset
  outputs$ref_overall <- butteR::survey_collapse(df = ref_svy,
                                                 vars_to_analyze = refugee_variables_no_subsets) %>% 
    mutate(population = "refugee")
  
  #  subsets
  dap_refugee_subset1 <- input_dap %>%
    filter( split %in%  c("all","refugee_only"), !is.na(subset_1))
  
  # refugee overall, subset 1
  dap_refugee_subset_split <- dap_refugee_subset1 %>%
    split(.$subset_1)
  
  ref_overall_subset1 <-list()
  
  for(i in seq_along(dap_refugee_subset_split)){
    print(i)
    subset_temp <- dap_refugee_subset_split[[i]]
    subset_value <- unique(subset_temp$subset_1)
    vars_temp <- subset_temp %>% pull(variable)
    ref_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = ref_svy,
                                                                   vars_to_analyze = vars_temp ,
                                                                   disag = c( subset_value)
    )
  }
  
  outputs$ref_overall_subset1 <- bind_rows(ref_overall_subset1) %>%
    mutate(population = "refugee")
  
  # host -----------------------------------------------------------------
  
  dap_host <- input_dap %>%
    filter(split %in%  c("all", "host_only"))
  
  # no subsets
  host_variables_no_subsets <- dap_host %>%
    pull(variable) %>% unique()
  
  # host overall, no additional subset
  outputs$host_overall <- butteR::survey_collapse(df = host_svy,
                                                  vars_to_analyze = host_variables_no_subsets ) %>%
    mutate(population = "host")
  
  # subsets
  dap_host_subset1 <- input_dap %>%
    filter( split %in%  c("all", "host_only"), !is.na(subset_1))
  
  dap_host_subset_split <- dap_host_subset1 %>%
    split(.$subset_1)
  
  # host overall, subset 1
  
  host_overall_subset1 <- list()
  
  for(i in seq_along(dap_host_subset_split)){
    print(i)
    subset_temp <- dap_host_subset_split[[i]]
    subset_value <- unique(subset_temp$subset_1)
    vars_temp <- subset_temp %>% pull(variable)
    host_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = host_svy,
                                                                    vars_to_analyze = vars_temp ,
                                                                    disag = c(subset_value)
    )
  }
  
  outputs$host_subset1 <- bind_rows(host_overall_subset1) %>%
    mutate(population = "host")
  
  # merge analysis ----------------------------------------------------------
  
  bind_rows(outputs)
}
analysis_support_after_survey_creation <- function(input_ref_svy, input_host_svy, input_dap) {
  
  # store analyses
  outputs <-list()
  
  # refugee -----------------------------------------------------------------
  
  dap_refugee <- input_dap %>% 
    filter(split %in% c("all", "refugee_only"))
  
  # no subsets
  refugee_variables_no_subsets <- dap_refugee %>% 
    pull(variable) %>% unique()
  
  # refugee overall, no additional subset
  outputs$ref_overall <- butteR::survey_collapse(df = input_ref_svy,
                                                 vars_to_analyze = refugee_variables_no_subsets) %>% 
    mutate(population = "refugee")
  
  #  subsets
  dap_refugee_subset1 <- input_dap %>%
    filter(subset_1 != "i.location_type", split %in%  c("all","refugee_only"), !is.na(subset_1))
  
  # refugee overall, subset 1
  dap_refugee_subset_split <- dap_refugee_subset1 %>%
    split(.$subset_1)
  
  ref_overall_subset1 <-list()
  
  for(i in seq_along(dap_refugee_subset_split)){
    print(i)
    subset_temp <- dap_refugee_subset_split[[i]]
    subset_value <- unique(subset_temp$subset_1)
    vars_temp <- subset_temp %>% pull(variable)
    ref_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = input_ref_svy,
                                                                   vars_to_analyze = vars_temp ,
                                                                   disag = c( subset_value)
    )
  }
  
  outputs$ref_overall_subset1 <- bind_rows(ref_overall_subset1) %>%
    mutate(population = "refugee")
  
  # host -----------------------------------------------------------------
  
  dap_host <- input_dap %>%
    filter(split %in%  c("all", "host_only"))
  
  # no subsets
  host_variables_no_subsets <- dap_host %>%
    pull(variable) %>% unique()
  
  # host overall, no additional subset
  outputs$host_overall <- butteR::survey_collapse(df = input_host_svy,
                                                  vars_to_analyze = host_variables_no_subsets ) %>%
    mutate(population = "host")
  
  # subsets
  dap_host_subset1 <- input_dap %>%
    filter(subset_1 != "i.location_type", split %in%  c("all", "host_only"), !is.na(subset_1))
  
  dap_host_subset_split <- dap_host_subset1 %>%
    split(.$subset_1)
  
  # host overall, subset 1
  
  host_overall_subset1 <- list()
  
  for(i in seq_along(dap_host_subset_split)){
    print(i)
    subset_temp <- dap_host_subset_split[[i]]
    subset_value <- unique(subset_temp$subset_1)
    vars_temp <- subset_temp %>% pull(variable)
    host_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = input_host_svy,
                                                                    vars_to_analyze = vars_temp ,
                                                                    disag = c(subset_value)
    )
  }
  
  outputs$host_subset1 <- bind_rows(host_overall_subset1) %>%
    mutate(population = "host")
  
  # merge analysis ----------------------------------------------------------
  
  bind_rows(outputs)
}
analysis_support_mofification_kampala <- function(input_df_cleaned, input_dap, input_dataset = "main_dataset") {
  
  # make composite indicator ------------------------------------------------
  if (input_dataset == "main_dataset") {
    df_with_composites <- create_composite_indicators_cpa_caregiver(input_df = input_df_cleaned)
  }else{
    df_with_composites <- create_composite_indicators_cpa_caregiver_repeats(input_df = input_df_cleaned) 
  }
  
  # split data into host and refugee
  
  df_ref <- df_with_composites %>% 
    filter(status == "refugee")
  
  # set up design objects
  
  ref_svy <- as_survey(.data = df_ref)
  
  # store analyses
  outputs <-list()
  
  # refugee -----------------------------------------------------------------
  
  dap_refugee <- input_dap %>% 
    filter(subset_1 == "i.location_type", split %in% c("all", "refugee_only"))
  
  # no subsets
  refugee_variables_no_subsets <- dap_refugee %>% 
    pull(variable) %>% unique()
  
  # refugee overall, no additional subset
  outputs$ref_overall <- butteR::survey_collapse(df = ref_svy,
                                                 vars_to_analyze = refugee_variables_no_subsets) %>% 
    mutate(population = "refugee_kampala")
  
  #  subsets
  dap_refugee_subset1 <- input_dap %>%
    filter(subset_1 == "i.location_type", split %in%  c("all","refugee_only"), !is.na(subset_1))
  
  # refugee overall, subset 1
  dap_refugee_subset_split <- dap_refugee_subset1 %>%
    split(.$subset_1)
  
  ref_overall_subset1 <-list()
  
  for(i in seq_along(dap_refugee_subset_split)){
    print(i)
    subset_temp <- dap_refugee_subset_split[[i]]
    subset_value <- unique(subset_temp$subset_1)
    vars_temp <- subset_temp %>% pull(variable)
    ref_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = ref_svy,
                                                                   vars_to_analyze = vars_temp ,
                                                                   disag = c( subset_value)
    )
  }
  
  outputs$ref_overall_subset1 <- bind_rows(ref_overall_subset1) %>%
    mutate(population = "refugee_kampala")
  
  # merge analysis ----------------------------------------------------------
  
  bind_rows(outputs)
}

create_union_counts_by_group <- function(microdata, grouping_var) {
  microdata %>% 
    select(year, month, union, orgwgt, all_of(grouping_var)) %>% 
    filter(union >= 0) %>% 
    summarize(
      union_count = sum(orgwgt*union), 
      org_count = sum(orgwgt),
      .by = c(year, month, all_of(grouping_var))
    ) %>% 
    group_by(group_num = .data[[grouping_var]]) %>% 
    arrange(group_num, year, month) %>% 
    mutate(across(
      union_count|org_count,
      ~ slide_sum(.x, before = 11, complete = TRUE) / 12
    )) %>% 
    ungroup() %>% 
    mutate(
      union_rate = union_count / org_count,
      group_label = as.character(as_factor(group_num)),
      group_var = grouping_var,
      month_date = ym(paste(year, month))
    ) %>% 
    select(
      year, 
      month,
      month_date,
      starts_with("group_"), 
      union_count, 
      union_rate
    ) %>% 
    zap_labels()
}

create_counts <- function(microdata) {
  groups <- c(
    "all", 
    "female", 
    "wbhao", 
    "educ",
    "age_group",
    "region",
    "public",
    "mind03",
    "mocc03",
    "rtw_state"
  )

  base_data <- microdata
  
  groups %>% 
    map(~ create_union_counts_by_group(base_data, .x)) %>% 
    list_rbind() %>% 
    mutate(group_var_legible = case_when(
      group_var == "all" ~ "All workers",
      group_var == "female" ~ "Gender",
      group_var == "wbhao" ~ "Race and ethnicity",
      group_var == "educ" ~ "Education",
      group_var == "age_group" ~ "Age group",
      group_var == "region" ~ "Region",
      group_var == "public" ~ "Private/public sector",
      group_var == "mind03" ~ "Industry",
      group_var == "mocc03" ~ "Occupation",
      group_var == "statefips" ~ "State",
      group_var == "firmsize" ~ "Firm size",
      group_var == "rtw_state" ~ "Right-to-work"
    ))
}

pull_latest_counts <- function(historical_count_data) {
  max_date <- historical_count_data %>% 
    filter(month_date == max(month_date)) %>% 
    mutate(data_month_end = format(month_date, "%B %Y")) %>% 
    mutate(data_month_begin = format(month_date - months(11), "%B %Y")) %>% 
    select(-year, -month, -month_date) %>% 
    mutate(union_count = round(union_count / 1000) * 1000) %>% 
    mutate(group_var_order = case_when(
      group_var == "all" ~ 0,
      .default = 1
    )) %>% 
    arrange(group_var_order, group_var_legible, group_label) %>% 
    select(-group_var_order)
}
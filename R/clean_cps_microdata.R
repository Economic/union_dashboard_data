create_org_microdata <- function(version) {
  
  org_vars <- c(
    "year", "month", "orgwgt", "wage", "wageotc", "paidhre", "hoursu1i",
    "a_earnhour", "a_weekpay", 
    "union", "unmem",
    "statefips", "region",
    "age", "wbhao", "wbho", "female", "educ", "citizen", "married",
    "mind03", "mocc03", "dind03", "docc03", "pubsec"
  )
  
  state_is_rtw <- c("AL", "AZ", "AR", "FL", "GA", "ID", "IN", "IA", "KS", "KY", "LA", "MS", "NE", "NV", "NC", "ND", "OK", "SC", "SD", "TN", "TX", "UT", "VA", "WV", "WI", "WY")
  
  output <- load_org(1979:2023, all_of(org_vars)) %>% 
    mutate(age_group = case_when(
      age >= 16 & age <= 24 ~ 1,
      age >= 25 & age <= 34 ~ 2,
      age >= 35 & age <= 44 ~ 3,
      age >= 45 & age <= 54 ~ 4,
      age >= 55 & age <= 64 ~ 5,
      age >= 65 ~ 6
    )) %>%
    mutate(new_wage = case_when(
      a_earnhour == 1 & paidhre == 1 ~ NA,
      a_weekpay == 1 & paidhre == 0 ~ NA,
      .default = wage
    )) %>% 
    mutate(new_wageotc = case_when(
      a_earnhour == 1 & paidhre == 1 ~ NA,
      a_weekpay == 1 & paidhre == 0 ~ NA,
      .default = wageotc
    )) %>%
    mutate(log_wage = log(new_wage), log_wageotc = log(new_wageotc)) %>% 
    mutate(part_time = hoursu1i < 35) %>% 
    # new groups
    mutate(all = 1) %>% 
    mutate(all = labelled(all, c("All workers" = 1))) %>% 
    mutate(age_group = case_when(
      age >= 16 & age <= 24 ~ 1,
      age >= 25 & age <= 34 ~ 2,
      age >= 35 & age <= 44 ~ 3,
      age >= 45 & age <= 54 ~ 4,
      age >= 55 & age <= 64 ~ 5,
      age >= 65 ~ 6
    )) |> 
    mutate(age_group = labelled(age_group , c(
      "Ages 16-24" = 1,
      "Ages 25-34" = 2,
      "Ages 35-44" = 3,
      "Ages 45-54" = 4,
      "Ages 55-64" = 5,
      "Ages 65 and above" = 6
    ))) %>% 
    mutate(rtw_state = if_else(
      as_factor(statefips) %in% state_is_rtw, 1, 0
    )) %>% 
    mutate(rtw_state = labelled(rtw_state, c(
      "Right-to-work state" = 1,
      "Non-right-to-work state" = 0
    ))) %>% 
    mutate(public = case_when(
      pubsec == 1 ~ 1,
      pubsec == 0 ~ 0
    )) %>% 
    mutate(public = labelled(public, c(
      "Government employee" = 1,
      "Private-sector employee" = 0
    ))) 
  
  attr(output, "data_version") <- version
  
  output
}
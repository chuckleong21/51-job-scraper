
# library -----------------------------------------------------------------

# source("scraper.R")
# box::use(magrittr[`%>%`])
library(dplyr)
library(tidyr)
library(stringr)
library(tidyjson)
library(purrr)


# api data ----------------------------------------------------------------

# purrr::walk(21:50, \(i) {
#   request_search(
#     keyword = "数据整理", 
#     page = i, 
#     requestId = "11ca3b6be89336cabfb0b3973bb131a2", 
#     u_atoken = "b0fd7093-a732-4a17-b307-d709edc30504", 
#     u_asession = "010uvbJzhm8kdD8v3MFLyXu76N4r0kXWK9tT3SKGjRGIhptS4OiMYzJ1gT5kph0wMHnKb5V5-_BiB3wy2M4YhPmdsq8AL43dpOnCClYrgFm6o", 
#     u_asig = "05ESJ0rAXmqHXPIepLzgwTZtF77tuO3Ij6DvCvAcSejG3kaGxUAiHXU25-uD6fgsYqiIDa4EvnJyF2h3l53DoWwqJXC1gP0JKC4EF18PfEmybYg5-UR1idY2kcKKnX3FnzQV2oFKKfh6XK6RP73gCx-SH24tedK3aDLQe-zG0ytLM68J5bTZtjQM3exlmvljQbksmHjM0JOodanL5-M1Qs1XCNx9iw6fN_d9sjrxi2j2ZKDRwcU3GREuSn6U41wDORhDvF_kEq5dysXxlAm3ST_NFnvfZUDYz8N5B6G3P4uM7UpLHxH1iRKZmnjAu0Zefw", 
#     u_aref = "IzqYoqSFRtwgVAF9puL%2Bd4NuuRE%3D"
#   ) %>% 
#     drv$edge$get()
#   Sys.sleep(0.5)
#   pass_captcha()
#   json_save(name = paste0("json_result.page.", i))
# }, .progress = TRUE)


# company info ------------------------------------------------------------

jobs <- map(seq_len(50), \(i) {
  tidyjson::read_json(glue::glue("json/json_result.page.{i}.json")) %>% 
    enter_object("resultbody", "job", "items") %>% 
    gather_array("itemId") %>% 
    spread_all()
}) %>% 
  list_rbind()

china <- read_json("json/city_area.json") %>% 
  gather_array() %>% 
  spread_all()

city_district <- china %>% 
  filter(parentProvinceCode == "") %>% 
  transmute(province_code = code, province = value) %>% 
  left_join(
    china %>% 
      filter(parentProvinceCode != "") %>% 
      transmute(province_code = parentProvinceCode, district_code = code, district = value), 
    join_by(province_code)
  )

target_district <- city_district %>% 
  filter(str_detect(district_code, "^030[2|6]\\d+")) %>% 
  pull(district_code)

jobs_by_district <- jobs %>% 
  filter(
    workAreaCode %in% target_district, 
    # str_detect(jobName, "(实习)|(管培)|(SQL)|(客服)|(销售)|(运营)|(会计)|(前台)|(商品)", negate = TRUE), 
    # str_detect(fullCompanyName, "友邦|服饰|服装|温雅|香精|奥威亚|时装|瑞众|内衣", negate = TRUE),
    str_length(provideSalaryString) > 0, 
    str_length(jobName) > 1, 
  ) %>% 
  select(
    jobId, workAreaCode, jobAreaLevelDetail.districtString, jobName, jobDescribe, 
    provideSalaryString, workYear, workYearString, degreeString, industryType1Str,
    coId, fullCompanyName, companyTypeString, companySizeCode, companySizeString, companyHref, 
    hrUid, hrName, hrPosition, mobileTel, updateDateTime, 
    lat, lon, 
  ) %>% 
  tibble::as_tibble() %>% 
  distinct() %>% 
  separate(provideSalaryString, c("provideSalaryString", "bonusString"), sep = "·") %>% 
  separate(provideSalaryString, c("provideSalaryString", "isMonthlySalary"), sep = "/") %>% 
  mutate(
    salaryMultiplier = substr(provideSalaryString, nchar(provideSalaryString), nchar(provideSalaryString)), 
    salaryLower = str_extract(provideSalaryString, "^(\\d\\.?\\d?[千|万]?)\\-(\\d\\.?\\d?)", group = 1),
    salaryUpper = str_extract(provideSalaryString, "^(\\d\\.?\\d?[千|万]?)\\-(\\d\\.?\\d?)", group = 2), 
    across(salaryLower:salaryUpper, readr::parse_number), 
    salaryLower = ifelse(salaryUpper > salaryLower & salaryMultiplier == "万", salaryLower * 10, salaryLower), 
    salaryUpper = ifelse(salaryMultiplier == "千", salaryUpper, salaryUpper * 10), 
    isMonthlySalary = is.na(isMonthlySalary), 
    across(salaryLower:salaryUpper, \(x) ifelse(isMonthlySalary, x, x / 12)),
    across(salaryLower:salaryUpper, \(x) ifelse(is.na(bonusString), x, 
                                                x * as.numeric(str_extract(bonusString, "\\d+")) / 12)),
  ) %>% 
  relocate(c(salaryLower, salaryUpper, salaryMultiplier), .after = provideSalaryString) %>% 
  select(-provideSalaryString, -salaryMultiplier, -bonusString)
  

# jobs visualization ----------------------------------------------------

library(ggplot2)
jobs_by_title <- openxlsx::read.xlsx("jobTitle.xlsx") %>% arrange(jobId) %>% select(-c(jobId:jobNameSegment)) %>% 
  as_tibble() %>% 
  bind_cols(jobs_by_district %>% arrange(jobId)) %>% 
  separate_rows(jobTitle, sep = ",") %>% 
  mutate(jobTitle = forcats::fct_lump_min(jobTitle, 10, other_level = "其他"), 
         jobAreaLevelDetail.districtString = replace_na(jobAreaLevelDetail.districtString, "不分区")) %>% 
  filter(!is.na(jobTitle), 
         !jobAreaLevelDetail.districtString %in% c("禅城区", "三水区", "顺德区")) 

jobs_by_title %>% 
  filter(!is.na(jobTitle)) %>% 
  count(jobTitle) %>% 
  mutate(jobTitle = forcats::fct_reorder(jobTitle, n)) %>% 
  ggplot(aes(x = n, y = jobTitle, fill = jobTitle)) +
  geom_col(show.legend = FALSE) + 
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  scale_fill_viridis_d() + 
  theme_light()

jobs_by_title %>% 
  group_by(jobTitle) %>% 
  mutate(jobTitle = forcats::fct_reorder(jobTitle, salaryLower, median, .desc = TRUE)) %>% 
  pivot_longer(salaryLower:salaryUpper, names_to = "salaryBound", values_to = "salary") %>% 
  ggplot(aes(x = jobAreaLevelDetail.districtString)) + 
  geom_boxplot(aes(y = salary, fill = salaryBound))  + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  facet_grid(cols = vars(jobTitle)) + 
  theme_light()

jobs_by_title %>% 
  ggplot(aes(x = salaryLower, y = salaryUpper)) + 
  stat_density_2d_filled(contour_var = "ndensity") + 
  facet_wrap(vars(jobTitle), scales = "free") + 
  # scale_x_log10() + 
  # scale_y_log10() + 
  theme_light()
  

# jobs_extend <- jobs %>% 
#   select(jobId, workAreaCode, companyHref) %>% 
#   mutate(addr_legal = map(companyHref, \(x) tryCatch(get_addr_legal(url = x), error = \(e) NA), 
#                           .progress = TRUE))
  

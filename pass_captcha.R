# box::use(
#   lubridate[now], 
#   magrittr[extract2, extract, `%>%`],
#   ./driver_init[driver_init],
#   purrr[`%||%`]
# )
pass_captcha <- function() {
  list2env(as.list(drv), envir = environment())
  block <- edge$find_element(by$ID, "nc_1_n1z")
  action$click_and_hold(block)$perform()
  i <- 0
  while(i < 300) {
    x <- runif(1, 5, 10)
    action$move_by_offset(x, 0)$perform()
    i <- i + x
  } 
}
json_save <- function(name) {
  pyautogui <- reticulate::import("pyautogui")
  pyautogui$hotkey('ctrl', "s")
  Sys.sleep(0.5)
  pyautogui$write(name)
  Sys.sleep(0.5)
  pyautogui$press('enter')
  Sys.sleep(0.5)
  pyautogui$press('enter')
}

request_search <- function(keyword, area = NULL, page = Inf) {
  box::use(httr2[request, req_url_path_append, req_url_query], 
           purrr[`%||%`],
           lubridate[now])
  
  r <- request("https://we.51job.com") %>% 
    req_url_path_append("api", "job", "search-pc") %>% 
    req_url_query(
      "api_key" = "51job",
      "timestamp" = as.integer(now()), 
      "keyword" = paste0("%", toupper(charToRaw(keyword)), collapse = ""), 
      "searchType" = 2, 
      "function" = "", 
      "industry" = "",
      "jobArea" = area %||% stringr::str_pad(30200, 6, pad = "0", side = "left"), 
      "jobArea2" = "", 
      "landmark" = "",
      "metro" = "",
      "salary" = "",
      "workYear" = "",
      "degree" = "",
      "companyType" = "",
      "companySize" = "",
      "jobType" = "",
      "issueDate" = "",
      "sortType" = 0,
      "pageNum" = page,
      "requestId" = "",
      "source" = 1,
      "accountId" = 196136108,
      "pageCode" = "sou%7Csou%7Csoulb",
      "u_atoken" = "0c52c751-fc4b-4887-a30c-7cf7b57b0cb0",
      "u_asseion" = "01bHkhL6BtxcWEfucfIvsbTAVQIpoxodcCss4-WyfGvV9JC6w8_VU3Yv5RBjKQyJel9BYns6yhxMaBWXLWEt3ZYNsq8AL43dpOnCClYrgFm6o",
      "u_asig" = "05lfnxthAnZ3a3HTZsdJJv5TPHsoKeWTUljx0-pKm3TQL2gy6Ew_X-1TEqnctyPsKFhOnzMEVFdpYmt1S4Qr3zj6AwO9LRuqW30ZsrAXOfIit-f61O1uz9juORAoflR3r9DnH_XVVkI6clVpRBmzSijnt4P-wy0CNqV4CaopR3Ej8irUP2IyWoB4x-1N-zzmIhksmHjM0JOodanL5-M1Qs1eh3TEmFE1lRoMXxx97Wtmau4ht3t28HoukB7UULkz49u5zLWxIFk1Tk8yRgNgBMs_99DEtPgZRXpgNbcwOMgA3UpLHxH1iRKZmnjAu0Zefw", 
      "u_aref" = "Oww7xjTlqkFocVqRjC%2F5kss9GQw%3D"
    )
  r$url
}

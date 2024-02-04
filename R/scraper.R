#' Title
#'
#' @param x a By class reference string
#' @param y reference value
#'
#' @return Selenium WebElement object
#' @export
#'
#' @examples
expect_element <- function(x, y) {
  box::use(reticulate[tuple])
  list2env(as.list(drv), envir = environment())
  wait$until(expected$presence_of_element_located(tuple(x, y)))
}

#' Captcha Automation
#'
#' @return
#' @export
#'
#' @examples
pass_captcha <- function(browser = drv) {
  list2env(as.list(browser), envir = environment())
  block <- tryCatch(expect_element(by$ID, "nc_1_n1z"), 
                    error = \(e) NA)
  if(!inherits(block, "try-error")) {
    action$click_and_hold(block)$perform()
    i <- 0
    while(i < 300) {
      x <- runif(1, 5, 10)
      action$move_by_offset(x, 0)$perform()
      i <- i + x
    } 
  }
}

#' Title
#'
#' @param name File name
#'
#' @return
#' @export
#'
#' @examples
json_save <- function(name) {
  pyautogui$hotkey('ctrl', "s")
  Sys.sleep(2)
  pyautogui$write(name)
  Sys.sleep(0.5)
  pyautogui$press('enter')
  Sys.sleep(0.5)
  pyautogui$press('enter')
}

#' Title
#'
#' @param keyword API parameter A string
#' @param area API parameter: An Integer. Default to city code for Guangzhou City
#' @param page API parameter: An Integer
#'
#' @return
#' @export
#'
#' @examples
request_search <- function(
    keyword, area = NULL, page, 
    requestId = NULL, 
    pageSize = NULL,
    accountId = NULL, 
    u_atoken = NULL, 
    u_asession = NULL, 
    u_asig = NULL, 
    u_aref = NULL
) {
  box::use(httr2[request, req_url_path_append, req_url_query, `%>%`], 
           purrr[`%||%`],
           lubridate[now], 
           stringr[str_pad])
  
  r <- request("https://we.51job.com") %>% 
    req_url_path_append("api", "job", "search-pc") %>% 
    req_url_query(
      "api_key" = "51job",
      "timestamp" = as.integer(now()), 
      # "keyword" = paste0("%", toupper(charToRaw(keyword)), collapse = ""),
      "keyword" = keyword,
      "searchType" = 2, 
      "function" = "", 
      "industry" = "",
      "jobArea" = area %||% str_pad(30200, 6, pad = "0", side = "left"), 
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
      "requestId" = requestId,
      "pageSize" = pageSize %||% 20,
      "source" = 1,
      "accountId" = accountId,
      "pageCode" = "sou%7Csou%7Csoulb",
      "u_atoken" = u_atoken,
      "u_asseion" = u_asession,
      "u_asig" = u_asig, 
      "u_aref" = u_aref
    )
  r$url
}

#' Title
#'
#' @param url API url
#'
#' @return A list of scraped values
#' @export
#'
#' @examples
get_addr_legal <- function(browser = drv, url = NULL) {
  box::use(stringr[str_detect, str_split], 
           magrittr[`%>%`, extract2], 
           purrr[set_names, map_chr, `%||%`, flatten])
  list2env(as.list(browser), envir = environment())
  url <- url %||% edge$current_url
  
  edge$get(url)
  pass_captcha()
  
  # if(!str_detect(edge$current_url, "(^https://jobs.51job.com/all/.+\\.html).+?$")) {
  #   stop("Invalid url", call. = FALSE)
  # }
  
  addr <- expect_element(by$CLASS_NAME, "desc")$text
  legal_info <- expect_element(by$CLASS_NAME, "info-list")$text %>% 
    str_split("\\n") %>% 
    extract2(1) %>% 
    str_split("：") %>% 
    head(4) %>% 
    set_names(c("company", "established", "capital", "legalperson")) %>% 
    map_chr(~.x[2]) %>% 
    as.list()
  flatten(list(addr = addr, legal_info))
}

box::use(./driver_init[driver_init])
driver_init()

# drv$edge_options$add_argument(
#   list(
#     "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0", 
#     "Accept" = "application/json, text/plain, */*", 
#     "Account-id" = "196136108", 
#     "Property" = "%7B%22partner%22%3A%22%22%2C%22webId%22%3A2%2C%22fromdomain%22%3A%2251job_web%22%2C%22frompageUrl%22%3A%22https%3A%2F%2Fwe.51job.com%2F%22%2C%22pageUrl%22%3A%22https%3A%2F%2Fwe.51job.com%2Fpc%2Fsearch%3Fkeyword%3D%25E6%2595%25B0%25E6%258D%25AE%25E6%2595%25B4%25E7%2590%2586%26searchType%3D2%26sortType%3D0%26metro%3D%22%2C%22identityType%22%3A%22%E8%81%8C%E5%9C%BA%E4%BA%BA%22%2C%22userType%22%3A%22%E8%80%81%E7%94%A8%E6%88%B7%22%2C%22isLogin%22%3A%22%E6%98%AF%22%2C%22accountid%22%3A%22196136108%22%7D", 
#     "Sign" = "59c2ce23f8a9fb56eef2f309a43aa8fd2fab03dcc3735dab7b13874e7a57a713", 
#     "User-token" = "e39eb7745c42c48a92bbd7936bf4e35a65a4da3f",
#     "Uuid" = "f559fbd62dbb0fbf05bf1ab2ec55d069"
#   )
# )
# 
# drv$edge(options = edge_options)

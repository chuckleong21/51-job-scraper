# source(here("src", "mods.R"))
# source(here("src", "helpers.R"))

driver_import <- function() {
  # the download method
  # download edge browser via driver$Edge(service = EXECUTABLE PATH)
  driver <- reticulate::import("selenium.webdriver")
  # from selenium.webdriver.common.service import Service as EdgeService is a bug
  driver_manager <- reticulate::import("webdriver_manager.microsoft")
  # webdriver serving class
  edge_service <-  driver$edge$service$Service
  # webdriver manager class
  edge_manager <- driver_manager$EdgeChromiumDriverManager
  
  component <- list(
    driver = driver, # webdriver module
    driver_wait = driver$support$wait, # webdriver wait module
    edge_executable = edge_service(edge_manager()$install()), # EXECUTABLE PATH
    edge_options = driver$EdgeOptions(), # webdriver options object
    expected = driver$support$expected_conditions, # webdriver condition module
    keys = driver$common$keys$Keys, 
    by = driver$common$by$By
  )
  
  invisible(list2env(component, envir = .GlobalEnv))
}

#' @export
driver_init <- function(sleep = 10, extensions = NULL, maximize = FALSE) {
  box::use(cli[cli_progress_step, cli_progress_done], 
           reticulate[import], 
           magrittr[`%>%`, extract], 
           rlang[env])
  
  drv <- env()
  cli_progress_step("Initiating Edge...", spinner = TRUE)
  
  driver <- import("selenium.webdriver")
  # from selenium.webdriver.common.service import Service as EdgeService is a bug
  
  # EXECUTABLE PATH equivalent in v3
  # edge_service <- driver$edge$service$Service
  edge_service <- driver %>% 
    extract('edge') %>% 
    extract('service') %>% 
    extract('Service')
  
  # download method in v4 instead of binary method in v3
  driver_manager <- import("webdriver_manager.microsoft")
  edge_manager <- driver_manager$EdgeChromiumDriverManager
  keys <- driver$common$keys$Keys
  by <- driver$common$by$By
  driver_wait <- driver$support$wait
  expected <- driver$support$expected_conditions
  
  # headers
  # opts <- list(
  #   "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36 Edg/120.0.0.0", 
  #   "Accept" = "application/json, text/plain, */*", 
  #   "Account-id" = "196136108", 
  #   "Property" = "%7B%22partner%22%3A%22%22%2C%22webId%22%3A2%2C%22fromdomain%22%3A%2251job_web%22%2C%22frompageUrl%22%3A%22https%3A%2F%2Fwe.51job.com%2F%22%2C%22pageUrl%22%3A%22https%3A%2F%2Fwe.51job.com%2Fpc%2Fsearch%3Fkeyword%3D%25E6%2595%25B0%25E6%258D%25AE%25E6%2595%25B4%25E7%2590%2586%26searchType%3D2%26sortType%3D0%26metro%3D%22%2C%22identityType%22%3A%22%E8%81%8C%E5%9C%BA%E4%BA%BA%22%2C%22userType%22%3A%22%E8%80%81%E7%94%A8%E6%88%B7%22%2C%22isLogin%22%3A%22%E6%98%AF%22%2C%22accountid%22%3A%22196136108%22%7D", 
  #   "Sign" = "59c2ce23f8a9fb56eef2f309a43aa8fd2fab03dcc3735dab7b13874e7a57a713", 
  #   "User-token" = "e39eb7745c42c48a92bbd7936bf4e35a65a4da3f",
  #   "Uuid" = "f559fbd62dbb0fbf05bf1ab2ec55d069"
  # )
  edge_options <- driver$EdgeOptions()
  # set window.navigator.webdriver to FALSE in order to pass captcha
  edge_options$add_argument('--disable-blink-features=AutomationControlled')
  
  if(!is.null(extensions)) {
    stopifnot("invalid extension file format" = str_detect(extensions, "\\.crx$"))
    if(length(extensions) > 1) {
      walk(extensions, \(x) edge_options$add_extension(x))
    } else edge_options$add_extension(extensions)
  }
  
  list2env(list(
    driver = driver,
    keys = keys, 
    by = by,
    expected = expected,
    edge_options = edge_options,
    edge = driver$Edge(
      service = edge_service(edge_manager()$install()),
      options = edge_options
    ) 
  ), envir = drv)
  
  list2env(
    list(wait = driver_wait$WebDriverWait(drv$edge, sleep), 
         action = driver$ActionChains(drv$edge)), envir = drv
  )
  
  on.exit(list2env(list(drv = drv), envir = .GlobalEnv))
  
  if(maximize) 
    drv$edge$maximize_window()
  
  cli_progress_done()
}

child_tab <- function(name, drv = edge) {
  stopifnot("Non-existed mother tab" = exists("tab", envir = .GlobalEnv))
  stopifnot("No child tab name supplied" = !missing(name))
  
  drv$execute_script("window.open('');")
  tab_copy <- get("tab", .GlobalEnv)
  tab_copy$child <- append(tab_copy$child, set_names(last(drv$window_handles), name))
  new_tab <- last(tab_copy$child)
  edge$switch_to$window(new_tab)
  tab <<- tab_copy 
  cli_alert_success(glue("Switched to child tab {names(last(tab_copy$child))}"))
}

child_tab_close <- function(name, mother_switch, drv = edge) {
        stopifnot("Non-existed mother tab" = exists("tab", envir = .GlobalEnv))
        stopifnot("No child tab name supplied" = !missing(name))
        stopifnot("Please indicate whether to switch back to mother tab" = !missing(mother_switch))
        
        drv$close()
        tab_copy <- get("tab", .GlobalEnv)
        if(sum(name %in% names(tab_copy$child)) > 1)
                warning("Eponymous tab states present")
        tab_copy$child <- set_names(drv$window_handles[-1], "WeTransfer")
        tab <<- tab_copy
        if(!mother_switch) {
                if(length(tab_copy$child) != 0) {
                        drv$switch_to$window(last(drv$window_handles))
                        cli_alert_success(steelblue(glue("Closed child tab {name}, Switched to child tab WeTransfer")))
                } else {
                        drv$switch_to$window(first(drv$window_handles))
                        cli_alert_success(steelblue(glue("Closed child tab {name}, Switched to mother tab {names(tab_copy$mother)}")))
                }
        } else {
                drv$switch_to$window(first(drv$window_handles))
        }
}


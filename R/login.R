box::use(
  purrr[`%||%`]
)
as.list.cookies <- function(x) {
  box::use(
    stringr[str_split, str_extract],
    purrr[reduce, set_names, `%>%`]
  )
  param <- str_split(x, "; ") %>% reduce(c)
  str_extract(param, "=(.*?)$", group = 1) %>% 
    as.list() %>% 
    set_names(str_extract(param, "^(.*?)=", group = 1))
}

login_cookies <- function(browser = NULL, cookies = NULL) {
  box::use(readr[read_lines], assertthat[is.scalar])
  
  browser <- browser %||% drv$edge
  
  if(is.null(cookies) && 'cookies.txt' %in% list.files()) {
    cookies <- as.list(structure(read_lines("cookies.txt"), class = c("list", "cookies")))
  } else if(is.scalar(cookies) & is.character(cookies)) {
    class(cookies) <- c("character", "cookies")
    cookies <- as.list(cookies)
  } else stop("Invalid cookies.\nSupply 'cookies.txt' file or a scalar character")
  
  for(i in seq_along(cookies)) {
    browser$add_cookie(list(name = names(cookies[i]), value = cookies[[i]]))
  }
  
  browser$refresh()
}


login_simulation <- function(browser = NULL, timeout = 60) {
  browser <- browser %||% drv$edge
  
  browser$find_element(drv$by$CLASS_NAME, "passIcon")$click()
  
  i <- 0
  while(browser$current_url != "https://we.51job.com/pc/my/myjob") {
    if(i < timeout) {
      Sys.sleep(1)
      i <- i + 1
    } else stop("Login Error", call. = FALSE)
  }
}


# -------------------------------------------------------------------------

login <- function(browser = NULL, method = c("cookies", "simulation"), cookies = NULL, timeout = 60) {
  
  browser <- browser %||% drv$edge
  browser$get("https://we.51job.com")
  switch(method, 
         "cookies" = login_cookies(browser = browser, cookies = cookies), 
         "simulation" = login_simulation(browser = browser, timeout = timeout))
}
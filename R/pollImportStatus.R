#' @export
#' @title pollImportStatus(r)
#'
#' @description Returns information on the status of an import.
#'
#' @param r The response from a post to DATIM.
#'
#' @return printed statements on data
#'

pollImportStatus <- function(r, d2_session) {
  
  start_time <- Sys.time()
  
  url <- paste0(stringr::str_replace(d2_session$base_url, "/$", ""),
                content(r)$response$relativeNotifierEndpoint)
  
  getImportStatus <- function(url) {
    
    httr:::GET(url, content_type_json(), handle = d2_session$handle) %>%
      httr::content() %>%
      purrr::map_lgl(~ .x[["completed"]] == "TRUE") %>% 
      any()
    
  }
  
  import_status <- FALSE
  Sys.sleep(10)
  
  while (!import_status) {
    import_status <- getImportStatus(url)
    print("Please wait....")
    Sys.sleep(10)
  }
  end_time <- Sys.time()
  
  #Get the task summary
  url <- gsub("tasks", "taskSummaries", url)
  ts <- httr:::GET(url, content_type_json(),
                   handle = d2_session$handle) %>%
    httr::content()
  
  print(paste("Process completed in",
              difftime(end_time, start_time, units = "mins") %>%
                as.numeric() %>%
                sprintf(., fmt = "%#.3f"),
              "minutes."))
  
}
#' @export
#' @title getTaskSummary(r, d2_session)
#'
#' @description Returns a summary of the import process for a user to check if there were any issues.
#'
#' @param r the repsonse from a post to DATIM.
#' @param d2_session the d2_session.
#'
#' @return console output on the process.
#'

getTaskSummary <- function(r, d2_session) {
  
  ep <- content(r)$response$relativeNotifierEndpoint %>%
    stringr::str_replace(., "^/", "") %>%
    stringr::str_replace(., "tasks", "taskSummaries")
  url <- paste0(d2_session$base_url, ep)
  #Get the task summary
  
  httr::GET(url, content_type_json(), handle = d2_session$handle) %>%
    httr::content()
}
#' @export
#' @title sendPayload(data, type, d2_session)
#'
#' @description ....
#'
#' @param data an import file.
#' @param type an import file.
#' @param d2_session a d2 session
#'
#' @return data
#'

sendPayload <- function(data, type, d2_session) {
  
  if(type == "DELETES") {
    
    print(
      paste0(
        "Deleting existing data in: ",
        d2_session$base_url
      )
    )
    
    url <-
      paste0(
        d2_session$base_url,
        "api/dataValueSets?importStrategy=DELETE&force=true&preheatCache=true&async=TRUE"
        )
    
    
    r <- httr::POST(url, body = data,
                    httr::content_type_json(),
                    handle = d2_session$handle)
   
    # print import summary
    httr::content(r)
    pollImportStatus(r, d2_session = d2_session)
    
    ts <- getTaskSummary(r, d2_session = d2_session)
    print(ts)
    if (ts$importCount$deleted != NROW(deletes)) {
      warning("Deleted count did not match the number of deletes!")
    }
    
    return(ts)
    
  } else if (type == "IMPORT_MAIN") {
    
    print(
      paste0(
        "Importing Main Data: ",
        d2_session$base_url
      )
    )
    
    
    url <-
      paste0(
        d2_session$base_url,
        "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
    r <- httr::POST(url, body = data,
                    httr::content_type_json(),
                    handle = d2_session$handle)
    # print import summary
    httr::content(r)
    pollImportStatus(r, d2_session = d2_session)
    ts <- getTaskSummary(r, d2_session = d2_session)
    print(ts)
    
    return(ts)
    
  } else if (type == "DEDUPES_00000") {
    
    print(
      paste0(
        "Importing Pure Dedupe Data 00000: ",
        d2_session$base_url
      )
    )
    
    url <-
      paste0(
        d2_session$base_url,
        "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
    r <- httr::POST(url, body = data,
                    httr::content_type_json(),
                    handle = d2_session$handle)
    
    # print import summary
    httr::content(r)
    pollImportStatus(r, d2_session = d2_session)
    ts <- getTaskSummary(r, d2_session = d2_session)
    print(ts)
    
    return(ts)

  } else if (type == "DEDUPES_00001") {
    
    print(
      paste0(
        "Importing Pure Dedupe Data 00001: ",
        d2_session$base_url
      )
    )
    
    url <-
      paste0(
        d2_session$base_url,
        "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
    r <- httr::POST(url, body = data,
                    httr::content_type_json(),
                    handle = d2_session$handle)
    # print import summary
    httr::content(r)
    pollImportStatus(r, d2_session = d2_session)
    ts <- getTaskSummary(r, d2_session = d2_session)
    print(ts)
    
    return(ts)
    
  }
}

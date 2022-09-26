#' @export
#' @title import(server, deletes_json, main_import_json, dedupes_0000_json, dedupes_00001_json)
#'
#' @description runs import procedure for all data into DATIM.
#'
#' @param d the datapack object which holds all relevant datapack information and validation results.
#' @param server the server we are importing to like triage or production.
#' @param import_data_json the import data object which holds the 4 types of import data in its prepped json format.
#' @param import_data the import data object which holds the 4 types of import data.
#'
#' @return printed information.
#'

importToDatim <- function(d, server, import_data_json, import_data, d2session) {
  
  # temporary functions ----
  cbind.fill <- function(...){
    nm <- list(...) 
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
      rbind(x, matrix(, n-nrow(x), ncol(x))))) 
  }
  
  unPackTaskSummary <- function(ts) {
    return(
      data.frame(
        cbind.fill(
          data.frame("responseType" = ts$responseType),
          data.frame("status" = ts$status),
          data.frame("description" = ts$description),
          data.frame("dataSetComplete" = ts$dataSetComplete),
          as.data.frame(do.call(cbind, ts$importOptions)),
          as.data.frame(do.call(cbind, ts$importCount)),
          as.data.frame(do.call(cbind, ts$conflicts))
        )
      )
      
    )
  }
  
  # initiate list
  import_results <- list()
  
  withProgress(message = "importing data", value = 0, {
    
  # deleting ----
  incProgress(0.25, detail = ("Starting deletes process..."))
  print(paste0(server, ": Deleting existing data"))
  
  #print("TRIAGE: Deleting existing data")
  url <-
    paste0(
      d2session$base_url,
      "api/dataValueSets?importStrategy=DELETE&force=true&preheatCache=true&async=TRUE")
  r <- httr::POST(url, body = import_data_json$deletes_json[["raw_file"]],
                  content_type_json(),
                  handle = d2session$handle)
  
  # print import summary
  httr::content(r)
  pollImportStatus(r, d2_session = d2session)
  print("getting task summary...")
  ts <- getTaskSummary(r, d2_session = d2session)
  # capture task results
  import_results$deletes <- unPackTaskSummary(ts)
  print(ts)
  if (ts$importCount$deleted != NROW(import_data$deletes)) {
    warning("Deleted count did not match the number of deletes!")
  }
  rm(ts)
  
  # importing main ----
  incProgress(0.25, detail = ("Importing main data..."))
  print(paste0(server, ": Importing main data"))
  #print("TRIAGE: Importing main data")
  url <-
    paste0(
      d2session$base_url,
      "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
  r <- httr::POST(url, body = import_data_json$main_import_json[["raw_file"]],
                  content_type_json(),
                  handle = d2session$handle)
  
  # print import summary
  httr::content(r)
  pollImportStatus(r, d2_session = d2session)
  ts <- getTaskSummary(r, d2_session = d2session)
  # capture task results
  import_results$main <- unPackTaskSummary(ts)
  print(ts)
  rm(ts)
  
  # importing dedupe data 00000 ----
  incProgress(0.25, detail = ("Importing dedupes 00000..."))
  print(paste0(server, ": Importing pure dedupe data 00000"))
  #print("TRIAGE: Importing pure dedupe data")
  url <-
    paste0(
      d2session$base_url,
      "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
  r <- httr::POST(url, body = import_data_json$dedupes_00000_json[["raw_file"]],
                  content_type_json(),
                  handle = d2session$handle)
  
  # print import summary
  httr::content(r)
  pollImportStatus(r, d2_session = d2session)
  ts <- getTaskSummary(r, d2_session = d2session)
  # capture task results
  import_results$dedupe_0000 <- unPackTaskSummary(ts)
  print(ts)
  rm(ts)
  
  # importing dedupe data 0001 ----
  incProgress(0.1, detail = ("Importing dedupes 00001..."))
  print(paste0(server, ": Importing pure dedupe data 00001"))
  #print("TRIAGE: Importing pure dedupe data")
  url <-
    paste0(
      d2session$base_url,
      "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
  r <- httr::POST(url, body = import_data_json$dedupes_00001_json[["raw_file"]],
                  content_type_json(),
                  handle = d2session$handle)
  # print import summary
  httr::content(r)
  pollImportStatus(r, d2_session = d2session)
  ts <- getTaskSummary(r, d2_session = d2session)
  # capture task results
  import_results$dedupe_0001 <- unPackTaskSummary(ts)
  print(ts)
  rm(ts)
  
  
  # checks----
  incProgress(0.1, detail = ("Finishing up import process..."))
  if (d$info$tool == "OPU Data Pack") {
    check <- datapackr::compareData_OpuDatapackVsDatim(d, d2_session = d2session)
  } else if (d$info$tool == "Data Pack") {
    check <- datapackr::compareData_DatapackVsDatim(d, d2_session = d2session)
  }
  
  print(paste(sum(check$psnu$difference != 0),
              "values differed between DATIM and the Datapack."))
  
  })
  
  #import_results_a <<- import_results
  res <- do.call(rbind, import_results)
  return(res)
  
}
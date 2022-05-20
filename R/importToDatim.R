#' @export
#' @title import(server, deletes_json, main_import_json, dedupes_0000_json, dedupes_00001_json)
#'
#' @description runs import procedure for all data into DATIM.
#'
#' @param server the server we are importing to like triage or production.
#' @param deletes_json the deletes json file used to delete existing data before import.
#' @param main_import_json the main imports imported to datim.
#' @param dedupes_00000_json the pure dedupe data 0000 imported to datim.
#' @param dedupes_00001_json the pure dedupe data 0001 imported to datim.
#'
#' @return data
#'

importToDatim <- function(d, server, import_data, d2session) {
  
  withProgress(message = "importing data", value = 0, {
    
  # deleting ----
  incProgress(0.25, detail = ("Starting deletes process..."))
  print(paste0(server, ": Deleting existing data"))
  
  #print("TRIAGE: Deleting existing data")
  url <-
    paste0(
      d2session$base_url,
      "api/dataValueSets?importStrategy=DELETE&force=true&preheatCache=true&async=TRUE")
  r <- httr::POST(url, body = import_data$deletes_json[["raw_file"]],
                  content_type_json(),
                  handle = d2session$handle)
  
  # print import summary
  httr::content(r)
  pollImportStatus(r, d2_session = d2session)
  print("getting task summary...")
  ts <- getTaskSummary(r, d2_session = d2session)
  print(ts)
  if (ts$importCount$deleted != NROW(import_data$deletes_json[["pl"]])) {
    warning("Deleted count did not match the number of deletes!")
  }
  
  # importing main ----
  incProgress(0.25, detail = ("Importing main data..."))
  print(paste0(server, ": Importing main data"))
  #print("TRIAGE: Importing main data")
  url <-
    paste0(
      d2session$base_url,
      "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
  r <- httr::POST(url, body = import_data$main_import_json[["raw_file"]],
                  content_type_json(),
                  handle = d2session$handle)
  
  # print import summary
  httr::content(r)
  pollImportStatus(r, d2_session = d2session)
  ts <- getTaskSummary(r, d2_session = d2session)
  print(ts)
  
  # importing dedupe data 00000 ----
  incProgress(0.25, detail = ("Importing dedupes 00000..."))
  print(paste0(server, ": Importing pure dedupe data 00000"))
  #print("TRIAGE: Importing pure dedupe data")
  url <-
    paste0(
      d2session$base_url,
      "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
  r <- httr::POST(url, body = import_data$dedupes_00000_json[["raw_file"]],
                  content_type_json(),
                  handle = d2session$handle)
  
  # print import summary
  httr::content(r)
  pollImportStatus(r, d2_session = d2session)
  ts <- getTaskSummary(r, d2_session = d2session)
  print(ts)
  
  # importing dedupe data 0001 ----
  incProgress(0.1, detail = ("Importing dedupes 00001..."))
  print(paste0(server, ": Importing pure dedupe data 00001"))
  #print("TRIAGE: Importing pure dedupe data")
  url <-
    paste0(
      d2session$base_url,
      "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&async=TRUE")
  r <- httr::POST(url, body = import_data$dedupes_00001_json[["raw_file"]],
                  content_type_json(),
                  handle = d2session$handle)
  # print import summary
  httr::content(r)
  pollImportStatus(r, d2_session = d2session)
  ts <- getTaskSummary(r, d2_session = d2session)
  print(ts)
  
  
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
  
}
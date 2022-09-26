#' Title getBaseURL()
#'
#' @return A base URL to be used throughout the application. If the BASE_URL
#' environment variable is not set,
#' this function will return https://www.datim.org,
#' otherwise, the value of the environment variable.
#' @export
#'
getBaseURL <- function() {
  if (Sys.getenv("BASE_URL") !=  "")  {
    return(Sys.getenv("BASE_URL"))
  } else {
    futile.logger::flog.warn("No BASE_URL environment variable found. Using www.datim.org")
    
    return("https://www.datim.org/")
    
  }
}

#' @export
#' @title getTaskSummary(r, d2_session)
#'
#' @description Returns a summary of the import process for a user to check if there were any issues.
#'
#' @param r the response from a post to DATIM.
#' @param d2_session the d2_session.
#'
#' @return a summary of the import process.
#'

getTaskSummary <- function(r, d2_session) {
  
  ep <- content(r)$response$relativeNotifierEndpoint %>%
    stringr::str_replace(., "^/", "") %>%
    stringr::str_replace(., "tasks", "taskSummaries")
  url <- paste0(d2_session$base_url, ep)
  #Get the task summary
  
  res <- httr::GET(url, content_type_json(), handle = d2_session$handle) %>%
    httr::content()
  
  return(res)
}

#' @export
#' @title getOperatingUnitFromCountryUIDs(country_uids)
#'
#' @description Returns a summary of the import process for a user to check if there were any issues.
#'
#' @param r the response from a post to DATIM.
#' @param d2_session the d2_session.
#'
#' @return a summary of the import process.
#'
getOperatingUnitFromCountryUIDs <- function(country_uids) {
  ou <- datapackr::valid_PSNUs %>%
    dplyr::select(ou, ou_id, country_name, country_uid) %>%
    dplyr::distinct() %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    dplyr::select(ou, ou_id) %>%
    dplyr::distinct()
  
  if (NROW(ou) != 1) {
    stop("Datapacks cannot belong to multiple operating units")
  }
  ou
}

#' @export
#' @title getCountryNameFromUID(country_uids)
#'
#' @description Returns a summary of the import process for a user to check if there were any issues.
#'
#' @param r the response from a post to DATIM.
#' @param d2_session the d2_session.
#'
#' @return a summary of the import process.
#'
getCountryNameFromUID <- function(uid) {
  
  paste0(getOption("baseurl"), "api/organisationUnits/",
         uid, "?fields=shortName") %>%
    URLencode(.) %>%
    httr::GET(., handle = user_input$d2_session$handle) %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(.) %>%
    purrr::pluck(., "shortName")
}

#' @export
#' @title createS3BucketTags(d)
#'
#' @description Returns a summary of the import process for a user to check if there were any issues.
#'
#' @param r the response from a post to DATIM.
#' @param d2_session the d2_session.
#'
#' @return a summary of the import process.
#'
createS3BucketTags <- function(d) {
  d$info$country_uids <- paste0(d$info$country_uids, sep = "", collapse = "_")
  tags <- c("tool",
            "country_uids",
            "cop_year",
            "has_error",
            "sane_name",
            "approval_status",
            "source_user")
  object_tags <- d$info[names(d$info) %in% tags]
  object_tags <- URLencode(paste(names(object_tags),
                                 object_tags,
                                 sep = "=",
                                 collapse = "&"))
  
  return(object_tags)
}

#' @export
#' @title saveDATIMExportToS3(d)
#'
#' @description Returns a summary of the import process for a user to check if there were any issues.
#'
#' @param r the response from a post to DATIM.
#' @param d2_session the d2_session.
#'
#' @return a summary of the import process.
#'
saveDATIMExportToS3 <- function(d) {
  #Write the flatpacked output
  tmp <- tempfile()
  
  if (d$info$tool == "Data Pack") {
    datim_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                     d$datim$MER) %>%
      dplyr::mutate(value = as.character(value))
  }
  
  # if (d$info$tool == "OPU Data Pack") {
  #   datim_export <- dplyr::bind_rows(d$datim$OPU) %>%
  #     dplyr::mutate(value = as.character(value))
  # }
  
  #Need better error checking here.
  write.table(
    datim_export,
    file = tmp,
    quote = FALSE,
    sep = "|",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  
  # Load the file as a raw binary
  read_file <- file(tmp, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)
  
  object_tags <- createS3BucketTags(d)
  
  object_name <- paste0("datim_export/",
                        gsub("^20", "cop", d$info$cop_year),
                        "/", d$info$sane_name, ".csv")
  s3 <- paws::s3()
  
  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         Tagging = object_tags,
                         ContentType = "text/csv")
    print("DATIM Export sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    print("DATIM Export could not be sent to  S3", name = "datapack")
    print(err, name = "datapack")
    FALSE
  })
  
  unlink(tmp)
  
  return(r)
  
}

#' @export
#' @title saveTimeStampLogToS3(d)
#'
#' @description Returns a summary of the import process for a user to check if there were any issues.
#'
#' @param r the response from a post to DATIM.
#' @param d2_session the d2_session.
#'
#' @return a summary of the import process.
#'
saveTimeStampLogToS3 <- function(d) {
  
  #Write an archived copy of the file
  s3 <- paws::s3()
  object_tags <- createS3BucketTags(d)
  object_name <-
    paste0("processed/",
           gsub("^20", "cop", d$info$cop_year),
           "/",
           d$info$sane_name,
           ".csv")
  #Save a timestamp of the upload
  #options(digits.secs=6)
  timestamp_info <- list(
    ou = d$info$operating_unit$ou,
    ou_id = d$info$operating_unit$ou_id,
    country_name = d$info$datapack_name,
    country_uids = paste(d$info$country_uids, sep = "", collapse = ","),
    upload_timestamp = strftime(as.POSIXlt(Sys.time(), "UTC"),
                                "%Y-%m-%d %H:%M:%S"),
    #uuid=d$info$uuid,
    #upload_timestamp=strftime(as.POSIXlt(Sys.time(), "UTC") , "%Y-%m-%d %H:%M:%OS"),
    filename = object_name
  )
  
  tmp <- tempfile()
  write.table(
    as.data.frame(timestamp_info),
    file = tmp,
    quote = FALSE,
    sep = "|",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  # Load the file as a raw binary
  read_file <- file(tmp, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)
  object_name <-
    paste0(
      "upload_timestamp/",
      gsub("^20", "cop", d$info$cop_year),
      "/",
      d$info$sane_name,
      ".csv"
    )
  
  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         Tagging = object_tags,
                         ContentType = "text/csv")
    print("Timestamp log sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    print("Timestamp log could not be saved to S3", name = "datapack")
    FALSE
  })
  unlink(tmp)
  return(r)
}

#' @export
#' @title checkApprovalStatus(d, d2_session)
#'
#' @description Returns a summary of the import process for a user to check if there were any issues.
#'
#' @param r the response from a post to DATIM.
#' @param d2_session the d2_session.
#'
#' @return a summary of the import process.
#'
checkApprovalStatus<-function(d,d2_session) {
  
  
  #Get the list of datasets which are subject to approval for MER targets
  approval_dataset_uids <- paste0(d2_session$base_url,"api/dataApprovalWorkflows/TAjCBkG6hl6?fields=dataSets[id]") %>% 
    httr::GET(.,handle = d2_session$handle) %>% 
    httr::content(.,"text") %>% 
    jsonlite::fromJSON(.) %>% 
    purrr::pluck("dataSets") %>% 
    dplyr::pull(id)
  
  dataset_uids <- datapackr::getDatasetUids((d$info$cop_year +1) , "mer_targets") 
  
  dataset_uids <- dataset_uids[dataset_uids %in% approval_dataset_uids]
  
  url<-paste0(d2_session$base_url,"api/dataApprovals/categoryOptionCombos",
              "?ds=",paste(dataset_uids,sep="",collapse="&ds="),
              "&pe=", paste0(d$info$cop_year,"Oct"),
              "&ou=", d$info$operating_unit$ou_id)
  r<-fromJSON(content(GET(url,handle = d2_session$handle),"text"))
  all(r$level$level == 3)
}
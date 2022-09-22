#' @export
#' @title validate(file, d2_session)
#'
#' @description Parses a datapack and returns datpack information as well as import file data.
#'
#' @param file The path of the datapack to parse.
#' @param d2_session The d2 session object.
#'
#' @return d object holding all datapack information.
#'

validate <- function(file, d2_session) {
  
  inFile <- file
  messages <- ""
  
  if (is.null(inFile)) {
    return(NULL)
  }
  
  messages <- list()
  
  
  withProgress(message = "importing data...", value = 0, {
    
    incProgress(0.3, detail = ("Unpacking your DataPack..."))
    
    d <- tryCatch({
      datapackr::unPackTool(inFile$datapath,
                            d2_session = d2_session)},
      error = function(e) {
        return(e)
      })
    
    if (inherits(d, "error")) {
      return("An error occurred. Please contact DATIM support.")
    }
    
    #Create some additional metadadta for S3 tagging
    d$info$sane_name <-
      paste0(stringr::str_extract_all(d$info$datapack_name,
                                      "[A-Za-z0-9_]",
                                      simplify = TRUE),
             sep = "", collapse = "")
    if (is.null(d$info$sane_name)) {
      stop("sane_name cannot be NULL!")
    }
    d$info$source_user <- d2_session$username
    if (is.null(d$info$source_user)) {
      stop("source_user cannot be NULL!")
    }
    
    #All self-service datapacks should be marked as unapproved for PAW
    d$info$approval_status <- "APPROVED"
    #Generate a unique identifier
    d$info$uuid <- uuid::UUIDgenerate()
    
    incProgress(0.3, detail = ("Building import files..."))
    if (d$info$tool ==  "Data Pack") {
      
      print("Datapack")
      
      # differences between datapack and datim at start of import
      # baseline_diff <- compareData_DatapackVsDatim(d)
      # 
      
      # extract data for import
      data <- d$datim$MER %>% 
        dplyr::bind_rows(d$datim$subnat_impatt)
      print("mer data exrtracted")
      #Remap mech codes to UIDs
      
      data$attributeOptionCombo <-
        datimvalidation::remapCategoryOptionCombos(data$attributeOptionCombo,
                                                   "code", "id", d2session = d2_session)
      print("datimvalidation ran")
      # double check all category option combos are uids, no "default" or mech codes
      assertthat::assert_that(
        all(datapackr::is_uidish(data$attributeOptionCombo)))
      assertthat::assert_that(
        all(datapackr::is_uidish(data$categoryOptionCombo)))
      print("assertions complete")
      
      # drop dedupe from main import
      d$import_files$main_import <-
        dplyr::filter(data,
                      !(attributeOptionCombo %in% c("X8hrDf6bLDC",
                                                    "YGT1o7UxfFu"))) %>%
        dplyr::mutate(value = as.character(value))
      
      print("dedupe drop complete")
      
      d$import_files$dedupes_00000 <- dplyr::filter(data, attributeOptionCombo == "X8hrDf6bLDC")
      
      d$import_files$dedupes_00001 <- dplyr::filter(data, attributeOptionCombo == "YGT1o7UxfFu")

      # delete prior cop subnat data
      # delete any pre existing COP22 data, generally only has impact
      # if we are reloading the DP
      d$import_files$deletes <- datapackr::getCOPDataFromDATIM(d$info$country_uids,
                                                               cop_year = 2021,
                                                               datastreams = c("subnat_targets"),
                                                               d2_session = d2_session
      ) %>% 
        dplyr::bind_rows(datapackr::getCOPDataFromDATIM(d$info$country_uids,
                                                        cop_year = 2022,
                                                        d2_session = d2_session)
        ) %>%
        dplyr::mutate(attributeOptionCombo =
                        datimvalidation::remapCategoryOptionCombos(
                          attributeOptionCombo,
                          "code",
                          "id",
                          d2session = d2_session),
                      categoryOptionCombo =
                        datimvalidation::remapCategoryOptionCombos(
                          categoryOptionCombo,
                          "code",
                          "id",
                          d2session = d2_session)) %>%
        dplyr::select(dataElement,
                      period,
                      orgUnit,
                      categoryOptionCombo,
                      attributeOptionCombo,
                      value)
    }
    
    incProgress(0.3, detail = ("Finishing up..."))
    print("generating json import files stored as reactive val...")
    
  })
  
  return(d)
  
}

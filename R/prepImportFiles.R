#' @export
#' @title prepImportFiles(data, tool, type)
#'
#' @description Preps the 4 import files
#'
#' @param d a d object result typically from a datapack being unpacked.
#' @param tool the type of tool, Data Pack or OPU Data Pack
#' @param the type of 
#'
#' @return data
#'
prepImportFiles <- function(d, tool, type) {
  
  stopifnot(d$info$tool == tool, TRUE)
  
  if( tool == "Data Pack") {
    
    # differences between datapack and datim at start of import
    baseline_diff <- compareData_DatapackVsDatim(d)
    
    # as opposed to full datapack import during COP, for OPUs we only
    # import the MER target data, no SUBNAT/IMPATT
    data <- d$datim$MER
    
    #Remap everything to UIDs for attribute option combos
    data$attributeOptionCombo <-
      datimvalidation::remapCategoryOptionCombos(data$attributeOptionCombo,
                                                 "code", "id")
    
    if(type == "DELETES") {
      
      ### delete all MER targets from the relevant cop year
      deletes <- datapackr::getCOPDataFromDATIM(d$info$country_uids,
                                                d$info$cop_year,
                                                datastreams = c("mer_targets")) %>%
        dplyr::mutate(attributeOptionCombo =
                        datimvalidation::remapCategoryOptionCombos(
                          attributeOptionCombo,
                          "code",
                          "id"),
                      categoryOptionCombo =
                        datimvalidation::remapCategoryOptionCombos(
                          categoryOptionCombo,
                          "code",
                          "id")) %>%
        dplyr::select(dataElement,
                      period,
                      orgUnit,
                      categoryOptionCombo,
                      attributeOptionCombo,
                      value)
      
    } else if (type == "IMPORT_MAIN") {
      
      # drop dedupe from main import
      main_import <-
        dplyr::filter(data, !(attributeOptionCombo %in% c("X8hrDf6bLDC", "YGT1o7UxfFu"))) %>%
        dplyr::mutate(value = as.character(value))
      
    } else if (type == "DEDUPES_00000") {
      
      dedupes_00000 <- dplyr::filter(data, attributeOptionCombo == "X8hrDf6bLDC")
      
    } else if (type == "DEDUPES_00001") {
      
      dedupes_00001 <- dplyr::filter(data, attributeOptionCombo == "YGT1o7UxfFu")
      
    } else {
      
      stop("NONE OF THESE PROCESSES ARE SUPPORTED!")
      
    }

  } else if (tool == "OPU Data Pack") {
    
    stop("cannot process opu yet ...")
    
  }
 
}

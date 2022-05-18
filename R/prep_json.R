#' @export
#' @title prep_json(data)
#'
#' @description Preps a json file based off the import files as data inputs. These are the jsons that are imported.
#'
#' @param data an import file.
#'
#' @return data
#'
prep_json <- function(data) {
  
  pl <- jsonlite::toJSON(list(dataValues = data), auto_unbox = TRUE)
  
  # import file handling to get ready for api call
  output_file <- tempfile()
  writeLines(pl, output_file)
  zip_file <- tempfile()
  zip(zipfile = zip_file, files = output_file)
  read_file <- file(paste0(zip_file, ".zip"), "rb")
  raw_file <-
    readBin(read_file, "raw", n = file.size(paste0(zip_file, ".zip")))
  close(read_file)
  return(list(pl = pl, raw_file = raw_file))
}
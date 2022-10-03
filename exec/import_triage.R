# Triage import test
# the following script can be executed as an import test
# currently unpacks in triage as "prod", but would eventually:
# 1. unpack in prod
# 2. import into triage to test import
# 3. user then runs import_prod.R

library(datapackr)
library(datimutils)
library(dplyr)

# login to datim - will be switched over to oauth for prod
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "triage.json")
datimutils::loginToDATIM(secrets)

# unpack data pack
d <- datapackr::unPackTool(file.choose())

# check for errors
if(d$info$has_error == TRUE) {
  
  error_waived <- readline(
    prompt = "There is an error with this data pack, review are you sure you want 
    to continue??? (make sure this error is waived) Enter YES or NO: ")
  if(!error_waived %in% c("YES", "NO")) {
    stop("Invalid entry, try again!") 
  } else if (error_waived == "NO") {
    stop("Stopping datapack import process due to error issue.")
  }
  
} else {
  
  print("Datapackr has not identified any validation errors in this datapack,
        building import files...")
  
}

# prep data ----

# prep import files
main_import <-
  prepImportFiles(d, tool = "Data Pack", type = "IMPORT_MAIN")
dedupes_00000 <-
  prepImportFiles(d, tool = "Data Pack", type = "DEDUPES_00000")
dedupes_00001 <-
  prepImportFiles(d, tool = "Data Pack", type = "DEDUPES_00001")
deletes <- prepImportFiles(d, tool = "Data Pack", type = "DELETES")

# transform df to json for upload
main_import_json <- prepJson(main_import)
dedupes_00000_json <- prepJson(dedupes_00000)
dedupes_00001_json <- prepJson(dedupes_00001)
deletes_json <-  prepJson(deletes)


# import process ----

# interactive confirmation for triage import
continue <- readline(prompt = paste0(
  d$info$datapack_name,
  " has unpacked in ",
  d2_default_session$base_url,
  ". Do you wish to continue with a test import into TRIAGE? Type YES or NO: "
))

if(continue == "NO") {
  stop("You elected to cancel triage import!")
} else if(!error_waived %in% c("YES", "NO")) {
  stop("Invalid entry, try again!") 
}

# test server
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "triage.json")
datimutils::loginToDATIM(secrets)
if(d2_default_session$base_url != "https://triage.testing.datim.org/") {
  stop("There was an issue logging into triage, process stopped. Try again.")
} else {
  print("Logged successfully into TRIAGE, continuing import process...")
}

# delete
res_deletes <-
  sendPayload(data = deletes_json[["raw_file"]],
              type = "DELETES",
              d2_session = d2_default_session)

# import main data
res_mains <-
  sendPayload(data = main_import_json[["raw_file"]],
              type = "IMPORT_MAIN",
              d2_session = d2_default_session)

# dedupes import 00000
res_00000 <-
  sendPayload(data = dedupes_00000_json[["raw_file"]],
              type = "DEDUPES_00000",
              d2_session = d2_default_session)

# dedupes import 00001
res_00001 <-
  sendPayload(data = dedupes_00001_json[["raw_file"]],
              type = "DEDUPES_00001",
              d2_session = d2_default_session)


print(
  paste0("TRIAGE import process was successfully completed at: ",
         d2_default_session$base_url,
         ". Review the console output for conflicts and issues."
         )
)

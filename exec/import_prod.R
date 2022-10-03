# PROD import 
# the following script can be executed as an import test
# currently runs fully on triage, but would eventually:
# 1. unpack in prod
# 2. import into triage

library(datapackr)
library(datimutils)
library(dplyr)

# login to datim
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "test.json")
datimutils::loginToDATIM(secrets)

# unpack data pack
d <- datapackr::unPackTool(file.choose())

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

# test server
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "test.json")
datimutils::loginToDATIM(secrets)
stopifnot(d2_default_session$base_url == "https://cop-test.datim.org/",
          TRUE)

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

# results to S3 ----
# saveTimeStampLogToS3(d)
# saveDATIMExportToS3(d)


# write out import files ----
#...
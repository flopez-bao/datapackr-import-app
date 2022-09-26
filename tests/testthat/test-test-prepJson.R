# test types ----
# can prep json import file from data

test_that("json can be prepped from import file...", {
  data <- data.frame(
    dataElement = c(1,2),
    period = c("zjWvbsJM43y", "zjWvbsJM43y"),
    orgUnit = c("2022Sep", "2022Sep"),
    categoryOptionCombo = c("Y0PDkS", "Y0PDkv"),
    attributeOptionCombo =  c("O4M73r7", "O4M73r7"),
    value = c(23, 25)
  )
  
  res <- prepJson(data)
  inp <- jsonlite::parse_json(res$pl)
  exp <- jsonlite::read_json("test_data/test_main_import_json.json")
  
  testthat::expect_equal(inp, exp)
  
  
})


message("----------------------SETUP")
options(shiny.testmode = FALSE)
vdoc <- local({
   #                      ##########
   # package_name is used # INSIDE # the sourced file below
   #                      ##########
   package_name <- "dv.reportrate"
   utils_file_path <- system.file("validation", "utils-validation.R", package = package_name, mustWork = TRUE)
   source(utils_file_path, local = TRUE)[["value"]]
})
specs <- vdoc[["specs"]]

#source(testthat::test_path("dummy-data.R"))

dummy_tp <- testthat::test_path("dummy-data.R")
message (dummy_tp)
check_path <- getwd()
message(check_path)

source("dummy-data.R")

message("----------------------SETUP END")

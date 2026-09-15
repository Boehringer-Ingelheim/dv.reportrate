library(dv.reportrate)

# Load dummy data for testing purpose
source("../../dummy-data.R")

test_ui <- function(request) {
   shiny::fluidPage(
      shiny::bookmarkButton(),
      dv.reportrate::report_rates_ui("reportrate")
   )
}

test_server <- function(input, output, session) {
   dm <- dm_dummy
   ds <- ds_dummy
   ae <- ae_dummy
   data <- list("dm" = dm, "ds" = ds, "ae" = ae)

   dv.reportrate::report_rates_server(module_id = "reportrate",
                                      dataset_list = shiny::reactive(data),
                                      subjid_var = "USUBJID",
                                      tooltip_decimal_places = 4L,
                                      disposition_events = list(event_var = "DSDECOD",
                                                                date_var = "DSSTDTC",
                                                                day_var = "DSSTDY",
                                                                entry_vals = c("RANDOMIZED"),
                                                                exit_vals = c("COMPLETED",
                                                                              "WITHDRAWAL BY SUBJECT",
                                                                              "DEATH")
                                      ),
                                      adverse_events = list(date_var = "AESTDTC",
                                                            day_var = "AESTDY"
                                      ),
                                      grouping_vars = list(choices = c("ACTARM", "ARM", "SEX", "SITEID"),
                                                      default_choice = "ARM"
                                      ),
                                      x_step_size_list = list(days = 50L, weeks = 2L, months = 1L)
   )

   exported_url <- shiny::reactiveVal(NULL)
   shiny::onBookmarked(function(url) {
      exported_url(url)
   })
   shiny::exportTestValues(url = exported_url())
}

shiny::shinyApp(test_ui, test_server, enableBookmarking = "url")

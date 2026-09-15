#'  Mock app for the Reporting Rates Module without the usage of the module manager.
#'
#' Launches an example app that shows a Reporting Rates module. The displayed
#' data is from the \pkg{pharmaversesdtm} package.
#' @export
mock_report_rates_app <- function() {

  # loading dummy data and preparation
  dm <- pharmaversesdtm::dm |> dplyr::mutate(SEX = as.factor(.data[["SEX"]]), # converting grouping vars to factors
                                             ARM = as.factor(.data[["ARM"]]),
                                             SITEID = as.factor(.data[["SITEID"]]))
  ds <- pharmaversesdtm::ds |> dplyr::mutate(DSSTDTC = as.Date(.data[["DSSTDTC"]]))
  ds <- pharmaversesdtm::ds |> dplyr::mutate(DSDECOD = as.factor(.data[["DSDECOD"]]))
  ae <- pharmaversesdtm::ae |> dplyr::mutate(AESTDTC = as.Date(.data[["AESTDTC"]]))
  data_list <- shiny::reactive({
    list(
      "dm" = dm,
      "ds" = ds |> dplyr::mutate(DSSTDTC = as.Date(.data[["DSSTDTC"]])),
      "ae" = ae |> dplyr::mutate(AESTDTC = as.Date(.data[["AESTDTC"]]))
    )
  })

  mock_report_rates_ui <- function() {
    shiny::fluidPage(report_rates_ui("rr"))
  }


  mock_report_rates_server <- function(input, output, session) {
    report_rates_server(
      module_id = "rr",
      dataset_list = data_list,
      subjid_var = "USUBJID",
      tooltip_decimal_places = 4L,
      disposition_events = list(event_var = "DSDECOD",
                                date_var = "DSSTDTC",
                                day_var = "DSSTDY",
                                entry_vals = c("RANDOMIZED"),
                                exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT", "DEATH")
      ),
      adverse_events = list(date_var = "AESTDTC",
                            day_var = "AESTDY"
      ),
      grouping_vars = list(choices = c("SITEID", "SEX", "ARM"),
                           default_choice = NULL),
      x_step_size_list = list(days = 50L, weeks = 2L, months = 1L)
    )
  }
  shiny::shinyApp(mock_report_rates_ui, mock_report_rates_server)
}

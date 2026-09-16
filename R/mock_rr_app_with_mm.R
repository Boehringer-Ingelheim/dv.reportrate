#' Mock app for the Reporting Rates Module integrated in the module manager.
#'
#' Launches an example app that shows a Reporting Rates module integrated in the module manager surface. The displayed
#' data is from the \pkg{pharmaversesdtm} package.
#' @export
mock_rr_app_with_mm <- function() {

  # loading dummy data and preparation
  dm <- pharmaversesdtm::dm
  ds <- pharmaversesdtm::ds
  ae <- pharmaversesdtm::ae

  # creating a second dataset for testing the behaviour of the app when switching datasets
  dm_2 <- pharmaversesdtm::dm |>
     dplyr::select(-.data[["SITEID"]]) |> # SITEID not in second dm
     dplyr::filter(.data[["ARM"]] %in% c("Xanomeline High Dose", "Placebo")) # less levels in ARM
  ds_2 <- ds
  ae_2 <- ae


  data_list <- list("dummy" = list("dm" = dm,
                                   "ds" = ds,
                                   "ae" = ae),
                    "dummy_2" = list("dm" = dm_2,
                                     "ds" = ds_2,
                                     "ae" = ae_2))

  data_list[["dummy"]]$ds <- data_list[["dummy"]]$ds |> dplyr::mutate(DSSTDTC = as.Date(.data[["DSSTDTC"]]))
  data_list[["dummy"]]$ae <- data_list[["dummy"]]$ae |> dplyr::mutate(AESTDTC = as.Date(.data[["AESTDTC"]]))
  data_list[["dummy_2"]]$ds <- data_list[["dummy_2"]]$ds |> dplyr::mutate(DSSTDTC = as.Date(.data[["DSSTDTC"]]))
  data_list[["dummy_2"]]$ae <- data_list[["dummy_2"]]$ae |> dplyr::mutate(AESTDTC = as.Date(.data[["AESTDTC"]]))


  reporting_rates <- mod_report_rates(module_id = "id_rr",
                                      dm_dataset_name = "dm",
                                      ds_dataset_name = "ds",
                                      ae_dataset_name = "ae",
                                      subjid_var = "USUBJID",
                                      tooltip_decimal_places = 4L,
                                      disposition_events = list(
                                         event_var = "DSDECOD",
                                         date_var = "DSSTDTC",
                                         day_var = "DSSTDY",
                                         entry_vals = c("RANDOMIZED"),
                                         exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT", "DEATH")
                                      ),
                                      adverse_events = list(
                                         date_var = "AESTDTC",
                                         day_var = "AESTDY"
                                      ),
                                      grouping_vars = list(choices = c("ARM", "ACTARM", "SEX", "SITEID"),
                                                           default_choice = "ARM"),
                                      x_step_size_list = list(days = 50L, weeks = 2L, months = 1L))

  # Launching the DaVinci app
  dv.manager::run_app(
    data = data_list,
    module_list = list("AE Reporting Rates" = reporting_rates),
    filter_data = "dm",
    filter_key = "USUBJID"
  )
}

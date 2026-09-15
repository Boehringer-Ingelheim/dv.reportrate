library(dv.reportrate)

# Load dummy data for testing purpose
source("../../dummy-data.R")

dm <- dm_dummy
ds <- ds_dummy
ae <- ae_dummy

# important for testing the behaviour when switching the dataset
dm_2 <- dm[1:30, ] |> dplyr::mutate(SEX = as.factor("F"), # removed level "M"
                                   SITEID = 1) # SITEID not a factor anymore
ds_2 <- ds[1:100, ]
ae_2 <- ae[1:180, ]

dm_3 <- dm |> dplyr::select(-ARM) # important for testing the behaviour of group dropdown when switching the dataset
ds_3 <- ds_2
ae_3 <- ae_2

data_list <- list("dummy" = list("dm" = dm,
                                 "ds" = ds,
                                 "ae" = ae
                                 ),
                  "dummy_2" = list("dm" = dm_2,
                                   "ds" = ds_2,
                                   "ae" = ae_2),
                  "dummy_3" = list("dm" = dm_3,
                                   "ds" =  ds_3,
                                   "ae" = ae_3)
                  )

reporting_rates <- mod_report_rates(module_id = "reportrate",
                                    ae_dataset_name = "ae",
                                    ds_dataset_name = "ds",
                                    dm_dataset_name = "dm",
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
                                    grouping_vars = list(),
                                    x_step_size_list = list(days = 50L, weeks = 2L, months = 1L)
)

dv.manager::run_app(
   data = data_list,
   module_list = list(
      "Reporting Rates" = reporting_rates
   ),
   filter_data = "dm",
   filter_key = "USUBJID"
   , filter_type = "datasets"
)

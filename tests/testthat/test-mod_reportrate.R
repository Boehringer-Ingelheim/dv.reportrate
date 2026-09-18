source(test_path("dummy-data.R"))

test_that("report_rates_ui() fails when argument type mismatches", {
   expect_error(report_rates_ui(""))
   expect_error(report_rates_ui(4))
})

server_func <- function(id, dataset_list, subjid_var, tooltip_decimal_places, disposition_events, adverse_events,
                        grouping, x_step_size_list) {
   report_rates_server(module_id = id, dataset_list = dataset_list, subjid_var = subjid_var,
                       tooltip_decimal_places = tooltip_decimal_places, disposition_events = disposition_events,
                       adverse_events = adverse_events, grouping_vars = grouping, x_step_size_list = x_step_size_list)
}

test_that("report_rates_server() fails when argument type mismatches", {
   # Prepare test arguments
   id_valid <- "test"
   id_null <- NULL
   id_num <- 4
   id_zero <- ""

   datalist_valid <- shiny::reactive({
      list("dm" = dm_dummy, "ae" = ae_dummy, "ds" = ds_dummy)
   })
   datalist_not_reactive <- list("dm" = dm_dummy, "ae" = ae_dummy, "ds" = ds_dummy)
   datalist_no_df <- shiny::reactive(list(string = "Not a df", num = 1))
   datalist_no_list <- shiny::reactive(ae_dummy)
   datalist_null <- shiny::reactive(NULL)
   datalist_unnamed <- shiny::reactive(list(dm_dummy, ae_dummy, ds_dummy))

   subjid_valid <- "USUBJID"
   subjid_null <- NULL
   subjid_num <- 3
   subjid_zero <- ""
   subjid_not_in_dm <- "SUBJECTID"

   tt_decimal_places_valid <- 4L
   tt_decimal_places_null <- NULL
   tt_decimal_places_negative <- -4L
   tt_decimal_places_double <- 4
   tt_decimal_places_string <- "4L"
   tt_decimal_places_vec <- c(3L, 4L)

   dispo_events_valid <- list(
      event_var = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_no_list <- "Not a list"
   dispo_events_null <- NULL
   dispo_events_unnamed <- list("DSDECOD",
                                "DSSTDTC",
                                "DSSTDY",
                                c("RANDOMIZED"),
                                c("COMPLETED", "WITHDRAWAL BY SUBJECT"))
   dispo_events_wrong_type <- list(
      event_var = TRUE,
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_missings <- list(
      event_var = "DSDECOD",
      date_var = NA_character_,
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_wrong_names <- list(
      event_variable = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_string_zero <- list(
      event_var = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_char_zero <- list(
      event_var = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c(""),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_vec <- list(
      event_var = c("DSDECOD", "DSTERM"),
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_event_var_not_in_ds <- list(
      event_var = "TEST",
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_date_var_not_in_ds <- list(
      event_var = "DSDECOD",
      date_var = "TEST",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_date_var_not_date <- list(
      event_var = "DSDECOD",
      date_var = "DSDECOD",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_day_var_not_in_ds <- list(
      event_var = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "TEST",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_day_var_not_numeric <- list(
      event_var = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "DSDECOD",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   dispo_events_intersecting_vals <- list(
      event_var = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED", "COMPLETED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )

   adverse_events_valid <- list(date_var = "AESTDTC", day_var = "AESTDY")
   adverse_events_no_list <- "Not a list"
   adverse_events_null <- NULL
   adverse_events_unnamed <- list("AESTDTC", "AESTDY")
   adverse_events_wrong_type <- list(date_var = "AESTDTC", day_var = 30)
   adverse_events_missings <- list(date_var = NA_character_, day_var = "AESTDY")
   adverse_events_wrong_names <- list(date_variable = "AESTDTC", day_var = "AESTDY")
   adverse_events_zero <- list(date_var = "", day_var = "AESTDY")
   adverse_events_vec <- list(date_var = c("AESTDTC", "AEENDTC"), day_var = "AESTDY")
   adverse_events_date_var_not_in_ae <- list(date_var = "TEST", day_var = "AESTDY")
   adverse_events_date_var_not_date <-  list(date_var = "AETERM", day_var = "AESTDY")
   adverse_events_day_var_not_in_ae <- list(date_var = "AESTDTC", day_var = "TEST")
   adverse_events_day_var_not_numeric <-  list(date_var = "AESTDTC", day_var = "AETERM")

   grouping_valid <- list(choices = c("ARM", "SEX", "SITEID"), default_choice = "SITEID")
   grouping_valid_2 <- list(choices = c("ARM", "SEX", "SITEID"))
   grouping_valid_3 <- list(default_choice = "SITEID")
   grouping_valid_4 <- list(choices = "ARM")
   grouping_valid_5 <- list(choices = c("ARM", "SEX", "SITEID", "TEST"), # doesnt fail when a choice is not in dm
                            default_choice = "SITEID")
   grouping_valid_6 <- list(choices = c("SITEID", "COUNTRY"), # doesnt fail when a choice isn't of type factor (COUNTRY)
                                     default_choice = "SITEID")
   grouping_valid_7 <- list(default_choice = "TEST") # doesnt fail when the default choice isn't in dm
   grouping_valid_8 <- list(default_choice = "COUNTRY") # doesnt fail when the default choice isn't a factor
   grouping_no_list <- "Not a list"
   grouping_null <- NULL
   grouping_unnamed <- list(c("ARM", "SEX", "SITEID"), "SITEID")
   grouping_wrong_type <- list(choices = c("ARM", "SEX", "SITEID"), default_choice = 1)
   grouping_choices_list <- list(choices = list("ARM", "SEX", "SITEID"), default_choice = "SITEID")
   grouping_wrong_names <- list(available_choices = c("ARM", "SEX", "SITEID"), default_choice = "SITEID")
   grouping_char_zero <- list(choices = c(""), default_choice = "SITEID")
   grouping_default_vec <- list(choices = c("ARM", "SEX", "SITEID"), default_choice = c("SITEID", "ARM"))
   grouping_default_not_subset <- list(choices = c("SEX", "SITEID"), default_choice = "ARM")

   step_size_valid <- list(days = 50L, weeks = 2L, months = 1L)
   step_size_valid_2 <- list(days = 100L)
   step_size_valid_3 <- list(weeks = 1L)
   step_size_valid_4 <- list(months = 2L)
   step_size_valid_5 <- list(weeks = 3L, months = 3L)
   step_size_valid_6 <- list()
   step_size_unnamed <- list(50L, 2L, 1L)
   step_size_double <- list(days = 50, weeks = 2, months = 1)
   step_size_wrong_names <- list(days = 50L, week = 2L, months = 1L)
   step_size_zero <- list(days = 0L, weeks = 2L, months = 1L)
   step_size_negative <- list(days = 50L, weeks = 2L, months = -1L)


   # Cases that expect an error
   test_id <- list(id_null, id_num, id_zero)
   test_datalist <- list(datalist_not_reactive, datalist_no_df, datalist_no_list, datalist_null, datalist_unnamed)
   test_subjid <- list(subjid_null, subjid_num, subjid_zero, subjid_not_in_dm)
   test_tt_decimal_places <- list(tt_decimal_places_null,
                                  tt_decimal_places_negative,
                                  tt_decimal_places_double,
                                  tt_decimal_places_string,
                                  tt_decimal_places_vec)
   test_dispo_events <- list(dispo_events_no_list,
                             dispo_events_null,
                             dispo_events_unnamed,
                             dispo_events_wrong_type,
                             dispo_events_missings,
                             dispo_events_wrong_names,
                             dispo_events_string_zero,
                             dispo_events_char_zero,
                             dispo_events_vec,
                             dispo_events_event_var_not_in_ds,
                             dispo_events_date_var_not_in_ds,
                             dispo_events_date_var_not_date,
                             dispo_events_day_var_not_in_ds,
                             dispo_events_day_var_not_numeric,
                             dispo_events_intersecting_vals
                             )
   test_adverse_events <- list(adverse_events_no_list,
                               adverse_events_null,
                               adverse_events_unnamed,
                               adverse_events_wrong_type,
                               adverse_events_missings,
                               adverse_events_wrong_names,
                               adverse_events_zero,
                               adverse_events_vec,
                               adverse_events_date_var_not_in_ae,
                               adverse_events_date_var_not_date,
                               adverse_events_day_var_not_in_ae,
                               adverse_events_day_var_not_numeric
                               )
   test_grouping <- list(grouping_null,
                         grouping_no_list,
                         grouping_unnamed,
                         grouping_wrong_type,
                         grouping_choices_list,
                         grouping_wrong_names,
                         grouping_char_zero,
                         grouping_default_vec,
                         grouping_default_not_subset
                         )
   test_step_size <- list(step_size_unnamed,
                          step_size_double,
                          step_size_wrong_names,
                          step_size_zero,
                          step_size_negative)

   # Execute test cases
   ## test module_id parameter
   purrr::walk(test_id, ~ expect_error(
      shiny::testServer(server_func, args = list(
         id = .x, dataset_list = datalist_valid, subjid_var = subjid_valid,
         tooltip_decimal_places = tt_decimal_places_valid, disposition_events = dispo_events_valid,
         adverse_events = adverse_events_valid, grouping = grouping_valid, x_step_size_list = step_size_valid
      ), {
            session$flushReact()
            expect_true(TRUE)
      })
   ))
   ## test dataset_list parameter
   purrr::walk(test_datalist, ~ expect_error(
      shiny::testServer(server_func, args = list(
         id = id_valid, dataset_list = .x, subjid_var = subjid_valid,
         tooltip_decimal_places = tt_decimal_places_valid, disposition_events = dispo_events_valid,
         adverse_events = adverse_events_valid, grouping = grouping_valid, x_step_size_list = step_size_valid
      ), {
         v_dataset_list() # call reactive to perform checks on the data frames to trigger error message
         session$flushReact() # ensure all reactive updates are done before assertions
         expect_true(TRUE)
      })
   ))
   ## test subjid_var parameter
   purrr::walk(test_subjid, ~ expect_error(
      shiny::testServer(server_func, args = list(
         id = id_valid, dataset_list = datalist_valid, subjid_var = .x,
         tooltip_decimal_places = tt_decimal_places_valid, disposition_events = dispo_events_valid,
         adverse_events = adverse_events_valid, grouping = grouping_valid, x_step_size_list = step_size_valid
      ), {
         v_dataset_list()
         session$flushReact() # ensure all reactive updates are done before assertions
         expect_true(TRUE)
      })
   ))
   ## test tooltip_decimal_places parameter
   purrr::walk(test_tt_decimal_places, ~ expect_error(
      shiny::testServer(server_func, args = list(
         id = id_valid, dataset_list = datalist_valid, subjid_var = subjid_valid,
         tooltip_decimal_places = .x, disposition_events = dispo_events_valid,
         adverse_events = adverse_events_valid, grouping = grouping_valid, x_step_size_list = step_size_valid
      ), {
         session$flushReact() # ensure all reactive updates are done before assertions
         expect_true(TRUE)
      })
   ))
   ## test disposition_events parameter
   purrr::walk(test_dispo_events, ~ expect_error(
      shiny::testServer(server_func, args = list(
         id = id_valid, dataset_list = datalist_valid, subjid_var = subjid_valid,
         tooltip_decimal_places = tt_decimal_places_valid, disposition_events = .x,
         adverse_events = adverse_events_valid, grouping = grouping_valid, x_step_size_list = step_size_valid
      ), {
         v_dataset_list()
         session$flushReact() # ensure all reactive updates are done before assertions
         expect_true(TRUE)
      })
   ))
   ## test adverse events parameter
   purrr::walk(test_adverse_events, ~ expect_error(
      shiny::testServer(server_func, args = list(
         id = id_valid, dataset_list = datalist_valid, subjid_var = subjid_valid,
         tooltip_decimal_places = tt_decimal_places_valid, disposition_events = dispo_events_valid,
         adverse_events = .x, grouping = grouping_valid, x_step_size_list = step_size_valid
      ), {
         v_dataset_list()
         session$flushReact() # ensure all reactive updates are done before assertions
         expect_true(TRUE)
      })
   ))
   ## test grouping parameter
   purrr::walk(test_grouping, ~ expect_error(
      shiny::testServer(server_func, args = list(
         id = id_valid, dataset_list = datalist_valid, subjid_var = subjid_valid,
         tooltip_decimal_places = tt_decimal_places_valid, disposition_events = dispo_events_valid,
         adverse_events = adverse_events_valid, grouping = .x, x_step_size_list = step_size_valid
      ), {
         v_dataset_list()
         session$flushReact() # ensure all reactive updates are done before assertions
         expect_true(TRUE)
      })
   ))
   ## test x_step_size_list parameter
   purrr::walk(test_step_size, ~ expect_error(
      shiny::testServer(server_func, args = list(
         id = id_valid, dataset_list = datalist_valid, subjid_var = subjid_valid,
         tooltip_decimal_places = tt_decimal_places_valid, disposition_events = dispo_events_valid,
         adverse_events = adverse_events_valid, grouping = grouping_valid, x_step_size_list = .x
      ), {
         session$flushReact() # ensure all reactive updates are done before assertions
         expect_true(TRUE)
      })
   ))

   # Verify that valid arguments launch the server as intended.
   test_valid_grouping <- list(grouping_valid,
                               grouping_valid_2,
                               grouping_valid_3,
                               grouping_valid_4,
                               grouping_valid_5,
                               grouping_valid_6,
                               grouping_valid_7,
                               grouping_valid_8)

   test_valid_step_size <- list(step_size_valid,
                                step_size_valid_2,
                                step_size_valid_3,
                                step_size_valid_4,
                                step_size_valid_5,
                                step_size_valid_6)

   # suppress info messages because validate_dm_dataset() and validate_grouping_selection() output info messages when a
   # choice or the default_choice is not valid. In this case the app should work successfully anyways because
   # validate_grouping_selection() takes care of setting only valid variables for grouping.
   purrr::walk(test_valid_grouping, ~ suppressMessages(
      expect_success(
         shiny::testServer(server_func, args = list(
            id = id_valid, dataset_list = datalist_valid, subjid_var = subjid_valid,
            tooltip_decimal_places = tt_decimal_places_valid, disposition_events = dispo_events_valid,
            adverse_events = adverse_events_valid, grouping = .x, x_step_size_list = step_size_valid
         ), {
            v_dataset_list()
            session$flushReact()
            expect_true(TRUE)
         })
      )
   ))

   purrr::walk(test_valid_step_size, ~ expect_success(
      shiny::testServer(server_func, args = list(
         id = id_valid, dataset_list = datalist_valid, subjid_var = subjid_valid,
         tooltip_decimal_places = tt_decimal_places_valid, disposition_events = dispo_events_valid,
         adverse_events = adverse_events_valid, grouping = grouping_valid, x_step_size_list = .x
      ), {
         v_dataset_list()
         session$flushReact()
         expect_true(TRUE)
      })
   ))


})





# Prepare flexible server function where parameters can be omitted
server_func_m_params <- function(id, ...) {
   args <- list(...)
   args$module_id <- id
   do.call(report_rates_server, args)
}


test_that("report_rates_server() fails when mandatory parameters aren't passed.", {
   id_valid <- "test"
   datalist_valid <- shiny::reactive({
      list("dm" = dm_dummy, "ae" = ae_dummy, "ds" = ds_dummy)
   })
   tt_decimal_places_valid <- 4L
   dispo_events_valid <- list(
      event_var = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   adverse_events_valid <- list(date_var = "AESTDTC", day_var = "AESTDY")

   ## testing the omission of the report_rates_server() parameter "module_id"
   expect_error(
      shiny::testServer(server_func_m_params, args = list(
         dataset_list = datalist_valid, disposition_events = dispo_events_valid, adverse_events = adverse_events_valid)
      )
   )
   ## testing the omission of the report_rates_server() parameter "dataset_list"
   expect_error(
      shiny::testServer(server_func_m_params, args = list(
         id = id_valid, disposition_events = dispo_events_valid, adverse_events = adverse_events_valid)
      )
   )
   ## testing the omission of the report_rates_server() parameter "disposition_events"
   expect_error(
      shiny::testServer(server_func_m_params, args = list(
         id = id_valid, dataset_list = datalist_valid, adverse_events = adverse_events_valid)
      )
   )
   ## testing the omission of the report_rates_server() parameter "adverse_events"
   expect_error(
      shiny::testServer(server_func_m_params, args = list(
         id = id_valid, dataset_list = datalist_valid, disposition_events = dispo_events_valid)
      )
   )

   ## Verify that the omission of optional parameters of report_rates_server() still launches the server as intended
   expect_success(
      shiny::testServer(server_func_m_params, args = list(
         id = id_valid, dataset_list = datalist_valid, disposition_events = dispo_events_valid,
         adverse_events = adverse_events_valid
      ), {
         v_dataset_list()
         session$flushReact()
         expect_true(TRUE)
      })
   )
})


test_that("If one of the dm-, ds- or ae- dataframe has zero rows or no levels are selected while the 'show
                    ungrouped' checkbox is FALSE, the message 'No data available' will be displayed instead of the
                    plot", {
   datalist <- shiny::reactive({
      list("dm" = dm_dummy, "ae" = ae_dummy, "ds" = ds_dummy)
   })
   dispo_events_valid <- list(
      event_var = "DSDECOD",
      date_var = "DSSTDTC",
      day_var = "DSSTDY",
      entry_vals = c("RANDOMIZED"),
      exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")
   )
   adverse_events_valid <- list(date_var = "AESTDTC", day_var = "AESTDY")

   #testing the case for no selected levels (but not empty dm, ds, ae)
   shiny::testServer(server_func, args = list(
            id = "test",
            dataset_list = datalist,
            subjid_var = "USUBJID",
            tooltip_decimal_places = 4L,
            disposition_events = dispo_events_valid,
            adverse_events = adverse_events_valid,
            grouping = list(choices = c("ARM", "SEX", "SITEID")),
            x_step_size_list = list(days = 50L, weeks = 2L, months = 1L)
         ), {
            # setting the inputs
            session$setInputs(!!REPORT_RATES$ID$METRIC_BUTTONS := REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)
            session$setInputs(!!REPORT_RATES$ID$TIMETYPE_BUTTONS := REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1)
            session$setInputs(!!REPORT_RATES$ID$GROUP_VAR := "SEX")
            session$flushReact()
            session$setInputs(!!REPORT_RATES$ID$LEVELS_DROPDOWN := c("F", "M"))
            session$setInputs(!!REPORT_RATES$ID$UNGROUPED_CHECKBOX := FALSE)
            session$flushReact()
            expect_no_error({
               output[[REPORT_RATES$ID$PLOT]]
            })
            # Deselection of all levels should lead to the message "No data available"
            session$setInputs(!!REPORT_RATES$ID$LEVELS_DROPDOWN := c())
            session$flushReact()
            expect_error(
               {output[[REPORT_RATES$ID$PLOT]]},
               regexp = "No data available.", class = "shiny.silent.error"
            )
            # If no levels are selected but the 'show ungrouped' checkbox is ticked, the plot should be shown
            session$setInputs(!!REPORT_RATES$ID$UNGROUPED_CHECKBOX := TRUE)
            session$flushReact()
            expect_no_error({
               output[[REPORT_RATES$ID$PLOT]]
            })
         }
   )

   # testing the case for empty dm dataframe
   datalist <- shiny::reactive({
      list("dm" = dm_dummy[0, , drop = FALSE], "ae" = ae_dummy, "ds" = ds_dummy)
   })
   shiny::testServer(server_func, args = list(
            id = "test",
            dataset_list = datalist,
            subjid_var = "USUBJID",
            tooltip_decimal_places = 4L,
            disposition_events = dispo_events_valid,
            adverse_events = adverse_events_valid,
            grouping = list(choices = c("ARM", "SEX", "SITEID")),
            x_step_size_list = list(days = 50L, weeks = 2L, months = 1L)
         ), {
            # setting the inputs
            session$setInputs(!!REPORT_RATES$ID$METRIC_BUTTONS := REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)
            session$setInputs(!!REPORT_RATES$ID$TIMETYPE_BUTTONS := REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1)
            session$setInputs(!!REPORT_RATES$ID$GROUP_VAR := "SEX")
            session$flushReact()
            # setting the ungrouped checkbox TRUE so that it is tested that the error is really caused by the empty dm
            session$setInputs(!!REPORT_RATES$ID$UNGROUPED_CHECKBOX := TRUE)
            session$flushReact()

            expect_error(
               {output[[REPORT_RATES$ID$PLOT]]},
               regexp = "No data available.", class = "shiny.silent.error"
            )
         }
   )

   # testing the case for empty ds dataframe
   datalist <- shiny::reactive({
      list("dm" = dm_dummy, "ae" = ae_dummy, "ds" = ds_dummy[0, , drop = FALSE])
   })
   shiny::testServer(server_func, args = list(
            id = "test",
            dataset_list = datalist,
            subjid_var = "USUBJID",
            tooltip_decimal_places = 4L,
            disposition_events = dispo_events_valid,
            adverse_events = adverse_events_valid,
            grouping = list(choices = c("ARM", "SEX", "SITEID")),
            x_step_size_list = list(days = 50L, weeks = 2L, months = 1L)
         ), {
            # setting the inputs
            session$setInputs(!!REPORT_RATES$ID$METRIC_BUTTONS := REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)
            session$setInputs(!!REPORT_RATES$ID$TIMETYPE_BUTTONS := REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1)
            session$setInputs(!!REPORT_RATES$ID$GROUP_VAR := "SEX")
            session$flushReact()
            session$setInputs(!!REPORT_RATES$ID$LEVELS_DROPDOWN := c("F", "M"))
            session$flushReact()

            expect_error(
               {output[[REPORT_RATES$ID$PLOT]]},
               regexp = "No data available.", class = "shiny.silent.error"
            )
         }
   )

   # testing the case for empty ae dataframe
   datalist <- shiny::reactive({
      list("dm" = dm_dummy, "ae" = ae_dummy[0, , drop = FALSE], "ds" = ds_dummy)
   })
   shiny::testServer(server_func, args = list(
            id = "test",
            dataset_list = datalist,
            subjid_var = "USUBJID",
            tooltip_decimal_places = 4L,
            disposition_events = dispo_events_valid,
            adverse_events = adverse_events_valid,
            grouping = list(choices = c("ARM", "SEX", "SITEID")),
            x_step_size_list = list(days = 50L, weeks = 2L, months = 1L)
         ), {
            # setting the inputs
            session$setInputs(!!REPORT_RATES$ID$METRIC_BUTTONS := REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1)
            session$setInputs(!!REPORT_RATES$ID$TIMETYPE_BUTTONS := REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1)
            session$setInputs(!!REPORT_RATES$ID$GROUP_VAR := "SEX")
            session$flushReact()
            session$setInputs(!!REPORT_RATES$ID$LEVELS_DROPDOWN := c("F", "M"))
            session$flushReact()

            expect_error(
               {output[[REPORT_RATES$ID$PLOT]]},
               regexp = "No data available.", class = "shiny.silent.error"
            )
         }
   )
})





# App tests
test_that("The default values are correct at app launch", {

   app_dir <- test_path("apps/reportrates_app")
   app_defaults <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "reportrates_app",
      options = list(shiny.trace = TRUE, chromote.headless = TRUE)
   )
   app_defaults$wait_for_idle()

   # Get values and test
   actual_metric <- app_defaults$get_value(input = "reportrate-metric_id")
   expected_metric <- "Cumulative rate per total exposure time"
   expect_identical(actual_metric, expected_metric)

   actual_grouping <- app_defaults$get_value(input = "reportrate-grouping-group_var")
   expected_grouping <- REPORT_RATES$CHOICES$GROUP_NO_SELECTION
   expect_identical(actual_grouping, expected_grouping)

   actual_levels <- app_defaults$get_value(input = "reportrate-selected_levels")
   expected_levels <- NULL
   expect_identical(actual_levels, expected_levels)

   actual_checkbox <- app_defaults$get_value(input = "reportrate-ungrouped")
   expected_checkbox <- FALSE
   expect_identical(actual_checkbox, expected_checkbox)

   actual_timetype <- app_defaults$get_value(input = "reportrate-type_id")
   expected_timetype <- "By study date"
   expect_identical(actual_timetype, expected_timetype)

   actual_unit <- app_defaults$get_value(input = "reportrate-unit_id")
   expected_unit <- "Week"
   expect_identical(actual_unit, expected_unit)

   app_defaults$stop()
})



test_that("The default values are correct at app launch with an argument passed to the
                    grouping$default_choice parameter of the server function" |>
                  vdoc[["add_spec"]](specs$app_creator_settings$grouping), {

   app_dir <- test_path("apps/default_choice_app")
   app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "default_choice_app",
      options = list(shiny.trace = TRUE, chromote.headless = TRUE)
   )
   app$wait_for_idle()

   # Get values and test
   actual_metric <- app$get_value(input = "reportrate-metric_id")
   expected_metric <- "Cumulative rate per total exposure time"
   expect_identical(actual_metric, expected_metric)

   actual_grouping <- app$get_value(input = "reportrate-grouping-group_var")
   expected_grouping <- "SEX" # because of the parameter grouping$default_choice in reportrates_app/app.R
   expect_identical(actual_grouping, expected_grouping)

   actual_levels <- app$get_value(input = "reportrate-selected_levels")
   expected_levels <- c("M", "F")
   expect_setequal(actual_levels, expected_levels)

   actual_checkbox <- app$get_value(input = "reportrate-ungrouped")
   expected_checkbox <- FALSE
   expect_identical(actual_checkbox, expected_checkbox)

   actual_timetype <- app$get_value(input = "reportrate-type_id")
   expected_timetype <- "By study date"
   expect_identical(actual_timetype, expected_timetype)

   actual_unit <- app$get_value(input = "reportrate-unit_id")
   expected_unit <- "Week"
   expect_identical(actual_unit, expected_unit)

   app$stop()
})



test_that("All available levels are selected by default after changing the grouping variable" |>
                       vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
   app_dir <- test_path("apps/reportrates_app")
   app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "reportrates_app",
      options = list(shiny.trace = TRUE, chromote.headless = TRUE)
   )
   app$wait_for_idle()

   # Get values and test
   app$set_inputs(`reportrate-grouping-group_var` = "ARM")
   app$wait_for_idle()

   actual_levels <- app$get_value(input = "reportrate-selected_levels")
   expected_levels <- unique(dm_dummy$ARM)
   expect_setequal(actual_levels, expected_levels)

   # now testing, that all levels are being selected when changing the grouping variable; even with same level names
   app$set_inputs(`reportrate-selected_levels` = c("Drug 1", "Drug 2")) # deselect "Placebo"
   app$set_inputs(`reportrate-grouping-group_var` = "ACTARM")
   actual_levels <- app$get_value(input = "reportrate-selected_levels")
   expected_levels <- unique(dm_dummy$ACTARM)
   expect_setequal(actual_levels, expected_levels)

   app$stop()
})



test_that("The 'show ungrouped' checkbox is set to FALSE when changing the grouping variable to no
                    grouping" |>
                  vdoc[["add_spec"]](specs$plot_creation$grouping_and_levels$ungrouped), {
   app_dir <- test_path("apps/reportrates_app")
   app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "reportrates_app",
      options = list(shiny.trace = TRUE, chromote.headless = TRUE)
   )
   app$wait_for_idle()

   # Get values and test
   app$set_inputs(`reportrate-grouping-group_var` = "ARM")
   app$wait_for_idle()
   app$set_inputs(`reportrate-ungrouped` = TRUE)
   app$set_inputs(`reportrate-grouping-group_var` = REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   app$wait_for_idle()
   actual_value <- app$get_value(input = "reportrate-ungrouped")
   expected_value <- FALSE
   expect_setequal(actual_value, expected_value)

   app$stop()
})



test_that("The default choice for grouping is only used once when starting the app" |>
                  vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
   app_dir <- test_path("apps/default_choice_app")
   app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "default_choice_app",
      options = list(shiny.trace = TRUE, chromote.headless = TRUE)
   )
   app$wait_for_idle()

   # Get values and test
   actual_grouping <- app$get_value(input = "reportrate-grouping-group_var")
   expected_grouping <- "SEX" # was passed as argument to the grouping$default_choice param in default_choice_app/app.R
   expect_identical(actual_grouping, expected_grouping)

   # should be reset to NULL after it was used (so that the default gets only used once right after starting the app)
   group_default_choice <- app$get_value(export = "reportrate-group_default_choice") # reactiveVal that stores the
                                                                                    # default choice inside the
                                                                                    # module server
   expect_null(group_default_choice)

   # When switching the dataset, the default choice shouldn't be used again
   app$set_inputs(`reportrate-grouping-group_var` = REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   app$set_inputs(selector = "dummy_2")
   app$wait_for_value(input = "selector", ignore = list("dummy"))
   app$wait_for_idle()
   actual_grouping <- app$get_value(input = "reportrate-grouping-group_var")
   expected_grouping <- REPORT_RATES$CHOICES$GROUP_NO_SELECTION
   expect_identical(actual_grouping, expected_grouping)

   app$stop()
})



test_that("The selected grouping variable and the levels are correct also when the dataset selection in the
                    module manager changes" |>
                  vdoc[["add_spec"]](specs$framework_specs$filter_and_datasets), {
   app_dir <- test_path("apps/mm_app")
   app <- shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "mm_app",
      options = list(shiny.trace = TRUE, chromote.headless = TRUE)
   )
   app$wait_for_idle()
   app$set_inputs(`reportrate-grouping-group_var` = "ARM")
   app$set_inputs(`reportrate-selected_levels` = c("Drug 1", "Drug 2")) # deselect placebo
   vals <- app$get_values()


   app$set_inputs(selector = "dummy_2")
   app$wait_for_idle()
   selected_df <- app$get_value(input = "selector")
   expect_equal(selected_df, "dummy_2") # verifying that the switch in the dataset selection worked

   # ARM as the selected grouping variable is present in both datasets "dummy" and "dummy_2".
   #     --> should be kept as the selected grouping variable
   app$wait_for_idle()
   actual_group <- app$get_value(input = "reportrate-grouping-group_var")
   expected_group <- "ARM"
   expect_identical(actual_group, expected_group)

   # Drug 1 and Drug 2 as the selected levels are present in both datasets --> should be kept as the selected levels
   actual_levels <- app$get_value(input = "reportrate-selected_levels")
   expected_levels <- c("Drug 1", "Drug 2")
   expect_setequal(actual_levels, expected_levels)

   # Column ARM is not in the dm dataset of dummy_3 --> grouping variable selection should be reset to no selection
   app$set_inputs(selector = "dummy_3")
   app$wait_for_idle()
   selected_df <- app$get_value(input = "selector")
   expect_equal(selected_df, "dummy_3") # verifying that the switch in the dataset selection worked

   app$wait_for_value(input = "reportrate-grouping-group_var", ignore = list("ARM")) # waiting until it's loaded
   actual_group <- app$get_value(input = "reportrate-grouping-group_var")
   expected_group <- REPORT_RATES$CHOICES$GROUP_NO_SELECTION
   expect_identical(actual_group, expected_group)

   # grouping variable was set to -None- so the selected levels should be NULL
   actual_levels <- app$get_value(input = "reportrate-selected_levels")
   expected_levels <- NULL
   expect_identical(actual_levels, expected_levels)

   # in "dummy_3" SEX has the levels "M" and "F". In "dummy_2" SEX has only the level "F". So when switching the dataset
   # "M" should be deselected automatically
   app$set_inputs(`reportrate-grouping-group_var` = "SEX")
   app$wait_for_idle()
   actual_levels <- app$get_value(input = "reportrate-selected_levels")
   expected_levels <- c("M", "F")
   expect_setequal(actual_levels, expected_levels)
   app$set_inputs(selector = "dummy_2")

   app$wait_for_value(input = "reportrate-selected_levels",
                      ignore = list(c("F", "M"), c("M", "F"))) # waiting until it's loaded
   actual_levels <- app$get_value(input = "reportrate-selected_levels")
   expected_levels <- c("F")
   expect_identical(actual_levels, expected_levels)

   # When switching back to "dummy", selected grouping variable and levels should still be the same
   app$set_inputs(selector = "dummy")
   app$wait_for_idle(duration = 1000)

   actual_group <- app$get_value(input = "reportrate-grouping-group_var")
   expected_group <- "SEX"
   actual_levels <- app$get_value(input = "reportrate-selected_levels")
   expected_levels <- c("F")
   expect_identical(actual_group, expected_group)
   expect_identical(actual_levels, expected_levels)

   # SITEID is not a factor in "dummy_2" so switching the dataset selection should reset the grouping sel to -None-
   app$set_inputs(`reportrate-grouping-group_var` = "SITEID")
   app$wait_for_idle(duration = 1000)
   app$set_inputs(selector = "dummy_2")

   app$wait_for_value(input = "reportrate-grouping-group_var", ignore = list("SITEID")) # waiting until it's loaded
   actual_group <- app$get_value(input = "reportrate-grouping-group_var")
   expected_group <- REPORT_RATES$CHOICES$GROUP_NO_SELECTION
   expect_identical(actual_group, expected_group)

   app$stop()
})

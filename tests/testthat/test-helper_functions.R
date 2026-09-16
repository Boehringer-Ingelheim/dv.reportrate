source(test_path("dummy-data.R"))

# Function validate_and_fill()
# the validation logic of the function is tested in "test-mod_reportrate.R" with the tests for report_rates_server()
test_that("validate_and_fill() fills in the default values for the step sizes (days, weeks, months) if they are not
          supplied." |>
             vdoc[["add_spec"]](specs$app_creator_settings$step_size), {
   input_args <- list(module_id = "test",
                      dataset_list = shiny::reactive({
                         list("dm" = dm_dummy, "ae" = ae_dummy, "ds" = ds_dummy)
                      }),
                      subjid_var = "USBUJID",
                      tooltip_decimal_places = 4L,
                      disposition_events = list(event_var = "DSDECOD",
                                                date_var = "DSSTDTC",
                                                day_var = "DSSTDY",
                                                entry_vals = c("RANDOMIZED"),
                                                exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT")),
                      adverse_events = list(date_var = "AESTDTC", day_var = "AESTDY"),
                      grouping_vars = list(choices = c("ARM", "SEX", "SITEID"), default_choice = "SITEID"),
                      x_step_size_list = list(days = 40L, weeks = 10L, months = 3L))

   days_default <- REPORT_RATES$DEFAULTS$STEPSIZE_DAYS
   weeks_default <- REPORT_RATES$DEFAULTS$STEPSIZE_WEEKS
   months_default <- REPORT_RATES$DEFAULTS$STEPSIZE_MONTHS

   # valid x_step_size_list shouldn't be changed
   res <- validate_and_fill(input_args)
   actual <- res$x_step_size_list
   expected <- list(days = 40L, weeks = 10L, months = 3L)
   expect_identical(actual, expected)

   # testing the filling with defaults for days:
   input_args$x_step_size_list$days <- NULL
   res <- validate_and_fill(input_args)
   actual_days <- res$x_step_size_list$days
   expect_identical(actual_days, days_default)
   # the other values should not be set to their defauts, because they were supplied
   actual_weeks <- res$x_step_size_list$weeks
   expect_identical(actual_weeks, 10L)
   actual_months <- res$x_step_size_list$months
   expect_identical(actual_months, 3L)
   input_args$x_step_size_list$days <- 40L # resetting the days to the passed value at the beginning

   # testing the filling with defaults for weeks:
   input_args$x_step_size_list$weeks <- NULL
   res <- validate_and_fill(input_args)
   actual_weeks <- res$x_step_size_list$weeks
   expect_identical(actual_weeks, weeks_default)
   # the other values should not be set to their defauts, because they were supplied
   actual_days <- res$x_step_size_list$days
   expect_identical(actual_days, 40L)
   actual_months <- res$x_step_size_list$months
   expect_identical(actual_months, 3L)
   input_args$x_step_size_list$weeks <- 10L # resetting the days to the passed value at the beginning

   # testing the filling with defaults for months:
   input_args$x_step_size_list$months <- NULL
   res <- validate_and_fill(input_args)
   actual_months <- res$x_step_size_list$months
   expect_identical(actual_months, months_default)
   # the other values should not be set to their defauts, because they were supplied
   actual_days <- res$x_step_size_list$days
   expect_identical(actual_days, 40L)
   actual_weeks <- res$x_step_size_list$weeks
   expect_identical(actual_weeks, 10L)
   input_args$x_step_size_list$months <- 3L # resetting the days to the passed value at the beginning

   # testing the filling with the defaults for 2 elements at once:
   input_args$x_step_size_list$days <- NULL
   input_args$x_step_size_list$months <- NULL
   res <- validate_and_fill(input_args)
   actual_days <- res$x_step_size_list$days
   expect_identical(actual_days, days_default)
   actual_months <- res$x_step_size_list$months
   expect_identical(actual_months, months_default)
   # the other value should still have its original value:
   actual_weeks <- res$x_step_size_list$weeks
   expect_identical(actual_weeks, 10L)

   # testing the filling with defaults for all 3 elements at once:
   input_args$x_step_size_list$weeks <- NULL
   res <- validate_and_fill(input_args)
   actual <- res$x_step_size_list
   expected <- list(days = days_default, weeks = weeks_default, months = months_default)
   expect_identical(actual, expected)
})



# Function validate_dm_dataset
df_validate_dm <- data.frame(
   USUBJID = c("01", "02", "03"),
   ARM = as.factor(c("a", "b", "c")),
   SEX = as.factor(c("F", "M", "F")),
   SITEID = c("SITE1", "SITE2", "SITE1"), # not a factor
   AGE = c(56, 63, 25) # not a factor
)
grouping_choices <- c("ARM", "SEX")
test_that("validate_dm_dataset() throws no error if valid arguments are passed to the function" |>
             vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
   expect_no_error(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = grouping_choices,
                          default_choice = "ARM")
   )
})

test_that("validate_dm_dataset() throws no error if the grouping list is NULL" |>
             vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
   expect_no_error(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = NULL,
                          default_choice = NULL)
   )
})

test_that("validate_dm_dataset() throws no error if the dataset has 0 rows." |>
             vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
   df_zero <- df_validate_dm |> dplyr::slice(0)
   expect_no_error(
      validate_dm_dataset(dataset = df_zero,
                          subjid_var = "USUBJID",
                          grouping_choices = grouping_choices,
                          default_choice = "ARM")
   )
})

test_that("validate_dm_dataset() throws no error if there is only valid grouping_choices or only a valid default_choice
          supplied" |>
             vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
   expect_no_error(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = grouping_choices,
                          default_choice = NULL)
   )
   expect_no_error(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = NULL,
                          default_choice = "ARM")
   )
})

test_that("validate_dm_dataset() throws no error but outputs an infomessage, if a choice out of grouping_choices or the
           default_choice isn't in the colnames of the dataset" |>
             vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
    # case for choices
    expect_message(
       validate_dm_dataset(dataset = df_validate_dm,
                           subjid_var = "USUBJID",
                           grouping_choices = c("ARM", "COUNTRY"),
                           default_choice = "ARM"),
       paste0("The specified choice COUNTRY was removed, because this column is not a subset of the selected ",
              "demographics dataset column names.")
    )
    ## for more than one choice being wrong:
    msgs <- utils::capture.output(# need to capture messages first, because 2 different messages will be outputted
       validate_dm_dataset(dataset = df_validate_dm,
                           subjid_var = "USUBJID",
                           grouping_choices = c("ARM", "COUNTRY", "TEST"),
                           default_choice = "ARM"),
       type = "message"
    )
    expect_match(msgs[1], paste0("The specified choice COUNTRY was removed, because this column is not a subset of ",
                                 "the selected demographics dataset column names."))
    expect_match(msgs[2], paste0("The specified choice TEST was removed, because this column is not a subset of the ",
                                 "selected demographics dataset column names."))
    # case for default_choice
    expect_message(
       validate_dm_dataset(dataset = df_validate_dm,
                           subjid_var = "USUBJID",
                           grouping_choices = NULL,
                           default_choice = "COUNTRY"),
       paste0("The default choice COUNTRY was ignored because this column is not a subset of the selected ",
              "demographics dataset column names.")
   )
   # case for both
   msgs <- utils::capture.output(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = c("ARM", "COUNTRY"),
                          default_choice = "COUNTRY"),
      type = "message"
   )
   expect_match(msgs[1], paste0("The specified choice COUNTRY was removed, because this column is not a subset of the ",
                                "selected demographics dataset column names."))
   expect_match(msgs[2], paste0("The default choice COUNTRY was ignored because this column is not a subset of the ",
                                "selected demographics dataset column names."))
})

test_that("validate_dm_dataset() throws error if the argument for dataset is not of class data.frame", {
   not_a_df <- "test"
   expect_error(
      validate_dm_dataset(dataset = not_a_df,
                          subjid_var = "USUBJID",
                          grouping_choices = grouping_choices,
                          default_choice = "ARM")
   )
})

test_that("validate_dm_dataset() throws error if the subjid_var is not a column in the dataset", {
   expect_error(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "NEWID",
                          grouping_choices = grouping_choices,
                          default_choice = "ARM")
   )
})

test_that("validate_dm_dataset() throws no error but outputs an infomessage, if a choice out of grouping_choices or the
           default_choice is in the columnnames of the dataset but the column is not of type factor." |>
             vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
   # case for grouping_choices
   expect_message(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = c("ARM", "SITEID"),
                          default_choice = NULL),
      paste0("The specified choice SITEID was removed, because this column is not of type factor in the selected ",
             "demographics dataset.")
   )
   ## for more than one choice being wrong:
   msgs <- utils::capture.output(# need to capture messages first, because 2 different messages will be outputted
       validate_dm_dataset(dataset = df_validate_dm,
                           subjid_var = "USUBJID",
                           grouping_choices = c("ARM", "SITEID", "AGE"),
                           default_choice = "ARM"),
       type = "message"
    )
    expect_match(msgs[1], paste0("The specified choice SITEID was removed, because this column is not of type factor ",
                                 "in the selected demographics dataset."))
    expect_match(msgs[2], paste0("The specified choice AGE was removed, because this column is not of type factor in ",
                                 "the selected demographics dataset."))
   # case for default_choice
   expect_message(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = NULL,
                          default_choice = "SITEID"),
      paste0("The default choice SITEID was ignored because this column is not of type factor in the selected ",
             "demographics dataset.")
   )
   # case for both
   msgs <- utils::capture.output(# need to capture messages first, because 2 different messages will be outputted
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = c("ARM", "SITEID"),
                          default_choice = "SITEID"),
      type = "message"
   )
   expect_match(msgs[1], paste0("The specified choice SITEID was removed, because this column is not of type factor ",
                                "in the selected demographics dataset."))
   expect_match(msgs[2], paste0("The default choice SITEID was ignored because this column is not of type factor ",
                                "in the selected demographics dataset."))
})

test_that("validate_dm_dataset() throws error if the default_choice is not in the grouping_choices" |>
             vdoc[["add_spec"]](specs$app_creator_settings$grouping), {
   expect_error(
      validate_dm_dataset(dataset = df_validate_dm,
                          subjid_var = "USUBJID",
                          grouping_choices = c("ARM"),
                          default_choice = "SEX")
   )
})



# Function validate_ds_dataset
df_validate_ds <- data.frame(
   USUBJID = c("01", "02", "03"),
   DSDECOD = as.factor(c("RANDOMIZED", "COMPLETED", "WITHDRAWAL BY SUBJECT")),
   DSSTDTC = as.Date(c("2024-01-01", "2024-01-05", "2024-06-16")),
   DSSTDY = c(1, 5, 161),
   not_a_date = c("2024-01-01", "2024-01-05", "2024-06-16"),
   not_a_factor = c("TEST", "COMPLETE", "WITHDRAWAL")
)
disposition_events <- list(event_var = "DSDECOD",
                           date_var = "DSSTDTC",
                           day_var = "DSSTDY",
                           entry_vals = c("RANDOMIZED"),
                           exit_vals = c("COMPLETED", "WITHDRAWAL BY SUBJECT", "DEATH"))
test_that("validate_ds_dataset() throws no error if valid arguments are passed to the function", {
   expect_no_error(
      validate_ds_dataset(dataset = df_validate_ds, disposition_events = disposition_events)
   )
})

test_that("validate_ds_dataset() throws no error if the dataset has 0 rows.", {
   df_zero <- df_validate_ds |> dplyr::slice(0)
   expect_no_error(
      validate_ds_dataset(dataset = df_zero, disposition_events = disposition_events)
   )
})

test_that("validate_ds_dataset() throws error if the argument for dataset is not of class data.frame", {
   not_a_df <- "test"
   expect_error(
      validate_ds_dataset(dataset = not_a_df, disposition_events = disposition_events)
   )
})

test_that("validate_ds_dataset() throws error if the event_var is not a column in the dataset", {
   ds_events_not_subset <- disposition_events
   ds_events_not_subset$event_var <- "TEST"
   expect_error(
      validate_ds_dataset(dataset = df_validate_ds, disposition_events = ds_events_not_subset)
   )
})

test_that("validate_ds_dataset() throws error if the event_var is not of type factor", {
   ds_events_not_factor <- disposition_events
   ds_events_not_factor$event_var <- "not_a_factor"
   expect_error(
      validate_ds_dataset(dataset = df_validate_ds, disposition_events = ds_events_not_factor)
   )
})

test_that("validate_ds_dataset() throws error if the date_var is not a column in the dataset", {
   ds_events_not_subset <- disposition_events
   ds_events_not_subset$date_var <- "date"
   expect_error(
      validate_ds_dataset(dataset = df_validate_ds, disposition_events = ds_events_not_subset)
   )
})

test_that("validate_ds_dataset() throws error if the date_var is not of type date", {
   ds_events_not_date <- disposition_events
   ds_events_not_date$date_var <- "not_a_date"
   expect_error(
      validate_ds_dataset(dataset = df_validate_ds, disposition_events = ds_events_not_date)
   )
})

test_that("validate_ds_dataset() throws error if the day_var is not a column in the dataset", {
   ds_events_not_subset <- disposition_events
   ds_events_not_subset$day_var <- "day"
   expect_error(
      validate_ds_dataset(dataset = df_validate_ds, disposition_events = ds_events_not_subset)
   )
})

test_that("validate_ds_dataset() throws error if the day_var is not of type numeric", {
   ds_events_not_date <- disposition_events
   ds_events_not_date$day_var <- "not_a_date" # also not a numeric
   expect_error(
      validate_ds_dataset(dataset = df_validate_ds, disposition_events = ds_events_not_date)
   )
})



# Function validate_ae_dataset
df_validate_ae <- data.frame(
   USUBJID = c("01", "02", "03"),
   AESTDTC = as.Date(c("2024-01-01", "2024-01-05", "2024-06-16")),
   AESTDY = c(1, 5, 161),
   not_a_date = c("2024-01-01", "2024-01-05", "2024-06-16")
)
adverse_events <- list(date_var = "AESTDTC",
                       day_var = "AESTDY")

test_that("validate_ae_dataset() throws no error if valid arguments are passed to the function", {
   expect_no_error(
      validate_ae_dataset(dataset = df_validate_ae, adverse_events = adverse_events)
   )
})

test_that("validate_ae_dataset() throws no error if the dataset has 0 rows.", {
   df_zero <- df_validate_ae |> dplyr::slice(0)
   expect_no_error(
      validate_ae_dataset(dataset = df_zero, adverse_events = adverse_events)
   )
})

test_that("validate_ae_dataset() throws error if the argument for dataset is not of class data.frame", {
   not_a_df <- "test"
   expect_error(
      validate_ae_dataset(dataset = not_a_df, adverse_events = adverse_events)
   )
})

test_that("validate_ae_dataset() throws error if the date_var is not a column in the dataset", {
   expect_error(
      validate_ae_dataset(dataset = df_validate_ae, adverse_events = list(date_var = "TEST", day_var = "AESTDY"))
   )
})

test_that("validate_ae_dataset() throws error if the date_var is not of type date", {
   expect_error(
      validate_ae_dataset(dataset = df_validate_ae, adverse_events = list(date_var = "not_a_date", day_var = "AESTDY"))
   )
})

test_that("validate_ae_dataset() throws error if the day_var is not a column in the dataset", {
   expect_error(
      validate_ae_dataset(dataset = df_validate_ae, adverse_events = list(date_var = "AESTDTC", day_var = "TEST"))
   )
})

test_that("validate_ae_dataset() throws error if the day_var is not of type numeric", {
   expect_error( # column "not_a_date" is also not of type numeric
      validate_ae_dataset(dataset = df_validate_ae, adverse_events = list(date_var = "AESTDTC", day_var = "not_a_date"))
   )
})



# Function validate_df_rows()
dataset_list <- list(
                  dm = data.frame(USUBJID = c("01")),
                  ds = data.frame(USUBJID = c("01")),
                  ae = data.frame(USUBJID = c("01"))
               )


test_that("validate_df_rows() raises silent validation error when no levels are selected and the ungrouped_checkbox is
          FALSE while there is a variable selected for grouping.", {
    expect_error(
       validate_df_rows(dataset_list = dataset_list,
                        selected_levels = character(0),
                        ungrouped_checkbox = FALSE,
                        selected_group = "SEX"),
       regexp = "No data available.", class = "shiny.silent.error"
    )

   expect_no_error(
      validate_df_rows(dataset_list = dataset_list,
                       selected_levels = c("F"),
                       ungrouped_checkbox = FALSE,
                       selected_group = "SEX")
   )

    expect_no_error(
       validate_df_rows(dataset_list = dataset_list,
                        selected_levels = character(0),
                        ungrouped_checkbox = TRUE,
                        selected_group = "SEX")
    )

    expect_no_error(
       validate_df_rows(dataset_list = dataset_list,
                        selected_levels = c("F", "M"),
                        ungrouped_checkbox = TRUE,
                        selected_group = "SEX")
    )

    expect_no_error(
       validate_df_rows(dataset_list = dataset_list,
                        selected_levels = character(0),
                        ungrouped_checkbox = FALSE,
                        selected_group = REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
    )
})

test_that("validate_df_rows() raises silent validation error when one of the datasets dm, ds or ae has 0 rows.", {
   # testing for dm
   dataset_list_zero_dm <- dataset_list
   dataset_list_zero_dm$dm <- data.frame(USUBJID = character())
   expect_error(
       validate_df_rows(dataset_list = dataset_list_zero_dm,
                        selected_levels = c("F", "M"),
                        ungrouped_checkbox = TRUE,
                        selected_group = "SEX"),
       regexp = "No data available.", class = "shiny.silent.error"
    )
   expect_error(
      validate_df_rows(dataset_list = dataset_list_zero_dm,
                       selected_levels = character(0),
                       ungrouped_checkbox = FALSE,
                       selected_group = REPORT_RATES$CHOICES$GROUP_NO_SELECTION),
      regexp = "No data available.", class = "shiny.silent.error"
   )
   # testing for ds
   dataset_list_zero_ds <- dataset_list
   dataset_list_zero_ds$ds <- data.frame(USUBJID = character())
   expect_error(
      validate_df_rows(dataset_list = dataset_list_zero_ds,
                       selected_levels = c("F", "M"),
                       ungrouped_checkbox = TRUE,
                       selected_group = "SEX"),
      regexp = "No data available.", class = "shiny.silent.error"
   )
   expect_error(
      validate_df_rows(dataset_list = dataset_list_zero_ds,
                       selected_levels = character(0),
                       ungrouped_checkbox = FALSE,
                       selected_group = REPORT_RATES$CHOICES$GROUP_NO_SELECTION),
      regexp = "No data available.", class = "shiny.silent.error"
   )
   # testing for ae
   dataset_list_zero_ae <- dataset_list
   dataset_list_zero_ae$ae <- data.frame(USUBJID = character())
   expect_error(
      validate_df_rows(dataset_list = dataset_list_zero_ae,
                       selected_levels = c("F", "M"),
                       ungrouped_checkbox = TRUE,
                       selected_group = "SEX"),
      regexp = "No data available.", class = "shiny.silent.error"
   )
   expect_error(
      validate_df_rows(dataset_list = dataset_list_zero_ae,
                       selected_levels = character(0),
                       ungrouped_checkbox = FALSE,
                       selected_group = REPORT_RATES$CHOICES$GROUP_NO_SELECTION),
      regexp = "No data available.", class = "shiny.silent.error"
   )

   # when every dataset has at least 1 row, no error should be raised:
   expect_no_error(
      validate_df_rows(dataset_list = dataset_list,
                       selected_levels = character(0),
                       ungrouped_checkbox = FALSE,
                       selected_group = REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   )
})



df <- data.frame(m = as.numeric(c(1, 0)),
                 ARM = factor(c("a", "b")))
# Function is_valid_grouping_var()
test_that("is_valid_grouping_var() returns the correct logical." |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$grouped,
                   specs$plot_creation$grouping_and_levels$ungrouped
                )
             ), {
   expect_equal(is_valid_grouping_var("ARM", df), TRUE)
   expect_equal(is_valid_grouping_var("SEX", df), FALSE)
   expect_equal(is_valid_grouping_var(NULL, df), FALSE)
   expect_error(is_valid_grouping_var("m", df), class = "simpleError")
})




# Function validate_grouping_selection()
test_that("validate_grouping_selection() returns REPORT_RATES$CHOICES$GROUP_NO_SELECTION if the selected group was
           REPORT_RATES$CHOICES$GROUP_NO_SELECTION" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$ungrouped,
                   specs$framework_specs$filter_and_datasets
                )
             ), {
   res <- validate_grouping_selection(dm = df, selected_group = REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_equal(res, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
})

test_that("validate_grouping_selection() returns REPORT_RATES$CHOICES$GROUP_NO_SELECTION if the selected grouping
           variable is not in the dataset or not a factor anymore" |>
             vdoc[["add_spec"]](specs$framework_specs$filter_and_datasets), {
   res_not_in_df <- suppressMessages(validate_grouping_selection(dm = df, selected_group = "SITEID"))
   expect_equal(res_not_in_df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)

   res_not_a_factor <- suppressMessages(validate_grouping_selection(dm = df, selected_group = "m"))
   expect_equal(res_not_a_factor, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
})

test_that("validate_grouping_selection() returns the selected grouping variable if it's in the dataset and of
          type factor" |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$grouped,
                   specs$framework_specs$filter_and_datasets
                )
             ), {
   res <- validate_grouping_selection(dm = df, selected_group = "ARM")
   expect_equal(res, "ARM")
})

test_that("validate_grouping_selection() outputs the correct message." |>
             vdoc[["add_spec"]](specs$framework_specs$filter_and_datasets), {
   expect_message(
      validate_grouping_selection(dm = df, selected_group = "SITEID"),
      regexp = "not a subset"
   )
   expect_message(
      validate_grouping_selection(dm = df, selected_group = "m"),
      regexp = "not of type factor"
   )
   expect_silent(validate_grouping_selection(dm = df, selected_group = REPORT_RATES$CHOICES$GROUP_NO_SELECTION))
   expect_silent(validate_grouping_selection(dm = df, selected_group = "ARM"))
})

test_that("validate_grouping_selection() behaves as usual when the df has no rows" |>
             vdoc[["add_spec"]](specs$framework_specs$filter_and_datasets), {
   df_empty <- df |> dplyr::slice(0)

   expect_silent(
      res_none <- validate_grouping_selection(dm = df_empty, selected_group = REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   )
   expect_equal(res_none, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_silent(
      res_valid <- validate_grouping_selection(dm = df_empty, selected_group = "ARM")
   )
   expect_equal(res_valid, "ARM")

   expect_message(
      res_not_in_df <- validate_grouping_selection(dm = df, selected_group = "SITEID"),
      regexp = "not a subset"
   )
   expect_equal(res_not_in_df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)

   expect_message(
      res_not_a_factor <- validate_grouping_selection(dm = df, selected_group = "m"),
      regexp = "not of type factor"
   )
   expect_equal(res_not_in_df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
})

ds <- data.frame(
   USUBJID = c("01", "02", "03", "04"),
   DSDECOD = c("RANDOMIZED", "COMPLETED", "SCREEN FAILURE", "WITHDRAWAL BY SUBJECT"),
   DATE = as.Date(c("2021-01-01", "2022-02-01", "2023-03-01", "2025-05-02")),
   DAY = c(1, 30, 10, 0),
   DUMMY_COL = c(1, 2, 3, 4),
   DSTERM = c("a", "b", "c", "d")
)
ae <- data.frame(
   USUBJID = c("01", "02", "03", "04"),
   DATE = as.Date(c("2021-01-01", "2022-02-01", "2023-03-01", "2025-05-02")),
   DAY = c(1, 30, 10, 0),
   DUMMY_COL = c(13, 42, 5, 0),
   AETERM = c("a", "b", "c", "g")
)
dm <- data.frame(USUBJID = c("01", "02"), ARM = factor(c("A", "B")))
ds_prepared <- data.frame(USUBJID = c("01", "02"), m = c(1, 0), n = c(0, 0))
ae_prepared <- data.frame(USUBJID = c("01", "02"), m = c(0, 0), n = c(1, 1))



# Function "merge_event_data_with_grouping()"
test_that("merge_event_data_with_grouping() returns correct dataframe depending on the inputs for grouping_var"  |>
             vdoc[["add_spec"]](
                c(
                   specs$plot_creation$grouping_and_levels$grouped,
                   specs$plot_creation$grouping_and_levels$ungrouped
                )
             ), {
   res_1 <- merge_event_data_with_grouping(dm_dataset = dm,
                                           ds_prep_dataset = ds_prepared,
                                           ae_prep_dataset = ae_prepared,
                                           subjid_var = "USUBJID",
                                           grouping_var = NULL) # testing grouping_var NULL
   expect_equal(nrow(res_1), 4)
   expect_false("ARM" %in% colnames(res_1))

   res_2 <- merge_event_data_with_grouping(dm, ds_prepared, ae_prepared, "USUBJID", "") # testing empty string
   expect_equal(nrow(res_2), 4)
   expect_false("ARM" %in% colnames(res_2))

   res_3 <- merge_event_data_with_grouping(dm, ds_prepared, ae_prepared, "USUBJID",
                                 REPORT_RATES$CHOICES$GROUP_NO_SELECTION) # testing no group selection
   expect_equal(nrow(res_3), 4)
   expect_false("ARM" %in% colnames(res_3))

   res_4 <- merge_event_data_with_grouping(dm, ds_prepared, ae_prepared, "USUBJID", "ARM") # testing valid grouping var
   expect_equal(nrow(res_4), 4)
   expect_true("ARM" %in% colnames(res_4))
   expect_equal(res_4$ARM, factor(c("A", "B", "A", "B")))
})

test_that("merge_event_data_with_grouping returns dataset with correct amount of rows" |>
            vdoc[["add_spec"]](specs$plot_creation$grouping_and_levels$grouped), {
   res <- merge_event_data_with_grouping(dm, ds_prepared, ae_prepared, "USUBJID", "ARM")
   expect_equal(nrow(res), 4)

   ae_prepared <- dplyr::bind_rows(ae_prepared, data.frame(USUBJID = c("02"), m = c(0), n = c(1)))
   res <- merge_event_data_with_grouping(dm, ds_prepared, ae_prepared, "USUBJID", "ARM")
   expect_equal(nrow(res), 5)

   # testing the cases where there are no rows in the datasets
   dm_no_rows <- dm |> dplyr::slice(0)
   ds_no_rows <- ds_prepared |> dplyr::slice(0)
   ae_no_rows <- ae_prepared |> dplyr::slice(0)
   res <- merge_event_data_with_grouping(dm, ds_no_rows, ae_prepared, "USUBJID", "ARM")
   expect_equal(nrow(res), 3)
   res <- merge_event_data_with_grouping(dm, ds_prepared, ae_no_rows, "USUBJID", "ARM")
   expect_equal(nrow(res), 2)
   res <- merge_event_data_with_grouping(dm_no_rows, ds_prepared, ae_prepared, "USUBJID", "ARM")
   expect_equal(nrow(res), 5)
   res <- merge_event_data_with_grouping(dm_no_rows, ds_no_rows, ae_no_rows, "USUBJID", "ARM")
   expect_equal(nrow(res), 0)
})



# Function "prepare_ds_data()"
test_that("prepare_ds_data() assigns correct m and n values for entry and exit events" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$cum_rate_per_total_exp_time
              )
         ), {
   res <- prepare_ds_data(
      dataset = ds,
      subjid_var = "USUBJID",
      event_var = "DSDECOD",
      entry_terms = c("RANDOMIZED"),
      exit_terms = c("COMPLETED", "WITHDRAWAL BY SUBJECT"),
      date_var = "DATE",
      day_var = "DAY"
   )
   expect_equal(res$m, c(1, -1, 0, -1))
   expect_equal(unique(res$n), 0)
})

test_that("prepare_ds_data() returns the correct columns."  |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$cum_rate_per_total_exp_time
             )
          ), {
   res <- prepare_ds_data(
      dataset = ds,
      subjid_var = "USUBJID",
      event_var = "DSDECOD",
      entry_terms = c("RANDOMIZED"),
      exit_terms = c("COMPLETED", "WITHDRAWAL BY SUBJECT"),
      date_var = "DATE",
      day_var = "DAY"
   )
   expect_setequal(colnames(res), c("USUBJID", "date", "day", "n", "m"))
})

test_that("prepare_ds_data() assigns m = 0 when entry_terms and exit_terms are empty" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$cum_rate_per_total_exp_time
             )
          ), {
   res <- prepare_ds_data(
      dataset = ds,
      subjid_var = "USUBJID",
      event_var = "DSDECOD",
      entry_terms = character(0),
      exit_terms = character(0),
      date_var = "DATE",
      day_var = "DAY"
   )
   expect_equal(unique(res$m), 0)
})

test_that("prepare_ds_data() returns empty dataset if the input dataset was empty" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$cum_rate_per_total_exp_time
             )
          ), {
   ds_no_rows <- ds |> dplyr::slice(0)

   res <- prepare_ds_data(
      dataset = ds_no_rows,
      subjid_var = "USUBJID",
      event_var = "DSDECOD",
      entry_terms = c("RANDOMIZED"),
      exit_terms = c("COMPLETED", "WITHDRAWAL BY SUBJECT"),
      date_var = "DATE",
      day_var = "DAY"
   )

   expect_equal(nrow(res), 0)
   expect_setequal(colnames(res), c("USUBJID", "date", "day", "n", "m"))
})



# Function prepare_ae_data()
test_that("prepare_ae_data() assigns correct m and n values" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$cum_rate_per_total_exp_time
             )
          ), {
   res <- prepare_ae_data(
      dataset = ae,
      subjid_var = "USUBJID",
      date_var = "DATE",
      day_var = "DAY"
   )

   expect_equal(unique(res$m), 0)
   expect_equal(unique(res$n), 1)
})

test_that("prepare_ae_data() returns the correct columns" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$cum_rate_per_total_exp_time
             )
          ), {
   res <- prepare_ae_data(
      dataset = ae,
      subjid_var = "USUBJID",
      date_var = "DATE",
      day_var = "DAY"
   )

   expect_setequal(colnames(res), c("USUBJID", "date", "day", "n", "m"))
})

test_that("prepare_ae_data() returns empty dataset if the input dataset was empty" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$cum_rate_per_total_exp_time
             )
          ), {
   ae_no_rows <- ae |> dplyr::slice(0)

   res <- prepare_ae_data(
      dataset = ae_no_rows,
      subjid_var = "USUBJID",
      date_var = "DATE",
      day_var = "DAY"
   )

   expect_equal(nrow(res), 0)
   expect_setequal(colnames(res), c("USUBJID", "date", "day", "n", "m"))
})



# Function rates_per_active_patient()
df_rr <- data.frame(
   USUBJID = c("02", "02", "03", "03", "01", "02", "03", "04", "01"),
   date = as.Date(c("2024-11-28", "2024-12-21", "2024-12-28", "2024-12-29", "2025-01-01", "2025-01-03", "2025-01-10",
                    "2025-02-02", "2025-03-04")),
   day = c(-31, -8, -1, 1, 4, 6, 13, 36, 65),
   m = c(0, 0, 0, 1, 1, 1, 0, 1, 0),
   n = c(1, 1, 1, 0, 0, 0, 1, 0, 1),
   ARM = as.factor(c("b", "b", "c", "c", "a", "b", "c", "c", "a"))
)

test_that("rates_per_active_patients() removes rows with NA values in the selected time column (date or day)" |>
             vdoc[["add_spec"]](specs$metric_calculation$rate_per_active_patients), {
   df_rr_na <- df_rr
   df_rr_na$date[2] <- NA # was 2024-12-21 before which led to the creation of the week interval starting on 2024-12-16
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_na,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_false(as.Date("2024-12-16") %in% res$time)

   df_rr_na$date[8] <- NA # was 2025-02-02 before which led to the creation of the month interval starting on 2025-02-01
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_na,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) #month
   expect_false(as.Date("2025-02-01") %in% res$time)

   df_rr_na$date[3] <- NA # was 2024-12-28 before. the week interval starting on 2024-12-23 should still exist because
                          # of the date 2024-12-29
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_na,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) # week
   expect_true(as.Date("2024-12-23") %in% res$time)
   # the month interval should also still exist because of the date 2024-12-29 (2024-12-21 and 2024-12-28 set to NA)
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_na,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # month
   expect_true(as.Date("2024-12-01") %in% res$time)


   df_rr_na$day[2] <- NA #was -8 before which led to the creation of the week interval -2
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_na,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_false(-2 %in% res$time)

   df_rr_na$day[8] <- NA #was 36 before which led to the creation of the month interval 2
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_na,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) #month
   expect_false(2 %in% res$time)

   df_rr_na$day[6] <- NA #was 6 before. the week interval 1 should still exist because of the days 1 and 4
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_na,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_true(1 %in% res$time)

   df_rr_na$day[7] <- NA #was 13 before. the month interval 1 should still exist because of the days 1 and 4
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_na,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) #month
   expect_true(1 %in% res$time)
})

test_that("rates_per_active_patient() groups the events into the right intervals" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$timetype_selection,
                specs$metric_calculation$binsize_selection
             )
          ), {
   # By study date
   ## Binsize week:
   res_date_week <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_equal(res_date_week$time, as.Date(c("2024-11-25", "2024-12-16", "2024-12-23", "2024-12-30", "2025-01-06",
                                              "2025-01-27", "2025-03-03")))
   ## Binsize month:
   res_date_month <- rates_per_active_patient(time_type = "date",
                                             group_dataset = df_rr,
                                             grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                             unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) #month
   expect_equal(res_date_month$time, as.Date(c("2024-11-01", "2024-12-01", "2025-01-01", "2025-02-01", "2025-03-01")))

   # By study day
   ## Binsize week
   res_day_week <- rates_per_active_patient(time_type = "day",
                                             group_dataset = df_rr,
                                             grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                             unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_equal(res_day_week$time, c(-5, -2, -1, 1, 2, 6, 10))
   ## Binsze month
   res_day_month <- rates_per_active_patient(time_type = "day",
                                            group_dataset = df_rr,
                                            grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                            unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) #month
   expect_equal(res_day_month$time, c(-2, -1, 1, 2, 3))

   # testing edge cases at the time interval limits
   ## by study date
   df_rr_test <- df_rr
   df_rr_test$date[2] <- as.Date("2024-12-22") # this date is now a sunday and should still be in the same week interval
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_equal(res$time, res_date_week$time)
   df_rr_test$date[2] <- as.Date("2024-12-23") # this date is now a monday and should be in another week time interval
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_equal(res$time, as.Date(c("2024-11-25", "2024-12-23", "2024-12-30", "2025-01-06", "2025-01-27",
                                    "2025-03-03")))
   df_rr_test$date[9] <- as.Date("2025-02-28") #this date is now in february and not march anymore
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) #month
   expect_equal(res$time, as.Date(c("2024-11-01", "2024-12-01", "2025-01-01", "2025-02-01")))

   ## by study day
   df_rr_test$day[2] <- -7 # was -8 before.this day shouldnt be in its own weekly interval anymore (and in interval -1)
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_equal(res$time, c(-5, -1, 1, 2, 6, 10))
   df_rr_test$day[2] <- -15 # was -7 before. this day should be in its own weekly interval -3
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   expect_equal(res$time, c(-5, -3, -1, 1, 2, 6, 10))
   df_rr_test$day[1] <- -30 # was -31 before.This day shouldnt be in its own monthly interval anymore(and in interval-1)
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # month
   expect_equal(res$time, c(-1, 1, 2, 3))
   df_rr_test$day[1] <- -61 # was -30 before. This day should be in its own monthly inverval -3
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # month
   expect_equal(res$time, c(-3, -1, 1, 2, 3))
   df_rr_test$day[8] <- 30 # was 36 before. Shouldnt be in its own monthly interval anymore and be in the interval 1
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # month
   expect_equal(res$time, c(-3, -1, 1, 3))
})

test_that("Grouping doesn't influence the binning into time intervals in rates_per_active_patient()." |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$plot_creation$grouping_and_levels$grouped,
                specs$plot_creation$grouping_and_levels$ungrouped,
                specs$metric_calculation$timetype_selection
             )
          ), {
   combinations <- tidyr::expand_grid(time_type = c("date", "day"), unit_selection = c("Week", "Month"))
   purrr::walk(split(combinations, seq_len(nrow(combinations))), ~ {
      res_no_grouping <- rates_per_active_patient(time_type = .x$time_type,
                                                  group_dataset = df_rr,
                                                  grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                                  unit_selection = .x$unit_selection)
      res_grouping <- rates_per_active_patient(time_type = .x$time_type,
                                               group_dataset = df_rr,
                                               grouping_var = "ARM",
                                               unit_selection = .x$unit_selection)
      # using expect_setequal instead of expect_equal because the grouping influences the amount of rows but not which
      # time intervals were created
      expect_setequal(res_no_grouping$time, res_grouping$time)
   })
})

test_that("rates_per_active_patient() calculates the m_cumsum correctly." |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$timetype_selection
             )
          ), {
   df_rr_m_sum <- df_rr |> dplyr::mutate(m = c(1, -1, 1, 0, -1, 1, 1, 0, 1))
   # By study date
   ## week binsize
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_m_sum,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_equal(res$m_cumsum, c(1, 0, 1, 1, 2, 2, 3))
   ## month binsize
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_m_sum,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2)
   expect_equal(res$m_cumsum, c(1, 1, 2, 2, 3))

   # By study day
   ## week binsize
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_m_sum,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_equal(res$m_cumsum, c(1, 0, 1, 1, 2, 2, 3))
   ## month binsize
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr_m_sum,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2)
   expect_equal(res$m_cumsum, c(1, 1, 2, 2, 3))
})

test_that("rates_per_active_patient() calculates m_cumsum globally and not per time interval" |>
         vdoc[["add_spec"]](specs$metric_calculation$rate_per_active_patients), {
   dates <- seq.Date(from = as.Date("2010-01-01"), by = "month", length.out = 100)
   days_neg <- seq(from = -1200, by = 30, length.out = 40)
   day_pos <- seq(from = 1, by = 30, length.out = 60)
   days <- c(days_neg, day_pos)
   set.seed(123)
   m_values <- sample(c(-1, 0, 1), size  = 100, replace = TRUE)

   df_test <- data.frame(
      date = as.Date(dates),
      day = days,
      m = m_values
   )
   df_test <- df_test |> dplyr::mutate(n = ifelse(m == 0, 1, 0))
   res_date_month <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # month
   res_date_week <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_test,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) # week
   res_day_month <- rates_per_active_patient(time_type = "day",
                                             group_dataset = df_test,
                                             grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                             unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # month
   res_day_week <- rates_per_active_patient(time_type = "day",
                                             group_dataset = df_test,
                                             grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                             unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) # month
   expect_equal(res_date_month$m_cumsum, cumsum(m_values))
   expect_equal(res_date_week$m_cumsum, res_date_month$m_cumsum)
   expect_equal(res_day_month$m_cumsum, cumsum(m_values))
   expect_equal(res_day_week$m_cumsum, res_day_month$m_cumsum)
   expect_equal(res_date_month$m_cumsum, res_day_month$m_cumsum)
})

test_that("rates_per_active_patient() keeps one time interval per group if > 1 groups appear in the same interval" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$plot_creation$grouping_and_levels$grouped
             )
          ), {
   # By study date
   ## month binsize
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr,
                                   grouping_var = "ARM",
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # month
   # months with > 1 groups: december --> b,c; january --> a,b,c
   expect_true(any(res$time == as.Date("2024-12-01") & res$ARM == "b"))
   expect_true(any(res$time == as.Date("2024-12-01") & res$ARM == "c"))
   expect_true(any(res$time == as.Date("2025-01-01") & res$ARM == "a"))
   expect_true(any(res$time == as.Date("2025-01-01") & res$ARM == "b"))
   expect_true(any(res$time == as.Date("2025-01-01") & res$ARM == "c"))
   # check that maximum 1 row is kept, even if there are > 1 rows in the original dataset for a group in an interval
   counts <- res |> dplyr::count(time, ARM)
   expect_true(all(counts$n == 1))
   ## week binsze
   res <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr,
                                   grouping_var = "ARM",
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) #week
   # weeks with > 1 groups: 30.dec - 5. jan --> a,b
   expect_true(any(res$time == as.Date("2024-12-30") & res$ARM == "a"))
   expect_true(any(res$time == as.Date("2024-12-30") & res$ARM == "b"))
   counts <- res |> dplyr::count(time, ARM)
   expect_true(all(counts$n == 1))
   # by study day
   ## month binsize
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr,
                                   grouping_var = "ARM",
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # month
   # months with > 1 groups: -1 --> b,c; 1 --> a,b,c
   expect_true(any(res$time == -1 & res$ARM == "b"))
   expect_true(any(res$time == -1 & res$ARM == "c"))
   expect_true(any(res$time == 1 & res$ARM == "a"))
   expect_true(any(res$time == 1 & res$ARM == "b"))
   expect_true(any(res$time == 1 & res$ARM == "c"))
   counts <- res |> dplyr::count(time, ARM)
   expect_true(all(counts$n == 1))
   ## week binsize
   res <- rates_per_active_patient(time_type = "day",
                                   group_dataset = df_rr,
                                   grouping_var = "ARM",
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) # week
   # weeks with > 1 groups: 1 --> c,a,b
   expect_true(any(res$time == 1 & res$ARM == "c"))
   expect_true(any(res$time == 1 & res$ARM == "a"))
   expect_true(any(res$time == 1 & res$ARM == "b"))
   counts <- res |> dplyr::count(time, ARM)
   expect_true(all(counts$n == 1))
})

test_that("rate_per_active_patient() calculates the n_cumsum correctly and grouped after time without a
          grouping variable" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$timetype_selection,
                specs$metric_calculation$binsize_selection,
                specs$plot_creation$grouping_and_levels$ungrouped
             )
          ), {
   df_rr_n_sum <- df_rr |> dplyr::mutate(n = c(1, 0, 1, 1, 0, 1, 1, 0, 1))
   # By study date
   ## week binsize
   res_date_weeks <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_n_sum,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_equal(res_date_weeks$n_cumsum, c(1, 0, 2, 1, 1, 0, 1))
   ## months binsize
   res_date_month <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_rr_n_sum,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2)
   expect_equal(res_date_month$n_cumsum, c(1, 2, 2, 0, 1))

   # By study day
   ## weeks binsize
   res_days_weeks <- rates_per_active_patient(time_type = "day",
                                              group_dataset = df_rr_n_sum,
                                              grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                              unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_equal(res_days_weeks$n_cumsum, c(1, 0, 1, 2, 1, 0, 1))
   ## months binsize
   res_days_months <- rates_per_active_patient(time_type = "day",
                                              group_dataset = df_rr_n_sum,
                                              grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                              unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2)
   expect_equal(res_days_months$n_cumsum, c(1, 1, 3, 0, 1))
})

test_that("rate_per_active_patient() calculates the n_cumsum correclty and grouped after time also when there is a
          valid grouping variable" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$plot_creation$grouping_and_levels$grouped,
                specs$metric_calculation$timetype_selection,
                specs$metric_calculation$binsize_selection
             )
          ), {
   df_rr_n_sum <- df_rr |> dplyr::mutate(n = c(1, 0, 1, 1, 0, 1, 1, 0, 1))
   # By study date
   ## month binsize
   res_date_months <- rates_per_active_patient(time_type = "date",
                                              group_dataset = df_rr_n_sum,
                                              grouping_var = "ARM",
                                              unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) |>
                                             dplyr::arrange(time)
   expect_equal(res_date_months$n_cumsum, c(1, 0, 2, 0, 1, 1, 0, 1))
   ## week binsize
   res_date_weeks <- rates_per_active_patient(time_type = "date",
                                               group_dataset = df_rr_n_sum,
                                               grouping_var = "ARM",
                                               unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) |>
                                             dplyr::arrange(time)
   expect_equal(res_date_weeks$n_cumsum, c(1, 0, 2, 0, 1, 1, 0, 1))

   # By study day
   ## month binsize
   res_days_months <- rates_per_active_patient(time_type = "day",
                                               group_dataset = df_rr_n_sum,
                                               grouping_var = "ARM",
                                               unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) |>
                                             dplyr::arrange(time)
   expect_equal(res_days_months$n_cumsum, c(1, 0, 1, 0, 1, 2, 0, 1))
   ## week binsize
   res_days_weeks <- rates_per_active_patient(time_type = "day",
                                               group_dataset = df_rr_n_sum,
                                               grouping_var = "ARM",
                                               unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) |>
                                             dplyr::arrange(time)
   expect_equal(res_days_weeks$n_cumsum, c(1, 0, 1, 0, 1, 1, 1, 0, 1))
})

test_that("rates_per_active_patient() behaves the same when theres no grouping and when theres grouping with only one
          level" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$plot_creation$grouping_and_levels$grouped,
                specs$plot_creation$grouping_and_levels$ungrouped
             )
          ), {
   df_rr_one_lvl <- df_rr |> dplyr::mutate(ARM = as.factor("a"))
   combinations <- tidyr::expand_grid(time_type = c("date", "day"), unit_selection = c("Week", "Month"))
   purrr::walk(split(combinations, seq_len(nrow(combinations))), ~ {
      res_no_group <- rates_per_active_patient(time_type = .x$time_type,
                                               group_dataset = df_rr_one_lvl,
                                               grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                               unit_selection = .x$unit_selection) |>
                                             dplyr::arrange(time)
      res_grouping <- rates_per_active_patient(time_type = .x$time_type,
                                               group_dataset = df_rr_one_lvl,
                                               grouping_var = "ARM",
                                               unit_selection = .x$unit_selection) |>
                                             dplyr::arrange(time)

      expect_equal(res_no_group, res_grouping)
   })
})

test_that("rates_per_active_patient() sets ratio = 0 for m_cumsum = 0" |>
         vdoc[["add_spec"]](specs$metric_calculation$rate_per_active_patients), {
   #ungrouped
   df_ratio_0_ungrouped <- df_rr |> dplyr::mutate(m = 0, # so that m_cumsum will be 0 in every row
                                                  n = 1)
   res_ungrouped <- rates_per_active_patient(time_type = "date",
                                   group_dataset = df_ratio_0_ungrouped,
                                   grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                   unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_equal(unique(res_ungrouped$ratio), 0)

   #grouped
   df_ratio_0_grouped <- data.frame(
      USUBJID = c("02", "02", "03", "03", "01", "02", "03", "04", "01"),
      date = as.Date(c("2024-11-28", "2024-12-21", "2024-12-28", "2024-12-29", "2025-01-01", "2025-01-03", "2025-01-10",
                       "2025-02-02", "2025-03-04")),
      day = c(-31, -8, -1, 1, 4, 6, 13, 36, 65),
      m = c(0, 0, 0, 0, 1, 0, 1, 0, 0),
      n = c(1, 1, 1, 1, 0, 1, 0, 1, 1),
      ARM = as.factor(c("b", "b", "c", "c", "a", "b", "c", "c", "a"))
   )
   res_grouped <- rates_per_active_patient(time_type = "date",
                                           group_dataset = df_ratio_0_grouped,
                                           grouping_var = "ARM",
                                           unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2)
   b_rows <- res_grouped |> dplyr::filter(ARM == "b")
   general_rows <- res_grouped |> dplyr::filter(m_cumsum == 0) # also checking rows of other groups where m_cumsum == 0
   expect_equal(unique(b_rows$ratio), 0)
   expect_equal(unique(general_rows$ratio), 0)
   expect_true(any(res_grouped$ratio != 0))
})

test_that("rates_per_active_patient() calculates the correct ratio." |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$binsize_selection,
                specs$metric_calculation$timetype_selection
             )
          ), {
   combinations <- tidyr::expand_grid(
      time_type = c("date", "day"),
      unit_selection = c("Week", "Month"),
      grouping = c(REPORT_RATES$CHOICES$GROUP_NO_SELECTION, "ARM")
   )
   purrr::walk(split(combinations, seq_len(nrow(combinations))), ~ {
      res <- rates_per_active_patient(time_type = .x$time_type,
                                      group_dataset = df_rr,
                                      grouping_var = .x$grouping,
                                      unit_selection = .x$unit_selection)
      expect_true("ratio" %in% colnames(res))
      expect_equal(res$ratio, ifelse(res$m_cumsum == 0, 0, res$n_cumsum / res$m_cumsum))
      expect_true(all(res$ratio >= 0))
   })
})



# Function fill_missing_intervals()
df_missing_intervals <- data.frame(                                                   # time col for ...
      date_month = as.Date(c("2024-10-01", "2025-01-01", "2025-02-01", "2025-06-01")), # ..."by study date" + month
      date_week = as.Date(c("2024-10-07", "2024-11-18", "2024-11-25", "2025-01-13")), # ..."by study date" + week
      days = c(-16, 1, 2, 5), # time column for by study day (binsize doesnt matter)
      ratio = c(0.7, 0.4, 0.2, 0.1),
      ARM = as.factor(c("a", "b", "c", "a"))
)
combinations_interval <- list(
   list(time_type = "date", time_unit = "month", time_col = "date_month"),
   list(time_type = "date", time_unit = "week", time_col = "date_week"),
   list(time_type = "day", time_unit = "month", time_col = "days"),
   list(time_type = "day", time_unit = "week", time_col = "days")
)
test_that("fill_missing_intervals() fills in all missing intervals with ratio = 0 without grouping" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$binsize_selection,
                specs$plot_creation$grouping_and_levels$ungrouped
             )
          ), {
   purrr::walk(combinations_interval, ~ {
      # rename the corresponding time column to "time". Necessary for fill_missing_intervals, because it works on the
      # column named "time"
      df <- df_missing_intervals |> dplyr::rename(time = !!rlang::sym(.x$time_col))

      res <- fill_missing_intervals(dataset = df,
                                    grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                    time_type = .x$time_type,
                                    time_unit = .x$time_unit)

      # create the expected consecutive sequence of the values in the time column.
      expected <- if (.x$time_type == "date") {
         seq.Date(from = min(df$time), to = max(df$time), by = .x$time_unit)
      } else {
         setdiff(seq(min(df$time), max(df$time), by = 1),
                 0) # remove the 0 from the expected values
      }
      # test that the values are consecutive
      expect_equal(res$time, expected)

      # test that the newly added rows have ratio = 0
      added_vals <- setdiff(res$time, df$time)
      if (length(added_vals) > 0) {
         if (.x$time_type == "date") added_vals <- as.Date(added_vals)
         expect_true(all(res$ratio[res$time %in% added_vals] == 0))
      }
   })
})

test_that("fill_missing_intervals() fills in all missing intervals with ratio = 0 with grouping" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$plot_creation$grouping_and_levels$grouped,
                specs$metric_calculation$binsize_selection
             )
          ), {
   purrr::walk(combinations_interval, ~ {
      # rename the corresponding time column to "time". Necessary for fill_missing_intervals, because it works on the
      # column named "time"
      df <- df_missing_intervals |> dplyr::rename(time = !!rlang::sym(.x$time_col))

      res <- fill_missing_intervals(dataset = df,
                                    grouping_var = "ARM",
                                    time_type = .x$time_type,
                                    time_unit = .x$time_unit)

      # create the expected consecutive sequence of the values in the time column.
      expected <- if (.x$time_type == "date") {
         seq.Date(from = min(df$time), to = max(df$time), by = .x$time_unit)
      } else {
         setdiff(seq(min(df$time), max(df$time), by = 1),
                 0) # remove the 0 from the expected values
      }

      # test that the values are consecutive within each group
      a_rows <- res |> dplyr::filter(ARM == "a")
      b_rows <- res |> dplyr::filter(ARM == "b")
      c_rows <- res |> dplyr::filter(ARM == "c")
      expect_equal(a_rows$time, expected)
      expect_equal(b_rows$time, expected)
      expect_equal(c_rows$time, expected)

      # test that the newly added rows have ratio = 0 and that the "original" rows that were already part of the
      # inputted dataset have the same ratio as before calling the function
      added_rows <- res |> dplyr::anti_join(df, by = c("time", "ARM"))
      original_rows <- res |> dplyr::semi_join(df, by = c("time", "ARM")) |> dplyr::arrange(time)

      if (nrow(added_rows) > 0) {
         if (.x$time_type == "date") added_rows <- added_rows |> dplyr::mutate(time = as.Date(time))
         expect_true(all(added_rows$ratio == 0))
      }
      expect_equal(original_rows$ratio, df$ratio)
   })
})

test_that("fill_missing_intervals() doesn't fill in intervals with time = 0" |>
             vdoc[["add_spec"]](
                c(
                   specs$metric_calculation$rate_per_active_patients,
                   specs$plot_creation$axis_handling$no_zero
                )
             ), {
   df <- data.frame(
      time = c(-2, -1, 1, 2), # time column for by study day (binsize doesnt matter)
      ratio = c(0.7, 0.4, 0.2, 0.1),
      ARM = as.factor(c("a", "b", "c", "a"))
   )
   res <- fill_missing_intervals(dataset = df,
                                 grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                 time_type = "day",
                                 time_unit = "week")
   expect_equal(res$time, df$time)

   df <- data.frame(
      time = c(-3, -2, 2, 3), # time column for by study day (binsize doesnt matter)
      ratio = c(0.7, 0.4, 0.2, 0.1),
      ARM = as.factor(c("a", "b", "a", "b"))
   )
   res_no_group <- fill_missing_intervals(dataset = df,
                                          grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                          time_type = "day",
                                          time_unit = "week")
   res_group <- fill_missing_intervals(dataset = df,
                                          grouping_var = "ARM",
                                          time_type = "day",
                                          time_unit = "week")
   expect_false(0 %in% res_no_group$time)
   expect_false(0 %in% res_group$time)
})

test_that("fill_missing_intervals() behaves same for time_unit = week and time_unit = month if the time_type = day." |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$binsize_selection
             )
          ), {
   df <- df_missing_intervals |> dplyr::rename(time = "days")

   res_week <- fill_missing_intervals(dataset = df,
                                      grouping_var = "ARM",
                                      time_type = "day",
                                      time_unit = "week")
   res_month <- fill_missing_intervals(dataset = df,
                                       grouping_var = "ARM",
                                       time_type = "day",
                                       time_unit = "month")
   expect_equal(res_week, res_month)
})

test_that("fill_missing_intervals() behaves same for grouping with only one level and no grouping and invalid grouping
          (except for the values in the ARM column)" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$plot_creation$grouping_and_levels$grouped,
                specs$plot_creation$grouping_and_levels$ungrouped
             )
          ), {
   df_group_test <- df_missing_intervals |> dplyr::mutate(ARM = as.factor(c("a", "a", "a", "a")))
   purrr::walk(combinations_interval, ~ {
      # rename the corresponding time column to "time". Necessary for fill_missing_intervals, because it works on the
      # column named "time"
      df <- df_group_test |> dplyr::rename(time = !!rlang::sym(.x$time_col))

      # when fill_missing_intervals() is called with a valid grouping variable, the added rows will contain the level in
      # the grouping variable. This is not the case for no grouping / invalid grouping. Thats why the ARM column gets
      # deselected
      res_no_group <- fill_missing_intervals(dataset = df,
                                             grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                             time_type = .x$time_type,
                                             time_unit = .x$time_unit) |> dplyr::select(-ARM)

      res_group <- fill_missing_intervals(dataset = df,
                                          grouping_var = "ARM",
                                          time_type = .x$time_type,
                                          time_unit = .x$time_unit) |> dplyr::select(-ARM)

      res_invalid_group <- fill_missing_intervals(dataset = df,
                                                  grouping_var = "SITEID", # not a column in df
                                                  time_type = .x$time_type,
                                                  time_unit = .x$time_unit) |> dplyr::select(-ARM)

      expect_equal(res_no_group, res_group)
      expect_equal(res_no_group, res_invalid_group)
   })

})

test_that("fill_missing_intervals() works correctly when there's only 1 time value (-> one row) in the df" |>
         vdoc[["add_spec"]](specs$metric_calculation$rate_per_active_patients), {
   df_one <- data.frame(
      date_month = as.Date(c("2024-07-01")), # this date is a monday --> can be used for both binsizes
      date_week = as.Date(c("2024-07-01")),
      days = c(-16),
      ratio = c(0.7),
      ARM = as.factor(c("a"))
   )
   purrr::walk(combinations_interval, ~ {
      # rename the corresponding time column to "time". Necessary for fill_missing_intervals, because it works on the
      # column named "time"
      df <- df_one |> dplyr::rename(time = !!rlang::sym(.x$time_col))

      res_no_group <- fill_missing_intervals(dataset = df,
                                    grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                    time_type = .x$time_type,
                                    time_unit = .x$time_unit)

      res_group <- fill_missing_intervals(dataset = df,
                                          grouping_var = "ARM",
                                          time_type = .x$time_type,
                                          time_unit = .x$time_unit)

      expect_equal(res_no_group, df)
      expect_equal(res_group, df)
   })
})



# Function add_latest_dummy_point()
df_dummy_row <- data.frame(
   time = as.Date(c("2024-01-01", "2024-02-01")),
   ratio = c(0.7, 0.4),
   ARM = as.factor(c("a", "b"))
)
df_dummy_group <- data.frame(
   time = as.Date(c("2023-12-01", "2024-01-01", "2024-01-01", "2024-02-01")),
   ratio = c(0.9, 0.7, 0.8, 0.4),
   ARM = as.factor(c("a", "a", "c", "b"))
)
test_that("add_latest_dummy_point() creates correct dummy row without grouping" |>
             vdoc[["add_spec"]](
                c(
                   specs$metric_calculation$rate_per_active_patients,
                   specs$metric_calculation$binsize_selection,
                   specs$plot_creation$grouping_and_levels$ungrouped
                )
             ), {
   # By study date
   ## binsize month
   res <- add_latest_dummy_point(dataset = df_dummy_row,
                                 grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                 time_type = "date",
                                 time_unit = "month")
   expect_equal(res$time, as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")))
   # binsize week
   df_week <- df_dummy_row |> dplyr::mutate(time = as.Date(c("2024-01-01", "2024-01-08")))
   res_week <- add_latest_dummy_point(dataset = df_week,
                                      grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                      time_type = "date",
                                      time_unit = "week")
   expect_equal(res_week$time, as.Date(c("2024-01-01", "2024-01-08", "2024-01-15")))

   # By study day
   df_day <- df_dummy_row |> dplyr::mutate(time = c(-1, 1))
   res_day <- add_latest_dummy_point(dataset = df_day,
                                     grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                     time_type = "day",
                                     time_unit = "week")
   expect_equal(res_day$time, c(-1, 1, 2))
})

test_that("add_latest_dummy_point() creates correct dummy rows with grouping" |>
             vdoc[["add_spec"]](
                c(
                   specs$metric_calculation$rate_per_active_patients,
                   specs$metric_calculation$binsize_selection,
                   specs$plot_creation$grouping_and_levels$grouped
                )
             ), {
   # By study date
   ## binsize month
   res_date_month <- add_latest_dummy_point(dataset = df_dummy_group,
                                            grouping_var = "ARM",
                                            time_type = "date",
                                            time_unit = "month") |> dplyr::arrange(time, ARM)
   expect_equal(res_date_month$time, as.Date(c("2023-12-01", "2024-01-01", "2024-01-01", "2024-02-01", "2024-02-01",
                                               "2024-02-01", "2024-03-01")))
   #checking if the values (except in the time column) stayed the same
   res_date_month <- res_date_month |> dplyr::select(-time)
   expect_equal(unlist(res_date_month[2, ]), unlist(res_date_month[4, ]))
   expect_equal(unlist(res_date_month[3, ]), unlist(res_date_month[6, ]))
   expect_equal(unlist(res_date_month[5, ]), unlist(res_date_month[7, ]))

   ## binsize week
   df_date_week <- df_dummy_group |> dplyr::mutate(time = as.Date(c("2023-12-25", "2024-01-01", "2023-12-25",
                                                                    "2024-01-15")))
   res_date_week <- add_latest_dummy_point(dataset = df_date_week,
                                           grouping_var = "ARM",
                                           time_type = "date",
                                           time_unit = "week") |> dplyr::arrange(time, ARM)
   expect_equal(res_date_week$time, as.Date(c("2023-12-25", "2023-12-25", "2024-01-01", "2024-01-01", "2024-01-08",
                                              "2024-01-15", "2024-01-22")))
   res_date_week <- res_date_week |> dplyr::select(-time)
   expect_equal(unlist(res_date_week[3, ]), unlist(res_date_week[5, ]))
   expect_equal(unlist(res_date_week[2, ]), unlist(res_date_week[4, ]))
   expect_equal(unlist(res_date_week[6, ]), unlist(res_date_week[7, ]))

   # By study day
   df_day <- df_dummy_group |> dplyr::mutate(time = c(-3, -2, -3, 3))
   res_day <- add_latest_dummy_point(dataset = df_day,
                                     grouping_var = "ARM",
                                     time_type = "day",
                                     time_unit = "week") |> dplyr::arrange(time, ARM)
   expect_equal(res_day$time, c(-3, -3, -2, -2, -1, 3, 4))
   res_day <- res_day |> dplyr::select(-time)
   expect_equal(unlist(res_day[3, ]), unlist(res_day[5, ]))
   expect_equal(unlist(res_day[2, ]), unlist(res_day[4, ]))
   expect_equal(unlist(res_day[6, ]), unlist(res_day[7, ]))
   df
})

test_that("add_latest_dummy_point() behaves the same for grouping with only one level and no / invalid grouping" |>
             vdoc[["add_spec"]](
                c(
                   specs$metric_calculation$rate_per_active_patients,
                   specs$plot_creation$grouping_and_levels$ungrouped,
                   specs$plot_creation$grouping_and_levels$grouped
                )
             ), {
   df <- df_dummy_row |> dplyr::mutate(ARM = as.factor("a"))
   res_no_group <- add_latest_dummy_point(dataset = df,
                                          grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                          time_type = "date",
                                          time_unit = "week")
   res_group <- add_latest_dummy_point(dataset = df,
                                       grouping_var = "ARM",
                                       time_type = "date",
                                       time_unit = "week")
   res_invalid_group <- add_latest_dummy_point(dataset = df,
                                               grouping_var = "SITEID",
                                               time_type = "date",
                                               time_unit = "week")
   expect_equal(res_no_group, res_group)
   expect_equal(res_no_group, res_invalid_group)
})

test_that("add_latest_dummy_point() behaves the same for both binsizes if time_type = day" |>
             vdoc[["add_spec"]](
                c(
                   specs$metric_calculation$rate_per_active_patients,
                   specs$metric_calculation$binsize_selection
                )
             ), {
   # without grouping
   df_no_group <- df_dummy_row |> dplyr::mutate(time = c(-1, 1))
   res_week <- add_latest_dummy_point(dataset = df_no_group,
                                      grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                      time_type = "day",
                                      time_unit = "week")
   res_month <- add_latest_dummy_point(dataset = df_no_group,
                                       grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                       time_type = "day",
                                       time_unit = "month")
   expect_equal(res_week, res_month)
   # with grouping
   df_group <- df_dummy_group |> dplyr::mutate(time = c(-3, -2, -3, 3))
   res_week <- add_latest_dummy_point(dataset = df_group,
                                   grouping_var = "ARM",
                                   time_type = "day",
                                   time_unit = "week")
   res_month <- add_latest_dummy_point(dataset = df_group,
                                      grouping_var = "ARM",
                                      time_type = "day",
                                      time_unit = "month")
   expect_equal(res_week, res_month)
})

test_that("add_latest_dummy_point() doesn't create dummy rows with time = 0" |>
             vdoc[["add_spec"]](
                c(
                   specs$metric_calculation$rate_per_active_patients,
                   specs$plot_creation$axis_handling$no_zero
                )
             ), {
   # without grouping
   df <- df_dummy_row |> dplyr::mutate(time = c(-2, -1))
   res <- add_latest_dummy_point(dataset = df,
                                 grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                 time_type = "day",
                                 time_unit = "week")
   expect_false(0 %in% res$time)

   # with grouping
   df_group <- df_dummy_group |> dplyr::mutate(time = c(-2, -1, -1, 1))
   res_group <- add_latest_dummy_point(dataset = df_group,
                                       grouping_var = "ARM",
                                       time_type = "day",
                                       time_unit = "month")
   expect_false(0 %in% res_group$time)
})



# Function cum_rate_per_total_exp_time()
df_cum_rate <- data.frame(
   USUBJID = c("01", "02", "03", "04", "01"),
   date = as.Date(c("2025-01-01", "2025-01-02", "2025-01-04", "2025-01-05", "2025-01-05")),
   day = c(1, 2, 1, 5, 5),
   m = c(1, 1, 1, 1, 0),
   n = c(0, 0, 0, 0, 1),
   ARM = as.factor(c("a", "b", "c", "c", "a"))
)

test_that("cum_rate_per_total_exp_time() calculates the cumulative sum of m and total_exp correctly" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$cum_rate_per_total_exp_time,
                specs$metric_calculation$timetype_selection
             )
          ), {
   #without grouping
   res <- cum_rate_per_total_exp_time("date", df_cum_rate, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_equal(res$m_cumsum, c(1, 2, 2, 3, 4))
   expect_equal(res$total_exp, c(1, 3, 5, 8, 12))

   df <- df_cum_rate |> dplyr::mutate(m = c(0, 0, 0, 0, 0))
   res <- cum_rate_per_total_exp_time("date", df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_equal(res$m_cumsum, c(0, 0, 0, 0, 0))
   expect_equal(res$total_exp, c(0, 0, 0, 0, 0))

   df <- df |> dplyr::mutate(m = c(1, -1, 1, 0, 0))
   res <- cum_rate_per_total_exp_time("day", df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_equal(res$m_cumsum, c(2, 1, 1, 1, 1))
   expect_equal(res$total_exp, c(2, 3, 4, 5, 6))

   # with grouping
   res <- cum_rate_per_total_exp_time("date", df_cum_rate, "ARM")
   expect_equal(res$m_cumsum, c(1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 2))
   expect_equal(res$total_exp, c(1, 0, 0, 2, 1, 0, 3, 2, 0, 4, 3, 1, 5, 4, 3))


   df_group_day <- data.frame(
      USUBJID = c("01", "02", "03", "04", "01"),
      date = as.Date(c("2025-01-01", "2025-01-02", "2025-01-04", "2025-01-05", "2025-01-05")),
      day = c(1, 1, 1, 3, 5),
      m = c(1, 1, 1, -1, 0),
      n = c(0, 0, 0, 0, 1),
      ARM = as.factor(c("a", "b", "c", "c", "a"))
   )
   res <- cum_rate_per_total_exp_time("day", df_group_day, "ARM")
   expect_equal(res$m_cumsum, c(1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0))
   expect_equal(res$total_exp, c(1, 1, 1, 2, 2, 2, 3, 3, 2, 4, 4, 2, 5, 5, 2))

   # with unvalid grouping_var
   res <- cum_rate_per_total_exp_time("day", df, "SITEID")
   expect_equal(res$m_cumsum, c(2, 1, 1, 1, 1))
   expect_equal(res$total_exp, c(2, 3, 4, 5, 6))
})

test_that("cum_rate_per_total_exp_time() only keeps the last row when there are multiple rows for a date/day in a
          group" |>
             vdoc[["add_spec"]](
                c(
                   specs$metric_calculation$cum_rate_per_total_exp_time,
                   specs$metric_calculation$timetype_selection,
                   specs$plot_creation$grouping_and_levels$ungrouped,
                   specs$plot_creation$grouping_and_levels$grouped
                )
             ), {
   # without grouping
   df <- df_cum_rate |> dplyr::mutate(date = as.Date("2025-01-01"))
   res <- cum_rate_per_total_exp_time("date", df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_equal(nrow(res), 1)

   df <- df |> dplyr::mutate(day = 1)
   res <- cum_rate_per_total_exp_time("day", df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_equal(nrow(res), 1)

   # with grouping
   res <- cum_rate_per_total_exp_time("date", df, "ARM")
   expect_equal(nrow(res), 3)

   res <- cum_rate_per_total_exp_time("day", df, "ARM")
   expect_equal(nrow(res), 3)

   # with unvalid grouping var
   res <- cum_rate_per_total_exp_time("day", df, "SITEID")
   expect_equal(nrow(res), 1)
})

test_that("cum_rate_per_total_exp_time() filters out rows where the value of the selected time (either day or date)
          is NA" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$cum_rate_per_total_exp_time,
                specs$metric_calculation$timetype_selection
             )
          ), {
   # for time = date
   df <- df_cum_rate
   df$date[2] <- NA # date of the row with USUBJID == "02"
   res <- cum_rate_per_total_exp_time("date", df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_false(any(is.na(res$date)))
   expect_false("02" %in% res$USUBJID)
   # Verifying that the row is kept if only the other time column contains a NA
   res <- cum_rate_per_total_exp_time("day", df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_true(any(is.na(res$date)))
   expect_true("02" %in% res$USUBJID)

   # for time = day
   df <- df_cum_rate
   df$day[2] <- NA
   res <- cum_rate_per_total_exp_time("day", df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_false(any(is.na(res$day)))
   expect_false("02" %in% res$USUBJID)
   # Verifying that the row is kept if only the other time column contains a NA
   res <- cum_rate_per_total_exp_time("date", df, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_true(any(is.na(res$day)))
   expect_true("02" %in% res$USUBJID)
})

test_that("cum_rate_per_total_exp_time() fills missing dates / days correctly" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$cum_rate_per_total_exp_time,
                specs$metric_calculation$timetype_selection,
                specs$plot_creation$grouping_and_levels$ungrouped,
                specs$plot_creation$grouping_and_levels$grouped
             )
          ), {
   # helper function for checking that the filled in rows should have NA in the cols m and n and the same m_cumsum and
   # n_cumsum as the row the were derived from
   compare_with_prev_row <- function(original_df, res_df, time_selection, grouping_var) {
      added <- setdiff(res_df[[time_selection]], original_df[[time_selection]])
      if (time_selection == "date") added <- as.Date(added)
      index <- which(res_df[[time_selection]] %in% added)
      prev_index <- index - 1

      expect_true(all(res_df$m_cumsum[index] ==  res_df$m_cumsum[prev_index]))
      expect_true(all(res_df$n_cumsum[index] == res_df$n_cumsum[prev_index]))
      expect_true(all(is.na(res_df$m[index])))
      expect_true(all(is.na(res_df$n[index])))
   }

   # the Date 2025-01-03 is not in the data but should be in the result
   res_date <- cum_rate_per_total_exp_time("date", df_cum_rate, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   res_day <- cum_rate_per_total_exp_time("day", df_cum_rate, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_true("2025-01-03" %in% res_date$time)
   expect_true(3 %in% res_day$time)
   compare_with_prev_row(df_cum_rate, res_date, "date", REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   compare_with_prev_row(df_cum_rate, res_day, "day", REPORT_RATES$CHOICES$GROUP_NO_SELECTION)

   df_time_fill <- data.frame(
      USUBJID = c("01", "02", "01", "02"),
      date = as.Date(c("2024-01-01", "2024-01-02", "2025-01-04", "2025-08-04")),
      day = c(-2, 1, 15, 17),
      m = c(1, 1, 0, 0),
      n = c(0, 0, 1, 1),
      ARM = as.factor(c("a", "b", "a", "b"))
   )

   # no grouping:
   res_date <- cum_rate_per_total_exp_time("date", df_time_fill, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   res_day <- cum_rate_per_total_exp_time("day", df_time_fill, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_equal(res_date$time, seq.Date(as.Date("2024-01-01"), as.Date("2025-08-04"), by = "day"))
   expect_equal(res_day$time, c(-2:17))
   compare_with_prev_row(df_time_fill, res_date, "date", REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   compare_with_prev_row(df_time_fill, res_day, "day", REPORT_RATES$CHOICES$GROUP_NO_SELECTION)

   ## validate that there is an error if n_cumsum or m_cumsum value of a filled in row isnt the same as in the prev row
   res_date_m_cumsum_error <- res_date |> dplyr::mutate(m_cumsum = ifelse(date == "2024-05-14", -1, m_cumsum))
   res_day_n_cumsum_error <- res_day |> dplyr::mutate(n_cumsum = ifelse(day == 13, -1, n_cumsum))
   expect_error(compare_with_prev_row(df_time_fill, res_date_m_cumsum_error, "date",
                                      REPORT_RATES$CHOICES$GROUP_NO_SELECTION))
   expect_error(compare_with_prev_row(df_time_fill, res_day_n_cumsum_error, "day",
                                      REPORT_RATES$CHOICES$GROUP_NO_SELECTION))

   ## validate that there is an error if the n or m value of a filled in row isnt NA
   res_day_m_error <- res_day |> dplyr::mutate(m = ifelse(day == 13, 99, m))
   res_date_n_error <- res_date |> dplyr::mutate(n = ifelse(date == "2024-05-14", 99, n))
   expect_error(compare_with_prev_row(df_time_fill, res_day_m_error, "day", REPORT_RATES$CHOICES$GROUP_NO_SELECTION))
   expect_error(compare_with_prev_row(df_time_fill, res_date_n_error, "date", REPORT_RATES$CHOICES$GROUP_NO_SELECTION))

   # grouping:
   res_date_grouped <- cum_rate_per_total_exp_time("date", df_time_fill, "ARM")
   res_day_grouped <- cum_rate_per_total_exp_time("day", df_time_fill, "ARM")
   for (lvl in unique(df_time_fill$ARM)) {
      subset_date_grouped  <-  res_date_grouped |> dplyr::filter(ARM == lvl)
      subset_day_grouped  <-  res_day_grouped |> dplyr::filter(ARM == lvl)
      expect_equal(subset_date_grouped$time, seq.Date(as.Date("2024-01-01"), as.Date("2025-08-04"), by = "day"))
      expect_equal(subset_day_grouped$time, c(-2:17))
   }
   # 2024-01-02 is a filled in data for group a but not for group b
   row <- res_date_grouped |> dplyr::filter(date == "2024-01-02" & ARM == "a")
   prev_row <- res_date_grouped |> dplyr::filter(date == "2024-01-01" & ARM == "a")
   expect_equal(row$m, as.double(NA))
   expect_equal(row$n, as.double(NA))
   expect_equal(row$m_cumsum, prev_row$m_cumsum)
   expect_equal(row$n_cumsum, prev_row$n_cumsum)
   # day 1 is a filled in data for group a but not for group b
   row <- res_day_grouped |> dplyr::filter(day == 1 & ARM == "a")
   prev_row <- res_day_grouped |> dplyr::filter(day == 0 & ARM == "a")
   prev_real_row <- res_day_grouped |> dplyr::filter(day == -2 & ARM == "a") # previous not filled in row
   expect_equal(row$m, as.double(NA))
   expect_equal(row$n, as.double(NA))
   expect_equal(row$m_cumsum, prev_row$m_cumsum)
   expect_equal(row$n_cumsum, prev_row$n_cumsum)
   expect_equal(row$m_cumsum, prev_real_row$m_cumsum)
   expect_equal(row$n_cumsum, prev_real_row$n_cumsum)
})

test_that("cum_rate_per_total_exp() sets ratio = 0 for m_cumsum = 0" |>
         vdoc[["add_spec"]](specs$metric_calculation$cum_rate_per_total_exp_time), {
   #ungrouped
   df_ratio_0_ungrouped <- df_cum_rate |> dplyr::mutate(m = 0, # so that m_cumsum will be 0 in every row
                                              n = 1, 1, 1, 1, 1)
   res_ungrouped <- cum_rate_per_total_exp_time("day", df_ratio_0_ungrouped, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_equal(unique(res_ungrouped$ratio), 0)

   #grouped
   df_ratio_0_grouped <- data.frame(
      USUBJID = c("01", "02", "03", "04", "02", "01"),
      date = as.Date(c("2025-01-01", "2025-01-02", "2025-01-04", "2025-01-05", "2025-01-05", "2025-01-05")),
      day = c(1, 2, 4, 5, 5, 5),
      m = c(0, 1, 1, 0, 0, 0),
      n = c(1, 0, 0, 1, 1, 1),
      ARM = as.factor(c("a", "b", "c", "c", "b", "a"))
   )
   res_grouped <- cum_rate_per_total_exp_time("day", df_ratio_0_grouped, "ARM")
   a_rows <- res_grouped |> dplyr::filter(ARM == "a")
   general_rows <- res_grouped |> dplyr::filter(m_cumsum == 0) # also checking rows of other groups where m_cumsum == 0
   expect_equal(unique(a_rows$ratio), 0)
   expect_equal(unique(general_rows$ratio), 0)
   expect_true(any(res_grouped$ratio != 0))
})

test_that("cum_rate_per_total_exp() calculates correct ratio" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$cum_rate_per_total_exp_time,
                specs$metric_calculation$timetype_selection,
                specs$plot_creation$grouping_and_levels$ungrouped,
                specs$plot_creation$grouping_and_levels$grouped
             )
          ), {
   res_day_ungrouped <- cum_rate_per_total_exp_time("day", df_cum_rate, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   res_date_ungrouped <- cum_rate_per_total_exp_time("date", df_cum_rate, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   res_day_grouped <- cum_rate_per_total_exp_time("day", df_cum_rate, "ARM")
   res_date_grouped <- cum_rate_per_total_exp_time("date", df_cum_rate, "ARM")
   test_res <- list(res_day_ungrouped, res_date_ungrouped, res_day_grouped, res_date_grouped)
   purrr::walk(test_res, ~ {
      expect_true("ratio" %in% colnames(.x))
      expect_equal(.x$ratio, ifelse(.x$total_exp == 0, 0, .x$n_cumsum / .x$total_exp))
      expect_true(all(.x$ratio >= 0))
   })
})



# Function compute_plot_metrics()
test_that("compute_plot_metrics() returns valid dataset for metric reporting rate per active patients" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$rate_per_active_patients,
                specs$metric_calculation$metric_selection
             )
          ), {
   combinations <- tidyr::expand_grid(
      time_selection = c(REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2),
      unit_selection = c(REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1, REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2)
   )
   purrr::walk(split(combinations, seq_len(nrow(combinations))), ~ {
      res <- compute_plot_metrics(dataset = df_rr,
                                  grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                  time_selection = .x$time_selection,
                                  metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                  unit_selection = .x$unit_selection)
      expect_s3_class(res, "data.frame")
      expect_true("ratio" %in% names(res))
   })
})

test_that("compute_plot_metrics() returns valid dataset for metric Cumulative rate per total exposure time" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$cum_rate_per_total_exp_time,
                specs$metric_calculation$metric_selection
             )
          ), {
   # By study date
   res_date <- compute_plot_metrics(dataset = df_cum_rate,
                               grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                               time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                               metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                               unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_s3_class(res_date, "data.frame")
   expect_true("ratio" %in% names(res_date))

   # By study day
   res_day <- compute_plot_metrics(dataset = df_cum_rate,
                               grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                               time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                               metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                               unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_s3_class(res_day, "data.frame")
   expect_true("ratio" %in% names(res_day))
})

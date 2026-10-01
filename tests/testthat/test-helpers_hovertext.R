df_date <- data.frame(time = as.Date(c("2014-01-01", "2015-03-01", "2017-01-01", "2020-05-01", "2017-01-01",
                                       "2020-05-01")),
                      time_for_plot = as.Date(c("2014-01-01", "2015-03-01", "2017-01-01", "2020-05-01", "2017-01-01",
                                                "2020-05-01")),
                      ARM = as.factor(c("a", "b", "a", "b", "a", "b")),
                      ratio = c(2.526, 2.26277, 1.6062, 1.46363, 0.436, 0.7357),
                      n_cumsum = c(14, 12, 16, 21, 53, 43))

df_day <- data.frame(time = c(-32, -1, 1, 43, 1, 43),
                     time_for_plot = c(-32, -1, 0, 42, 0, 42),
                     ARM = as.factor(c("a", "b", "a", "b", "a", "b")),
                     ratio = c(2.526, 2.26277, 1.6062, 1.46363, 0.436, 0.7357),
                     n_cumsum = c(14, 12, 16, 21, 53, 43))

# helper for removing dummy rows because their ht_time_base will be set to "END"
remove_dummy_rows <- function(dataset, grouping_var) {
   dataset <- dataset |>
      dplyr::group_by(.data[[grouping_var]]) |>
      dplyr::filter(dplyr::row_number() < dplyr::n()) |>
      dplyr::ungroup()
}

# Function get_hovertext()
test_that("get_hovertext() works correct for the metric cumulative rate" |>
         vdoc[["add_spec"]](specs$plot_creation$hovering), {
   # with "By study date":
   res_date <- get_hovertext(dataset = df_date,
                             grouping_var = "ARM",
                             metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                             time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # by date
                             unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                             tt_dec_places = 3L)
   expect_true("hovertext" %in% colnames(res_date))
   expect_true(all(grepl("ARM: ", res_date$hovertext)))
   expect_true(all(grepl("Date: ", res_date$hovertext)))
   expect_true(all(grepl("Rate: ", res_date$hovertext)))
   expect_true(all(grepl("Number of AEs: ", res_date$hovertext)))
   expect_true(all(grepl("<br>", res_date$hovertext)))

   # with "By study day":
   res_day <- get_hovertext(dataset = df_day,
                             grouping_var = "ARM",
                             metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                             time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # by day
                             unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                             tt_dec_places = 3L)
   expect_true("hovertext" %in% colnames(res_day))
   expect_true(all(grepl("ARM: ", res_day$hovertext)))
   expect_true(all(grepl("Day: ", res_day$hovertext)))
   expect_true(all(grepl("Rate: ", res_day$hovertext)))
   expect_true(all(grepl("Number of AEs: ", res_day$hovertext)))
   expect_true(all(grepl("<br>", res_day$hovertext)))
})

test_that("get_hovertext() works correct for the metric reporting rate per active patients" |>
         vdoc[["add_spec"]](specs$plot_creation$hovering), {
   # with "By study date":
   res_date <- get_hovertext(dataset = df_date,
                                       grouping_var = "ARM",
                                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # by date
                                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                                       tt_dec_places = 3L)
   res_date_no_dummy <- remove_dummy_rows(dataset = res_date, grouping_var = "ARM")
   expect_true("hovertext" %in% colnames(res_date))
   expect_true(all(grepl("ARM: ", res_date$hovertext)))
   expect_true(all(grepl("Interval: ", res_date_no_dummy$hovertext)))
   expect_true(all(grepl(" --- ", res_date_no_dummy$hovertext)))
   expect_true(all(grepl("Rate: ", res_date$hovertext)))
   expect_true(all(grepl("Number of AEs: ", res_date$hovertext)))
   expect_true(all(grepl("<br>", res_date$hovertext)))

   # with "By study day:"
   res_day <- get_hovertext(dataset = df_day,
                             grouping_var = "ARM",
                             metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                             time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # by day
                             unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                             tt_dec_places = 3L)
   res_day_no_dummy <- remove_dummy_rows(dataset = res_day, grouping_var = "ARM")
   expect_true("hovertext" %in% colnames(res_day))
   expect_true(all(grepl("ARM: ", res_date$hovertext)))
   expect_true(all(grepl("Interval: ", res_day_no_dummy$hovertext)))
   expect_true(all(grepl(" --- ", res_day_no_dummy$hovertext)))
   expect_true(all(grepl("Rate: ", res_day$hovertext)))
   expect_true(all(grepl("Number of AEs: ", res_day$hovertext)))
   expect_true(all(grepl("<br>", res_day$hovertext)))
})

test_that("get_hovertext() returns dataset only containing the correct columns", {
   res_cum_date <- get_hovertext(dataset = df_date,
                         grouping_var = "ARM",
                         metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1, # cumulative rate
                         time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # by date
                         unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                         tt_dec_places = 3L)
   res_cum_day <- get_hovertext(dataset = df_day,
                                grouping_var = "ARM",
                                metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                                time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # by day
                                unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                                tt_dec_places = 3L)
   res_rr_date <- get_hovertext(dataset = df_date,
                                grouping_var = "ARM",
                                metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2, # rr per active patients
                                time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # by date
                                unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                                tt_dec_places = 3L)
   res_rr_day <- get_hovertext(dataset = df_day,
                               grouping_var = "ARM",
                               metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                               time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # by day
                               unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                               tt_dec_places = 3L)
   res_no_group <- get_hovertext(dataset = df_date,
                                 grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                 metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1, # cumulative rate
                                 time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # by date
                                 unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1,
                                 tt_dec_places = 3L)
   test_res <- list(res_cum_date, res_cum_day, res_rr_date, res_rr_day, res_no_group)

   purrr::walk(test_res, ~ {
      expect_true(all(c("hovertext", "ratio", "n_cumsum", "ARM", "time_for_plot", "time") %in% colnames(.x)))
      expect_false(
         any(c("ht_time_base", "ht_lvls_display", "ht_time_info", "interval_start", "interval_end") %in% colnames(.x)))
   })
})



# Function get_hovertext_time_base()
test_that("get_hovertext_time_base() works correct for the metric cumulative rate" |>
          vdoc[["add_spec"]](
             c(
                specs$metric_calculation$timetype_selection,
                specs$plot_creation$hovering
             )
          ), {
   # with "By study date":
   res_date <- get_hovertext_time_base(dataset = df_date,
                                       grouping_var = "ARM",
                                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_true("ht_time_base" %in% colnames(res_date))
   expect_true(all(grepl("^Date: ", res_date$ht_time_base)))
   expect_true(#checking if the value of df$time is in the res$ht_time_base
      all(mapply(function(x, y) grepl(x, y), df_date$time, res_date$ht_time_base))
   )

   # with "By study day":
   res_day <- get_hovertext_time_base(dataset = df_day,
                                       grouping_var = "ARM",
                                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1,
                                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   expect_true("ht_time_base" %in% colnames(res_day))
   expect_true(all(grepl("^Day: ", res_day$ht_time_base)))
   expect_true(
      all(mapply(function(x, y) grepl(x, y), df_day$time, res_day$ht_time_base))
      )
})

test_that("get_hovertext_time_base() works correct for the metric reporting rate per active patients and the time
          selection 'By study date'" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$hovering,
                specs$metric_calculation$timetype_selection,
                specs$metric_calculation$binsize_selection
             )
          ), {
   # with binsize week:
   res_week <- get_hovertext_time_base(dataset = df_date,
                                       grouping_var = "ARM",
                                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) # == week

   res_week <- remove_dummy_rows(res_week, "ARM")
   df_date_no_dummy <- remove_dummy_rows(df_date, "ARM")
   expect_true(all(c("interval_end", "ht_time_base") %in% colnames(res_week)))
   expect_true(all(grepl("^Interval: ", res_week$ht_time_base)))
   expect_true(all(grepl("---", res_week$ht_time_base)))
   expect_equal(res_week$interval_end, df_date_no_dummy$time + lubridate::days(6))
   expect_true(
      all(mapply(function(x, y) grepl(x, y), df_date_no_dummy$time, res_week$ht_time_base))
   )
   expect_true(
      all(mapply(function(x, y) grepl(x, y), res_week$interval_end, res_week$ht_time_base))
   )

   # with binsize month:
   res_month <- get_hovertext_time_base(dataset = df_date,
                                        grouping_var = "ARM",
                                        metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                        time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1,
                                        unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # == month

   res_month <- remove_dummy_rows(res_month, "ARM")
   expect_true(all(c("interval_end", "ht_time_base") %in% colnames(res_month)))
   expect_true(all(grepl("^Interval: ", res_month$ht_time_base)))
   expect_true(all(grepl("---", res_month$ht_time_base)))
   expect_equal(res_month$interval_end, df_date_no_dummy$time + lubridate::days(30))
   expect_true(
      all(mapply(function(x, y) grepl(x, y), df_date_no_dummy$time, res_month$ht_time_base))
   )
   expect_true(
      all(mapply(function(x, y) grepl(x, y), res_month$interval_end, res_month$ht_time_base))
   )
})

test_that("get_hovertext_time_base() creates the correct ht_time_base for the metric reporting rate per active patients
           and the time selection 'By study day'" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$hovering,
                specs$metric_calculation$timetype_selection
             )
          ), {
   # with binsize week:
   res_week <- get_hovertext_time_base(dataset = df_day,
                                       grouping_var = "ARM",
                                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) # == week
   res_week <- remove_dummy_rows(res_week, "ARM")
   expect_true(all(c("interval_start", "interval_end", "ht_time_base") %in% colnames(res_week)))
   expect_true(all(grepl("^Interval: Study Days ", res_week$ht_time_base)))
   expect_true(all(grepl("---", res_week$ht_time_base)))
   expect_true(
      all(mapply(function(start, end, ht) {
                  grepl(start, ht) && grepl(end, ht)
               }, res_week$interval_start, res_week$interval_end, res_week$ht_time_base))
   )

   # with binsize month:
   res_month <- get_hovertext_time_base(dataset = df_day,
                                        grouping_var = "ARM",
                                        metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                        time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                                        unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # == month
   res_month <- remove_dummy_rows(res_month, "ARM")
   expect_true(all(c("interval_start", "interval_end", "ht_time_base") %in% colnames(res_month)))
   expect_true(all(grepl("^Interval: Study Days ", res_month$ht_time_base)))
   expect_true(all(grepl("---", res_month$ht_time_base)))
   expect_true(
       all(mapply(function(start, end, ht) {
          grepl(start, ht) && grepl(end, ht)
       }, res_month$interval_start, res_month$interval_end, res_month$ht_time_base))
   )
})

test_that("get_hovertext_time_base() calculates the correct interval end and start for the metric reporting rate per
          active patients and the time selection 'By study day'" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$hovering,
                specs$metric_calculation$timetype_selection,
                specs$metric_calculation$binsize_selection
             )
          ), {
   # with binsize week:
   res_week <- get_hovertext_time_base(dataset = df_day,
                                       grouping_var = "ARM",
                                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) # == week
   res_week <- remove_dummy_rows(res_week, "ARM")
   df_day_no_dummy <- remove_dummy_rows(df_day, "ARM")
   expected_start <- ifelse(df_day_no_dummy$time_for_plot < 0,
                            df_day_no_dummy$time_for_plot * 7,
                            df_day_no_dummy$time_for_plot * 7 + 1)
   expected_end <- ifelse(df_day_no_dummy$time_for_plot < 0,
                         (df_day_no_dummy$time_for_plot + 1) * 7 - 1,
                         (df_day_no_dummy$time_for_plot + 1) * 7)

   expect_equal(res_week$interval_start, expected_start)
   expect_equal(res_week$interval_end, expected_end)

   # with binsize month:
   res_month <- get_hovertext_time_base(dataset = df_day,
                                        grouping_var = "ARM",
                                        metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                        time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2,
                                        unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT2) # == month
   res_month <- remove_dummy_rows(res_month, "ARM")
   expected_start <- ifelse(df_day_no_dummy$time_for_plot < 0,
                            df_day_no_dummy$time_for_plot * 30,
                            df_day_no_dummy$time_for_plot * 30 + 1)
   expected_end <- ifelse(df_day_no_dummy$time_for_plot < 0,
                         (df_day_no_dummy$time_for_plot + 1) * 30 - 1,
                         (df_day_no_dummy$time_for_plot + 1) * 30)

   expect_equal(res_month$interval_start, expected_start)
   expect_equal(res_month$interval_end, expected_end)
})

test_that("get_hovertext_time_base() removes hovertext from dummy rows" |>
         vdoc[["add_spec"]](specs$plot_creation$hovering), {
   # by study date
   res_date <- get_hovertext_time_base(dataset = df_date,
                                       grouping_var = "ARM",
                                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1, # == by study date
                                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   # extract dummy rows
   dummy_rows <- res_date  |>
      dplyr::group_by(ARM) |>
      dplyr::filter(dplyr::row_number() == dplyr::n()) |>
      dplyr::ungroup()

   expect_equal(unique(dummy_rows$ht_time_base), "END")


   # by study day
   res_day <- get_hovertext_time_base(dataset = df_day,
                                       grouping_var = "ARM",
                                       metric_selection = REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2,
                                       time_selection = REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2, # == by study day
                                       unit_selection = REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1)
   # extract dummy rows
   dummy_rows <- res_day  |>
      dplyr::group_by(ARM) |>
      dplyr::filter(dplyr::row_number() == dplyr::n()) |>
      dplyr::ungroup()

   expect_equal(unique(dummy_rows$ht_time_base), "END")
})



# Function remove_ht_from_dummy_rows()
df_time_base <- data.frame(
   time = c(-1, 1, 2, 3),
   ht_time_base = c("Day: -1", "Day: 1", "Day: 2", "Day: 3"),
   ARM = factor(c("a", "a", "b", "b"))
)
test_that("remove_ht_from_dummy_rows() replaces ht_time_base in last row of each group with END" |>
             vdoc[["add_spec"]](specs$plot_creation$hovering), {
   res <- remove_ht_from_dummy_rows(df_time_base, "ARM")

   expect_true("ht_time_base" %in% colnames(res))
   expect_equal(res$ht_time_base[c(2, 4)], c("END", "END"))
   expect_equal(res$ht_time_base[c(1, 3)], c("Day: -1", "Day: 2"))
})

test_that("remove_ht_from_dummy_rows() only replaces the ht_time_base in last row with END when there is no
          grouping" |>
             vdoc[["add_spec"]](specs$plot_creation$hovering), {
   res <- remove_ht_from_dummy_rows(df_time_base, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_true("ht_time_base" %in% colnames(res))
   expect_equal(res$ht_time_base, c(c("Day: -1", "Day: 1", "Day: 2", "END")))
})

test_that("remove_ht_from_dummy_rows() only replaces the ht_time_base in last row with END when there is only 1 level
          in the grouping variable" |>
             vdoc[["add_spec"]](specs$plot_creation$hovering), {
   df <- df_time_base |> dplyr::mutate(ARM = factor("a"))
   res <- remove_ht_from_dummy_rows(df, "ARM")
   expect_true("ht_time_base" %in% colnames(res))
   expect_equal(res$ht_time_base, c(c("Day: -1", "Day: 1", "Day: 2", "END")))
})

test_that("remove_ht_from_dummy_rows() works when groups only have 1 row" |>
             vdoc[["add_spec"]](specs$plot_creation$hovering), {
    df <- df_time_base
    df$ARM <- factor(c("a", "a", "c", "d"))
    res <- remove_ht_from_dummy_rows(df, "ARM")
    expect_true("ht_time_base" %in% colnames(res))
    expect_equal(res$ht_time_base, c(c("Day: -1", "END", "END", "END")))

    df$ARM <- factor(c("a", "b", "c", "d"))
    res <- remove_ht_from_dummy_rows(df, "ARM")
    expect_equal(unique(res$ht_time_base), "END")
})



# Function get_hovertext_levels_display()
test_that("get_hovertext_levels_display() works correct with valid grouping" |>
         vdoc[["add_spec"]](specs$plot_creation$hovering), {
   res <- get_hovertext_levels_display(df_day, "ARM")
   expect_true("ht_lvls_display" %in% colnames(res))
   expect_true(all(grepl("^ARM: ", res$ht_lvls_display)))
   expect_true(
      all(mapply(function(x, y) grepl(x, y), df_day$ARM, res$ht_lvls_display))
   )
   expect_false(any(grepl("Ungrouped", res$ht_lvls_display)))
})

test_that("get_hovertext_levels_display() works correct without grouping" |>
         vdoc[["add_spec"]](specs$plot_creation$hovering), {
   res <- get_hovertext_levels_display(df_day, REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
   expect_true("ht_lvls_display" %in% colnames(res))
   expect_equal(unique(res$ht_lvls_display), "Ungrouped")

   # with not valid grouping var:
   res_not_valid <- get_hovertext_levels_display(df_day, "SITEID")
   expect_true("ht_lvls_display" %in% colnames(res_not_valid))
   expect_equal(unique(res_not_valid$ht_lvls_display), "Ungrouped")
   expect_false(any(grepl("SITEID", res$ht_lvls_display)))
})



# Function get_hovertext_ae_info()
df_ae_info <- data.frame(
   ratio = c(0.1234, 2.9999, 3.3735),
   n_cumsum = c(10, 62, 85)
)
test_that("get_hovertext_ae_info() returns dataset with ht_ae_info column containing the correct elements" |>
         vdoc[["add_spec"]](specs$plot_creation$hovering), {
   res <- get_hovertext_ae_info(df_ae_info, 3L)
   expect_true("ht_ae_info" %in% colnames(res))
   expect_true(all(grepl("^Rate: ", res$ht_ae_info)))
   expect_true(all(grepl("<br>Number of AEs: ", res$ht_ae_info)))
   expect_true(
      all(mapply(function(x, y) grepl(x, y), df_ae_info$n_cumsum, res$ht_ae_info))
   )
   expect_true(
      all(mapply(function(x, y) grepl(x, y), round(df_ae_info$ratio, 3), res$ht_ae_info))
   )

   # for ratio and cumsum = 0
   df_zero <- df_ae_info |> dplyr::mutate(ratio = 0, n_cumsum = 0)
   res_zero <- get_hovertext_ae_info(df_zero, 3L)
   expect_true("ht_ae_info" %in% colnames(res_zero))
   expect_true(all(grepl("^Rate: 0", res_zero$ht_ae_info)))
   expect_true(all(grepl("<br>Number of AEs: 0", res_zero$ht_ae_info)))
})

test_that("get_hovertext_ae_info() rounds the ratio correctly" |>
          vdoc[["add_spec"]](
             c(
                specs$plot_creation$hovering,
                specs$app_creator_settings$tt_decimal_places
             )
          ), {
   res_0 <- get_hovertext_ae_info(df_ae_info, 0L)
   expect_true(
      all(mapply(function(x, y) grepl(x, y), paste0(round(df_ae_info$ratio, 0), "<br>"), res_0$ht_ae_info))
   )
   res_1 <- get_hovertext_ae_info(df_ae_info, 1L)
   expect_true(
      all(mapply(function(x, y) grepl(x, y), paste0(round(df_ae_info$ratio, 1), "<br>"), res_1$ht_ae_info))
   )
   res_3 <- get_hovertext_ae_info(df_ae_info, 3L)
   expect_true(
      all(mapply(function(x, y) grepl(x, y), paste0(round(df_ae_info$ratio, 3), "<br>"), res_3$ht_ae_info))
   )
   res_10 <- get_hovertext_ae_info(df_ae_info, 10L)
   expect_true(
      all(mapply(function(x, y) grepl(x, y), paste0(round(df_ae_info$ratio, 10), "<br>"), res_10$ht_ae_info))
   )
})

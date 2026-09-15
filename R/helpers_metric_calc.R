#' Internal helper function for preparing the ds dataframe by adding a column that indicates study entry or exit
#'
#' @param dataset `[data.frame]` The disposition dataframe.
#' @param subjid_var `[character(1)]` Character name of the unique subject identifier column in all datasets.
#' @param event_var `[character(1)]` Name of the variable that contains the description of the event.
#' @param entry_terms `[character(1+)]` Defining which values out of event_var should be entry events.
#' @param exit_terms `[character(1+)]` Defining which values out of event_var should be exit events.
#' @param date_var `[character(1)]` Name of the variable containing the date of the event.
#' @param day_var `[character(1)]` Name of the variable containing the study day of the event.
#'
#' @returns A dataframe that contains an additional column "m" which indicates study entry (m = 1) or exit (m = -1).
#' These markers are used in downstream calculations to derive the number of active subjects in the study at any time
#' point, and the cumulative exposure time.
#' The column "n" with the only value 0 indicates that these rows don't describe Adverse Events.
#' @keywords internal
prepare_ds_data <- function(dataset, subjid_var, event_var, entry_terms, exit_terms, date_var, day_var) {
  dataset <- dataset |>
    dplyr::mutate(
      m = dplyr::case_when(
        .data[[event_var]] %in% entry_terms ~ 1,
        .data[[event_var]] %in% exit_terms ~ -1,
        TRUE ~ 0
      ),
      date = .data[[date_var]],
      day = .data[[day_var]],
      n = 0
    ) |>
    dplyr::select(dplyr::all_of(c(subjid_var, "date", "day", "m", "n")))

  return(dataset)
}



#'  Internal helper function for preparing the ae dataframe by adding a column that indicates the occurrence of
#'  adverse events.
#'
#' @param dataset `[data.frame]` The Adverse Event dataframe.
#' @param subjid_var `[character(1)]` Character name of the unique subject identifier column in all datasets.
#' @param date_var `[character(1)]` Name of the variable containing the date of the event.
#' @param day_var `[character(1)]` Name of the variable containing the study day of the event.
#'
#' @returns  A dataframe that contains an additional column "n" with the value 1 in every row, which indicates the
#' occurrence of an adverse event.
#' The column "m" with the only value 0 indicates that these rows don't describe Entry or Exit Events.
#' @keywords internal
prepare_ae_data <- function(dataset, subjid_var, date_var, day_var) {
  dataset <- dataset |>
    dplyr::mutate(
      n = 1,
      m = 0,
      date = .data[[date_var]],
      day = .data[[day_var]]
    ) |>
    dplyr::select(dplyr::all_of(c(subjid_var, "date", "day", "m", "n")))

  return(dataset)
}



#' Internal helper function for combining the preprocessed disposition (DS) and adverse event (AE) datasets into one
#' unified event stream. If a valid grouping variable is provided, the function enriches the combined dataset with the
#' corresponding group assignment from the demographics (DM) domain.
#'
#' @param dm_dataset `[data.frame]` The demographics dataframe.
#' @param ds_prep_dataset `[data.frame]` The disposition dataframe returned by the prepare_ds_data() function.
#' @param ae_prep_dataset  `[data.frame]` The adverse event dataframe returned by the prepare_ae_data() function.
#' @param subjid_var `[character(1)]` Character name of the unique subject identifier column in all dataframes.
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the "dm_dataset". The
#' values of this column define the different groups in the plot.
#'
#' @returns A dataframe containing all DS and AE events row‑bound into a unified event dataset. The result always
#' includes the harmonised columns required for the downstream computation of both metrics. If a valid grouping_var is
#' provided, an additional column with the group assignment is attached to each event record via a left join.
#' @keywords internal
merge_event_data_with_grouping <- function(dm_dataset, ds_prep_dataset, ae_prep_dataset, subjid_var, grouping_var) {
  combined <- rbind(ds_prep_dataset, ae_prep_dataset)

  if (is.null(grouping_var) || grouping_var == "") {
    return(combined)
  }

  if (grouping_var == REPORT_RATES$CHOICES$GROUP_NO_SELECTION) {
    return(combined)
  }

  dm_tmp <- dm_dataset |> dplyr::select(dplyr::all_of(c(subjid_var, grouping_var)))

  res <- combined |> dplyr::left_join(dm_tmp, by = subjid_var)
  return(res)
}



#' Internal helper function that calls the right functions for metric calculation depending on the inputted parameters
#' by the user. Either  rates_per_active_patient() with additional helper functions or cum_rate_per_total_exp_time().
#'
#' @param dataset `[data.frame]` Dataframe containing the unique identifier column; date-column, day-column;
#' n-column (Adverse Events); m-column(study entries/exits); and the group-column if selected. This dataframe was
#' returned by merge_event_data_with_grouping().
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the "dm_dataset". The
#' values of this column define the different groups in the plot.
#' @param time_selection `[character(1)]` Determining if the time on the x-axis should be study dates (calendar based)
#' or study days (subject based) Either TIMETYPE_BUTTONS_OPT1 or TIMETYPE_BUTTONS_OPT2.
#' @param metric_selection `[character(1)]` for determining the calculated rate on the y-axis. Either
#' METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.
#' @param unit_selection  `[character(1)]` Determining the binsize when computing the reporting rate per active
#' patients. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.
#'
#' @returns A processed dataframe which contains the calculated column "ratio" which will be the y-Value used for the
#' plot. Independently of the selected time alignment ("By study date" or "By study day"), the returned dataframe always
#' includes a unified time column, which holds either dates or study days. This ensures a consistent downstream workflow
#' @keywords internal
compute_plot_metrics <- function(dataset, grouping_var, time_selection, metric_selection, unit_selection) {
  time_type <- if (time_selection == REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1) "date" else "day"

  if (metric_selection == REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT2) {
    result_df <- rates_per_active_patient(time_type = time_type,
                                          group_dataset = dataset,
                                          grouping_var = grouping_var,
                                          unit_selection = unit_selection)

    time_unit <- if (unit_selection == REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) "week" else "month"

    result_df <- fill_missing_intervals(dataset = result_df,
                                        grouping_var = grouping_var,
                                        time_type = time_type,
                                        time_unit = time_unit)

    result_df <- add_latest_dummy_point(dataset = result_df,
                                        grouping_var = grouping_var,
                                        time_type = time_type,
                                        time_unit = time_unit)
  } else {
    result_df <- cum_rate_per_total_exp_time(time_type = time_type,
                                             group_dataset = dataset,
                                             grouping_var = grouping_var)
  }

  return(result_df)
}



#' Internal helper function for preprocessing the data if reporting rates per active patient is selected as the metric.
#'
#' @param time_type `[character(1)]`  Either "date" (calendar based) or "day" (subject based). Reflects the x-axis of
#' the plots.
#' @param group_dataset `[data.frame]` Containing the unique identifier column; date-column, day-column;
#' n-column (Adverse Events); m-column(study entries/exits); and the group-column if selected in the UI. This data frame
#' was returned by merge_event_data_with_grouping().
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the "dm_dataset". The
#' values of this column define the different groups in the plot.
#' @param unit_selection `[character(1)]` Determining the binsize. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.
#'
#' @returns A data frame that now contains the column "ratio", which is used as the y-Value in the plot. The rows of the
#' data frame represent the intervals in the plot later. Independently of the selected time alignment ("By study date"
#' or "By study day"), the returned dataframe always includes a unified time column, which holds either dates or study
#' days. This ensures a consistent downstream workflow.
#' @keywords internal
rates_per_active_patient <- function(time_type, group_dataset, grouping_var, unit_selection) {
  if (time_type == "date") {
    group_dataset <- group_dataset |>
      dplyr::filter(!is.na(date)) |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        time = lubridate::floor_date(date, unit = tolower(unit_selection), week_start = 1)
      )
  } else {
    binsize <- if (tolower(unit_selection) == "week") 7 else 30
    group_dataset <- group_dataset |>
      dplyr::filter(!is.na(.data[["day"]])) |>
      dplyr::arrange(.data[["day"]]) |>
      dplyr::mutate(
        time = dplyr::case_when( # grouping study days into bins
          day > 0 ~ ceiling(.data[["day"]] / binsize),
          day < 0 ~ floor(.data[["day"]] / binsize) # use floor for negative days
        )
      )
  }

  if (is_valid_grouping_var(grouping_var, group_dataset)) {
    group_dataset <- group_dataset |> dplyr::group_by(.data[[grouping_var]])
  }

  group_dataset <- group_dataset |>
    dplyr::mutate(m_cumsum = cumsum(.data[["m"]])) |> # calculating the amount of active patients before time grouping
    dplyr::group_by(.data[["time"]], .add = TRUE) |>
    dplyr::mutate(n_cumsum = cumsum(.data[["n"]])) |> # calculating the amount of adverse events per interval
    # only keep last row of each interval which will represent the respective intervals in the plot:
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ratio = dplyr::if_else(.data[["m_cumsum"]] == 0, 0, .data[["n_cumsum"]] / .data[["m_cumsum"]])
    )

  return(group_dataset)
}



#' Internal helper function for preprocessing the data if cumulative rate per total exposure time is the selected metric
#'
#' @inheritParams rates_per_active_patient
#'
#' @importFrom rlang :=
#' @returns A data frame that now contains the column "ratio", which is used as the y-Value in the plot. The rows of the
#' data frame each represent a date / study day (for their respective group if selected) in the plot later.
#' Independently of the selected time alignment ("By study date" or "By study day"), the returned dataframe always
#' includes a unified time column, which holds either dates or study days. This ensures a consistent downstream workflow
#' @keywords internal
cum_rate_per_total_exp_time <- function(time_type, group_dataset, grouping_var) {
  group_dataset <- group_dataset |>
    dplyr::filter(!is.na(.data[[time_type]])) |>
    dplyr::arrange(.data[[time_type]])

  if (is_valid_grouping_var(grouping_var, group_dataset)) {
    group_dataset <- group_dataset |> dplyr::group_by(.data[[grouping_var]])
  }

  df_tmp <- group_dataset |>
    dplyr::mutate(
      m_cumsum = cumsum(.data[["m"]]), # amount of active patients over the time
      n_cumsum = cumsum(.data[["n"]]) # amount of cumulated AE's
    ) |>
    dplyr::group_by(.data[[time_type]], .add = TRUE) |>
    dplyr::slice_tail(n = 1) |> #only keep the last row per date (if there is more than one record for a day)
    dplyr::ungroup()

  # Fill missing days in. This is important for the correct calculation of the total exposure time.
  by_val <- if (time_type == "date") "day" else 1
  full_dates <- tibble::tibble(
    !!rlang::sym(time_type) := seq(min(df_tmp[[time_type]]),
                                   max(df_tmp[[time_type]]),
                                   by = by_val)
  )

  if (is_valid_grouping_var(grouping_var, group_dataset)) {
    group_levels <- unique(df_tmp[[grouping_var]])
    full_dates <- full_dates |>
       tidyr::crossing(!!rlang::sym(grouping_var) := group_levels) # create all combinations of date and group values

    result_df <- full_dates |>
      dplyr::left_join(df_tmp, by = c(time_type, grouping_var)) |>
      dplyr::group_by(.data[[grouping_var]])
  } else {
    result_df <- full_dates |> dplyr::left_join(df_tmp, by = c(time_type))
  }

  result_df <- result_df |>
    # fills in the values for the manually created days in the step above with the last known value
    tidyr::fill(dplyr::all_of(c("m_cumsum", "n_cumsum")), .direction = "down") |>
    dplyr::mutate(
      m_cumsum = ifelse(is.na(.data[["m_cumsum"]]), 0, .data[["m_cumsum"]]), # required for cumsum(m_cumsum) to work
    ) |>
    dplyr::mutate(
      total_exp = cumsum(.data[["m_cumsum"]]), # exposure time is the sum of all active patients on each day over time
      ratio = dplyr::if_else(.data[["total_exp"]] == 0, 0, .data[["n_cumsum"]] / .data[["total_exp"]]),
      time = .data[[time_type]]
    ) |>
    dplyr::ungroup()

  return(result_df)
}



#' Internal helper function for filling in all the missing intervals if "reporting rate per active patients" is
#' selected as the metric.
#'
#' This is necessary to correctly show the value zero of the intervals where there was no adverse event or disposition
#' event, and because of that no row was generated in the first place. Without this function the value of the last
#' interval would get used falsely for the current interval where the value is = 0 in reality. It also enables hovering
#' for the missing intervals.
#'
#' @param dataset `[data.frame]` Data frame returned by rates_per_active_patients() function.
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the "dm_dataset". The
#' values of this column define the different groups in the plot.
#' @param time_type `[character(1)]` Determining if the time on the x-axis should be study dates (calendar based)
#' or study days (subject based). Either "date" or "day".
#' @param time_unit `[character(1)]` Determining the binsize. Either "week" or "month".
#'
#' @returns A data frame which contains now one row per interval and group.
#' @keywords internal
fill_missing_intervals <- function(dataset, grouping_var, time_type, time_unit) {
   if (time_type == "day") {
    all_intervals <- seq(
      from = min(dataset$time),
      to = max(dataset$time),
      by = 1
    )
    all_intervals <- all_intervals[all_intervals != 0] # because there is no week 0 or month 0
  } else {
    all_intervals <- seq.Date(
      from = min(dataset$time),
      to = max(dataset$time),
      by = time_unit
    )
  }

  if (is_valid_grouping_var(grouping_var, dataset)) {
    complete_template <- dataset |>
      dplyr::distinct(!!rlang::sym(grouping_var)) |>
      tidyr::crossing(time = all_intervals)

    missing_rows <- complete_template |>
      dplyr::anti_join(dataset, by = c(grouping_var, "time")) |>
      dplyr::mutate(ratio = 0.0)

    filled_dataset <- dplyr::bind_rows(dataset, missing_rows) |>
                      dplyr::arrange(!!rlang::sym(grouping_var), .data[["time"]])
  } else {
    complete_template <- tibble::tibble(time = all_intervals)

    missing_rows <- complete_template |>
      dplyr::anti_join(dataset, by = "time") |>
      dplyr::mutate(ratio = 0.0)

    filled_dataset <- dplyr::bind_rows(dataset, missing_rows) |>
      dplyr::arrange(.data[["time"]])
  }

  return(filled_dataset)
}



#' Internal helper function for adding a last dummy point (for each group) which marks the end of the last
#' interval (of each group).
#'
#' This is necessary for drawing the last interval. Otherwise there would be just a point for the last interval, because
#' an interval is drawn from the starting point to the x coordinates of the starting point of the next interval and
#' if there would be no next starting point, the interval is not drawn.
#'
#' @param dataset `[data.frame]` Data frame returned by fill_missing_intervals() function.
#' @inheritParams fill_missing_intervals
#'
#' @importFrom rlang .data
#' @returns A data frame which contains an additional dummy row for each group after the last real interval.
#' @keywords internal
add_latest_dummy_point <- function(dataset, grouping_var, time_type, time_unit) {
  if (is_valid_grouping_var(grouping_var, dataset)) {
    dummy_rows <- dataset |>
      dplyr::group_by(.data[[grouping_var]]) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
  } else {
    dummy_rows <- dataset |>
      dplyr::slice_tail(n = 1)
  }

  if (time_type == "date" && time_unit == "month") {
    dummy_rows <- dummy_rows |> dplyr::mutate(time = lubridate::`%m+%`(.data[["time"]], months(1)))
  } else if (time_type == "date") { # so for week binsize
    dummy_rows <- dummy_rows |> dplyr::mutate(time = .data[["time"]] + lubridate::duration(1, units = time_unit))
  } else {
    dummy_rows <- dummy_rows |> dplyr::mutate(time = .data[["time"]] + ifelse(.data[["time"]] == -1, 2, 1))
  }

  if (time_type == "date") dummy_rows$time <- as.Date(dummy_rows$time)
  else dummy_rows$time <- as.numeric(dummy_rows$time)

  extended_dataset <- dplyr::bind_rows(dataset, dummy_rows)
  return(extended_dataset)
}

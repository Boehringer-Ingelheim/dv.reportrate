#' Internal helper function to create the dynamic hovertext that is displayed when hovering over the interactive plot.
#'
#' @param dataset `[data.frame]` Data frame returned by the get_time_for_plot() function.
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the demographics data frame.
#' The values of this column define the different groups in the plot.
#' @param metric_selection `[character(1)]` For determining the calculated rate on the y-axis. Either
#' METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.
#' @param time_selection `[character(1)]` Determining if the time on the x-axis should be study dates (calendar based)
#' or study days (subject based) Either TIMETYPE_BUTTONS_OPT1 or TIMETYPE_BUTTONS_OPT2.
#' @param unit_selection `[character(1)]` Determining the binsize when computing the reporting rate per active
#' patients. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.
#' @param tt_dec_places `[integer(1)]` Number of decimal places to round the value shown in the hovertext.
#'
#' @returns A processed data frame which now contains an additional column "hovertext".
#' @keywords internal
get_hovertext <- function(dataset, grouping_var, metric_selection, time_selection, unit_selection, tt_dec_places) {
   dataset <- get_hovertext_time_base(dataset, grouping_var, metric_selection, time_selection, unit_selection)
   dataset <- get_hovertext_levels_display(dataset, grouping_var)
   dataset <- get_hovertext_ae_info(dataset, tt_dec_places)

   dataset <- dataset |>
      dplyr::mutate(hovertext = paste0(.data[["ht_lvls_display"]], "<br><br>",
                                       .data[["ht_time_base"]], "<br><br>",
                                       .data[["ht_ae_info"]])) |>
      dplyr::select(-dplyr::all_of(c("ht_lvls_display", "ht_time_base", "ht_ae_info"))) |>
      dplyr::select(-dplyr::any_of(c("interval_start", "interval_end")))

   return(dataset)
}



#' Internal helper function for getting the part of the hovertext that contains the time information.
#'
#' @inheritParams get_hovertext
#'
#' @returns A processed data frame with the additional columns "interval_end", "ht_time_base" and also "interval_start"
#' if the time_selection was "by study date". The values in these columns are later a part of the generated hovertext.
#' @keywords internal
get_hovertext_time_base <- function(dataset, grouping_var, metric_selection, time_selection, unit_selection) {

   # for the metric cumulative rate
   if (metric_selection == REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1) {
      description <- ifelse((time_selection == REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1), "Date: ", "Day: ")
      dataset <- dataset |> dplyr::mutate(ht_time_base = paste0(description, dataset$time))
      return(dataset)
   }

   # for the metric report rates per active patients
   if (unit_selection == REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) {
      time_unit <- "week"
      time_mult <- 7
   } else {
      time_unit <- "month"
      time_mult <- 30
   }

   if (time_selection == REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1) { # if by study date
      dataset <- dataset |>
         dplyr::mutate(
            interval_end = .data[["time"]] + lubridate::period(1, time_unit) - 1,
            ht_time_base = paste0("Interval: ", .data[["time"]], " --- ", as.Date(.data[["interval_end"]]))
         )
   } else {
      dataset <- dataset |>
         dplyr::mutate(
            interval_start = ifelse((.data[["time_for_plot"]] < 0),
                                    .data[["time_for_plot"]] * time_mult,
                                    (.data[["time_for_plot"]] * time_mult) + 1),
            interval_end = ifelse((.data[["time_for_plot"]] < 0),
                                  ((.data[["time_for_plot"]] + 1) * time_mult) - 1,
                                  (.data[["time_for_plot"]] + 1) * time_mult),
            ht_time_base = paste0("Interval: Study Days ", .data[["interval_start"]], "  ---  ", .data[["interval_end"]])
         )
   }
   dataset <- remove_ht_from_dummy_rows(dataset, grouping_var)

   return(dataset)
}



#' Internal helper function for removing the ht_time_base from the dummy rows that were only created to show the last
#' time interval correctly in the first place, and exchange it with "END" instead.
#'
#' @param dataset `[data.frame]` Data frame that was passed to the get_hovertext_time function and processed by
#' the function.
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the demographics data frame.
#'  The values of this column define the different groups in the plot.
#'
#' @returns A processed data frame without the ht_time_base and instead "END" in the dummy rows.
#' @keywords internal
remove_ht_from_dummy_rows <- function(dataset, grouping_var) {
   dataset <- dataset |> dplyr::arrange(.data[["time"]])
   if (is_valid_grouping_var(grouping_var, dataset)) {
      dataset <- dataset |> dplyr::group_by(.data[[grouping_var]])
   }

   dataset <- dataset |>
      dplyr::mutate(ht_time_base = ifelse(dplyr::row_number() == dplyr::n(), "END", .data[["ht_time_base"]])) |>
      dplyr::ungroup()

   return(dataset)
}



#' Internal helper function for getting the part of the hovertext that contains the levels information.
#'
#' @param dataset `[data.frame]` Data frame returned by the get_time_for_plot() function.
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the demographics data frame.
#'  The values of this column define the different groups in the plot.
#'
#' @returns A processed data frame with the additional column "ht_levels_display". The values in this column are later
#' a part of the generated hovertext.
#' @keywords internal
get_hovertext_levels_display <- function(dataset, grouping_var) {
   valid_grouping <- is_valid_grouping_var(grouping_var, dataset)
   dataset <- dataset |>
      dplyr::mutate(
         ht_lvls_display = if (valid_grouping) paste0(grouping_var, ": ", .data[[grouping_var]]) else "Ungrouped"
      )
   return(dataset)
}



#' Internal helper function for getting the part of the hovertext which shows the ratio and the number of AEs.
#'
#' @param dataset `[data.frame]` Data frame returned by the get_time_for_plot() function.
#' @param tt_dec_places `[integer(1)]` Number of decimal places to round the value shown in the hovertext.
#'
#' @returns A processed data frame with the additional column "ht_ae_info". The values in this column are later a part
#' of the generated hovertext.
#' @keywords internal
get_hovertext_ae_info <- function(dataset, tt_dec_places) {
   dataset <- dataset |> dplyr::mutate(ht_ae_info = paste0("Rate: ", round(.data[["ratio"]], tt_dec_places),
                                                           "<br>Number of AEs: ", .data[["n_cumsum"]]))
   return(dataset)
}

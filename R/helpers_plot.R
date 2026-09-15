#' Internal helper function for creating the ggplot2 base for the dynamic and interactive plot.
#'
#' @param dm `[data.frame]` The demographics data frame.
#' @param ds `[data.frame]` The disposition data frame returned by the prepare_ds_data() function.
#' @param ae `[data.frame]` The adverse event data frame returned by the prepare_ae_data() function.
#' @param subjid_var `[character(1)]` Character name of the unique subject identifier column in all datasets.
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the dm data frame. The
#' values of this column define the different groups in the plot.
#' @param selected_levels `[character(0+)]` Character vector containing the currently selected grouping levels that
#' should be displayed in the plot.
#' @param metric_selection `[character(1)]` For determining the calculated rate on the y-axis. Either
#' METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.
#' @param time_selection `[character(1)]` Determining if the time on the x-axis should be study dates (calendar based)
#' or study days (subject based). Either TIMETYPE_BUTTONS_OPT1 or TIMETYPE_BUTTONS_OPT2.
#' @param unit_selection `[character(1)]` Determining the binsize when computing the reporting rate per active
#' patients. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.
#' @param tooltip_decimal_places `[integer(1)]` Number of decimal places to round the value shown in the hovertext.
#' @param x_step_size_list `[list]` Named list specifying the tick interval (step size) on the x-Axis depending on the
#' time unit. Composed of:
#' * days `[integer(1)]`
#' Integer value specifying the tick interval in days when the x-axis is displayed in days.
#' * weeks `[integer(1)]`
#' Integer value specifying the tick interval in weeks when the x-axis is displayed in weeks.
#' * months `[integer(1)]`
#' Integer value specifying the tick interval in months when the x-axis is displayed in months.
#' @param color_palette `[named character(1+)]` Named character vector of color values, where the names correspond to
#' the levels of the grouping variable. Always contains the entry "ungrouped" which is fixed to the value "#000000".
#' @param show_ungrouped `[logical]` Specifying if the ungrouped data should be displayed when a grouping variable is
#' selected.
#' @returns A `[ggplot2::ggplot]` object. A static ggplot-based visualization showing calculated rates over time
#' based on the selected grouping variable and levels, metric, and time unit.
#' @keywords internal
create_plot <- function(dm, ds, ae, subjid_var, grouping_var, selected_levels, metric_selection, time_selection,
                        unit_selection, tooltip_decimal_places, x_step_size_list, color_palette, show_ungrouped) {

   df_list <- get_plot_datasets(dm = dm,
                                ae = ae,
                                ds = ds,
                                subjid_var = subjid_var,
                                grouping_var = grouping_var,
                                time_selection = time_selection,
                                metric_selection = metric_selection,
                                unit_selection = unit_selection,
                                tooltip_decimal_places = tooltip_decimal_places,
                                show_ungrouped = show_ungrouped)

   x_axis <- get_x_axis(dataset = df_list$axis_df,
                        metric_selection = metric_selection,
                        time_selection = time_selection,
                        unit_selection = unit_selection,
                        step_size_days = x_step_size_list$days,
                        step_size_weeks = x_step_size_list$weeks,
                        step_size_months = x_step_size_list$months)

   x_scale <- get_x_scale(time_selection = time_selection, x_axis = x_axis)

   # list to fill dynamically depending on what should be drawn. Makes sure, that the geom_point_interactive() are drawn
   # last. This is important for the correct functioning of the hovering
   line_layers <- create_line_layers(df_list = df_list,
                                     grouping_var = grouping_var,
                                     selected_levels = selected_levels,
                                     metric_selection = metric_selection)
   point_layers <- create_point_layers(df_list = df_list,
                                       grouping_var = grouping_var,
                                       selected_levels = selected_levels)

   plt <- ggplot2::ggplot() +
          line_layers +
          point_layers +
          ggplot2::labs(y = metric_selection, x = x_axis$title, colour = get_legend_text(dataset = dm,
                                                                                         grouping_var = grouping_var)) +
          x_scale +
          ggplot2::scale_color_manual(values = color_palette) +
          ggplot2::theme_linedraw(base_size = 9, base_rect_size = 0.5) +
          ggplot2::theme(title = ggplot2::element_text(size = 10),
                        axis.text.x = ggplot2::element_text(size = get_x_axis_text_size(x_axis$breaks)),
                        legend.text = ggplot2::element_text(size = 6),
                        legend.title = ggplot2::element_text(size = 8),
                        legend.key.size = ggplot2::unit(0.3, "cm"),
                        legend.margin = ggplot2::margin(0, 0, 0, 0),
                        legend.position = "inside",
                        legend.position.inside = c(0.998, 0.998),
                        legend.justification = c("right", "top"),
                        aspect.ratio = 0.7,
                        plot.margin = ggplot2::margin(t = 0, r = 0, b = 50, l = 0),
                        text = ggplot2::element_text(family = "mono"))

   return(plt)
}



#' Internal helper function to convert the ggplot object into an interactive girafe object. Applies all interactive
#' settings such as tooltip behaviour, hover effects, selection styling, zoom options, and toolbar configuration.
#'
#' @param ggplot_obj `[ggplot2::ggplot]` The ggplot object generated by create_plot().
#' @param selected_levels `[character(0+)]` Character vector containing the currently selected grouping levels that
#' should be displayed in the plot.
#' @returns A `[ggiraph::girafe]` object. An interactive ggplot-based visualization showing calculated rates over time
#' based on the selected grouping variable and levels, metric, and time unit.
convert_to_girafe <- function(ggplot_obj, selected_levels) {
   #specifying the options
   plt <- ggiraph::girafe(ggobj = ggplot_obj,
                          options = list(
                             ggiraph::opts_tooltip(use_fill = TRUE,
                                                   opacity = 0.85,
                                                   delay_mouseout = 2000,
                                                   delay_mouseover = 0),
                             ggiraph::opts_hover(css = ""),
                             ggiraph::opts_selection(
                                css = "",
                                type = "single"
                             ),
                             ggiraph::opts_selection_inv(
                                css = paste0("opacity:",
                                             get_opacity(length(selected_levels)),
                                             "; filter:saturate(40%);")),
                             ggiraph::opts_zoom(min = 1, max = 10),
                             ggiraph::opts_sizing(rescale = TRUE),
                             ggiraph::opts_toolbar(position = "topright", fixed = TRUE)
                          ))
   return(plt)
}



#' Internal helper function for preparing grouped and/or ungrouped datasets for plotting based on the user selections.
#' Besides that the function also takes care of the selection of the dataset that should be used for generating the x
#' Axis. If the ungrouped dataset is available, it is used for the x Axis, because it contains the entire time span.
#'
#' @inheritParams create_plot
#' @returns A list containing the ungrouped dataset or NULL; the grouped dataset or NULL and the dataset that should be
#' used for axis calculation.
#' @keywords internal
get_plot_datasets <- function(dm, ae, ds, subjid_var, grouping_var, time_selection, metric_selection, unit_selection,
                              tooltip_decimal_places, show_ungrouped) {
   dataset_ungrouped <- NULL
   dataset_grouped <- NULL
   if (grouping_var == REPORT_RATES$CHOICES$GROUP_NO_SELECTION) {
      dataset_ungrouped <- prepare_plot_data(dm = dm,
                                             ae = ae,
                                             ds = ds,
                                             subjid_var = subjid_var,
                                             grouping_var = grouping_var,
                                             time_selection = time_selection,
                                             metric_selection = metric_selection,
                                             unit_selection = unit_selection,
                                             tt_dec_places = tooltip_decimal_places)
      axis_df <- dataset_ungrouped
   } else {
      dataset_grouped <- prepare_plot_data(dm = dm,
                                           ae = ae,
                                           ds = ds,
                                           subjid_var = subjid_var,
                                           grouping_var = grouping_var,
                                           time_selection = time_selection,
                                           metric_selection = metric_selection,
                                           unit_selection = unit_selection,
                                           tt_dec_places = tooltip_decimal_places)
      axis_df <- dataset_grouped
      if (isTRUE(show_ungrouped)) {
         dataset_ungrouped <- prepare_plot_data(dm = dm,
                                                ae = ae,
                                                ds = ds,
                                                subjid_var = subjid_var,
                                                grouping_var = REPORT_RATES$CHOICES$GROUP_NO_SELECTION,
                                                time_selection = time_selection,
                                                metric_selection = metric_selection,
                                                unit_selection = unit_selection,
                                                tt_dec_places = tooltip_decimal_places)
         axis_df <- dataset_ungrouped
      }
   }

   df_list <- list("ungrouped" = dataset_ungrouped, "grouped" = dataset_grouped, "axis_df" = axis_df)
   return(df_list)
}



#' Internal helper function for creating the line layers to be shown in the plot.
#'
#' @param df_list `[list]` Named list returned by get_plot_datasets(). Composed of
#' * ungrouped `[data.frame | NULL]`
#' The data frame containing the ungrouped data.
#' * grouped `[data.frame | NULL]`
#' The data frame containing the grouped data.
#' * axis_df `[data.frame]` If not NULL the ungrouped data frame, else the grouped data frame.
#' @inheritParams create_plot
#'
#' @returns A list containing the layers for the lines to be shown in the plot.
#' @keywords internal
create_line_layers <- function(df_list, grouping_var, selected_levels, metric_selection) {
   layers <- list()
   geom_func <- select_geom(metric_selection)
   dataset_ungrouped <- df_list$ungrouped
   dataset_grouped <- df_list$grouped
   if (!is.null(dataset_ungrouped)) {
      layers <- append(layers, geom_func(data = dataset_ungrouped,
                                          mapping = ggplot2::aes(x = .data[["time_for_plot"]],
                                                                 y = .data[["ratio"]],
                                                                 group = 1,
                                                                 data_id = "ungrouped",
                                                                 color = "ungrouped"),
                                          hover_nearest = FALSE))
   }

   # add the layers for the levels after grouping
   if (!is.null(dataset_grouped)) {
      grouped_data <- dataset_grouped |> dplyr::filter(.data[[grouping_var]] %in% selected_levels)
      layers <- append(layers, geom_func(data = grouped_data,
                                         mapping = ggplot2::aes(x = .data[["time_for_plot"]],
                                                                y = .data[["ratio"]],
                                                                color = .data[[grouping_var]],
                                                                group = .data[[grouping_var]],
                                                                data_id = .data[[grouping_var]]),
                                         hover_nearest = FALSE))
   }
   return(layers)
}



#' Internal helper function for creating the point layers to be shown in the plot
#'
#' @inheritParams create_line_layers
#'
#' @returns A list containing the layers for the points to be shown in the plot
#' @keywords internal
create_point_layers <- function(df_list, grouping_var, selected_levels) {
   layers <- list()
   dataset_ungrouped <- df_list$ungrouped
   dataset_grouped <- df_list$grouped
   if (!is.null(dataset_ungrouped)) {
      layers <- append(layers, get_geom_point(dataset_ungrouped)) # get_geom_point inline screiben?
   }

   if (!is.null(dataset_grouped)) {
      grouped_data <- dataset_grouped |> dplyr::filter(.data[[grouping_var]] %in% selected_levels)
      layers <- append(layers, get_geom_point(grouped_data, grouping_var = grouping_var))
   }
   return(layers)
}



#' Internal helper function for getting the right geom_..._interactive depending on the selected metric.
#'
#' @param metric_selection `[character(1)]` For determining the calculated rate on the y-axis. Either
#' METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2
#'
#' @returns Either a ggiraph::geom_line_interactive or a ggiraph::geom_step_interactive function object.
#' @keywords internal
select_geom <- function(metric_selection) {
   if (metric_selection == REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1) {
      return(ggiraph::geom_line_interactive)
   } else {
      return(ggiraph::geom_step_interactive)
   }
}



#' Internal helper function for getting the breaks and labels for the x axis
#'
#' @param time_selection  `[character(1)]` Determining if the time on the x-axis should be study dates (calendar based)
#' or study days (subject based) Either TIMETYPE_BUTTONS_OPT1 or TIMETYPE_BUTTONS_OPT2.
#' @param x_axis `[list]` A list returned by get_x_axis(). Composed of:
#' * breaks `[numeric(1+)]`
#' A numeric vector setting the values at which ticks on this axis appear. Only contained in the list if the time
#' selection was "By study day".
#' * ticktext `[numeric(1+)]`
#' A character vector setting the text displayed at the ticks position via "breaks". Only contained in the list if the
#' time selection was "By study day".
#' * breaks `[Date(1+)]` Vector of the dates where the ticks should be in the plot. Only contained in the list if the
#' time selection was "By study date".
#' * title `[character(1)]` Not used in this function.
#'
#' @returns a ggplot2 scale object used to configure the x-axis.
#' * `[ggplot2::ScaleContinuousPosition]`when time selection is "By study day" (numeric axis).
#' * `[ggplot2::ScaleDatePosition]` when time_selection is "By study date" (date axis).
#' This object can then be added to a ggplot to define tick positions and labels for the x-axis.
#' @keywords internal
get_x_scale <- function(time_selection, x_axis) {
   if (time_selection == REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2) {
      res <- ggplot2::scale_x_continuous(
         breaks = x_axis$breaks,
         labels = x_axis$ticktext
      )
   } else {
      res <- ggplot2::scale_x_date(breaks = x_axis$breaks)
   }
   return(res)
}



#' Internal helper function for getting the right geom_point_interactive depending on the grouping setting.
#'
#' @param dataset `[data.frame]` A data frame returned by prepare_plot_data() function.
#' @param grouping_var `[character(1) | NULL]` Character name of the grouping variable (column) in the dm data frame.
#' The values of this column define the different groups in the plot.
#' @returns A ggiraph::geom_point_interactive() layer object configured for either grouped or ungrouped data depending
#' on grouping_var.
#' @keywords internal
get_geom_point <- function(dataset, grouping_var = NULL) {
   if (is.null(grouping_var)) { #ungrouped case
      res <- ggiraph::geom_point_interactive(data = dataset,
                                             mapping = ggplot2::aes(x = .data[["time_for_plot"]], y = .data[["ratio"]],
                                                                    data_id = "ungrouped",
                                                                    colour = "ungrouped",
                                                                    tooltip = .data[["hovertext"]]),
                                             hover_nearest = FALSE, alpha = 0.0, size = 0)

   } else {
      res <- ggiraph::geom_point_interactive(data = dataset,
                                             mapping = ggplot2::aes(x = .data[["time_for_plot"]], y = .data[["ratio"]],
                                                                    colour = .data[[grouping_var]],
                                                                    data_id = .data[[grouping_var]],
                                                                    tooltip = .data[["hovertext"]]),
                                             hover_nearest = FALSE, alpha = 0, size = 0)
   }
   return(res)
}



#' Internal helper function for generating a color palette, so that the color of the plots of the different levels stay
#' consistent when selecting or deselecting levels. For <= 9 levels (including the ungrouped data as an own level) the
#' "Okabe-Ito" palette is used. For <= 15 levels (including the ungrouped data as an own level) the EXTENDED palette is
#' used. Those palettes are colorblind-friendly. For > 15 levels  (including the ungrouped data as an own level) the
#' "viridis" palette is used.
#'
#' @param all_levels `[character(0+)]` Vector of *all* the available levels that are in the current grouping variable
#' column in the current demographics dataset. Which levels out of all available levels are selected in the UI dropdown
#' doesn't matter.
#'
#' @returns A named character vector of color values, where the names correspond to the levels of the grouping variable.
#' @keywords internal
generate_palette <- function(all_levels) {
   all_levels <- sort(all_levels) # sort the palette so that the same set of levels always lead to the same palette.
   n_levels <- length(all_levels)  + 1 # +1 because of the ungrouped data

   if (n_levels <= 9) {
      palette <- grDevices::palette.colors(n_levels, palette = "Okabe-Ito", recycle = FALSE)
   } else if (n_levels <= length(REPORT_RATES$PALETTES$EXTENDED)) {
      palette <- rep(REPORT_RATES$PALETTES$EXTENDED, length.out = n_levels)
   } else {
      palette <- viridis::viridis(n = n_levels)
      # need to assign black explicitly for the ungrouped data when using viridis palette.
      # (Okabe-Ito and EXTENDED use #000000 for their first value by default.)
      palette[1] <- "#000000"
   }

   names(palette) <- c("ungrouped", all_levels)
   return(palette)
}



#' Internal helper function for deciding if a new palette should be generated. If there are new levels that weren't
#' already part of palette_stored(). A refresh is triggered when no palette is present, when the new levels appear that
#' were not already part of the current_palette, or when a switch to a smaller, more color-blind-friendly palette
#' becomes possible (from viridis to paletteMartin, or from paletteMartin to okabe-Ito).
#'
#' @param current_palette `[named character(1+) | NULL]`  A named character vector of color values, where the names
#' correspond to the levels of the grouping variable.
#' @param all_levels `[character(0+)]` Vector of *all* the available levels that are in the current grouping variable
#' column in the current demographics dataset.
#'
#' @returns Logical, indicating if a new palette should be generated. If TRUE a new palette will be generated.
#' @keywords internal
palette_refresh_check <- function(current_palette, all_levels) {
   n_current <- if (is.null(current_palette)) 0 else length(current_palette)
   n_new <- length(all_levels) + 1 # + 1 for "ungrouped"

   if (is.null(current_palette)) return(TRUE)
   if (length(setdiff(c("ungrouped", all_levels), names(current_palette)))) return(TRUE) # grouping changed or new lvls
   if (n_current > 15 && n_new <= 15) return(TRUE) # switch from viridis to paletteMartin
   if (n_current > 9 && n_new <= 9) return(TRUE) # switch from paletteMartin to okabeIto

   return(FALSE)
}



#' Internal helper function for preparing the data for the plot depending on the selected inputs by the user (and the
#' rounding accuracy specified by the app creator). It orchestrates the sequence of helper functions that merge,
#' process and enrich the DS, AE and DM datasets into a unified, plot-ready dataframe with computed metrics, time
#' variables and hovertext.
#'
#' @inheritParams get_plot_datasets
#' @param tt_dec_places `[integer(1)]` Number of decimal places to round the value shown in the hovertext.
#'
#' @returns A data frame containing processed data for plotting, including calculated metrics, time variables
#'  and hover text.
#' @keywords internal
prepare_plot_data <- function(dm, ds, ae, subjid_var, grouping_var, time_selection, metric_selection, unit_selection,
                              tt_dec_places) {

   dataset <- merge_event_data_with_grouping(dm_dataset = dm,
                                             ds_prep_dataset = ds,
                                             ae_prep_dataset = ae,
                                             subjid_var = subjid_var,
                                             grouping_var = grouping_var)

   dataset <- compute_plot_metrics(dataset = dataset,
                                   grouping_var = grouping_var,
                                   time_selection = time_selection,
                                   metric_selection = metric_selection,
                                   unit_selection = unit_selection)

   dataset <- get_time_for_plot(dataset = dataset, time_selection = time_selection)

   dataset <- get_hovertext(dataset = dataset,
                            grouping_var = grouping_var,
                            metric_selection = metric_selection,
                            time_selection = time_selection,
                            unit_selection = unit_selection,
                            tt_dec_places = tt_dec_places)
   return(dataset)
}



#' Internal helper function for tackling the problem that there is no study day 0 and still show the plot correctly and
#' without a visual gap. To show the correct plot, the labeling of the x-Axis needs to be adjusted also. This is done by
#' the get_x_axis() and get_x_scale() functions.
#'
#' @param dataset `[data.frame]`  Data frame returned by the compute_plot_metrics() function.
#' @param time_selection `[character(1)]` Determining if the time on the x-axis should be study dates (calendar based)
#' or study days (subject based). Either TIMETYPE_BUTTONS_OPT1 or TIMETYPE_BUTTONS_OPT2.
#'
#' @returns A processed data frame with an additional column "time_for_plot" which fixes the problem that the study day
#' -1 is followed by study day 1 directly (and there is no study day 0). If "By Study date" is selected, this
#' column contains exactly the values out of the column "time". The "time_for_plot" column can now be passed to the
#' x-Parameters of ggplot2:: functions.
#' @keywords internal
get_time_for_plot <- function(dataset, time_selection) {
   if (time_selection == REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1) {
      dataset <- dataset |>
         dplyr::mutate(
            time_for_plot = .data[["time"]]
         )
   } else {
      dataset <- dataset |>
         dplyr::filter(dataset$time != 0) |> # if there was a day 0 through autogeneration (because the real data
         # shouldn't contain study day = 0 anyways)
         dplyr::mutate(
            time_for_plot = ifelse((.data[["time"]] < 0), .data[["time"]], .data[["time"]] - 1)
         )
   }
   return(dataset)
}



#' Internal helper function to get the desired labeling of the x-Axis of the plot.
#'
#' @param dataset `[data.frame]` Data frame returned by the prepare_plot_data() function
#' @param metric_selection `[character(1)]` For determining the calculated rate on the y-axis. Either
#' METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.
#' @param time_selection `[character(1)]` Determining if the time on the x-axis should be study dates (calendar based)
#' or study days (subject based). Either TIMETYPE_BUTTONS_OPT1 or TIMETYPE_BUTTONS_OPT2.
#' @param unit_selection `[character(1)]` Determining the binsize when computing the reporting rate per active
#' patients. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.
#' @param step_size_days `[integer(1)]` Integer value specifying the tick interval in days when the x-axis is
#' displayed in days.
#' @param step_size_weeks `[integer(1)]` Integer value specifying the tick interval in weeks when the x-axis is
#' displayed in weeks.
#' @param step_size_months `[integer(1)]` Integer value specifying the tick interval in months when the x-axis is
#' displayed in months.
#' @returns A list which is passed later to a ggplot2 scale object. Composed of:
#' * breaks `[numeric(1+)]`
#' A numeric vector setting the values at which ticks on this axis appear. Only contained in the list if the time
#' selection was "By study day".
#' * ticktext `[numeric(1+)]`
#' A character vector setting the text displayed at the ticks position via "breaks". Only contained in the list if the
#' time selection was "By study day".
#' * breaks `[Date(1+)]` Vector of the dates where the ticks should be in the plot. Only contained in the list if the
#' time selection was "By study date".
#' * title `[character(1)]` Title that is used on the x-axis.
#' @keywords internal
get_x_axis <- function(dataset, metric_selection, time_selection, unit_selection, step_size_days,
                       step_size_weeks, step_size_months) {
   label <- get_x_axis_label(metric_selection = metric_selection,
                             time_selection = time_selection,
                             unit_selection = unit_selection)

   if (time_selection == REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1) {
      # validate_df_rows() in the server takes only care of empty datasets but not NA in the time
      checkmate::assert_false(all(is.na(dataset$time_for_plot)))

      first_date <- min(dataset$time_for_plot)
      last_date <- max(dataset$time_for_plot)
      start_seq <- lubridate::floor_date(first_date, unit = "6 months")
      end_seq <- lubridate::ceiling_date(last_date, unit = "6 months")
      breaks_seq <- seq(from = start_seq, to = end_seq, by = "6 months")
      res <- list(title = label, breaks = breaks_seq)
      return(res)
   } else {
      tick_vals <- dataset$time_for_plot
      tick_text <- dataset$time

      if (metric_selection == REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1) step_size <- step_size_days
      else if (unit_selection == REPORT_RATES$CHOICES$BINSIZE_BUTTONS_OPT1) step_size <- step_size_weeks
      else step_size <- step_size_months
   }

   indices <- get_indices_for_tickvals(step_size = step_size, tick_text = tick_text)
   tick_vals <- tick_vals[indices]
   tick_text <- tick_text[indices]
   res <- list(title = label, breaks = unique(tick_vals), ticktext = unique(tick_text))
   return(res)
}



#' Internal helper function for getting the desired label of the x-axis, depending on the inputted timetype and metric.
#'
#' @inheritParams get_x_axis
#'
#' @returns The label for the x Axis. Either "Date", "Days", "Weeks" or "Months".
#' @keywords internal
get_x_axis_label <- function(metric_selection, time_selection, unit_selection) {
   if (time_selection == REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT1) return("Date")
   else if (metric_selection == REPORT_RATES$CHOICES$METRIC_BUTTONS_OPT1) return("Days")
   else return(paste0(unit_selection, "s"))
}



#' Internal helper function creating a logical vector for the indexing of the ticks on the x-Axis of the plot. Applying
#' the logic on the tick_text (and not tick_vals) because that's what displayed in the plot in the end.
#'
#' @param step_size `[integer(1)]` Integer value specifying the tick interval on the x axis.
#' @param tick_text `[character(1+)]` A character vector setting the text displayed at the ticks positions.
#'
#' @returns A logical vector specifying the indices at which ticks should be shown in the plot on the x axis.
#' @keywords internal
get_indices_for_tickvals <- function(step_size, tick_text) {
   indices <- which(tick_text %% step_size == 0 | tick_text == 1)
   return(indices)
}



#' Internal helper function for getting a dynamic textsize for the ticks on the x axis. If there is a high amount of
#' ticks (either because a low step size or a big overall time range in the data) those texts can overlap if their size
#' is to big.
#'
#' @param breaks `[numeric(1+)]` (if "By study day" is selected) or `[Date(1+)]` (if "By study date" is selected) vector
#' setting the values at which ticks on this axis appear.
#'
#' @returns `[numeric(1)]` That is passed to the axis.text.x parameter of the ggplot2::theme() function.
#' @keywords internal
get_x_axis_text_size <- function(breaks) {
   # logic may need to be changed if app creators should be able to specify tick intervals also for a x-axis using dates
   # in the future, because dates take up more space than numbers.
   tick_amount <- length(breaks)
   if (tick_amount < 15) return(8)
   if (tick_amount < 25) return(7)
   if (tick_amount < 35) return(6)
   else return(4)
}



#' Internal helper function for getting a dynamic opacity for the not selected lines in the plot. The returned value
#' is used in the ggiraph::opts_selection_inv(). Fixes the problem when theres a high amount of selected levels and when
#' their lines overlap, the opacity of the "overlapped total line" is higher. Thats why the overall opacity needs to be
#' lower in such cases.
#'
#' @param amount_levels `[numeric(1)]` The amount of the current selected levels
#'
#' @returns A numeric for setting the opacity of the not selected lines in the plot based on the amount of selected
#' levels. A higher amount leading to a reduced opacity.
#' @keywords internal
get_opacity <- function(amount_levels) {
   if (amount_levels == 0) return(1)
   else if (amount_levels == 1) return(0.5) # because of ungrouped which is not included in amount_levels
   else return(1 / amount_levels)
}



#' Internal helper function for getting a dynamic description for the levels in the legend of the plot. Returns the
#' label of the currently selected grouping variable if there is one. Else it just returns the grouping variable string.
#'
#' @param dataset `[data.frame]` The demographics dataframe which contains the possible variables for grouping and their
#' labels.
#' @param grouping_var `[character(1)]` Character name of the grouping variable (column) in the dm data frame
#'
#' @returns A string which is either the label of the grouping variable or the grouping variable itself.
#' @keywords internal
get_legend_text <- function(dataset, grouping_var) {
   lbl <- attr(dataset[[grouping_var]], "label")
   if (is.null(lbl) || is.na(lbl) || identical(lbl, "")) return(grouping_var) else return(as.character(lbl))
}



#' Internal helper function for getting column names with their labels for the UI grouping dropdown.
#'
#' @param dataset `[data.frame]` The demographics dataframe which contains the possible variables for grouping and their
#' labels.
#' @param columns `[character(0+)]` Character vector of the columnnames that should be returned with their label.
#'
#' @returns Named character vector where values are the original columnnames and the names are the string to display in
#' the format "columnname (columnlabel)" or only "columnname" if no label exsits.
#' @keywords internal
get_columns_with_labels <- function(dataset, columns) {
   labels <- vapply(columns, function(v) {
      lbl <- attr(dataset[[v]], "label")
      if (is.null(lbl) || is.na(lbl) || identical(lbl, "")) "" else as.character(paste0(v, "  [", lbl, "]"))
   }, FUN.VALUE = character(1))

   names(columns) <- labels
   return(columns)
}

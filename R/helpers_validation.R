#' Internal helper function for checkmate validation and also for filling the lists with the defaults correctly.
#'
#' Without this function there was the following issue: If there were parameters inside a parameter list like
#' "x_step_size_list" and there wasn't an argument for each of the lists elements passed, when calling the
#' mod_report_rates(); it was overwritten with NULL and not with the defaults specified inside the definition of
#' mod_report_rates. For example when mod_report_rates() was called like this mod_report_rates
#' (..., x_step_size_list = list(weeks = 4), ...) the values of x_step_size_list$days and x_step_size_list$months was
#' NULL instead of the desired defaults.
#'
#' @param input_args `[list]` A named list of the arguments, that were passed to mod_report_rates() function.
#' Composed of:
#' * @param module_id `[character(1)]` A unique ID string to create a namespace.
#' * @param dm_dataset_name `[character(1)]` Name of the Demographics dataset.
#' * @param ds_dataset_name `[character(1)]` Name of the Disposition dataset.
#' * @param ae_dataset_name `[character(1)]` Name of the Adverse Event dataset.
#' * @param subjid_var `[character(1)]` Character name of the unique subject identifier column in all datasets
#' (default is USUBJID). Must be a single value.
#' * @param tooltip_decimal_places `[integer(1)]` Number of decimal places to round the value shown in the hovertext.
#' * @param disposition_events `[list]` Named list specifying variables and values used to define entry and exit events
#' in the disposition dataset.
#' Composed of:
#' *  event_var `[character(1)]` Name of the variable that contains the description of the event.
#' *  date_var `[character(1)]` Name of the variable containing the date of the event.
#' *  day_var `[character(1)]` Name of the variable containing the study day of the event.
#' *  entry_vals `[character(1+)]` Character vector of values defining which values out of var should be entry events.
#' *  exit_vals `[character(1+)]` Character vector of values defining which values out of var should be exit events.
#' * @param adverse_events `[list]` Named list specifying the variables that define the date and study day of adverse
#' event occurence. Composed of:
#' *  date_var `[character(1)]` Name of the variable containing the date of the event.
#' *  day_var `[character(1)]` Name of the variable containing the study day of the event.
#' * @param grouping_vars `[list]` Named list specifying grouping options for the plot. Composed of:
#' *  choices `[character(1+) | NULL]` Character vector of variable names out of the demographics dataset, specifying
#' which variables are available for grouping. If provided, only these variables can be selected. If \code{NULL}, all
#' categorical variables from the demographics dataset will be available for grouping.
#' *  default_choice `[character(1) | NULL]` Character specifying the default grouping variable to be used. If
#' \code{NULL}, no variable is selected for grouping by default.
#' * @param x_step_size_list `[list]` Named list specifying the tick interval (step size) on the x-Axis depending on
#' the time unit. Composed of:
#' *  days `[integer(1) | NULL]` Integer value specifying the tick interval in days when the x-axis is displayed in
#' days.
#' *  weeks `[integer(1) | NULL]` Integer value specifying the tick interval in weeks when the x-axis is displayed in
#' weeks.
#' *  months `[integer(1) | NULL]` Integer value specifying the tick interval in months when the x-axis is displayed in
#' months.
#'
#' @returns A named list of all arguments (and their defaults if they were missing beforehand) that can be passed to
#' mod_report_rates().
#' @keywords internal
validate_and_fill <- function(input_args) {
  if (is.null(input_args$x_step_size_list$days)) {
    input_args$x_step_size_list$days <- REPORT_RATES$DEFAULTS$STEPSIZE_DAYS
  }
  if (is.null(input_args$x_step_size_list$weeks)) {
    input_args$x_step_size_list$weeks <- REPORT_RATES$DEFAULTS$STEPSIZE_WEEKS
  }
  if (is.null(input_args$x_step_size_list$months)) {
    input_args$x_step_size_list$months <- REPORT_RATES$DEFAULTS$STEPSIZE_MONTHS
  }

  checkmate::assert(
    checkmate::check_string(input_args$module_id, min.chars = 1),

    checkmate::check_multi_class(input_args$dataset_list, c("reactive", "shinymeta_reactive")),

    checkmate::check_string(input_args$subjid_var, min.chars = 1),

    checkmate::check_integer(input_args$tooltip_decimal_places, lower = 0, len = 1),

    checkmate::check_list(input_args$disposition_events, names = "named", types = "character", any.missing = FALSE),
    checkmate::check_subset(names(input_args$disposition_events),
                            choices = c("event_var", "date_var", "day_var", "entry_vals", "exit_vals")),
    checkmate::check_string(input_args$disposition_events$event_var, min.chars = 1),
    checkmate::check_string(input_args$disposition_events$date_var, min.chars = 1),
    checkmate::check_string(input_args$disposition_events$day_var, min.chars = 1),
    checkmate::check_character(input_args$disposition_events$entry_vals, min.chars = 1),
    checkmate::check_character(input_args$disposition_events$exit_vals, min.chars = 1),
    checkmate::check_true(isTRUE(all(!input_args$disposition_events$entry_vals %in%
                                       input_args$disposition_events$exit_vals))),

    checkmate::check_list(input_args$adverse_events, names = "named", types = "character", any.missing = FALSE),
    checkmate::check_subset(names(input_args$adverse_events), choices = c("date_var", "day_var")),
    checkmate::check_string(input_args$adverse_events$date_var, min.chars = 1),
    checkmate::check_string(input_args$adverse_events$day_var, min.chars = 1),

    checkmate::check_list(input_args$grouping_vars, names = "named", types = c("character", "null")),
    checkmate::check_subset(names(input_args$grouping_vars), choices = c("choices", "default_choice")),
    checkmate::check_character(input_args$grouping_vars$choices, min.chars = 1, null.ok = TRUE),
    checkmate::check_string(input_args$grouping_vars$default_choice, null.ok = TRUE),

    checkmate::check_list(input_args$x_step_size_list, names = "named", types = "integer"),
    checkmate::check_subset(names(input_args$x_step_size_list), choices = c("days", "weeks", "months")),
    checkmate::check_integer(input_args$x_step_size_list$days, lower = 1),
    checkmate::check_integer(input_args$x_step_size_list$weeks, lower = 1),
    checkmate::check_integer(input_args$x_step_size_list$months, lower = 1),

    combine = "and"
  )

  return(input_args)
}




#' Internal helper function to perform checks on the demographics dataset out of the dataset_list() and on the argument
#' that was passed to the grouping_vars parameter of the mod_report_rates() function, in a reactive context. Info
#' messages may be outputted in the console.
#'
#' @param dataset `[data.frame]` The demographics data set.
#' @param subjid_var `[character(1)]` Character name of the unique subject identifier column in all datasets.
#' @param grouping_choices `[character(1+) | NULL]` Character vector (of variable names out of dm), specifying which
#' variables are available for grouping.
#' @param default_choice `[character(1) | NULL]` Character specifying the default grouping variable to be used. The
#' difference to grouping$default_choices is, that the argument that is passed to the default_choice param of this
#' function is a reactiveVal, which contains the default_choice in a reactive context (so either the value of
#' grouping$default_choice when first starting the app (given there was one provided), or NULL after that).
#' This ensures, that no unnecessary info messages will be outputted, because the default_choice is only used when first
#' starting the app.
#' @keywords internal
validate_dm_dataset <- function(dataset, subjid_var, grouping_choices, default_choice) {
  checkmate::assert_data_frame(dataset, min.rows = 0, min.cols = 1)
  checkmate::assert_subset(subjid_var, colnames(dataset))
  if (!is.null(grouping_choices)) {
    existing_choices <- intersect(grouping_choices, colnames(dataset))
    removed_choices <- setdiff(grouping_choices, existing_choices)
    # printing the information which columns out of the grouping_choices were removed into the console. The removal
    # itself happens in the "available_grouping_choices" reactive in the module server.
    if (length(removed_choices) > 0) {
      for (rc in removed_choices) {
        msg <- paste0("The specified choice ", rc, " was removed, because this column is not a subset of the selected demographics dataset column names.") #nolint
        message(msg)
      }
    }

    if (length(existing_choices) > 0) {
      non_factors <- existing_choices[vapply(existing_choices, function(n) !is.factor(dataset[[n]]), logical(1))]
      for (nf in non_factors) {
        msg <- paste0("The specified choice ", nf, " was removed, because this column is not of type factor in the selected demographics dataset.") #nolint
        message(msg)
      }
    }
    if (!is.null(default_choice)) checkmate::assert_subset(default_choice, grouping_choices)
  }

  # printing information if the default_choice is not a column / a factor in the dataset. The correction of the
  # selection itself happens in the observer updating the grouping dropdown (which observes the
  # available_grouping_choices reactive) through the function validate_grouping_selection()
  if (!is.null(default_choice)) {
    default_c_exists <- default_choice %in% colnames(dataset)
    if (!default_c_exists) {
      message(paste0("The default choice ", default_choice, " was ignored because this column is not a subset of the selected demographics dataset column names.")) # nolint
    } else {
      if (!is.factor(dataset[[default_choice]])) {
        message(paste0("The default choice ", default_choice, " was ignored because this column is not of type factor in the selected demographics dataset.")) # nolint
      }
    }
  }
}



#' Internal helper function to perform checkmate validation checks on the disposition dataset out of the dataset_list()
#' in a reactive context.
#'
#' @param dataset `[data.frame]` The disposition data set.
#' @param disposition_events `[list]`  Named list specifying variables and values used to define entry and exit events
#' in the disposition dataset. Composed of:
#' * event_var `[character(1)]`
#' Name of the variable that contains the description of the event.
#' * date_var `[character(1)]`
#' Name of the variable containing the date of the event.
#' * day_var `[character(1)]`
#' Name of the variable containing the study day of the event.
#' * entry_vals `[character(1+)]`
#' Character vector of values defining which values out of event_var should be entry events.
#' * exit_vals `[character(1+)]`
#' Character vector of values defining which values out of event_var should be exit events.
#'
#' @keywords internal
#'
validate_ds_dataset <- function(dataset, disposition_events) {
  event_var <- disposition_events$event_var
  date_var <- disposition_events$date_var
  day_var <- disposition_events$day_var

  checkmate::assert_data_frame(dataset, min.rows = 0, min.cols = 1)
  checkmate::assert_subset(event_var, colnames(dataset))
  checkmate::assert_factor(dataset[[event_var]])

  checkmate::assert_subset(date_var, colnames(dataset))
  checkmate::assert_date(dataset[[date_var]])
  checkmate::assert_subset(day_var, colnames(dataset))
  checkmate::assert_numeric(dataset[[day_var]])
  # entry/exit values are not asserted, since they may vary for studies and missing values shouldn't trigger errors
  # when switching studies
}



#' Internal helper function to perform checkmate validation checks on the adverse event dataset out of the
#' dataset_list() in a reactive context.
#'
#' @param dataset `[data.frame]` The adverse event data set
#' @param adverse_events `[list]` Named list specifying the variables that define the date and study day of
#' adverse event occurrence. Composed of:
#' * date_var `[character(1)]`
#' Name of the variable containing the date of the event.
#' * day_var `[character(1)]`
#' Name of the variable containing the study day of the event.
#'
#' @keywords internal
validate_ae_dataset <- function(dataset, adverse_events) {
  date_var <- adverse_events$date_var
  day_var <- adverse_events$day_var

  checkmate::assert_data_frame(dataset, min.rows = 0, min.cols = 1)
  checkmate::assert_subset(date_var, colnames(dataset))
  checkmate::assert_date(dataset[[date_var]])
  checkmate::assert_subset(day_var, colnames(dataset))
  checkmate::assert_numeric(dataset[[day_var]])
}



#' Internal helper function that checks if at least one of the data frames "dm", "ds" or "ae" contains zero rows OR
#' no levels are selected AND the "show ungrouped" checkbox isn't ticked while a grouping variable is selected.
#' If so, the UI shows a message "No data available", instead of the module proceeding with plot computation and showing
#' an empty coordinate system.
#'
#' @param dataset_list `[list]` A list composed of:
#' * dm `[data.frame]` The dm dataframe.
#' * ds `[data.frame]` The ds dataframe.
#' * ae `[data.frame]` The ae dataframe.
#' @param selected_levels `[character(1+) | NULL]` Character vector of the selected levels in the selected_group or NULL
#' if no levels are selected.
#' @param ungrouped_checkbox `[logical]` Specifying if the ungrouped data should be displayed when a grouping variable
#' is selected.
#' @param selected_group `[character(1)]` Character name of the selected grouping variable (column).
#' @keywords internal
validate_df_rows <- function(dataset_list, selected_levels, ungrouped_checkbox, selected_group) {
  one_df_empty <- (nrow(dataset_list$dm) == 0 ||
                     nrow(dataset_list$ds) == 0 ||
                     nrow(dataset_list$ae) == 0)

  no_levels <- (selected_group != REPORT_RATES$CHOICES$GROUP_NO_SELECTION &&
                  (length(selected_levels) == 0 || is.null(selected_levels)) &&
                  ungrouped_checkbox == FALSE)

  dont_show <- (one_df_empty || no_levels)
  if (dont_show) {
    shiny::validate(shiny::need(!dont_show, "No data available."))
  }
}



#' Internal helper function for checking if a variable is valid for grouping. The variable can't be null, has to
#' appear in the column names of the dataset passed to the function and must be of type factor to be considered as valid
#'
#' @param grouping_var `[character(1) | NULL]` Character name of the variable that should be checked.
#' @param group_dataset `[data.frame]` The data frame in which is looked for the grouping_var.
#'
#' @returns logical value. TRUE if the grouping_var is valid, otherwise FALSE.
#' @keywords internal
is_valid_grouping_var <- function(grouping_var, group_dataset) {
  if (!is.null(grouping_var) && grouping_var %in% names(group_dataset)) {
    checkmate::assert_factor(group_dataset[[grouping_var]])
    return(TRUE)
  }
  return(FALSE)
}



#' Internal helper function for validating the current selected grouping variable.
#'
#' Ensuring that when another demographics dataset gets selected, the grouping variable is reset to "-None-" if the
#'  previous selected grouping variable isn't in the new dm dataset anymore or not of type factor anymore.
#'  An informative message will we printed in the console if the grouping variable gets resetted.
#' @param dm `[data.frame]` The demographics dataframe.
#' @param selected_group `[character(1)]` Name of the current selected grouping variable
#'
#' @returns Either the current selection of the grouping variable or REPORT_RATES$CHOICES$GROUP_NO_SELECTION if one of
#' the tests are FALSE.
#' @keywords internal
validate_grouping_selection <- function(dm, selected_group) {
  if (is.null(selected_group)) return(REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
  if (selected_group != REPORT_RATES$CHOICES$GROUP_NO_SELECTION) {
    is_subset <- checkmate::test_subset(selected_group, colnames(dm))
    is_factor <- checkmate::test_factor(dm[[selected_group]])
    if (is_subset && is_factor) {
      return(selected_group)
    }

    msg <- paste0("Grouping variable ", selected_group, " reset to ", REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
    if (!is_subset) {
      msg <- paste0(msg, " because this column is not a subset of the selected demographics dataset column names.")
    } else {
      msg <- paste0(msg, " because it is not of type factor in the selected demographics dataset.")
    }
    message(msg)
  }
  return(REPORT_RATES$CHOICES$GROUP_NO_SELECTION)
}



#' Internal helper function for validating user-specified axis limits.
#'
#' Checks whether both a minimum and maximum value were provided for an axis
#' and verifies that the minimum is strictly smaller than the maximum.
#' Supports both numeric values and Date objects. Missing values specified
#' as NULL or NA are treated as "not provided" and do not trigger validation
#' errors.
#'
#' @param min_value `[numeric(1) | Date(1) | NULL]`
#' Lower axis limit specified by the user.
#' @param max_value `[numeric(1) | Date(1) | NULL]`
#' Upper axis limit specified by the user.
#' @param axis_name `[character(1)]`
#' Name of the axis used in the validation message (e.g. "X" or "Y").
#'
#' @returns Either NULL if the limits are valid or incomplete, or a character
#' string containing an error message suitable for use with
#' \code{shiny::validate()}.
#'
#' @keywords internal
validate_axis_limits <- function(min_value, max_value, axis_name) {
  # Treat NULL and NA uniformly as "not provided"
  has_min <- !is.null(min_value) && length(min_value) == 1 && !is.na(min_value)
  has_max <- !is.null(max_value) && length(max_value) == 1 && !is.na(max_value)

  if (has_min && has_max) {
    # Comparison is safe now: both are non-NA scalars (numeric or Date)
    if (min_value >= max_value) {
      return(sprintf(
        "%s-axis minimum must be smaller than the %s-axis maximum.",
        axis_name,
        axis_name
      ))
    }
  }

  NULL
}



#' Internal helper function for converting displayed study day values to plot
#' coordinates.
#'
#' In the study-day view, the x-axis labels shown to the user start at Day 1,
#' while the underlying plot coordinates start at 0. This helper converts
#' user-specified study day limits into the corresponding plot coordinates so
#' that axis zooming can be applied correctly.
#'
#' Missing values, NULL values and negative study days are preserved.
#'
#' @param day_labels `[numeric()]`
#' Study day values displayed to the user.
#'
#' @returns A numeric vector containing the corresponding plot coordinates.
#'
#' @keywords internal
study_day_to_plot_coord <- function(day_labels) {
  if (is.null(day_labels)) return(NULL)
  if (length(day_labels) == 0) return(day_labels)
  if (all(is.na(day_labels))) return(day_labels)

  ifelse(is.na(day_labels) | day_labels < 0, day_labels, day_labels - 1)
}



#' Internal helper function for applying user-defined axis limits to a plot.
#'
#' Adds a \code{ggplot2::coord_cartesian()} layer to a plot using the axis
#' limits specified by the user. Supports both one-sided and two-sided limits
#' for the x- and y-axes. Missing values specified as NULL or NA are treated as
#' unrestricted limits.
#'
#' When displaying study days, user-entered x-axis limits are converted from
#' displayed study day labels to the corresponding plot coordinates via
#' \code{study_day_to_plot_coord()}.
#'
#' @param plt `[ggplot]`
#' ggplot object to which the axis limits should be applied.
#' @param x_min `[numeric(1) | Date(1) | NULL]`
#' Lower x-axis limit specified by the user.
#' @param x_max `[numeric(1) | Date(1) | NULL]`
#' Upper x-axis limit specified by the user.
#' @param y_min `[numeric(1) | NULL]`
#' Lower y-axis limit specified by the user.
#' @param y_max `[numeric(1) | NULL]`
#' Upper y-axis limit specified by the user.
#' @param time_selection `[character(1) | NULL]`
#' Current time alignment selection. Used to determine whether x-axis limits
#' should be interpreted as study dates or study days.
#'
#' @returns A ggplot object with the requested axis limits applied via
#' \code{ggplot2::coord_cartesian()}.
#'
#' @keywords internal
apply_axis_limits <- function(plt,
                              x_min = NULL, x_max = NULL,
                              y_min = NULL, y_max = NULL,
                              time_selection = NULL) {

  # Helper to test "is a single non-NA value"
  is_valid_scalar <- function(x) {
    !is.null(x) && length(x) == 1 && !is.na(x)
  }

  # ---- X limits ----
  x_limits <- NULL
  if (!is.null(time_selection)) {
    if (identical(time_selection, REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2)) {
      # By study day: convert displayed labels to plot coordinates
      x_min_c <- study_day_to_plot_coord(x_min)
      x_max_c <- study_day_to_plot_coord(x_max)
    } else {
      # By study date: already Date objects
      x_min_c <- x_min
      x_max_c <- x_max
    }

    x_has_min <- is_valid_scalar(x_min_c)
    x_has_max <- is_valid_scalar(x_max_c)

    if (x_has_min && x_has_max) {
      x_limits <- c(x_min_c, x_max_c)
    } else if (x_has_min) {
      # One-sided: ggplot2 accepts NA for the other end
      x_limits <- c(x_min_c, NA)
    } else if (x_has_max) {
      x_limits <- c(NA, x_max_c)
    }
    # else: no X limits
  }

  # ---- Y limits ----
  y_has_min <- is_valid_scalar(y_min)
  y_has_max <- is_valid_scalar(y_max)

  y_limits <- NULL
  if (y_has_min && y_has_max) {
    y_limits <- c(y_min, y_max)
  } else if (y_has_min) {
    y_limits <- c(y_min, NA)
  } else if (y_has_max) {
    y_limits <- c(NA, y_max)
  }

  plt + ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits)
}



#' Internal helper function for validating user-specified axis limits.
#'
#' Checks whether both a minimum and maximum value were provided for an axis
#' and verifies that the minimum is strictly smaller than the maximum.
#' Supports both numeric values and Date objects. Missing values specified
#' as NULL or NA are treated as "not provided" and do not trigger validation
#' errors.
#'
#' @param min_value `[numeric(1) | Date(1) | NULL]`
#' Lower axis limit specified by the user.
#' @param max_value `[numeric(1) | Date(1) | NULL]`
#' Upper axis limit specified by the user.
#' @param axis_name `[character(1)]`
#' Name of the axis used in the validation message (e.g. "X" or "Y").
#'
#' @returns Either NULL if the limits are valid or incomplete, or a character
#' string containing an error message suitable for use with
#' \code{shiny::validate()}.
#'
#' @keywords internal
validate_axis_limits <- function(min_value, max_value, axis_name) {
  # Treat NULL and NA uniformly as "not provided"
  has_min <- !is.null(min_value) && length(min_value) == 1 && !is.na(min_value)
  has_max <- !is.null(max_value) && length(max_value) == 1 && !is.na(max_value)

  if (has_min && has_max) {
    # Comparison is safe now: both are non-NA scalars (numeric or Date)
    if (min_value >= max_value) {
      return(sprintf(
        "%s-axis minimum must be smaller than the %s-axis maximum.",
        axis_name,
        axis_name
      ))
    }
  }

  NULL
}



#' Internal helper function for converting displayed study day values to plot
#' coordinates.
#'
#' In the study-day view, the x-axis labels shown to the user start at Day 1,
#' while the underlying plot coordinates start at 0. This helper converts
#' user-specified study day limits into the corresponding plot coordinates so
#' that axis zooming can be applied correctly.
#'
#' Missing values, NULL values and negative study days are preserved.
#'
#' @param day_labels `[numeric()]`
#' Study day values displayed to the user.
#'
#' @returns A numeric vector containing the corresponding plot coordinates.
#'
#' @keywords internal
study_day_to_plot_coord <- function(day_labels) {
  if (is.null(day_labels)) return(NULL)
  if (length(day_labels) == 0) return(day_labels)
  if (all(is.na(day_labels))) return(day_labels)

  ifelse(is.na(day_labels) | day_labels < 0, day_labels, day_labels - 1)
}



#' Internal helper function for applying user-defined axis limits to a plot.
#'
#' Adds a \code{ggplot2::coord_cartesian()} layer to a plot using the axis
#' limits specified by the user. Supports both one-sided and two-sided limits
#' for the x- and y-axes. Missing values specified as NULL or NA are treated as
#' unrestricted limits.
#'
#' When displaying study days, user-entered x-axis limits are converted from
#' displayed study day labels to the corresponding plot coordinates via
#' \code{study_day_to_plot_coord()}.
#'
#' @param plt `[ggplot]`
#' ggplot object to which the axis limits should be applied.
#' @param x_min `[numeric(1) | Date(1) | NULL]`
#' Lower x-axis limit specified by the user.
#' @param x_max `[numeric(1) | Date(1) | NULL]`
#' Upper x-axis limit specified by the user.
#' @param y_min `[numeric(1) | NULL]`
#' Lower y-axis limit specified by the user.
#' @param y_max `[numeric(1) | NULL]`
#' Upper y-axis limit specified by the user.
#' @param time_selection `[character(1) | NULL]`
#' Current time alignment selection. Used to determine whether x-axis limits
#' should be interpreted as study dates or study days.
#'
#' @returns A ggplot object with the requested axis limits applied via
#' \code{ggplot2::coord_cartesian()}.
#'
#' @keywords internal
apply_axis_limits <- function(plt,
                              x_min = NULL, x_max = NULL,
                              y_min = NULL, y_max = NULL,
                              time_selection = NULL) {

  # Helper to test "is a single non-NA value"
  is_valid_scalar <- function(x) {
    !is.null(x) && length(x) == 1 && !is.na(x)
  }

  # ---- X limits ----
  x_limits <- NULL
  if (!is.null(time_selection)) {
    if (identical(time_selection, REPORT_RATES$CHOICES$TIMETYPE_BUTTONS_OPT2)) {
      # By study day: convert displayed labels to plot coordinates
      x_min_c <- study_day_to_plot_coord(x_min)
      x_max_c <- study_day_to_plot_coord(x_max)
    } else {
      # By study date: already Date objects
      x_min_c <- x_min
      x_max_c <- x_max
    }

    x_has_min <- is_valid_scalar(x_min_c)
    x_has_max <- is_valid_scalar(x_max_c)

    if (x_has_min && x_has_max) {
      x_limits <- c(x_min_c, x_max_c)
    } else if (x_has_min) {
      # One-sided: ggplot2 accepts NA for the other end
      x_limits <- c(x_min_c, NA)
    } else if (x_has_max) {
      x_limits <- c(NA, x_max_c)
    }
    # else: no X limits
  }

  # ---- Y limits ----
  y_has_min <- is_valid_scalar(y_min)
  y_has_max <- is_valid_scalar(y_max)

  y_limits <- NULL
  if (y_has_min && y_has_max) {
    y_limits <- c(y_min, y_max)
  } else if (y_has_min) {
    y_limits <- c(y_min, NA)
  } else if (y_has_max) {
    y_limits <- c(NA, y_max)
  }

  plt + ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits)
}

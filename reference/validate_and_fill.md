# Internal helper function for checkmate validation and also for filling the lists with the defaults correctly.

Without this function there was the following issue: If there were
parameters inside a parameter list like "x_step_size_list" and there
wasn't an argument for each of the lists elements passed, when calling
the mod_report_rates(); it was overwritten with NULL and not with the
defaults specified inside the definition of mod_report_rates. For
example when mod_report_rates() was called like this mod_report_rates
(..., x_step_size_list = list(weeks = 4), ...) the values of
x_step_size_list\$days and x_step_size_list\$months was NULL instead of
the desired defaults.

## Usage

``` r
validate_and_fill(input_args)
```

## Arguments

- input_args:

  `[list]`

  A named list of the arguments, that were passed to mod_report_rates()
  function. Composed of:

  - @param module_id `[character(1)]`

    A unique ID string to create a namespace.

  - @param dm_dataset_name `[character(1)]`

    Name of the Demographics dataset.

  - @param ds_dataset_name `[character(1)]`

    Name of the Disposition dataset.

  - @param ae_dataset_name `[character(1)]`

    Name of the Adverse Event dataset.

  - @param subjid_var `[character(1)]`

    Character name of the unique subject identifier column in all
    datasets (default is USUBJID). Must be a single value.

  - @param tooltip_decimal_places `[integer(1)]`

    Number of decimal places to round the value shown in the hovertext.

  - @param disposition_events `[list]`

    Named list specifying variables and values used to define entry and
    exit events in the disposition dataset. Composed of:

    - event_var `[character(1)]`

      Name of the variable that contains the description of the event.

    - date_var `[character(1)]`

      Name of the variable containing the date of the event.

    - day_var `[character(1)]`

      Name of the variable containing the study day of the event.

    - entry_vals `[character(1+)]`

      Character vector of values defining which values out of var should
      be entry events.

    - exit_vals `[character(1+)]`

      Character vector of values defining which values out of var should
      be exit events.

  - @param adverse_events `[list]`

    Named list specifying the variables that define the date and study
    day of adverse event occurence. Composed of:

    - date_var `[character(1)]`

      Name of the variable containing the date of the event.

    - day_var `[character(1)]`

      Name of the variable containing the study day of the event.

  - @param grouping_vars `[list]`

    Named list specifying grouping options for the plot. Composed of:

    - choices `[character(1+) | NULL]`

      Character vector of variable names out of the demographics
      dataset, specifying which variables are available for grouping. If
      provided, only these variables can be selected. If `NULL`, all
      categorical variables from the demographics dataset will be
      available for grouping.

    - default_choice `[character(1) | NULL]`

      Character specifying the default grouping variable to be used. If
      `NULL`, no variable is selected for grouping by default.

  - @param x_step_size_list `[list]`

    Named list specifying the tick interval (step size) on the x-Axis
    depending on the time unit. Composed of:

    - days `[integer(1) | NULL]`

      Integer value specifying the tick interval in days when the x-axis
      is displayed in days.

    - weeks `[integer(1) | NULL]`

      Integer value specifying the tick interval in weeks when the
      x-axis is displayed in weeks.

    - months `[integer(1) | NULL]`

      Integer value specifying the tick interval in months when the
      x-axis is displayed in months.

## Value

A named list of all arguments (and their defaults if they were missing
beforehand) that can be passed to mod_report_rates().

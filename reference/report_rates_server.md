# Create server for dv.reportrate module

Create server for dv.reportrate module

## Usage

``` r
report_rates_server(
  module_id,
  dataset_list,
  dm_dataset_name,
  ds_dataset_name,
  ae_dataset_name,
  subjid_var = REPORT_RATES$DEFAULTS$SUBJID,
  tooltip_decimal_places = REPORT_RATES$DEFAULTS$TT_DECIMAL_PLACES,
  disposition_events,
  adverse_events,
  grouping_vars = list(choices = NULL, default_choice = NULL),
  x_step_size_list = list(days = REPORT_RATES$DEFAULTS$STEPSIZE_DAYS, weeks =
    REPORT_RATES$DEFAULTS$STEPSIZE_WEEKS, months = REPORT_RATES$DEFAULTS$STEPSIZE_MONTHS)
)
```

## Arguments

- module_id:

  `[character(1)]`

  A unique ID string to create a namespace. Must match the ID provided
  to [`report_rates_ui()`](report_rates_ui.md)

- dataset_list:

  `[shiny::reactive(list(data.frame))]`

  A reactive list of named datasets. Usually obtained from module
  manager.

- dm_dataset_name:

  `[character(1)]`

  Name of the demographics dataset in the module manager.

- ds_dataset_name:

  `[character(1)]`

  Name of the disposition dataset in the module manager.

- ae_dataset_name:

  `[character(1)]`

  Name of the adverse-event dataset in the module manager.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all datasets
  (default is USUBJID). Must be a single value.

- tooltip_decimal_places:

  `[integer(1)]`

  Number of decimal places to round the value shown in the hovertext.
  Defaults to 3L.

- disposition_events:

  `[list]`

  Named list specifying variables and values used to define entry and
  exit events in the disposition dataset. Composed of:

  - event_var `[character(1)]`

    Name of the variable that contains the description of the event.
    Defaults to "DSDECOD".

  - date_var `[character(1)]`

    Name of the variable containing the date of the event. Defaults to
    "DSSTDTC".

  - day_var `[character(1)]`

    Name of the variable containing the study day of the event. Defaults
    to "DSSTDY".

  - entry_vals `[character(1+)]`

    Character vector of values defining which values out of var should
    be entry events. Defaults to c("RANDOMIZED").

  - exit_vals `[character(1+)]`

    Character vector of values defining which values out of var should
    be exit events. Defaults to c("COMPLETED", "WITHDRAWAL BY SUBJECT",
    "DEATH").

- adverse_events:

  `[list]`

  Named list specifying the variables that define the date and study day
  of adverse event occurrence. Composed of:

  - date_var `[character(1)]`

    Name of the variable containing the date of the event. Defaults to
    "AESTDTC".

  - day_var `[character(1)]`

    Name of the variable containing the study day of the event. Defaults
    to "AESTDY".

- grouping_vars:

  `[list]`

  Named list specifying grouping options for the plot. Composed of:

  - choices `[character(1+) | NULL]`

    Character vector of variable names out of the demographics dataset,
    specifying which variables are available for grouping. If provided,
    only these variables can be selected. Defaults to `NULL`, meaning
    all factor variables from the demographics dataset will be available
    for grouping.

  - default_choice `[character(1) | NULL]`

    Character specifying the default grouping variable to be used when
    starting the app. Defaults to `NULL`, meaning no variable is
    selected for grouping by default.

- x_step_size_list:

  `[list]`

  Controls the spacing of tick marks on the plot’s x-axis. This affects
  only the appearance of the axis it does not change the time intervals
  used to calculate rates, the bin size, or the visible x-axis range.
  Composed of:

  - days `[integer(1) | NULL]`

    Tick-mark interval, in study days, when **Time alignment** is *By
    study day*. Defaults to `50L`.

  - weeks `[integer(1) | NULL]`

    Tick-mark interval, in weeks, when **Time alignment** is *By study
    day* and **Bin size** is *Week*. Defaults to `2L`.

  - months `[integer(1) | NULL]`

    Tick-mark interval, in months, when **Time alignment** is *By study
    day* and **Bin size** is *Month*. Defaults to `1L`.

# Internal helper function for creating the ggplot2 base for the dynamic and interactive plot.

Internal helper function for creating the ggplot2 base for the dynamic
and interactive plot.

## Usage

``` r
create_plot(
  dm,
  ds,
  ae,
  subjid_var,
  grouping_var,
  selected_levels,
  metric_selection,
  time_selection,
  unit_selection,
  tooltip_decimal_places,
  x_step_size_list,
  color_palette,
  show_ungrouped
)
```

## Arguments

- dm:

  `[data.frame]`

  The demographics data frame.

- ds:

  `[data.frame]`

  The disposition data frame returned by the prepare_ds_data() function.

- ae:

  `[data.frame]`

  The adverse event data frame returned by the prepare_ae_data()
  function.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all
  datasets.

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the dm data frame.
  The values of this column define the different groups in the plot.

- selected_levels:

  `[character(0+)]`

  Character vector containing the currently selected grouping levels
  that should be displayed in the plot.

- metric_selection:

  `[character(1)]`

  For determining the calculated rate on the y-axis. Either
  METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.

- time_selection:

  `[character(1)]`

  Determining if the time on the x-axis should be study dates (calendar
  based) or study days (subject based). Either TIMETYPE_BUTTONS_OPT1 or
  TIMETYPE_BUTTONS_OPT2.

- unit_selection:

  `[character(1)]`

  Determining the binsize when computing the reporting rate per active
  patients. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.

- tooltip_decimal_places:

  `[integer(1)]`

  Number of decimal places to round the value shown in the hovertext.

- x_step_size_list:

  `[list]`

  Named list specifying the tick interval (step size) on the x-Axis
  depending on the time unit. Composed of:

  - days `[integer(1)]`

    Integer value specifying the tick interval in days when the x-axis
    is displayed in days.

  - weeks `[integer(1)]`

    Integer value specifying the tick interval in weeks when the x-axis
    is displayed in weeks.

  - months `[integer(1)]`

    Integer value specifying the tick interval in months when the x-axis
    is displayed in months.

- color_palette:

  `[named character(1+)]`

  Named character vector of color values, where the names correspond to
  the levels of the grouping variable. Always contains the entry
  "ungrouped" which is fixed to the value "#000000".

- show_ungrouped:

  `[logical]`

  Specifying if the ungrouped data should be displayed when a grouping
  variable is selected.

## Value

A `[ggplot2::ggplot]` object. A static ggplot-based visualization
showing calculated rates over time based on the selected grouping
variable and levels, metric, and time unit.

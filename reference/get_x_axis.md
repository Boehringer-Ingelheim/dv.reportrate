# Internal helper function to get the desired labeling of the x-Axis of the plot.

Internal helper function to get the desired labeling of the x-Axis of
the plot.

## Usage

``` r
get_x_axis(
  dataset,
  metric_selection,
  time_selection,
  unit_selection,
  step_size_days,
  step_size_weeks,
  step_size_months
)
```

## Arguments

- dataset:

  `[data.frame]`

  Data frame returned by the prepare_plot_data() function

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

- step_size_days:

  `[integer(1)]`

  Integer value specifying the tick interval in days when the x-axis is
  displayed in days.

- step_size_weeks:

  `[integer(1)]`

  Integer value specifying the tick interval in weeks when the x-axis is
  displayed in weeks.

- step_size_months:

  `[integer(1)]`

  Integer value specifying the tick interval in months when the x-axis
  is displayed in months.

## Value

A list which is passed later to a ggplot2 scale object. Composed of:

- breaks `[numeric(1+)]`

  A numeric vector setting the values at which ticks on this axis
  appear. Only contained in the list if the time selection was "By study
  day".

- ticktext `[numeric(1+)]` A character vector setting the text displayed
  at the ticks position via "breaks". Only contained in the list if the
  time selection was "By study day".

- breaks `[Date(1+)]` Vector of the dates where the ticks should be in
  the plot. Only contained in the list if the time selection was "By
  study date".

- title `[character(1)]` Title that is used on the x-axis.

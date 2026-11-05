# Internal helper function for getting the desired label of the x-axis, depending on the inputted timetype and metric.

Internal helper function for getting the desired label of the x-axis,
depending on the inputted timetype and metric.

## Usage

``` r
get_x_axis_label(metric_selection, time_selection, unit_selection)
```

## Arguments

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

## Value

The label for the x Axis. Either "Date", "Days", "Weeks" or "Months".

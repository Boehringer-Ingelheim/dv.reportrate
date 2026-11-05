# Internal helper function for getting the part of the hovertext that contains the time information.

Internal helper function for getting the part of the hovertext that
contains the time information.

## Usage

``` r
get_hovertext_time_base(
  dataset,
  grouping_var,
  metric_selection,
  time_selection,
  unit_selection
)
```

## Arguments

- dataset:

  `[data.frame]`

  Data frame returned by the get_time_for_plot() function.

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the demographics
  data frame. The values of this column define the different groups in
  the plot.

- metric_selection:

  `[character(1)]`

  For determining the calculated rate on the y-axis. Either
  METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.

- time_selection:

  `[character(1)]`

  Determining if the time on the x-axis should be study dates (calendar
  based) or study days (subject based) Either TIMETYPE_BUTTONS_OPT1 or
  TIMETYPE_BUTTONS_OPT2.

- unit_selection:

  `[character(1)]`

  Determining the binsize when computing the reporting rate per active
  patients. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.

## Value

A processed data frame with the additional columns "interval_end",
"ht_time_base" and also "interval_start" if the time_selection was "by
study date". The values in these columns are later a part of the
generated hovertext.

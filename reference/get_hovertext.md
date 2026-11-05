# Internal helper function to create the dynamic hovertext that is displayed when hovering over the interactive plot.

Internal helper function to create the dynamic hovertext that is
displayed when hovering over the interactive plot.

## Usage

``` r
get_hovertext(
  dataset,
  grouping_var,
  metric_selection,
  time_selection,
  unit_selection,
  tt_dec_places
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

- tt_dec_places:

  `[integer(1)]`

  Number of decimal places to round the value shown in the hovertext.

## Value

A processed data frame which now contains an additional column
"hovertext".

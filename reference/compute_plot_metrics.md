# Internal helper function that calls the right functions for metric calculation depending on the inputted parameters by the user. Either rates_per_active_patient() with additional helper functions or cum_rate_per_total_exp_time().

Internal helper function that calls the right functions for metric
calculation depending on the inputted parameters by the user. Either
rates_per_active_patient() with additional helper functions or
cum_rate_per_total_exp_time().

## Usage

``` r
compute_plot_metrics(
  dataset,
  grouping_var,
  time_selection,
  metric_selection,
  unit_selection
)
```

## Arguments

- dataset:

  `[data.frame]`

  Dataframe containing the unique identifier column; date-column,
  day-column; n-column (Adverse Events); m-column(study entries/exits);
  and the group-column if selected. This dataframe was returned by
  merge_event_data_with_grouping().

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the "dm_dataset".
  The values of this column define the different groups in the plot.

- time_selection:

  `[character(1)]`

  Determining if the time on the x-axis should be study dates (calendar
  based) or study days (subject based) Either TIMETYPE_BUTTONS_OPT1 or
  TIMETYPE_BUTTONS_OPT2.

- metric_selection:

  `[character(1)]`

  for determining the calculated rate on the y-axis. Either
  METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.

- unit_selection:

  `[character(1)]`

  Determining the binsize when computing the reporting rate per active
  patients. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.

## Value

A processed dataframe which contains the calculated column "ratio" which
will be the y-Value used for the plot. Independently of the selected
time alignment ("By study date" or "By study day"), the returned
dataframe always includes a unified time column, which holds either
dates or study days. This ensures a consistent downstream workflow

# Internal helper function for preprocessing the data if cumulative rate per total exposure time is the selected metric

Internal helper function for preprocessing the data if cumulative rate
per total exposure time is the selected metric

## Usage

``` r
cum_rate_per_total_exp_time(time_type, group_dataset, grouping_var)
```

## Arguments

- time_type:

  `[character(1)]`

  Either "date" (calendar based) or "day" (subject based). Reflects the
  x-axis of the plots.

- group_dataset:

  `[data.frame]`

  Containing the unique identifier column; date-column, day-column;
  n-column (Adverse Events); m-column(study entries/exits); and the
  group-column if selected in the UI. This data frame was returned by
  merge_event_data_with_grouping().

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the "dm_dataset".
  The values of this column define the different groups in the plot.

## Value

A data frame that now contains the column "ratio", which is used as the
y-Value in the plot. The rows of the data frame each represent a date /
study day (for their respective group if selected) in the plot later.
Independently of the selected time alignment ("By study date" or "By
study day"), the returned dataframe always includes a unified time
column, which holds either dates or study days. This ensures a
consistent downstream workflow

# Internal helper function for filling in all the missing intervals if "reporting rate per active patients" is selected as the metric.

This is necessary to correctly show the value zero of the intervals
where there was no adverse event or disposition event, and because of
that no row was generated in the first place. Without this function the
value of the last interval would get used falsely for the current
interval where the value is = 0 in reality. It also enables hovering for
the missing intervals.

## Usage

``` r
fill_missing_intervals(dataset, grouping_var, time_type, time_unit)
```

## Arguments

- dataset:

  `[data.frame]`

  Data frame returned by rates_per_active_patients() function.

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the "dm_dataset".
  The values of this column define the different groups in the plot.

- time_type:

  `[character(1)]`

  Determining if the time on the x-axis should be study dates (calendar
  based) or study days (subject based). Either "date" or "day".

- time_unit:

  `[character(1)]`

  Determining the binsize. Either "week" or "month".

## Value

A data frame which contains now one row per interval and group.

# Internal helper function for tackling the problem that there is no study day 0 and still show the plot correctly and without a visual gap. To show the correct plot, the labeling of the x-Axis needs to be adjusted also. This is done by the get_x_axis() and get_x_scale() functions.

Internal helper function for tackling the problem that there is no study
day 0 and still show the plot correctly and without a visual gap. To
show the correct plot, the labeling of the x-Axis needs to be adjusted
also. This is done by the get_x_axis() and get_x_scale() functions.

## Usage

``` r
get_time_for_plot(dataset, time_selection)
```

## Arguments

- dataset:

  `[data.frame]`

  Data frame returned by the compute_plot_metrics() function.

- time_selection:

  `[character(1)]`

  Determining if the time on the x-axis should be study dates (calendar
  based) or study days (subject based). Either TIMETYPE_BUTTONS_OPT1 or
  TIMETYPE_BUTTONS_OPT2.

## Value

A processed data frame with an additional column "time_for_plot" which
fixes the problem that the study day -1 is followed by study day 1
directly (and there is no study day 0). If "By Study date" is selected,
this column contains exactly the values out of the column "time". The
"time_for_plot" column can now be passed to the x-Parameters of
ggplot2:: functions.

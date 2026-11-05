# Internal helper function for converting displayed study day values to plot coordinates.

In the study-day view, the x-axis labels shown to the user start at Day
1, while the underlying plot coordinates start at 0. This helper
converts user-specified study day limits into the corresponding plot
coordinates so that axis zooming can be applied correctly.

## Usage

``` r
study_day_to_plot_coord(day_labels)
```

## Arguments

- day_labels:

  `[numeric()]`

  Study day values displayed to the user.

## Value

A numeric vector containing the corresponding plot coordinates.

## Details

Missing values, NULL values and negative study days are preserved.

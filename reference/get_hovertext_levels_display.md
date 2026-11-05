# Internal helper function for getting the part of the hovertext that contains the levels information.

Internal helper function for getting the part of the hovertext that
contains the levels information.

## Usage

``` r
get_hovertext_levels_display(dataset, grouping_var)
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

## Value

A processed data frame with the additional column "ht_levels_display".
The values in this column are later a part of the generated hovertext.

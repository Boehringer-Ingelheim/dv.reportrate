# Internal helper function for getting the right geom_point_interactive depending on the grouping setting.

Internal helper function for getting the right geom_point_interactive
depending on the grouping setting.

## Usage

``` r
get_geom_point(dataset, grouping_var = NULL)
```

## Arguments

- dataset:

  `[data.frame]`

  A data frame returned by prepare_plot_data() function.

- grouping_var:

  `[character(1) | NULL]`

  Character name of the grouping variable (column) in the dm data frame.
  The values of this column define the different groups in the plot.

## Value

A ggiraph::geom_point_interactive() layer object configured for either
grouped or ungrouped data depending on grouping_var.

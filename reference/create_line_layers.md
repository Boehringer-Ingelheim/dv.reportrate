# Internal helper function for creating the line layers to be shown in the plot.

Internal helper function for creating the line layers to be shown in the
plot.

## Usage

``` r
create_line_layers(df_list, grouping_var, selected_levels, metric_selection)
```

## Arguments

- df_list:

  `[list]`

  Named list returned by get_plot_datasets(). Composed of

  - ungrouped `[data.frame | NULL]`

    The data frame containing the ungrouped data.

  - grouped `[data.frame | NULL]`

    The data frame containing the grouped data.

  - axis_df `[data.frame]`

    If not NULL the ungrouped data frame, else the grouped data frame.

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the dm data frame.
  The values of this column define the different groups in the plot.

- selected_levels:

  `[character(0+)]`

  Character vector containing the currently selected grouping levels
  that should be displayed in the plot.

- metric_selection:

  `[character(1)]`

  For determining the calculated rate on the y-axis. Either
  METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.

## Value

A list containing the layers for the lines to be shown in the plot.

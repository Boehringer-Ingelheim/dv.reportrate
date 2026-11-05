# Internal helper function for getting a dynamic description for the levels in the legend of the plot. Returns the label of the currently selected grouping variable if there is one. Else it just returns the grouping variable string.

Internal helper function for getting a dynamic description for the
levels in the legend of the plot. Returns the label of the currently
selected grouping variable if there is one. Else it just returns the
grouping variable string.

## Usage

``` r
get_legend_text(dataset, grouping_var)
```

## Arguments

- dataset:

  `[data.frame]`

  The demographics dataframe which contains the possible variables for
  grouping and their labels.

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the dm data frame

## Value

A string which is either the label of the grouping variable or the
grouping variable itself.

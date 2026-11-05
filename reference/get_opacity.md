# Internal helper function for getting a dynamic opacity for the not selected lines in the plot. The returned value is used in the ggiraph::opts_selection_inv(). Fixes the problem when theres a high amount of selected levels and when their lines overlap, the opacity of the "overlapped total line" is higher. Thats why the overall opacity needs to be lower in such cases.

Internal helper function for getting a dynamic opacity for the not
selected lines in the plot. The returned value is used in the
ggiraph::opts_selection_inv(). Fixes the problem when theres a high
amount of selected levels and when their lines overlap, the opacity of
the "overlapped total line" is higher. Thats why the overall opacity
needs to be lower in such cases.

## Usage

``` r
get_opacity(amount_levels)
```

## Arguments

- amount_levels:

  `[numeric(1)]`

  The amount of the current selected levels

## Value

A numeric for setting the opacity of the not selected lines in the plot
based on the amount of selected levels. A higher amount leading to a
reduced opacity.

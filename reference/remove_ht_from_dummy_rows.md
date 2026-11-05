# Internal helper function for removing the ht_time_base from the dummy rows that were only created to show the last time interval correctly in the first place, and exchange it with "END" instead.

Internal helper function for removing the ht_time_base from the dummy
rows that were only created to show the last time interval correctly in
the first place, and exchange it with "END" instead.

## Usage

``` r
remove_ht_from_dummy_rows(dataset, grouping_var)
```

## Arguments

- dataset:

  `[data.frame]` Data frame that was passed to the get_hovertext_time
  function and processed by the function.

- grouping_var:

  `[character(1)]` Character name of the grouping variable (column) in
  the demographics data frame. The values of this column define the
  different groups in the plot.

## Value

A processed data frame without the ht_time_base and instead "END" in the
dummy rows.

# Internal helper function for validating the current selected grouping variable.

Ensuring that when another demographics dataset gets selected, the
grouping variable is reset to "-None-" if the previous selected grouping
variable isn't in the new dm dataset anymore or not of type factor
anymore. An informative message will we printed in the console if the
grouping variable gets resetted.

## Usage

``` r
validate_grouping_selection(dm, selected_group)
```

## Arguments

- dm:

  `[data.frame]`

  The demographics dataframe.

- selected_group:

  `[character(1)]`

  Name of the current selected grouping variable

## Value

Either the current selection of the grouping variable or
REPORT_RATES\$CHOICES\$GROUP_NO_SELECTION if one of the tests are FALSE.

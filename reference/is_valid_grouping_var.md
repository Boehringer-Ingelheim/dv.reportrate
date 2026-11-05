# Internal helper function for checking if a variable is valid for grouping. The variable can't be null, has to appear in the column names of the dataset passed to the function and must be of type factor to be considered as valid

Internal helper function for checking if a variable is valid for
grouping. The variable can't be null, has to appear in the column names
of the dataset passed to the function and must be of type factor to be
considered as valid

## Usage

``` r
is_valid_grouping_var(grouping_var, group_dataset)
```

## Arguments

- grouping_var:

  `[character(1) | NULL]`

  Character name of the variable that should be checked.

- group_dataset:

  `[data.frame]`

  The data frame in which is looked for the grouping_var.

## Value

logical value. TRUE if the grouping_var is valid, otherwise FALSE.

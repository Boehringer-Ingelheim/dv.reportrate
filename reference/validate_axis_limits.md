# Internal helper function for validating user-specified axis limits.

Checks whether both a minimum and maximum value were provided for an
axis and verifies that the minimum is strictly smaller than the maximum.
Supports both numeric values and Date objects. Missing values specified
as NULL or NA are treated as "not provided" and do not trigger
validation errors.

## Usage

``` r
validate_axis_limits(min_value, max_value, axis_name)
```

## Arguments

- min_value:

  `[numeric(1) | Date(1) | NULL]`

  Lower axis limit specified by the user.

- max_value:

  `[numeric(1) | Date(1) | NULL]`

  Upper axis limit specified by the user.

- axis_name:

  `[character(1)]`

  Name of the axis used in the validation message (e.g. "X" or "Y").

## Value

Either NULL if the limits are valid or incomplete, or a character string
containing an error message suitable for use with
[`shiny::validate()`](https://rdrr.io/pkg/shiny/man/validate.html).

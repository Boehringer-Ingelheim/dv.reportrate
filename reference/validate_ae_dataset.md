# Internal helper function to perform checkmate validation checks on the adverse event dataset out of the dataset_list() in a reactive context.

Internal helper function to perform checkmate validation checks on the
adverse event dataset out of the dataset_list() in a reactive context.

## Usage

``` r
validate_ae_dataset(dataset, adverse_events)
```

## Arguments

- dataset:

  `[data.frame]`

  The adverse event data set

- adverse_events:

  `[list]`

  Named list specifying the variables that define the date and study day
  of adverse event occurrence. Composed of:

  - date_var `[character(1)]`

    Name of the variable containing the date of the event.

  - day_var `[character(1)]`

    Name of the variable containing the study day of the event.

# Internal helper function to perform checkmate validation checks on the disposition dataset out of the dataset_list() in a reactive context.

Internal helper function to perform checkmate validation checks on the
disposition dataset out of the dataset_list() in a reactive context.

## Usage

``` r
validate_ds_dataset(dataset, disposition_events)
```

## Arguments

- dataset:

  `[data.frame]`

  The disposition data set.

- disposition_events:

  `[list]`

  Named list specifying variables and values used to define entry and
  exit events in the disposition dataset. Composed of:

  - event_var `[character(1)]`

    Name of the variable that contains the description of the event.

  - date_var `[character(1)]`

    Name of the variable containing the date of the event.

  - day_var `[character(1)]`

    Name of the variable containing the study day of the event.

  - entry_vals `[character(1+)]`

    Character vector of values defining which values out of event_var
    should be entry events.

  - exit_vals `[character(1+)]`

    Character vector of values defining which values out of event_var
    should be exit events.

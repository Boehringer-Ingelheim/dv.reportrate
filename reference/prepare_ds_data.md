# Internal helper function for preparing the ds dataframe by adding a column that indicates study entry or exit

Internal helper function for preparing the ds dataframe by adding a
column that indicates study entry or exit

## Usage

``` r
prepare_ds_data(
  dataset,
  subjid_var,
  event_var,
  entry_terms,
  exit_terms,
  date_var,
  day_var
)
```

## Arguments

- dataset:

  `[data.frame]`

  The disposition dataframe.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all
  datasets.

- event_var:

  `[character(1)]`

  Name of the variable that contains the description of the event.

- entry_terms:

  `[character(1+)]`

  Defining which values out of event_var should be entry events.

- exit_terms:

  `[character(1+)]`

  Defining which values out of event_var should be exit events.

- date_var:

  `[character(1)]`

  Name of the variable containing the date of the event.

- day_var:

  `[character(1)]`

  Name of the variable containing the study day of the event.

## Value

A dataframe that contains an additional column "m" which indicates study
entry (m = 1) or exit (m = -1). These markers are used in downstream
calculations to derive the number of active subjects in the study at any
time point, and the cumulative exposure time. The column "n" with the
only value 0 indicates that these rows don't describe Adverse Events.

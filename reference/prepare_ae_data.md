# Internal helper function for preparing the ae dataframe by adding a column that indicates the occurrence of adverse events.

Internal helper function for preparing the ae dataframe by adding a
column that indicates the occurrence of adverse events.

## Usage

``` r
prepare_ae_data(dataset, subjid_var, date_var, day_var)
```

## Arguments

- dataset:

  `[data.frame]`

  The Adverse Event dataframe.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all
  datasets.

- date_var:

  `[character(1)]`

  Name of the variable containing the date of the event.

- day_var:

  `[character(1)]`

  Name of the variable containing the study day of the event.

## Value

A dataframe that contains an additional column "n" with the value 1 in
every row, which indicates the occurrence of an adverse event. The
column "m" with the only value 0 indicates that these rows don't
describe Entry or Exit Events.

# Internal helper function for preparing grouped and/or ungrouped datasets for plotting based on the user selections. Besides that the function also takes care of the selection of the dataset that should be used for generating the x Axis. If the ungrouped dataset is available, it is used for the x Axis, because it contains the entire time span.

Internal helper function for preparing grouped and/or ungrouped datasets
for plotting based on the user selections. Besides that the function
also takes care of the selection of the dataset that should be used for
generating the x Axis. If the ungrouped dataset is available, it is used
for the x Axis, because it contains the entire time span.

## Usage

``` r
get_plot_datasets(
  dm,
  ae,
  ds,
  subjid_var,
  grouping_var,
  time_selection,
  metric_selection,
  unit_selection,
  tooltip_decimal_places,
  show_ungrouped
)
```

## Arguments

- dm:

  `[data.frame]`

  The demographics data frame.

- ae:

  `[data.frame]`

  The adverse event data frame returned by the prepare_ae_data()
  function.

- ds:

  `[data.frame]`

  The disposition data frame returned by the prepare_ds_data() function.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all
  datasets.

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the dm data frame.
  The values of this column define the different groups in the plot.

- time_selection:

  `[character(1)]`

  Determining if the time on the x-axis should be study dates (calendar
  based) or study days (subject based). Either TIMETYPE_BUTTONS_OPT1 or
  TIMETYPE_BUTTONS_OPT2.

- metric_selection:

  `[character(1)]`

  For determining the calculated rate on the y-axis. Either
  METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2.

- unit_selection:

  `[character(1)]`

  Determining the binsize when computing the reporting rate per active
  patients. Either BINSIZE_BUTTONS_OPT1 or BINSIZE_BUTTONS_OPT2.

- tooltip_decimal_places:

  `[integer(1)]`

  Number of decimal places to round the value shown in the hovertext.

- show_ungrouped:

  `[logical]`

  Specifying if the ungrouped data should be displayed when a grouping
  variable is selected.

## Value

A list containing the ungrouped dataset or NULL; the grouped dataset or
NULL and the dataset that should be used for axis calculation.

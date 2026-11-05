# Internal helper function for preparing the data for the plot depending on the selected inputs by the user (and the rounding accuracy specified by the app creator). It orchestrates the sequence of helper functions that merge, process and enrich the DS, AE and DM datasets into a unified, plot-ready dataframe with computed metrics, time variables and hovertext.

Internal helper function for preparing the data for the plot depending
on the selected inputs by the user (and the rounding accuracy specified
by the app creator). It orchestrates the sequence of helper functions
that merge, process and enrich the DS, AE and DM datasets into a
unified, plot-ready dataframe with computed metrics, time variables and
hovertext.

## Usage

``` r
prepare_plot_data(
  dm,
  ds,
  ae,
  subjid_var,
  grouping_var,
  time_selection,
  metric_selection,
  unit_selection,
  tt_dec_places
)
```

## Arguments

- dm:

  `[data.frame]`

  The demographics data frame.

- ds:

  `[data.frame]`

  The disposition data frame returned by the prepare_ds_data() function.

- ae:

  `[data.frame]`

  The adverse event data frame returned by the prepare_ae_data()
  function.

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

- tt_dec_places:

  `[integer(1)]`

  Number of decimal places to round the value shown in the hovertext.

## Value

A data frame containing processed data for plotting, including
calculated metrics, time variables and hover text.

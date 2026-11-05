# Internal helper function for getting the right geom\_...\_interactive depending on the selected metric.

Internal helper function for getting the right geom\_...\_interactive
depending on the selected metric.

## Usage

``` r
select_geom(metric_selection)
```

## Arguments

- metric_selection:

  `[character(1)]`

  For determining the calculated rate on the y-axis. Either
  METRIC_BUTTONS_OPT1 or METRIC_BUTTONS_OPT2

## Value

Either a ggiraph::geom_line_interactive or a
ggiraph::geom_step_interactive function object.

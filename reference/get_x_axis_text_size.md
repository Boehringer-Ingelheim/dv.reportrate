# Internal helper function for getting a dynamic textsize for the ticks on the x axis. If there is a high amount of ticks (either because a low step size or a big overall time range in the data) those texts can overlap if their size is to big.

Internal helper function for getting a dynamic textsize for the ticks on
the x axis. If there is a high amount of ticks (either because a low
step size or a big overall time range in the data) those texts can
overlap if their size is to big.

## Usage

``` r
get_x_axis_text_size(breaks)
```

## Arguments

- breaks:

  `[numeric(1+)]`

  (if "By study day" is selected) or `[Date(1+)]` (if "By study date" is
  selected) vector setting the values at which ticks on this axis
  appear.

## Value

`[numeric(1)]` That is passed to the axis.text.x parameter of the
ggplot2::theme() function.

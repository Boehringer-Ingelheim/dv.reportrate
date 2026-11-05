# Internal helper function for getting the breaks and labels for the x axis

Internal helper function for getting the breaks and labels for the x
axis

## Usage

``` r
get_x_scale(time_selection, x_axis)
```

## Arguments

- time_selection:

  `[character(1)]`

  Determining if the time on the x-axis should be study dates (calendar
  based) or study days (subject based) Either TIMETYPE_BUTTONS_OPT1 or
  TIMETYPE_BUTTONS_OPT2.

- x_axis:

  `[list]`

  A list returned by get_x_axis(). Composed of:

  - breaks `[numeric(1+)]`

    A numeric vector setting the values at which ticks on this axis
    appear. Only contained in the list if the time selection was "By
    study day".

  - ticktext `[numeric(1+)]`

    A character vector setting the text displayed at the ticks position
    via "breaks". Only contained in the list if the time selection was
    "By study day".

  - breaks `[Date(1+)]`

    Vector of the dates where the ticks should be in the plot. Only
    contained in the list if the time selection was "By study date".

  - title `[character(1)]`

    Not used in this function.

## Value

a ggplot2 scale object used to configure the x-axis.

- `[ggplot2::ScaleContinuousPosition]`when time selection is "By study
  day" (numeric axis).

- `[ggplot2::ScaleDatePosition]` when time_selection is "By study date"
  (date axis). This object can then be added to a ggplot to define tick
  positions and labels for the x-axis.

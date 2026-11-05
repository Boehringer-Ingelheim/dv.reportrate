# Internal helper function for applying user-defined axis limits to a plot.

Adds a
[`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)
layer to a plot using the axis limits specified by the user. Supports
both one-sided and two-sided limits for the x- and y-axes. Missing
values specified as NULL or NA are treated as unrestricted limits.

## Usage

``` r
apply_axis_limits(
  plt,
  x_min = NULL,
  x_max = NULL,
  y_min = NULL,
  y_max = NULL,
  time_selection = NULL
)
```

## Arguments

- plt:

  `[ggplot]`

  ggplot object to which the axis limits should be applied.

- x_min:

  `[numeric(1) | Date(1) | NULL]`

  Lower x-axis limit specified by the user.

- x_max:

  `[numeric(1) | Date(1) | NULL]`

  Upper x-axis limit specified by the user.

- y_min:

  `[numeric(1) | NULL]`

  Lower y-axis limit specified by the user.

- y_max:

  `[numeric(1) | NULL]`

  Upper y-axis limit specified by the user.

- time_selection:

  `[character(1) | NULL]`

  Current time alignment selection. Used to determine whether x-axis
  limits should be interpreted as study dates or study days.

## Value

A ggplot object with the requested axis limits applied via
[`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html).

## Details

When displaying study days, user-entered x-axis limits are converted
from displayed study day labels to the corresponding plot coordinates
via [`study_day_to_plot_coord()`](study_day_to_plot_coord.md).

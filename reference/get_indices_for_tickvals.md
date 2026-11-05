# Internal helper function creating a logical vector for the indexing of the ticks on the x-Axis of the plot. Applying the logic on the tick_text (and not tick_vals) because that's what displayed in the plot in the end.

Internal helper function creating a logical vector for the indexing of
the ticks on the x-Axis of the plot. Applying the logic on the tick_text
(and not tick_vals) because that's what displayed in the plot in the
end.

## Usage

``` r
get_indices_for_tickvals(step_size, tick_text)
```

## Arguments

- step_size:

  `[integer(1)]`

  Integer value specifying the tick interval on the x axis.

- tick_text:

  `[character(1+)]`

  A character vector setting the text displayed at the ticks positions.

## Value

A logical vector specifying the indices at which ticks should be shown
in the plot on the x axis.

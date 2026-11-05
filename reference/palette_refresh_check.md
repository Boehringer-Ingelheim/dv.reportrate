# Internal helper function for deciding if a new palette should be generated. If there are new levels that weren't already part of palette_stored(). A refresh is triggered when no palette is present, when the new levels appear that were not already part of the current_palette, or when a switch to a smaller, more color-blind-friendly palette becomes possible (from viridis to paletteMartin, or from paletteMartin to okabe-Ito).

Internal helper function for deciding if a new palette should be
generated. If there are new levels that weren't already part of
palette_stored(). A refresh is triggered when no palette is present,
when the new levels appear that were not already part of the
current_palette, or when a switch to a smaller, more
color-blind-friendly palette becomes possible (from viridis to
paletteMartin, or from paletteMartin to okabe-Ito).

## Usage

``` r
palette_refresh_check(current_palette, all_levels)
```

## Arguments

- current_palette:

  `[named character(1+) | NULL]`

  A named character vector of color values, where the names correspond
  to the levels of the grouping variable.

- all_levels:

  `[character(0+)]`

  Vector of *all* the available levels that are in the current grouping
  variable column in the current demographics dataset.

## Value

Logical, indicating if a new palette should be generated. If TRUE a new
palette will be generated.

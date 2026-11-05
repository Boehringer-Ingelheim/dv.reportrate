# Internal helper function for generating a color palette, so that the color of the plots of the different levels stay consistent when selecting or deselecting levels. For \<= 9 levels (including the ungrouped data as an own level) the "Okabe-Ito" palette is used. For \<= 15 levels (including the ungrouped data as an own level) the EXTENDED palette is used. Those palettes are colorblind-friendly. For \> 15 levels (including the ungrouped data as an own level) the "viridis" palette is used.

Internal helper function for generating a color palette, so that the
color of the plots of the different levels stay consistent when
selecting or deselecting levels. For \<= 9 levels (including the
ungrouped data as an own level) the "Okabe-Ito" palette is used. For \<=
15 levels (including the ungrouped data as an own level) the EXTENDED
palette is used. Those palettes are colorblind-friendly. For \> 15
levels (including the ungrouped data as an own level) the "viridis"
palette is used.

## Usage

``` r
generate_palette(all_levels)
```

## Arguments

- all_levels:

  `[character(0+)]`

  Vector of *all* the available levels that are in the current grouping
  variable column in the current demographics dataset. Which levels out
  of all available levels are selected in the UI dropdown doesn't
  matter.

## Value

A named character vector of color values, where the names correspond to
the levels of the grouping variable.

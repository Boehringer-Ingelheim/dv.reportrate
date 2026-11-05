# Internal helper function for getting the part of the hovertext which shows the ratio and the number of AEs.

Internal helper function for getting the part of the hovertext which
shows the ratio and the number of AEs.

## Usage

``` r
get_hovertext_ae_info(dataset, tt_dec_places)
```

## Arguments

- dataset:

  `[data.frame]`

  Data frame returned by the get_time_for_plot() function.

- tt_dec_places:

  `[integer(1)]`

  Number of decimal places to round the value shown in the hovertext.

## Value

A processed data frame with the additional column "ht_ae_info". The
values in this column are later a part of the generated hovertext.

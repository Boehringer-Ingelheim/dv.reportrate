# Internal helper function for combining the preprocessed disposition (DS) and adverse event (AE) datasets into one unified event stream. If a valid grouping variable is provided, the function enriches the combined dataset with the corresponding group assignment from the demographics (DM) domain.

Internal helper function for combining the preprocessed disposition (DS)
and adverse event (AE) datasets into one unified event stream. If a
valid grouping variable is provided, the function enriches the combined
dataset with the corresponding group assignment from the demographics
(DM) domain.

## Usage

``` r
merge_event_data_with_grouping(
  dm_dataset,
  ds_prep_dataset,
  ae_prep_dataset,
  subjid_var,
  grouping_var
)
```

## Arguments

- dm_dataset:

  `[data.frame]`

  The demographics dataframe.

- ds_prep_dataset:

  `[data.frame]`

  The disposition dataframe returned by the prepare_ds_data() function.

- ae_prep_dataset:

  `[data.frame]`

  The adverse event dataframe returned by the prepare_ae_data()
  function.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all
  dataframes.

- grouping_var:

  `[character(1)]`

  Character name of the grouping variable (column) in the "dm_dataset".
  The values of this column define the different groups in the plot.

## Value

A dataframe containing all DS and AE events row‑bound into a unified
event dataset. The result always includes the harmonised columns
required for the downstream computation of both metrics. If a valid
grouping_var is provided, an additional column with the group assignment
is attached to each event record via a left join.

# Internal helper function that checks if at least one of the data frames "dm", "ds" or "ae" contains zero rows OR no levels are selected AND the "show ungrouped" checkbox isn't ticked while a grouping variable is selected. If so, the UI shows a message "No data available", instead of the module proceeding with plot computation and showing an empty coordinate system.

Internal helper function that checks if at least one of the data frames
"dm", "ds" or "ae" contains zero rows OR no levels are selected AND the
"show ungrouped" checkbox isn't ticked while a grouping variable is
selected. If so, the UI shows a message "No data available", instead of
the module proceeding with plot computation and showing an empty
coordinate system.

## Usage

``` r
validate_df_rows(
  dataset_list,
  dm_dataset_name,
  ds_dataset_name,
  ae_dataset_name,
  selected_levels,
  ungrouped_checkbox,
  selected_group
)
```

## Arguments

- dataset_list:

  `[list]`

  A list composed of:

  - dm `[data.frame]`

    The dm dataframe.

  - ds `[data.frame]`

    The ds dataframe.

  - ae `[data.frame]`

    The ae dataframe.

- selected_levels:

  `[character(1+) | NULL]`

  Character vector of the selected levels in the selected_group or NULL
  if no levels are selected.

- ungrouped_checkbox:

  `[logical]`

  Specifying if the ungrouped data should be displayed when a grouping
  variable is selected.

- selected_group:

  `[character(1)]`

  Character name of the selected grouping variable (column).

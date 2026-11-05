# Internal helper function for getting column names with their labels for the UI grouping dropdown.

Internal helper function for getting column names with their labels for
the UI grouping dropdown.

## Usage

``` r
get_columns_with_labels(dataset, columns)
```

## Arguments

- dataset:

  `[data.frame]`

  The demographics dataframe which contains the possible variables for
  grouping and their labels.

- columns:

  `[character(0+)]`

  Character vector of the columnnames that should be returned with their
  label.

## Value

Named character vector where values are the original columnnames and the
names are the string to display in the format "columnname (columnlabel)"
or only "columnname" if no label exsits.

# Read obspack attributes (.nc)

Each obspack file has a header with metadata and this function reads
selected fields from the metadata and add them as columns. This new
columns are used later to be filtered

## Usage

``` r
obs_read_nc_att(index, as_list = FALSE, verbose = FALSE, warnings = FALSE)
```

## Arguments

- index:

  data.table

- as_list:

  Logical to return as list

- verbose:

  Logical to show more information

- warnings:

  Logical to show warnings when reading NetCDF, especially global
  attributes

## Value

A data.frame with with an index obspack.

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
} # }
```

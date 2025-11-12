# Read obspack (.nc)

Each obspack file has a header with metadata and this function reads
selected fields from the metadata and add them as columns. This new
columns are used later to be filtered

## Usage

``` r
obs_read_nc(
  index,
  categories = "flask",
  att = FALSE,
  expr = NULL,
  solar_time = if (grepl("aircraft", categories)) FALSE else TRUE,
  as_list = FALSE,
  verbose = FALSE,
  warnings = FALSE
)
```

## Arguments

- index:

  data.table

- categories:

  character; ONE category : of c("aircraft-pfp", "aircraft-insitu",
  "surface-insitu", "tower-insitu", "aircore", "surface-pfp",
  "shipboard-insitu", "flask").

- att:

  Logical, if global attributes should be added to data.table

- expr:

  String expressions to filter data.table internally

- solar_time:

  Logical, add solar time? Default: if categories include aircraft,
  FALSE, otherwise, TRUE

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

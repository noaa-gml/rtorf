# Summary of the ObsPack files (.txt)

This function returns a data.frame index of all files available in
ObsPack.

This function returns a data.frame index of all files available in
obspack.

## Usage

``` r
obs_summary(
  obs,
  categories = c("aircraft-pfp", "aircraft-insitu", "surface-insitu", "tower-insitu",
    "aircore", "surface-pfp", "shipboard-insitu", "flask"),
  lnchar = 11,
  out = paste0(tempfile(), "_index.csv"),
  verbose = TRUE,
  aslist = FALSE
)

obs_index(
  obs,
  categories = c("aircraft-pfp", "aircraft-insitu", "surface-insitu", "tower-insitu",
    "aircore", "surface-pfp", "shipboard-insitu", "flask"),
  lnchar = 11,
  out = paste0(tempfile(), "_index.csv"),
  verbose = TRUE
)
```

## Arguments

- obs:

  Path to the Obspack GLOBALview txt data

- categories:

  character; default are c("aircraft-pfp", "aircraft-insitu",
  "surface-insitu", "tower-insitu", "aircore", "surface-pfp",
  "shipboard-insitu", "flask"). The ideia is that,as the file names
  include these words, this function identifies which files has these
  words and add them as columns.

- lnchar:

  Integer; last nchar, default = 11.

- out:

  Path to the Obspack index output

- verbose:

  Logical to show more information

- aslist:

  Logical to return list of index and summary

## Value

A data.frame with with an index of the obspack Globalview.

A data.frame with with an index of the obspack Globalview.

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
} # }
{
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
} # }
}
```

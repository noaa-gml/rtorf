# Obspack Table Summary

This function reads the obsPack directory providing a summary for the
columns: "value", "time", "time_decimal", "latitude" and "longitude".
These summary are made by the columns "name", "sector", "site_name",
"site_country", "type_altitude", "lab_1_abbr" and "site_utc2lst"

## Usage

``` r
obs_table(
  df,
  cols = c("value", "time", "time_decimal", "latitude", "longitude")
)
```

## Arguments

- df:

  data.table

- cols:

  String of columns to be summarized.

## Value

A data.frame with with an index obspack.

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
dx <- obs_table(dt)
} # }
```

# Aggregates Observations by time

This function add aggregate cols by \`key_time\`

## Usage

``` r
obs_agg(
  dt,
  cols = c("year", "month", "day", "hour", "minute", "second", "time", "time_decimal",
    "value", "latitude", "longitude", "altitude", "pressure", "u", "v", "temperature",
    "type_altitude"),
  by = c("key_time", "site_code", "altitude_final", "type_altitude", "lab_1_abbr",
    "dataset_calibration_scale"),
  fn = "mean",
  verbose = TRUE
)
```

## Arguments

- dt:

  data.table

- cols:

  Character which defines columns to be aggregated

- by:

  String with columns to be aggregated

- fn:

  Function to be applied to the columns, default mean

- verbose:

  logical to show more information

## Value

A data.frame with with an index.

## Note

By default add column timeUTC based on the input column \`key_time\`

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
} # }
```

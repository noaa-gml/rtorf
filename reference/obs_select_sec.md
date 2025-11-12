# Select Observations by closest time (seconds)

This function select by closest time. Good for aircrafts

## Usage

``` r
obs_select_sec(
  dt,
  origin = "1970-01-01",
  tz = "UTC",
  seconds = 30,
  verbose = TRUE
)
```

## Arguments

- dt:

  data.table

- origin:

  a date-time object, default "1970-01-01"

- tz:

  time zone string

- seconds:

  seocnds target to select and filter data

- verbose:

  logical to show more information

## Value

A filtered data.frame.

## Note

By default add column timeUTC based on the input column \`key_time\`

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
dx <- obs_read(index, expr = "altitude_final == '5800'")
dx <- obs_addtime(dx[, -"site_code"])
dy <- obs_select_sec(dx)
} # }
```

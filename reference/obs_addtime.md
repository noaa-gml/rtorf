# Add times

This function add timeUTC as POSIX class, local time and ending sampling
time

## Usage

``` r
obs_addtime(dt, verbose = TRUE, tz = "UTC", timeonly = FALSE)
```

## Arguments

- dt:

  data.table

- verbose:

  Logical, to show more info

- tz:

  Timezone, default "UTC"

- timeonly:

  return only timeUTC column

## Value

A data.frame with with an index obspack.

## Note

timeUTC is calculated based on the field column start_time, the
timeUTC_end is calculated using this approach: 1. If the column
time_interval is not found, proceed with the calculation using
midpoint_time 2. Else, use column time_interval

## See also

Other time:
[`obs_addltime()`](https://noaa-gml.github.io/rtorf/reference/obs_addltime.md),
[`obs_addmtime()`](https://noaa-gml.github.io/rtorf/reference/obs_addmtime.md),
[`obs_addstime()`](https://noaa-gml.github.io/rtorf/reference/obs_addstime.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <-  obs_summary(obs)
dt <- obs_read(index)
dt <- obs_addtime(dt)
} # }
```

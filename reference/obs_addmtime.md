# Add matlab time

This function add timeUTC as POSIX class, local time and ending sampling
time

## Usage

``` r
obs_addmtime(time, origin = "1970-01-01", tz = "UTC")
```

## Arguments

- time:

  numeric time from MATLAB

- origin:

  default "1970-01-01"

- tz:

  Timezone, default "UTC"

## Value

return the same data.frame adding solar time

## See also

Other time:
[`obs_addltime()`](https://noaa-gml.github.io/rtorf/reference/obs_addltime.md),
[`obs_addstime()`](https://noaa-gml.github.io/rtorf/reference/obs_addstime.md),
[`obs_addtime()`](https://noaa-gml.github.io/rtorf/reference/obs_addtime.md)

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

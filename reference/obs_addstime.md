# Add solar time

This function add timeUTC as POSIX class, local time and ending sampling
time

## Usage

``` r
obs_addstime(dt, tz = "UTC")
```

## Arguments

- dt:

  obspack data.table

- tz:

  Timezone, default "UTC"

## Value

return the same data.frame adding solar time

## See also

Other time:
[`obs_addltime()`](https://noaa-gml.github.io/rtorf/reference/obs_addltime.md),
[`obs_addmtime()`](https://noaa-gml.github.io/rtorf/reference/obs_addmtime.md),
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

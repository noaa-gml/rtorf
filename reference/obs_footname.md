# Expected footprint name

return the expected name for the NetCDF footprint

## Usage

``` r
obs_footname(
  time = NULL,
  year,
  month,
  day,
  hour,
  minute,
  second,
  lat,
  lon,
  alt,
  fullpath = FALSE,
  out,
  ...
)
```

## Arguments

- time:

  POSIXct time to extract time variblaes

- year:

  numeric number

- month:

  numeric number

- day:

  numeric number

- hour:

  numeric number

- minute:

  numeric number

- second:

  numeric number, optional

- lat:

  numeric number

- lon:

  numeric number

- alt:

  numeric number

- fullpath:

  Logical, to add or not YYYY/MO/hysplit to id

- out:

  outfile path.

- ...:

  data.table::fwrite arguments.

## Note

source https://stackoverflow.com/a/47015304/2418532

\# IMPORTANT!!! \# This function will generate the expected NetCDF file
name. \# It assumes that the name was generated under the following
considerations: \# time variables (year, month, day, etc) have a format
of two digits, eg "0.1" \# latitude and longitude have been round with 4
decimals \# The format for latitude is 2 integers, a point and 4
decimals \# The format for longitude is 3 integers, a point and 4
decimals

## See also

Other helpers:
[`fex()`](https://noaa-gml.github.io/rtorf/reference/fex.md),
[`obs_format()`](https://noaa-gml.github.io/rtorf/reference/obs_format.md),
[`obs_freq()`](https://noaa-gml.github.io/rtorf/reference/obs_freq.md),
[`obs_list.dt()`](https://noaa-gml.github.io/rtorf/reference/obs_list.dt.md),
[`obs_out()`](https://noaa-gml.github.io/rtorf/reference/obs_out.md),
[`obs_rbind()`](https://noaa-gml.github.io/rtorf/reference/obs_rbind.md),
[`obs_read_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_read_csvy.md),
[`obs_roundtime()`](https://noaa-gml.github.io/rtorf/reference/obs_roundtime.md),
[`obs_trunc()`](https://noaa-gml.github.io/rtorf/reference/obs_trunc.md),
[`obs_write_csvy()`](https://noaa-gml.github.io/rtorf/reference/obs_write_csvy.md),
[`rtorf-deprecated`](https://noaa-gml.github.io/rtorf/reference/rtorf-deprecated.md),
[`sr()`](https://noaa-gml.github.io/rtorf/reference/sr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs_footname(time = Sys.time(),
             second = data.table::second(Sys.Time()),
             lat = 0,
             lon = 0,
             alt = 0)
obs_footname(year = 2020,
             month = 12,
             day = 30,
             hour = 9,
             minute = 54,
             lat = 3.2133,
             lon = 30.9131,
             alt = 497,
             fullpath = TRUE)
obs_footname(year = 2020,
             month = 12,
             day = 30,
             hour = 9,
             minute = 54,
             lat = 1,
             lon = -130.9131,
             alt = 497,
             fullpath = TRUE)
} # }
```

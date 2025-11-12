# add local time

Calculate an approximation of local hour

## Usage

``` r
obs_addltime(
  dt,
  timeUTC = "timeUTC",
  utc2lt = "site_utc2lst",
  longitude = "longitude",
  tz = "UTC",
  timeonly = FALSE
)
```

## Arguments

- dt:

  data.table

- timeUTC:

  Character indicating the Time column as POSIXct

- utc2lt:

  Character indicating the integer column to convert to local time if
  available

- longitude:

  Character indicating the column with the lingitude

- tz:

  Timezone, default "UTC"

- timeonly:

  return only local_time column

## Value

data.table with local time columns

## Note

time depending n longitude is by John Miller (GML/NOAA)

## See also

Other time:
[`obs_addmtime()`](https://noaa-gml.github.io/rtorf/reference/obs_addmtime.md),
[`obs_addstime()`](https://noaa-gml.github.io/rtorf/reference/obs_addstime.md),
[`obs_addtime()`](https://noaa-gml.github.io/rtorf/reference/obs_addtime.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
} # }
```

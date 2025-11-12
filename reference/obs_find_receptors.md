# Compares expected receptors

This function creates a data.frame with the expected footprints in .nc
and compare with the actual footprints. The idea is verify is there are
missing footprints.

## Usage

``` r
obs_find_receptors(
  path,
  year,
  month,
  day,
  hour,
  minute,
  lat,
  lon,
  alt,
  out = paste0(tempfile(), ".csvy"),
  verbose = FALSE
)
```

## Arguments

- path:

  String with path hwere are stored the footprints

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

- lat:

  numeric number

- lon:

  numeric number

- alt:

  numeric number

- out:

  outfile path.

- verbose:

  logical to show more information

## Value

A data.frame with with expected footprints

## Examples

``` r
if (FALSE) { # \dontrun{
# do not run
p <- "/path/to/continuous/"
# here we have year/month/hysplit*.nc
x <- dt
dt <- obs_find_receptors(p, year, month....)
} # }
```

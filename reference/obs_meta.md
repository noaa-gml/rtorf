# Read obspack metadata

Read obspack metadata

## Usage

``` r
obs_meta(
  index,
  verbose = TRUE,
  n_site_code = 15,
  n_site_name = 15,
  n_site_latitude = 18,
  n_site_longitude = 19,
  n_site_country = 18,
  n_dataset_project = 21,
  n_lab = 16,
  n_scales = 31,
  n_site_elevation = 20,
  n_altitude_comment = 22,
  n_utc = 18,
  fill_value = -1e+34,
  as_list = FALSE
)
```

## Arguments

- index:

  data.table

- verbose:

  Logical to show more information

- n_site_code:

  number of characters extraced from metadata after search

- n_site_name:

  number of characters extraced from metadata after search

- n_site_latitude:

  number of characters extraced from metadata after search

- n_site_longitude:

  number of characters extraced from metadata after search

- n_site_country:

  number of characters extraced from metadata after search

- n_dataset_project:

  number of characters extraced from metadata after search

- n_lab:

  number of characters extraced from metadata after search

- n_scales:

  number of characters extraced from metadata after search

- n_site_elevation:

  number of characters extraced from metadata after search

- n_altitude_comment:

  number of characters extraced from metadata after search

- n_utc:

  number of characters extraced from metadata after search

- fill_value:

  fill value. Appeared in aoa_aircraft-flask_19_allvalid.txt

- as_list:

  Logical to return as list

## Value

A data.frame with with an index obspack.

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_meta(index)
} # }
```

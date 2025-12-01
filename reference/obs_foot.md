# obs_footprints

This function returns a arrays (or list of arrays) of convolved
footpritns with flux

## Usage

``` r
obs_foot(
  foot_path = "AAA",
  name_foot = "foot1",
  flon = "foot1lon",
  flat = "foot1lat",
  time_foot,
  fn = NULL,
  as_list = FALSE,
  verbose = TRUE
)
```

## Arguments

- foot_path:

  path for footprint (length 1).

- name_foot:

  name of the footprint variable in the NetCDF file.

- flon:

  name of lons

- flat:

  name of lats

- time_foot:

  Time of the footprints (in the name file) or third dimension

- fn:

  string with function to aggregate convolved fluxes, e.g. \`mean\`,
  \`sum\`, \`max\`, etc.

- as_list:

  Logical, to return list of arrays

- verbose:

  Logical, to display more information

## Examples

``` r
if (FALSE) { # \dontrun{
# do not run
} # }
```

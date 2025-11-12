# obs_convolve

This function returns a arrays (or list of arrays) of convolved
footpritns with flux

## Usage

``` r
obs_convolve(
  foot_path = "AAA",
  name_foot = "foot1",
  flon = "foot1lon",
  flat = "foot1lat",
  time_foot,
  flux = "CTCO2",
  df_flux,
  flux_format = "%Y%m%d.nc",
  name_var_flux = 1,
  factor = 1e+09,
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

- flux:

  String, default NOAA "CTCO2". Implies bio, ocn, fossil, fire.

- df_flux:

  data.table with columns f (full path) and nf file name 'Ymd.nc'

- flux_format:

  string with date format 'Ymd.nc'

- name_var_flux:

  string or numeric of position for the name var in the flux format.
  e.g.: If missing, asusmes date ("2020-01-01"), if string something
  like ("flux"), if numeric, identify the var names and you select which
  of them (first, second, etc)

- factor:

  number to multiply fluxes.

- fn:

  string with function to aggregate convolved fluxes, e.g. \`mean\`,
  \`sum\`, \`max\`, etc.

- as_list:

  Logical, to return list of arrays

- verbose:

  Logical, to display more information

## Note

main assumption is that fluxes have same spatial dimensions as
footprints when flux is "monthly", "daily" or "yearly", it assumes flux
variable has the same name as "

## Examples

``` r
if (FALSE) { # \dontrun{
# do not run
} # }
```

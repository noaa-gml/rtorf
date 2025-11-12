# obs_nc_get

Reads NetCDF var and returns the spatial array

## Usage

``` r
obs_nc_get(
  nc_path = "AAA",
  nc_name = "foot1",
  nc_lat = "foot1lat",
  nc_lon = "foot1lon",
  verbose = FALSE,
  all = FALSE
)
```

## Arguments

- nc_path:

  String pointing to the target NetCDF

- nc_name:

  String indicating the spatial array

- nc_lat:

  String to extract latitude.

- nc_lon:

  String to extract longitude

- verbose:

  Logical, to display more information.

- all:

  Logical, if TRUE, return list of array, lon and lats

## Value

Array of convolved footprints, or lis tof convolved fluxes and lat lon

## Examples

``` r
if (FALSE) { # \dontrun{
#nc_path <- paste0("Z:/footprints/aircraft/flask/2018/04",
#"/hysplit2018x04x08x15x15x38.7459Nx077.5584Wx00594.nc")
#f <- obs_nc_get(nc_path = nc_path)
} # }
```

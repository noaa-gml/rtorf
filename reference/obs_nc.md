# obs_nc

Creates NetCDF based on dimension from another NetCDF with custom
dimensions, and attributes

## Usage

``` r
obs_nc(
  lat,
  lon,
  time_nc,
  vars_out = c("total", "bio", "ocn", "fossil", "fire"),
  units_out,
  nc_out,
  larrays,
  verbose = FALSE
)
```

## Arguments

- lat:

  lats array

- lon:

  longs arrau

- time_nc:

  Times.

- vars_out:

  names for the variables to be created in the NetCDF.

- units_out:

  units for the NetCDF variables to be created. NO DEFAULT.

- nc_out:

  path for the created NetCDF.

- larrays:

  list of arrays, length equal to vars_out.

- verbose:

  Logical, to display more information.

## Value

A NetCDF file with the custom attributes and units

## Examples

``` r
if (FALSE) { # \dontrun{
# nc_path <- paste0("Z:/footprints/aircraft/flask/2018",
# "/04/hysplit2018x04x08x15x15x38.7459Nx077.5584Wx00594.nc")
# foot <- obs_nc_get(nc_path, all = TRUE)
# nco <- paste0(tempfile(), "2.nc")
# file.remove(nco)
# obs_nc(lat = foot$lat,
#        lon = foot$lon,
#        time_nc = ISOdatetime(2018, 4, 8, 15, 15, 38),
#        units_out = "(ppb/nanomol m-2 s-1)*nanomol m-2 s-1",
#        vars_out = c("a", "b"),
#        nc_out  = nco,
#        larrays = list(a = foot, b = foot),
#        verbose = TRUE)
} # }
```

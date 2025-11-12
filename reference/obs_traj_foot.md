# obs_traj_foot

return trajectory

## Usage

``` r
obs_traj_foot(
  ident,
  part = NULL,
  timelabel = NULL,
  pathname = "",
  foottimes = 0:240,
  zlim = c(0, 0),
  coarse = 1,
  dmassTF = TRUE,
  numpix.x = 70,
  numpix.y = 70,
  lon.ll = -145,
  lat.ll = 11,
  lon.res = 1,
  lat.res = 1,
  npar = 500,
  eps.global = 0.1
)
```

## Arguments

- ident:

  foot id

- part:

  data.table with PARTICLE.DAT information

- timelabel:

  timelabel

- pathname:

  pathname

- foottimes:

  vector of times between which footprint or influence will be
  integrated

- zlim:

  zlim

- coarse:

  default 1

- dmassTF:

  default TRUE weighting by accumulated mass due to violation of mass
  conservation in met fields

- numpix.x:

  number of pixels in x directions in grid

- numpix.y:

  number of pixels in y directions in grid

- lon.ll:

  lower left lon

- lat.ll:

  lower left lat

- lon.res:

  resolution

- lat.res:

  resolution

- npar:

  default 500, number of particles hysplit was run with (required in
  order to account for those cases where a thinned particle table that
  may not contain all particle indices is used)

- eps.global:

  default 0.01

## Value

return footprint

## See also

Other helpers legacy:
[`obs_grid()`](https://noaa-gml.github.io/rtorf/reference/obs_grid.md),
[`obs_id2pos()`](https://noaa-gml.github.io/rtorf/reference/obs_id2pos.md),
[`obs_info2id()`](https://noaa-gml.github.io/rtorf/reference/obs_info2id.md),
[`obs_julian()`](https://noaa-gml.github.io/rtorf/reference/obs_julian.md),
[`obs_normalize_dmass()`](https://noaa-gml.github.io/rtorf/reference/obs_normalize_dmass.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
} # }
```

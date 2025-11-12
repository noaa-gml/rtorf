# obs_info2id

return footprint/receptor id

## Usage

``` r
obs_info2id(yr, mo, dy, hr, mn = 0, lat, lon, alt, sep = "x", long = T)
```

## Arguments

- yr:

  year

- mo:

  month

- dy:

  day

- hr:

  hour

- mn:

  minute

- lat:

  latitude

- lon:

  longitude

- alt:

  altitude above ground level

- sep:

  character, default"x"

- long:

  Logical, to add minute, and rounded with 2 decimals, instead of 4.
  default TRUE

## Value

a string with the receptor id

## See also

Other helpers legacy:
[`obs_grid()`](https://noaa-gml.github.io/rtorf/reference/obs_grid.md),
[`obs_id2pos()`](https://noaa-gml.github.io/rtorf/reference/obs_id2pos.md),
[`obs_julian()`](https://noaa-gml.github.io/rtorf/reference/obs_julian.md),
[`obs_normalize_dmass()`](https://noaa-gml.github.io/rtorf/reference/obs_normalize_dmass.md),
[`obs_traj_foot()`](https://noaa-gml.github.io/rtorf/reference/obs_traj_foot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Do not run
obs_info2id(yr = 2002,
            mo = 8,
            dy = 3,
            hr = 10,
            mn = 0,
            lat = 42,
            lon = -90,
            alt = 1) [1]
} # }
```

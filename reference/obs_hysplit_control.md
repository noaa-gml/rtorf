# obs_hysplit_control

This function creates a CONTROL file for HYSPLIT model. It uses inputs
from a data.frame with the receptor information.

## Usage

``` r
obs_hysplit_control(
  df,
  year,
  month,
  day,
  hour,
  minute,
  lat,
  lon,
  alt,
  start_time = NULL,
  nlocations = 1,
  duration = -240,
  vertical_motion = 5,
  top_model_domain = 10000,
  met = c("hrrr", "nams", "gfs0p25"),
  nmet = abs(duration/24) + 1,
  metpath = c("/work/noaa/lpdm/metfiles/hrrr/", "/work/noaa/lpdm/metfiles/nams/",
    "/work/noaa/lpdm/metfiles/gfs0p25/"),
  metformat = c(hrrr = "%Y%m%d_hrrr", nams = "%Y%m%d_hysplit.t00z.namsa", gfs0p25 =
    "%Y%m%d_gfs0p25", era5 = "ERA5_%Y%m%d.ARL"),
  ngases = 1,
  gas = "Foot",
  emissions_rate = 0,
  hour_emissions = 0.01,
  release_start = NULL,
  nsim_grids = 1,
  center_conc_grids = c(0, 0),
  grid_spacing = c(0.1, 0.1),
  grid_span = c(30, 30),
  nconc = "cdump",
  nvert_levels = 1,
  height_vert_levels = 50,
  sampling_start_time = c(0, 0, 0, 0, 0),
  sampling_end_time = c(0, 0, 0, 0, 0),
  sampling_interval_type = c(0, abs(duration), 0),
  npol_depositing = 1,
  particle_params = c(0, 0, 0),
  dmwsrdre = c(0, 0, 0, 0, 0),
  wrhcicbc = c(0, 0, 0),
  radiactive_decay = 0,
  pol_res = 0,
  control = "CONTROL",
  verbose = FALSE
)
```

## Arguments

- df:

  data.frame of receptor information. Must include, "year", "month",
  "day", "hour" (0:23), "minute", "latitude", "longitude", "altitude"

- year:

  year, if missing df.

- month:

  month, if missing df.

- day:

  day, if missing df.

- hour:

  hour, if missing df.

- minute:

  minute, if missing df.

- lat:

  latitude, if missing df.

- lon:

  longitude, if missing df.

- alt:

  altitude, if missing df.

- start_time:

  String to be added, default derived from df.

- nlocations:

  number of locations.

- duration:

  number of hours of release. (Negative is backwards in time).

- vertical_motion:

  Vertical motion option. (0:data 1:isob 2:isen 3:dens 4:sigma 5:diverg
  6:msl2agl 7:average 8:damped). . The default "data" selection will use
  the meteorological model's vertical velocity fields; other options
  include isobaric, isentropic, constant density, constant internal
  sigma coordinate, computed from the velocity divergence, a special
  transformation to correct the vertical velocities when mapped from
  quasi-horizontal surfaces (such as relative to MSL) to HYSPLIT's
  internal terrain following sigma coordinate, and a special option (7)
  to spatially average the vertical velocity. The averaging distance is
  automatically computed from the ratio of the temporal frequency of the
  data to the horizontal grid resolution. Default 5

- top_model_domain:

  altitude above ground level (m).

- met:

  meteorological models to be used.

- nmet:

  Number of days for the meteorological files. Default is number of days
  in duration plus two days. nmet is the number of simultaneous input
  meteorological files. For instance, 11 means that for each
  meteorological grid, 11 files are expected. Usually, the files are
  daily. Note that the same number of files are required for each grid
  in this approach. Hysplit expects something like 2 11, which means 2
  meteorological grids with 11 files each. The number 2 comes from the
  length of met files.

- metpath:

  paths for each meteorological model output.

- metformat:

  format to applied to the meteorological daily file

- ngases:

  Default 1.

- gas:

  default "Foot".

- emissions_rate:

  Default 0

- hour_emissions:

  hour release, depend of type of release, instantaneous 0.01,
  continuous 1.

- release_start:

  derived from df.

- nsim_grids:

  Number of simulated grids.

- center_conc_grids:

  center coordinates for conc grid.

- grid_spacing:

  grid spacing, default is 0.1 degree.

- grid_span:

  model extension in degrees by dimension.

- nconc:

  name of the concentration file, default is "cdump"

- nvert_levels:

  number of vertical levels

- height_vert_levels:

  hright vertical levels (50)

- sampling_start_time:

  2 digits year, month, day, hour, minute

- sampling_end_time:

  2 digits year, month, day, hour, minute

- sampling_interval_type:

  type, hour, minure

- npol_depositing:

  number of pollutant depositing

- particle_params:

  Particle diameter, density, and shape

- dmwsrdre:

  DepVel, MW, SurfReRa, DifRa, EHenry

- wrhcicbc:

  Wet removal: HC, incloud, belowcloud

- radiactive_decay:

  days

- pol_res:

  Pollutant Resuspension (1/m)

- control:

  name of the file, default "CONTROL"

- verbose:

  logical, to show more information

## Value

A CONTROL file

## Examples

``` r
{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
df <- dt[1]
control_file <- tempfile()
obs_hysplit_control(df, control = control_file)
ff <- readLines(control_file)

cat(ff, sep =  "\n")
}
#> Number of files of index: 1
#>           sector     N
#>           <char> <int>
#> 1:         flask     1
#> 2: Total sectors     1
#> Detected 0 files with agl
#> Detected 1 files without agl
#> Searching flask...
#> 1: ch4_aoa_aircraft-flask_19_allvalid.txt
#> 11 02 16 02 17
#> 1
#> 34.4290 141.0370 5800.0
#> -240
#> 5
#> 10000.0
#> 3 11
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110216_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110215_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110214_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110213_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110212_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110211_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110210_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110209_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110208_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110207_hrrr
#> /work/noaa/lpdm/metfiles/hrrr/2011/
#> 20110206_hrrr
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110216_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110215_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110214_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110213_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110212_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110211_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110210_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110209_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110208_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110207_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/nams/2011/
#> 20110206_hysplit.t00z.namsa
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110216_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110215_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110214_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110213_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110212_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110211_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110210_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110209_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110208_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110207_gfs0p25
#> /work/noaa/lpdm/metfiles/gfs0p25/2011/
#> 20110206_gfs0p25
#> 1
#> Foot
#> 0
#> 0.01
#> 11 02 16 02 17
#> 1
#> 0.0 0.0
#> 0.10 0.10
#> 30.0 30.0
#> ./
#> cdump
#> 1
#> 50
#> 00 00 00 00 00
#> 00 00 00 00 00
#> 00 240 00
#> 1
#> 0.0 0.0 0.0
#> 0.0 0.0 0.0 0.0 0.0
#> 0.0 0.0 0.0
#> 0.0
#> 0.0
```

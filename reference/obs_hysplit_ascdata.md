# obs_hysplit_ascdata

This function creates a ASCDATA.CFG file for HYSPLIT model.

## Usage

``` r
obs_hysplit_ascdata(
  llc = c(-90, -180),
  spacing = c(1, 1),
  n = c(180, 360),
  landusecat = 2,
  rough = 0.2,
  bdyfiles = "../bdyfiles/",
  ascdata = "ASCDATA.CFG"
)
```

## Arguments

- llc:

  Lower left corner, default c(-90.0, -180.0)

- spacing:

  spacing in degress, default c(1.0, 1.0)

- n:

  number of data points, default c(180, 360)

- landusecat:

  land use category, default 2

- rough:

  default roughness length (meters), default 0.2

- bdyfiles:

  directory location of the data files, default '../bdyfiles/'

- ascdata:

  file, default ASCDATA.CFG

## Value

A ASCDATA.CFG file

## Examples

``` r
{
# Do not run
ascdata_file <- tempfile()
obs_hysplit_ascdata(ascdata = ascdata_file)
cat(readLines(ascdata_file), sep =  "\n")
}
#> -90.0  -180.0  lat/lon of lower left corner (last record in file)
#> 1.0     1.0    lat/lon spacing in degrees between data points
#> 180     360    lat/lon number of data points
#> 2              default land use category
#> 0.2            default roughness length (meters)
#> '../bdyfiles/' directory location of data files
```

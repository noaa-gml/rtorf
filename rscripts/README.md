README
================

### files:

    ##  [1] "r/aircore_year.R"        "r/aircraft_year.R"      
    ##  [3] "r/flask_non_noaa_year.R" "r/index.R"              
    ##  [5] "r/inputs_inv.R"          "r/inputs_invs.R"        
    ##  [7] "r/surface_insitu_year.R" "r/tower_insitu_year.R"  
    ##  [9] "README.md"               "README.R"               
    ## [11] "README.Rmd"

The directory `rscripts` contains the required files to generate the
list of receptors using `robspack` The scripts will run from a defined
working directory

### 1. Run index (summary)

The script `r/index.R` will create the tree directory to store the
processed ObsPack files Also, it will create summaries and store them
for each category in the directory `obs`

### 2. Categories in the directory r

These scripts need to be run using Rscript command examples:

- `Rscript r/aircore_year.R 2020 obs/obs_aircore.csv.gz`
- `Rscript r/aircraft_year.R 2020 obs/obs_aircraft-insitu.csv.gz`
- `Rscript r/flask_year.R 2020 obs/obs_flask.csv.gz`
- `Rscript r/surface_year.R 2020 obs/obs_surface-insitu.csv.gz`
- `Rscript r/tower_year.R 2020 obs/obs_tower-insitu.csv.gz`

Windows users can adapt the following lines

- “C:/Users/*User*/My Documents/R/R-4.1.3/bin/Rscript.exe”
  r/tower_insitu_year.R 2020 obs/obs_tower-insitu.csv.gz
- “C:/Users/*User*/My Documents/R/R-4.1.3/bin/Rscript.exe”
  r/aircore_year.R 2020 obs/obs_aircore.csv.gz

### 3. Run Hysplit

Once if we have the receptor, we can run Hysplit to generate footprints
in NetCDF format

### 4. Generate receptor info

Once we have the receptor list and the footprints were generated, we can
generate the receptor info, which consists in text files. The script is
`r/inputs_inv.R`

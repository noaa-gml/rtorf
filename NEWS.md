NEWS
===========


### rtorf 0.9.3 (Release date: 2024-07-09)

- Add cols argument to obs_read_nc to avoid reading all columns by default

### rtorf 0.9.2 (Release date: 2024-04-23)

- reformatted obs_table to read df from obs_read
- obs_read now read intake_height, site_longitude and site_latitude

obs_addstime return a POSIXct vector, (fix #6 )

### rtorf 0.9.1 (Release date: 2024-03-05)

- order summary index by name (obs_summary)

### rtorf 0.9.0 (Release date: 2024-03-05)

- Rename robspack to rtorf.
- Prepare to submit to JOSS and https://github.com/noaa-gml.


### robspack 0.8.1 (Release date: 2023-11-02)

- Adding solar time in NetCDF.

### robspack 0.8.0 (Release date: 2023-09-13)

- Adding support for NetCDF.
- obs_index detect file extension

### robspack 0.7.0 (Release date: 2023-04-18)

- rename package to `robspack`
- rename obs_write to obs_write_csvy
- add rscripts in GitHub repo

### robspackfilter 0.6.0 (Release date: 2023-04-04)

- add obs_table, to summarize ObsPack

### robspackfilter 0.5.1 (Release date: 2023-03-22)

- add obs_format to format data
- add obs_plot
- Change method to plot invfiles, based on obs_plot
- Rename obs_index to obs_summary

### robspackfilter 0.4.1 (Release date: 2023-02-01)

- Deprecate some functions

### robspackfilter 0.4.0 (Release date: 2023-01-19)

- add obs_meta, which creates a data.table based on metadata


### robspackfilter 0.3.4 (Release date: 2022-10-03)

- Reverse obs_footname, from obs_trunc to sprintf
- add round (x, 4) into lat lon obs_footname to match receptors.
- Adding institution and Scale into obs_invfiles

### robspackfilter 0.3.3 (Release date: 2022-08-29)

- add by lab_1_abbr, dataset_calibration_scale into obs_add, assuming site_code matches scale and lab_1

### robspackfilter 0.3.3 (Release date: 2022-08-29)

- add obs_footname
- add obs_invfiles.
- add dataset_calibration_scale into obs_read


### robspackfilter 0.3.1 (Release date: 2022-08-25)

- Prepare submission to JOSS
- obs_index now does not write temporal file for index,
when out argument is missing. 

### robspackfilter 0.3.0 (Release date: 2022-08-11)

- add function obs_trunc and used inside obs_find_receptors

### robspackfilter 0.2.2 (Release date: 2022-08-01)

- rename write_meta to obs_write
- obs_find_receptors: change  
  sprintf(lon, fmt = '%7.4f') to 
   sprintf(lon, fmt = '%07.4f')  to
  formatC(lon, digits = 4, width = 8, format = "f", flag = "0")


### robspackfilter 0.2.1 (Release date: 2022-07-19)

- Add obs_find_receptors

### robspackfilter 0.2.0 (Release date: 2022-07-14)

- Add write_meta which generates a CSVY file

### robspackfilter 0.1.4 (Release date: 2022-07-12)

- add obs_addltime to return a data.table with local time columns

### robspackfilter 0.1.3 (Release date: 2022-07-06)

- changed all functions to obs_*
- add obs_agg to aggregate columns by time interval
- imported lubridate to calculate decimal date
- obs_agg also add some character vars


### robspackfilter 0.1.2 (Release date: 2022-07-04)

- add addtime_obs into read_obs
- add option into addtime_obs to find intervals
time data into desired frequency
eg. 20, using coumn 20
- Add name of file as variable in read_obs
- add option in addtime_obs to aggregate numeric by mean

### robspackfilter 0.1.1 (Release date: 2022-06-29)

- add variable n into index and read_obs
- add verbose to avoid mistakes in read_obs
- add add_zero,  c(1, 10) -> c("01", "10")
- addtime_obs add column dif_time (end - start)


### robspackfilter 0.1.0 (Release date: 2022-06-24)

- Create robspackfilter

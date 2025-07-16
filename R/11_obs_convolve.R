#' #' obs_hysplit_control
#' #'
#' #' This function creates a CONTROL file for HYSPLIT model.
#' #' It uses inputs from a data.frame with the receptor information.
#' #'
#' #' @param df data.frame of receptor information. Must include, "year",
#' #' "month", "day", "hour" (0:23), "minute", "latitude", "longitude", "altitude"
#' #' @param year year, if missing df.
#' #' @param month month, if missing df.
#' #' @param day day, if missing df.
#' #' @param hour hour, if missing df.
#' #' @param minute minute, if missing df.
#' #' @param lat latitude, if missing df.
#' #' @param lon longitude, if missing df.
#' #' @param alt altitude, if missing df.
#' #' @param nlocations number of locations.
#' #' @param duration number of hours of release. (Negative is backwards in time).
#' #' @param vertical_motion Vertical motion option.  (0:data 1:isob 2:isen 3:dens
#' #'  4:sigma 5:diverg 6:msl2agl 7:average 8:damped).
#' #'  . The default "data" selection will use the meteorological model's
#' #'  vertical velocity fields; other options include isobaric, isentropic,
#' #'  constant density, constant internal sigma coordinate, computed from the
#' #'  velocity divergence, a special transformation to correct the vertical
#' #'  velocities when mapped from quasi-horizontal surfaces (such as relative
#' #'   to MSL) to HYSPLIT's internal terrain following sigma coordinate, and a
#' #'   special option (7) to spatially average the vertical velocity.
#' #'   The averaging distance is automatically computed from the ratio of
#' #'   the temporal frequency of the data to the horizontal grid resolution.
#' #'   Default 5
#' #' @param top_model_domain altitude above ground level (m).
#' #' @param met meteorological models to be used.
#' #' @param nmet Number of days for the meteorological files. Default is
#' #' number of days in duration plus two days.
#' #' nmet is the number of simultaneous input meteorological files. For instance,
#' #' 11 means that for each meteorological grid, 11 files are expected. Usually,
#' #' the files are daily. Note that the same number of files are required for
#' #' each grid in this approach. Hysplit expects something like 2 11,
#' #' which means 2 meteorological grids with 11 files each. The number
#' #' 2 comes from the length of met files.
#' #' @param metpath paths for each meteorological model output.
#' #' @param metformat format to applied to the meteorological daily file
#' #' @param ngases Default 1.
#' #' @param gas default "Foot".
#' #' @param emissions_rate Default 0
#' #' @param hour_emissions hour release, depend of type of release, instantaneous 0.01,
#' #' continuous 1.
#' #' @param release_start derived from df.
#' #' @param nsim_grids Number of simulated grids.
#' #' @param center_conc_grids center coordinates for conc grid.
#' #' @param grid_spacing grid spacing, default is 0.1 degree.
#' #' @param grid_span model extension in degrees by dimension.
#' #' @param nconc name of the concentration file, default is "cdump"
#' #' @param nvert_levels number of vertical levels
#' #' @param height_vert_levels hright vertical levels (50)
#' #' @param sampling_start_time 2 digits year, month, day, hour, minute
#' #' @param sampling_end_time  2 digits year, month, day, hour, minute
#' #' @param sampling_interval_type  type, hour, minure
#' #' @param npol_depositing number of pollutant depositing
#' #' @param particle_params Particle diameter, density, and shape
#' #' @param dmwsrdre DepVel, MW, SurfReRa, DifRa, EHenry
#' #' @param wrhcicbc Wet removal: HC, incloud, belowcloud
#' #' @param radiactive_decay days
#' #' @param pol_res Pollutant Resuspension (1/m)
#' #' @param control name of the file, default "CONTROL"
#' #' @return A CONTROL file
#' #' @export
#' #' @examples {
#' #' # Do not run
#' #' obs <- system.file("data-raw", package = "rtorf")
#' #' index <- obs_summary(obs)
#' #' dt <- obs_read(index)
#' #' df <- dt[1]
#' #' control_file <- tempfile()
#' #' obs_hysplit_control(df, control = control_file)
#' #' ff <- readLines(control_file)
#' #'
#' #' cat(ff, sep =  "\n")
#' #' }
#' #
#'
#' # dy <- dtx[ugs  == ug[j]] # assuming I have that
#'
#' # OK, we have a dataset , 1 hysplit confg related with time and co2 fluxes
#' # the CO2 fluxes (or other) must be already resampled matching footprints dimensions
#' # eg cdo, R, python, gdal, etc
#' i = 1
#'
#' ncpath = "AA"
#'
#' obs_convolve <- function(foot_path = "AAA",
#'                          name_foot = "foot1",
#'                          flat = "foot1lat",
#'                          flon = "foot1lon",
#'                          time_foot = as.POSIXct("2020-01-01 00:00:00",
#'                                                   tz = "UTC"),
#'
#'                          flux = "CTCO2", #implies bio, ocn, fossil, fire
#'                          flux_format = "%Y%m%d.nc",
#'                          fn = NULL) # function to aggregate convolved fluxes
#'
#' if(verbose) cat(paste0("Reading ", foot_path))
#'
#' nc <- ncdf4::nc_open(foot_path)
#'
#' nf1 <- grep(name_foot, names(nc$var), value = T)[1]
#'
#' foot <- ncdf4::ncvar_get(nc, nf1)
#'
#' foot1lat <- ncdf4::ncvar_get(nc, flat)
#'
#' foot1lon <- ncdf4::ncvar_get(nc, flon)
#'
#' if(verbose) {
#'
#' cf <- ncdf4::ncatt_get(nc, 0, "Conventions")$value
#'
#' if(any(grepl("CF", cf))) print(paste0("Footprint Conventions = ", cf))
#'
#' }
#'
#' ncdf4::nc_close(nc)
#'
#' # crs(foot) <- "epsg:4326"
#'
#' df_times_foot <- data.table::data.table(
#'   time_start  = rep(time_foot - (dim(foot)[3] - 1)*3600, # 239
#'                     dim(foot)[3]) # 240
#' )
#'
#'
#' df_times_foot[, seq_time_start :=  seq.POSIXt(time_start[1],
#'                                               length.out = dim(foot)[3],
#'                                               by = "1 hour")]
#'
#' # CTCO2 has three hour average file order from 1 to 8
#' df_times_foot[, hr := fifelse(
#'   hour(seq_time_start) %in% 0:(0+2), 1,
#'   fifelse(
#'     hour(seq_time_start) %in% 3:(3+2), 2,
#'     fifelse(
#'       hour(seq_time_start) %in% 6:(6+2), 3,
#'       fifelse(
#'         hour(seq_time_start) %in% 9:(9+2), 4,
#'         fifelse(
#'           hour(seq_time_start) %in% 12:(12+2), 5,
#'           fifelse(
#'             hour(seq_time_start) %in% 15:(15+2), 6,
#'             fifelse(
#'               hour(seq_time_start) %in% 18:(18+2), 7,
#'               fifelse(
#'                 hour(seq_time_start) %in% 21:(21+2), 8,
#'                 0))))))))]
#'
#' dim_names <- list(
#'   paste0("lon_", sprintf("%02d", 1:dim(foot)[1])),
#'   paste0("lat_", sprintf("%02d", 1:dim(foot)[1])),
#'   strftime(df_times_foot$seq_time_start,
#'            format = "%Y-%m-%d %H:%M:%S") # this  can vary, what if I have hourly fluxes?, or m
#' )
#'
#' dimnames(foot) <- dim_names
#'
#'
#' # convolving matching times bio
#'
#' conv_bio <- foot # to preserve same dimenisons and iterate in last index
#' conv_ocn <- foot # to preserve same dimenisons and iterate in last index
#' conv_fossil <- foot # to preserve same dimenisons and iterate in last index
#' conv_fire <- foot # to preserve same dimenisons and iterate in last index
#'
#'
#' for(j in 1:dim(foot)[3]) {
#'
#'
#'   dx_co2 <- df_co2[na == strftime(df_times_foot$seq_time_start,
#'                                   format = flux_format)[j]]
#'
#'
#'   if(verbose) cat(paste0("Reading ", strftime(df_times_foot$seq_time_start,
#'                                               format = flux_format)[j],
#'                          "\n"))
#'
#'
#'   if(verbose) cat(paste0("Reading ", dx_co2, "\n"))
#'
#'
#'     # flux
#'   dflux_co2 <- dx_co2$f
#'
#'   nc_co2 <- ncdf4::nc_open(dflux_co2)
#'
#'   names(nc_co2$var)
#'
#'   # bio
#'   flux_bio <- ncdf4::ncvar_get(nc_co2, paste0("bio_flux_opt_",
#'                                               df_times_foot$hr[j]))*nmol
#'
#'   conv_bio[,, j] <- foot[,, j]*flux_bio
#'
#'   # ocn
#'   flux_ocn <- ncdf4::ncvar_get(nc_co2, paste0("ocn_flux_opt_",
#'                                               df_times_foot$hr[j]))*nmol
#'
#'   conv_ocn[,, j] <- foot[,, j]*flux_ocn
#'
#'   # fossil
#'   flux_fossil <- ncdf4::ncvar_get(nc_co2, paste0("fossil_flux_imp_",
#'                                                  df_times_foot$hr[j]))*nmol
#'
#'   conv_fossil[,, j] <- foot[,, j]*flux_bio
#'
#'   # bio
#'   flux_fire <- ncdf4::ncvar_get(nc_co2, paste0("fire_flux_imp_",
#'                                                df_times_foot$hr[j]))*nmol
#'
#'   conv_fire[,, j] <- foot[,, j]*flux_fire
#'
#'   ncdf4::nc_close(nc_co2)
#'
#'   # print(paste0(round(j/240*100, 2), " %"))
#'
#' }
#'
#' conv_total <- conv_bio +
#'   conv_ocn  +
#'   conv_fossil +
#'   conv_fire
#'
#' if(!is.null(fn)) {
#'
#' a <- apply(conv_total, c(1,2), fn)
#' sum(a[])
#'
#' dim(a)
#' a[] <- ifelse(a[]<= 0, NA, a[])
#'
#' }
#'
#' # filled.contour(a[, 70:1])
#' # filled.contour(apply(foot, 1:2, sum)[, 70:1])
#'
#' # Do I need to create a new NetCDF?
#' # yes, with option to store full dimensions or aggregated convolved fluxes
#' unique(as.vector(foot1lat))
#'
#' lon_vals <- seq(from = 10.5, to = 79.5, by = 1)
#' lat_vals <- seq(from = -29.5, to = 39.5, by = 1)
#'
#' lon <- ncdf4::ncdim_def( "longitude", "degreesE", as.double(lon_vals))
#' lat <- ncdf4::ncdim_def( "latitude", "degreesN", as.double(lat_vals))
#'
#' time_back <- ncdf4::ncdim_def( "Time", " seconds since 1970-01-01 00:00:00 UTC",
#'                                as.numeric(df_times_foot$seq_time_start),
#'                                unlim=TRUE)
#' conv_vars <- c("total",
#'                "bio",
#'                "ocn",
#'                "fossil",
#'                "fire")
#' lv <- lapply(seq_along(conv_vars), function(l) {
#'   ncdf4::ncvar_def(name =conv_vars[l],
#'                    units = "(ppb/nanomol m-2 s-1)*nanomol m-2 s-1",
#'                    dim = list(
#'                      lon,
#'                      lat,
#'                      time_back
#'                    ))
#'
#' })
#'
#' names(lv) <- conv_vars
#'
#'
#'
#' a <- ncdf4::nc_create("convolve/test.nc",
#'                       vars = lv,
#'                       force_v4 = TRUE,
#'                       verbose = TRUE)
#'
#' for(k in seq_along(conv_vars)) {
#'
#'   ncdf4::ncvar_put(nc = a,
#'                    varid = conv_vars[k],
#'                    vals = list(conv_total,
#'                                conv_bio,
#'                                conv_ocn,
#'                                conv_fossil,
#'                                conv_fire)[[k]]
#'   )
#'
#' }
#'
#' ncdf4::nc_close(a)
#'
#'
#' rtest <- rast("convolve/test.nc",
#'               "total")
#'
#' rtest
#' names(rtest)
#' plot(sum (rtest))
#' maps::map(add = T, col = "white")

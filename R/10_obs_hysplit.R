#' obs_hysplit_control
#'
#' This function creates a CONTROL file for HYSPLIT model.
#' It uses inputs from a data.frame with the receptor information.
#'
#' @param df data.frame of receptor information.must include time and
#' spacial parameters
#' @param nlocations number of locations.
#' @param duration number of hours of release. (Negative is backwards in time).
#' @param vertical_motion Vertical motion option.
#' @param top_model_domain altitude above ground level (m).
#' @param met meteorological models to be used.
#' @param nmet Number of days for the meteorological files. Default is
#' number of days in duration plus two days
#' @param metpath paths for each meteorological model output.
#' @param ngases Default 1.
#' @param gas default "Foot".
#' @param emissions_rate Default 0
#' @param hour_emissions hour release, depend of type of release, instantaneous 0.01,
#' continuous 1.
#' @param release_start derived from df.
#' @param nsim_grids Number of simulated grids.
#' @param center_conc_grids center coordinates for conc grid.
#' @param grid_spacing grid spacing, default is 0.1 degree.
#' @param grid_span model extension in degrees by dimension.
#' @param nconc name of the concentration file, default is "cdump"
#' @param nvert_levels number of vertical levels
#' @param height_vert_levels hright vertical levels (50)
#' @param sampling_start_time 2 digits year, month, day, hour, minute
#' @param sampling_end_time  2 digits year, month, day, hour, minute
#' @param sampling_interval_type  type, hour, minure
#' @param npol_depositing number of pollutant depositing
#' @param particle_params Particle diameter, density, and shape
#' @param dmwsrdre DepVel, MW, SurfReRa, DifRa, EHenry
#' @param wrhcicbc Wet removal: HC, incloud, belowcloud
#' @param radiactive_decay days
#' @param pol_res Pollutant Resuspension (1/m)
#' @param control name of the file, default "CONTROL"
#' @return A CONTROL file
#' @export
#' @examples {
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' df <- dt[1]
#' control_file <- tempfile()
#' obs_hysplit_control(df, control = control_file)
#' cat(readLines(control_file),sep =  "\n")
#' }
#
obs_hysplit_control <- function(df,
                                nlocations = 1,
                                duration = -240,
                                vertical_motion = 0,
                                top_model_domain = 20000,
                                met = c("nams", "gfs0p25"),
                                nmet =  abs(duration/24) + 2, # 10 days plus 2, defaultdays
                                metpath = c("/work/noaa/lpdm/metfiles/nams/",
                                            "/work/noaa/lpdm/metfiles/gfs0p25"),
                                ngases = 1,
                                gas = "Foot",
                                emissions_rate = 0,
                                hour_emissions = 0.01,
                                release_start = NULL,
                                nsim_grids = 1,
                                center_conc_grids = c(0,0),
                                grid_spacing = c(0.1, 0.1),
                                grid_span = c(30, 30),
                                nconc = "cdump",
                                nvert_levels = 1,
                                height_vert_levels = 50,
                                sampling_start_time  = c(0,0,0,0,0),
                                sampling_end_time  = c(0,0,0,0,0),
                                sampling_interval_type = c(0, abs(duration), 0),
                                npol_depositing = 1,
                                particle_params = c(0,0,0),
                                dmwsrdre = c(0,0,0,0,0),
                                wrhcicbc = c(0,0,0),
                                radiactive_decay = 0,
                                pol_res = 0,
                                control = "CONTROL"){
  # lat lon height
  lat <- df$latitude
  lon <- df$longitude
  agl <- df$altitude_final

  lat <- sprintf(lat, fmt = '%#.4f')

  lon <- sprintf(lon, fmt = '%#.4f')

  agl <- sprintf(agl, fmt = '%#.4f')

  start_loc <- paste(lat, lon, agl)


  yr <- df$year
  mo <- df$month
  dy <- df$day
  ho <- df$hour
  mi <- df$minute

  rel_start <- paste(substr(yr, 3, 4), #I need to confirm
                     sprintf(mo, fmt = '%02d'),
                     sprintf(dy, fmt = '%02d'),
                     sprintf(ho, fmt = '%02d'),
                     sprintf(mi, fmt = '%02d'))

  nmodels <- length(met)


  hydate <- as.Date(ISOdate(yr, mo, dy + 1, ho, mi, 0, tz = "UTC"))

  # magick ####
  sink(control)

  cat(nlocations, "\n")

  cat(start_loc, "\n")

  cat(duration, "\n")

  cat(vertical_motion, "\n")

  cat(top_model_domain, "\n")

  cat(paste(length(met), nmet),"\n")


  for(j in seq_along(met)) {
    metx <- met[j]

    if(metx == "nams") {
      for(i in  1:nmet){
        hyd <- as.Date(hydate - i)

        cat(
          paste0(metpath[j],
                 data.table::year(hyd),
                 "/"),
          "\n")

        cat(
          paste0(strftime(hyd, format = "%Y%m%d"),
                 "_hysplit.t00z.namsa"),
          "\n")
      }
    }

    if(metx == "gfs0p25") {
      for(i in  1:nmet){
        hyd <- as.Date(hydate - i)

        cat(
          paste0(metpath[j],
                 data.table::year(hyd),
                 "/"),
          "\n")

        cat(
          paste0(strftime(hyd, format = "%Y%m%d"),
                 "_gfs0p25"),
          "\n")
      }
    }
  }

  cat(ngases, "\n")

  cat(gas, "\n")

  cat(emissions_rate, "\n")

  cat(hour_emissions, "\n")

  cat(rel_start, "\n")

  cat(nsim_grids, "\n")

  cat(center_conc_grids, "\n")

  cat(grid_spacing, "\n")

  cat(grid_span, "\n")

  cat("./\n")

  cat(nconc, "\n")

  cat(nvert_levels, "\n")

  cat(height_vert_levels, "\n")

  cat(sampling_start_time, "\n")

  cat(sampling_end_time, "\n")

  cat(sampling_interval_type, "\n")

  cat(npol_depositing, "\n")

  cat(particle_params, "\n")

  cat(dmwsrdre, "\n")

  cat(wrhcicbc, "\n")

  cat(radiactive_decay, "\n")

  cat(pol_res, "\n")

  sink()

}



#' obs_hysplit_setup
#'
#' This function creates a SETUP.CFG file for HYSPLIT model.
#'
#' @param idsp particle dispersion scheme 1:HYSPLIT 2:STILT
#' @param capemin -1 no convection; -2 Grell convection scheme;
#' -3 extreme convection; >0 enhanced vertical mixing when CAPE exceeds this value (J/kg)
#' @param vscales vertical Lagrangian time scale (sec) for stable PBL
#' @param kbls boundary layer stability derived from 1:fluxes 2:wind_temperature
#' @param kblt  boundary layer turbulence parameterizations 1:Beljaars 2:Kanthar
#'  3:TKE 4:Measured 5:Hanna
#' @param kmixd mixed layer obtained from 0:input 1:temperature 2:TKE 3:modified Richardson
#' @param initd initial distribution, particle, puff, or combination
#' @param veght Height below which particle's time is spent is tallied to
#' calculate footprint for PARTICLE_STILT.DAT less than or equal to 1.
#' 0: fraction of PBL height; greater than 1.0: heightAGL (m)
#' @param kmix0 minimum mixing depth
#' @param numpar number of puffs or particles to released per cycle
#' @param maxpar maximum number of particles carried in simulation
#' @param ichem chemistry conversion modules 0:none 1:matrix 2:convert 3:dust
#' 4: conc grid equal to met grid, 5: divide output mass by air density (kg/m3)
#' to sum as mixing ration, 7: transport deposited particles on the ocean surface,
#' 8: stilt mode mixing ratio and varying layer,
#' 9: set concentration layer one to a fraction of the boundary layer,
#' 10: restructure concentration grid into time-varying transfer matrix,
#' 11: enable daughter produyct calculation.
#' @param krand 0 method to calculate random number
#' 0=precompute if NUMPAR greater than 5000 or dynamic if NUMPAR less than or equal to 5000;
#' 1=precalculated; 2=calculated in pardsp;
#' 3=none; 4=random initial seed number and calculated in pardsp;
#' 10=same as 0 with random initial seed for non-dispersion applications;
#' 11=same as 1 with random initial seed for non-dispersion applications;
#' 12=same as 2 with random initial seed for non-dispersion applications;
#' 13=same as 3 with random initial seed for non-dispersion applications
#' @param ivmax 0 number of variables written to PARTICLE_STILT.DAT. Must
#' equal the number of variables listed for variable VARSIWANT
#' @param varsiwant ='TIME','INDX','LONG','LATI','ZAGL','SIGW','TLGR','ZSFC', TEMP',
#' 'SAMT','FOOT','SHTF','DMAS','DENS','RHFR','SPHU','DSWF','WOUT','MLHT','PRES'
#' variables written to PARTICLE_STILT.DAT.
#'
#' However, the default in this case is:
#' c('time','indx', 'lati','long','zagl', 'zsfc','foot','samt', 'temp',
#' 'dswf','mlht','dens','dmas','sigw','tlgr'
#' @param outdt  Default 15. defines the output frequency in minutes of the endpoint positions
#' in the PARTICLE.DAT file when the STILT emulation mode is configured.
#' The default value of 0 results in output each time step while the positive
#' value gives the output frequency in minutes. A negative value disables output. The
#' output frequency should be an even multiple of the time step and be evenly
#' divisible into 60. In STILT emulation mode, the time step is forced to one minute.
#' @param extra_params more parameters
#' @param setup Default SETUP.CFG
#' @return A SETUP.CFG file
#' @note The var description comes from hysplit 5.3 manual page 214
#' @export
#' @examples {
#' # Do not run
#' setup_file <- tempfile()
#' obs_hysplit_setup(setup = setup_file)
#' cat(readLines(setup_file),sep =  "\n")
#' }
#
obs_hysplit_setup <- function(idsp = 2,
                              capemin = 500,
                              vscales = -1.0,
                              kbls = 1,
                              kblt = 5,
                              kmixd = 0,
                              initd = 0,
                              veght = 0.5,
                              kmix0 = 150,
                              numpar = 500,
                              maxpar = 500,
                              ichem = 8,
                              krand = 4,
                              varsiwant = c('time','indx',
                                            'lati','long','zagl',
                                            'zsfc','foot','samt',
                                            'temp','dswf','mlht','dens',
                                            'dmas','sigw','tlgr'),
                              ivmax = length(varsiwant),
                              outdt = 15,
                              extra_params,
                              setup = "SETUP.CFG"){

  sink(setup)

  cat("&SETUP\n")

  cat(paste0(" idsp = ", idsp, ",\n"))

  cat(paste0(" capemin = ", capemin, ",\n"))

  cat(paste0(" vscales = ", sprintf("%#.1f", vscales), ",\n"))

  cat(paste0(" kbls = ", kbls, ",\n"))

  cat(paste0(" kblt = ", kblt, ",\n"))

  cat(paste0(" kmixd = ", kmixd, ",\n"))

  cat(paste0(" initd = ", initd, ",\n"))

  cat(paste0(" veght = ", veght, ",\n"))

  cat(paste0(" kmix0 = ", kmix0, ",\n"))

  cat(paste0(" numpar = ", numpar, ",\n"))

  cat(paste0(" maxpar = ", maxpar, ",\n"))

  cat(paste0(" ichem = ", ichem, ",\n"))

  cat(paste0(" krand = ", krand, ",\n"))

  cat(paste0(" ivmax = ", ivmax, ",\n"))

  cat(" varsiwant=")

  cat(sQuote(varsiwant, q = ""), sep = ",")

  cat(",\n")

  cat(paste0(" outdt = ", outdt, ",\n"))

  if(!missing(extra_params)) {

    for(i in seq_along(extra_params)) {
      cat(paste0(" ",
          eval(parse(text = extra_params[i])),
          " = ",
          extra_params[i],
          ",\n"))
    }
  }
  cat(" /\n")

  sink()

}


#' obs_hysplit_ascdata
#'
#' This function creates a ASCDATA.CFG file for HYSPLIT model.
#'
#' @param llc Lower left corner, default c(-90.0, -180.0)
#' @param spacing spacing in degress, default c(1.0, 1.0)
#' @param n number of data points, default c(180, 360)
#' @param landusecat land use category, default 2
#' @param rough default roughness length (meters), default 0.2
#' @param bdyfiles  directory location of the data files,
#' default '../bdyfiles/'
#' @param ascdata file, default ASCDATA.CFG
#' @return A ASCDATA.CFG file
#' @export
#' @examples {
#' # Do not run
#' ascdata_file <- tempfile()
#' obs_hysplit_ascdata(ascdata = ascdata_file)
#' cat(readLines(ascdata_file), sep =  "\n")
#' }
#
obs_hysplit_ascdata <- function(llc =  c(-90.0, -180.0),
                                spacing = c(1.0, 1.0),
                                n = c(180, 360),
                                landusecat = 2,
                                rough = 0.2,
                                bdyfiles = '../bdyfiles/',
                                ascdata = "ASCDATA.CFG"){

  sink(ascdata)

  cat(sprintf("%#.1f", llc[1]))

  cat("  ")

  cat(sprintf("%#.1f", llc[2]))

  cat(" lat/lon of lower left corner (last record in file)\n")


  cat(sprintf("%#.1f", spacing[1]))

  cat("     ")

  cat(sprintf("%#.1f", spacing[2]))

  cat("    lat/lon spacing in degrees between data points\n")


  cat(n, sep = "     ")

  cat("    lat/lon number of data points\n")

  cat(landusecat)

  cat("              default land use category\n")

  cat(rough)

  cat("            default roughness length (meters)\n")

  cat(sQuote(bdyfiles, q = ""))

  cat(" directory location of data files\n")

  sink()
}

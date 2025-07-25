#' obs_convolve
#'
#' This function returns a arrays (or list of arrays) of convolved footpritns with flux
#'
#' @param foot_path path for footprint (length 1).
#' @param name_foot name of the footprint variable in the NetCDF file.
#' @param flon name of lons
#' @param flat name of lats
#' @param time_foot Time of the footprints (in the name file) or third dimension
#' @param flux String, default NOAA "CTCO2". Implies bio, ocn, fossil, fire.
#' @param df_flux data.table with columns f (full path) and nf file name 'Ymd.nc'
#' @param flux_format string with date format 'Ymd.nc'
#' @param factor number to multiply fluxes.
#' @param fn string with function to aggregate convolved fluxes, e.g. `mean`, `sum`, `max`, etc.
#' @param as_list Logical, to return list of arrays
#' @param verbose Logical, to display more information
#' @note main assumption is that fluxes have same spatial dimensions as footprints
#' @export
#' @import ncdf4 data.table
#' @examples {
#' # obs_convolve(...)
#' }
obs_convolve <- function(foot_path = "AAA",
                         name_foot = "foot1",
                         flon = "foot1lon",
                         flat = "foot1lat",
                         time_foot,
                         flux = "CTCO2",
                         df_flux,
                         flux_format = "%Y%m%d.nc",
                         factor = 1e9,
                         fn = NULL,
                         as_list = FALSE,
                         verbose = TRUE){

  # footprint information

  if(length(foot_path) != 1) {
    stop("foot_path must be a single string")
  }

  if(verbose) cat(paste0("Reading ", foot_path, "\n"))


  if(missing(df_flux)) stop("df_flux is missing, it must be a data.table with columns `f` and `nf`")

  if(!inherits(df_flux, "data.table")) {
    stop("df_flux must be a data.table with columns `f` (full path) and `nf` (file name)")
  }

  if(!any("f" %in% names(df_flux))) {
    stop("df_flux must be a data.table with columns `f` (full path) and `nf` (file name)")
  }


  if(!any("nf" %in% names(df_flux))) {
    stop("df_flux must be a data.table with columns `f` (full path) and `nf` (file name)")
  }

  nc <- ncdf4::nc_open(foot_path)

  if(verbose) {
    cf <- ncdf4::ncatt_get(nc, 0, "Conventions")$value
    if(any(grepl("CF", cf))) print(paste0("Footprint Conventions = ", cf))
  }

  if(any(name_foot %in% names(nc$var))){
    if(verbose) cat(paste0("Reading ",
                           name_foot,
                           "\n"))
    foot <- ncdf4::ncvar_get(nc, name_foot)
  } else {
    stop(paste0("Variable ", name_foot, " not found among: ", names(nc$var)))
  }


  foot1lat <- ncdf4::ncvar_get(nc, flat)
  foot1lon <- ncdf4::ncvar_get(nc, flon)

  ncdf4::nc_close(nc)

  # footprint times

  df_times_foot <- data.table::data.table(
    time_start  = rep(time_foot,
                      dim(foot)[3]) # 240
  )

  seq_time_start <- time_start <- nf <- hr <- NULL
  df_times_foot[, seq_time_start :=  seq.POSIXt(time_start[1],
                                                length.out = dim(foot)[3],
                                                by = "-1 hour")]
  # first time is the footprint time, ;last time, 240 hours in the past.

  if(verbose){
    cat("footprints, from ",
        strftime(df_times_foot$seq_time_start[1],
                 format = "%Y-%m-%d %H:%M:%S"),
        " to ",
        strftime(df_times_foot$seq_time_start[dim(foot)[3]],
                 format = "%Y-%m-%d %H:%M:%S"), "\n")
  }

  dim_names <- list(
    paste0("lon_", sprintf("%02d", 1:dim(foot)[1])),
    paste0("lat_", sprintf("%02d", 1:dim(foot)[2])),
    strftime(df_times_foot$seq_time_start,
             format = "%Y-%m-%d %H:%M:%S") # this  can vary, what if I have hourly fluxes?, or m
  )

  dimnames(foot) <- dim_names

  # flux information
  if(flux == "CTCO2") {

    # CTCO2 has three hour average file order from 1 to 8
    df_times_foot[, hr := data.table::fifelse(
      data.table::hour(seq_time_start) %in% 0:(0+2), 1,
      data.table::fifelse(
        data.table::hour(seq_time_start) %in% 3:(3+2), 2,
        data.table::fifelse(
          data.table::hour(seq_time_start) %in% 6:(6+2), 3,
          data.table::fifelse(
            data.table::hour(seq_time_start) %in% 9:(9+2), 4,
            data.table::fifelse(
              data.table::hour(seq_time_start) %in% 12:(12+2), 5,
              data.table::fifelse(
                data.table::hour(seq_time_start) %in% 15:(15+2), 6,
                data.table::fifelse(
                  data.table::hour(seq_time_start) %in% 18:(18+2), 7,
                  data.table::fifelse(
                    data.table::hour(seq_time_start) %in% 21:(21+2), 8,
                    0))))))))]


    # convolving matching times bio

    conv_bio <- foot # to preserve same dimensions and iterate in last index
    conv_ocn <- foot # to preserve same dimensions and iterate in last index
    conv_fossil <- foot # to preserve same dimensions and iterate in last index
    conv_fire <- foot # to preserve same dimensions and iterate in last index


    for(j in 1:dim(foot)[3]) {

      dx_f <- df_flux[nf == strftime(df_times_foot$seq_time_start,
                                     format = flux_format)[j]]


      # flux
      f <- dx_f$f

      if(verbose) cat(paste0("Reading ", f, "\n"))

      nc_f <- ncdf4::nc_open(f)


      # bio
      flux_bio <- ncdf4::ncvar_get(nc_f, paste0("bio_flux_opt_",
                                                df_times_foot$hr[j]))*factor

      conv_bio[,, j] <- foot[,, j]*flux_bio

      # ocn
      flux_ocn <- ncdf4::ncvar_get(nc_f, paste0("ocn_flux_opt_",
                                                df_times_foot$hr[j]))*factor

      conv_ocn[,, j] <- foot[,, j]*flux_ocn

      # fossil
      flux_fossil <- ncdf4::ncvar_get(nc_f, paste0("fossil_flux_imp_",
                                                   df_times_foot$hr[j]))*factor

      conv_fossil[,, j] <- foot[,, j]*flux_fossil

      # bio
      flux_fire <- ncdf4::ncvar_get(nc_f, paste0("fire_flux_imp_",
                                                 df_times_foot$hr[j]))*factor

      conv_fire[,, j] <- foot[,, j]*flux_fire

      ncdf4::nc_close(nc_f)

      # print(paste0(round(j/240*100, 2), " %"))

    }

    conv_total <- conv_bio +
      conv_ocn  +
      conv_fossil +
      conv_fire

    if(!is.null(fn)) {

      conv_total <- apply(conv_total, c(1,2), fn)
      conv_bio <- apply(conv_bio, c(1,2), fn)
      conv_ocn <- apply(conv_ocn, c(1,2), fn)
      conv_fossil <- apply(conv_fossil, c(1,2), fn)
      conv_fire <- apply(conv_fire, c(1,2), fn)
      foot <- apply(foot, c(1,2), fn)
    }


    if(!as_list)  {
      simplify2array(list(
        foot = foot,
        conv_total = conv_total,
        conv_bio = conv_bio,
        conv_ocn = conv_ocn,
        conv_fossil = conv_fossil,
        conv_fire = conv_fire
      )) -> out
      return(out)
    } else{
      return(list(
        foot = foot,
        conv_total = conv_total,
        conv_bio = conv_bio,
        conv_ocn = conv_ocn,
        conv_fossil = conv_fossil,
        conv_fire = conv_fire,
        lat = foot1lat,
        lon = foot1lon,
        time = if(!missing(fn)) time_foot else df_times_foot$seq_time_start
      ))

    }

  }
}

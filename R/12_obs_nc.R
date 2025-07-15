#' obs_nc
#'
#' Creates NetCDF based on dimension from another NetCDF
#' with custom dimensions, and attributtes
#'
#' @param nc_path String pointing to the target NetCDF
#' @param nc_name String indicating the spatial array
#' @param nc_lat String to extract latitude.
#' @param nc_lon String to extract longitude
#' @param time_nc Time of the NetCDF (length 1).
#' @param vars_out names for the variables to be created in the NetCDF.
#' @param units units for the NetCDF variables to be created.
#' @param nc_out path for the created NetCDF.
#' @param larrays list of arrays, length equal to vars_out.
#' @param verbose Logical, to display more information.
#' @return A NetCDF file with the custom attributes and units
#' @export
#' @examples {
#' # Do not run
#' }
obs_nc <- function(nc_path = "AAA",
                   nc_name = "foot1",
                   nc_lat = "foot1lat",
                   nc_lon = "foot1lon",
                   time_nc = Sys.time(),
                   vars_out = c("total",
                            "bio",
                            "ocn",
                            "fossil",
                            "fire"),
                   units_out = "(ppb/nanomol m-2 s-1)*nanomol m-2 s-1",
                   nc_out,
                   larrays,
                   verbose = FALSE
){ # function to aggregate convolved fluxes

  if(length(vars_out) != length(larrays)) {
    stop("Length of vars_out must be equal to length of larrays")
  }

  if(length(nc_path) != 1) {
    stop("nc_path must be a single string")
  }

  if(length(nc_name) != 1) {
    stop("nc_name must be a single string")
  }
  if(length(nc_lat) != 1) {
    stop("nc_lat must be a single string")
  }
  if(length(nc_lon) != 1) {
    stop("nc_lon must be a single string")
  }
  if(length(time_nc) != 1) {
    stop("time_nc must be a single value")
  }

  if(verbose) cat(paste0("Reading ", nc_path))

  nc <- ncdf4::nc_open(nc_path)

  nf1 <- grep(nc_name, names(nc$var), value = T)[1]

  foot <- ncdf4::ncvar_get(nc, nf1) # here we have the dimensions

  foot1lat <- ncdf4::ncvar_get(nc, nc_lat)

  foot1lon <- ncdf4::ncvar_get(nc, nc_lon)

  if(verbose) {

    cf <- ncdf4::ncatt_get(nc, 0, "Conventions")$value

    if(any(grepl("CF", cf))) print(paste0("Footprint Conventions = ", cf))

  }

  ncdf4::nc_close(nc)

  # lon_vals <- seq(from = 10.5, to = 79.5, by = 1)
  # lat_vals <- seq(from = -29.5, to = 39.5, by = 1)

  lon <- ncdf4::ncdim_def( "longitude", "degreesE", as.double(foot1lat))
  lat <- ncdf4::ncdim_def( "latitude", "degreesN", as.double(foot1lon))

  time_back <- ncdf4::ncdim_def( "Time", " seconds since 1970-01-01 00:00:00 UTC",
                                 as.numeric(time_nc),
                                 unlim=TRUE)

  lv <- lapply(seq_along(vars_out), function(l) {
    ncdf4::ncvar_def(name = vars_out[l],
                     units = units_out,
                     dim = list(
                       lon,
                       lat,
                       time_back
                     ))

  })

  names(lv) <- vars_out

  a <- ncdf4::nc_create(nc_out,
                        vars = lv,
                        force_v4 = TRUE,
                        verbose = TRUE)

  for(k in seq_along(vars)) {

    ncdf4::ncvar_put(nc = a,
                     varid = vars_out[k],
                     vals = larrays[[k]]
    )

  }

  g_atributos  <- ncdf4::ncatt_get(a, 0)
  g_atributos  <- c( list(TITLE = "rtorf::obs_nc",
                          History = paste("created on",
                                          format(Sys.time(),
                                                 "%Y-%m-%d at %H:%M")),
                          Author = paste0("R package rtorf v",
                                          utils::packageVersion("rtorf"))),
                     g_atributos[4:length(g_atributos)])

  for(i in 1:length(g_atributos)){
    ncdf4::ncatt_put(a,
                     varid = 0,
                     attname = names(g_atributos)[i],
                     attval = g_atributos[[i]])
  }


  ncdf4::nc_close(a)


}

#' Generate files to perform inverse modeling
#'
#' This function return a list of three elements.
#'
#' 1. Footprints_hera_hysplit with the  full path to the NetCDF footprints\
#'
#' 2. Obs_hysplit concentration associazted iwth each receptor
#'
#' 3. Receptor_info_hysplit with receptor info
#'
#' @param master data.table master file after filtering obspack
#' @param nc column name of master which indicate NetCDF footprints
#' @param path first part of the full path of the .nc footprint files
#' @param value column name with the pollutant
#' @param value_factor numeric factor
#' @param Type "continuous", "flask" or "hatsflask"
#' @param SubType "aircraft-insitu", "tower-insitu", "surface-insitu" and so on
#' @param Surface_Elev Site elevation, default 99999
#' @param Model_agl Model for agl, if missing is column `altitude_final`
#' @param Data_agl Model for agl, if missing is column `altitude_final`
#' @param Event Number of event
#' @param Institution Institution
#' @param Scale Scale
#' @param outdir String for the output dir
#' @return A list of data.frame
#' @export
#' @examples \dontrun{
#' # do not run
#' }
obs_invfiles <- function(master,
                         nc = "nc",
                         path,
                         value = "value",
                         value_factor = 1e+9,
                         Type = "continuous",
                         SubType = "tower-insitu",
                         Surface_Elev = 99999,
                         Model_agl,
                         Data_agl,
                         Event = 99999,
                         Institution,
                         Scale,
                         outdir) {

  # check existing files and then merge
  x <- list.files(path = path,
                  pattern = ".nc",
                  full.names = TRUE,
                  recursive = TRUE)
  hs <- pol <- NULL
  if(!nc %in% names(master)) stop(paste0("Need column ", nc))
  master$hs <- paste0(path, master$nc)

  if(!value %in% names(master)) stop(paste0("Need column ", value))
  master$pol <- master[[value]]*value_factor

  # receptor_info_hysplit
  master$File <- substr(x = master[[nc]],
                        start = 9,
                        stop = 100)

  master$Type <- Type

  master$SubType <- SubType

  Model <- "hysplit"
  master$Model <- Model

  if(!"timeUTC" %in% names(master)) stop("Need column 'timeUTC'")

  master$Date <- as.Date(master$timeUTC,
                         tz = "UTC")

  master$Time <- strftime(master$timeUTC,
                          tz = "UTC",
                          format = "%H:%M:%S")

  if(!"latitude" %in% names(master)) stop("Need column 'latitude'")
  master$Latitude <- sprintf(master$latitude,
                             fmt = '%07.6f')


  if(!"longitude" %in% names(master)) stop("Need column 'longitude'")
  master$Longitude <- formatC(sapply(master$longitude,
                                     obs_trunc,
                                     6),
                              digits = 6,
                              width = 8,
                              format = "f",
                              flag = "0")

  if(!"altitude_final" %in% names(master)) stop("Need column 'altitude_final'")
  master$Alt <- sprintf(master$altitude_final,
                        fmt = '%07.6f')


  if(!"type_altitude" %in% names(master)) stop("Need column 'type_altitude'")
  master$Alt_Type  <- ifelse(master$type_altitude == 0,
                             "agl",
                             "asl")

  if(!"site_code" %in% names(master)) stop("Need column 'site_code'")
  master$Site <- master$site_code


  master$Surface_Elev <- Surface_Elev


  if(missing(Model_agl)){
    cat("Assuming `Model_agl` as altitude\n")
    master$Model_agl <- master$Alt
  } else {
    master$Model_agl <- Surface_Elev
  }

  if(missing(Model_agl)){
    cat("Assuming `Data_agl` as altitude\n")
    master$Data_agl <- master$Alt
  } else {
    master$Data_agl <- Data_agl
  }

  master$Event <- Event

  if(!"time_decimal" %in% names(master)) stop("Need column 'time_decimal'")
  master$DecYr <- master$time_decimal

  if(!"lab_1_abbr" %in% names(master)) stop("Need column 'lab_1_abbr'")
  master$Institution <- master$lab_1_abbr


  if(!"dataset_calibration_scale" %in% names(master)) {
    warning("Need column 'dataset_calibration_scale'. Assuming unknown")
    master$Scale <- "unknown"
  } else {
    master$Scale <- master$dataset_calibration_scale
  }

  lx <- list(
    footprints_hera_hysplit = master[["hs"]],
    obs_hysplit = master[["pol"]],
    receptor_info_hysplit =  master[,
                                    c("File",
                                      "Type",
                                      "SubType",
                                      "Model",
                                      "Date",
                                      "Time",
                                      "Latitude",
                                      "Longitude",
                                      "Alt",
                                      "Alt_Type",
                                      "Site",
                                      "Surface_Elev",
                                      "Model_agl",
                                      "Data_agl",
                                      "Event",
                                      "DecYr",
                                      "Institution",
                                      "Scale")],
    master = master
  )

  class(lx) <- append(class(lx), "invfile")

  if(missing(outdir)) {
    return(lx)
  } else {
    data.table::fwrite(x = lx$footprints_hera_hysplit,
                       file = paste0(outdir,
                                     "/footprints_hera_hysplit.txt"),
                       row.names = FALSE,
                       quote = FALSE)
    cat(paste0(outdir, "/footprints_hera_hysplit.txt"))

    data.table::fwrite(x = lx$obs_hysplit,
                       file = paste0(outdir,
                                     "/obs_hysplit.txt"),
                       row.names = FALSE,
                       quote = FALSE)
    cat(paste0(outdir, "/obs_hysplit.txt"))

    data.table::fwrite(x = lx$receptor_info_hysplit,
                       file = paste0(outdir,
                                     "/receptor_info_hysplit.txt"),
                       row.names = FALSE,
                       quote = FALSE)
    cat(paste0(outdir, "/receptor_info_hysplit.txt"))

    return(lx)

  }
}

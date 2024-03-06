#' Obspack Table
#'
#' This function reads the obsPack directory
#' providing a summary for the columns: "value", "time",
#' "time_decimal", "latitude" and "longitude". These summary
#' are made by the columns "name", "sector", "site_name",
#' "site_country", "type_altitude", "lab_1_abbr" and
#' "site_utc2lst"
#'
#' @param index data.table
#' @param categories character; ONE category
#' : of c("aircraft-pfp", "aircraft-insitu",
#' "surface-insitu", "tower-insitu", "aircore", "surface-pfp", "shipboard-insitu",
#' "flask").
#' @param verbose Logical to show more information
#' @param n_site_code number of characters extraced from metadata after search
#' @param n_site_lat number of characters for latitude
#' @param n_site_lon number of characters for longitude
#' @param n_site_name number of characters extraced from metadata after search
#' @param n_site_country number of characters extraced from metadata after search
#' @param n_dataset_project number of characters extraced from metadata after search
#' @param n_lab number of characters extraced from metadata after search
#' @param n_scales number of characters extraced from metadata after search
#' @param n_site_elevation number of characters extraced from metadata after search
#' @param n_altitude_comment number of characters extraced from metadata after search
#' @param n_utc number of characters extraced from metadata after search
#' @param fill_value fill value. Appeared in aoa_aircraft-flask_19_allvalid.txt
#' @return A data.frame with with an index obspack.
#' @importFrom data.table fwrite ".N" ":=" rbindlist "%chin%"
#' @export
#' @note Categories are defined in `obs_summary`. If aircraft* is used,
#' then `obs_table` will calculate stats for altitude_final.
#' @examples {
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs, "aircraft-flask")
#' dx <- obs_table(index, categories = "aircraft-flask")
#' }
obs_table <- function(index,
                      categories = "tower-insitu",
                      verbose = FALSE,
                      n_site_code = 15,
                      n_site_lat = 18,
                      n_site_lon = 19,
                      n_site_name = 15,
                      n_site_country = 18,
                      n_dataset_project = 21,
                      n_lab = 16,
                      n_scales = 31,
                      n_site_elevation = 20,
                      n_altitude_comment = 22,
                      n_utc = 18,
                      fill_value = -1e+34){


  # categories = "tower-insitu"
  # verbose = TRUE
  # n_site_code = 15
  # n_site_name = 15
  # n_site_country = 18
  # n_dataset_project = 21
  # n_lab = 16
  # n_scales = 31
  # n_site_elevation = 20
  # n_altitude_comment = 22
  # n_utc = 18
  # fill_value = -1e+34

  if(nrow(index) == 0) stop("empty index")

  if(verbose) cat(paste0("Searching ", categories, "...\n"))
  sector <- NULL
  df <- index[sector %chin% categories]
  if(nrow(df) == 0) {
    stop(paste0("I did not find any ", categories))
  }

  x1 <- df$id

  lapply(seq_along(x1), function(i) {

    agl <- df$agl[i]
    n <- df$n

    if(verbose) cat(paste0(i, ": ", df$name[i], "\n"))

    nr <- data.table::fread(x1[i], nrows = 1)$V4

    att <- readLines(x1[i], n = nr)

    # site code ####
    pattern <- grep(pattern = "site_code",
                    x = att,
                    value = T)

    # everything after char 15
    site_code <- substr(x = pattern,
                        start = n_site_code,
                        stop = nchar(pattern))

    # site_name ####
    pattern <- grep(pattern = "site_name",
                    x = att,
                    value = T)

    # everything after char 15
    site_name <- substr(x = pattern,
                        start = n_site_name,
                        stop = nchar(pattern))

    # site_country ####
    pattern <- grep(pattern = "site_country",
                    x = att,
                    value = T)[1]

    # everything after char 18
    site_country <- substr(x = pattern,
                           start = n_site_country,
                           stop = nchar(pattern))

    # site_latitude ####
    pattern <- grep(pattern = "site_latitude",
                    x = att,
                    value = T)[1]

    # everything after char 18
    (site_latitude <- substr(x = pattern,
                             start = n_site_lat,
                             stop = nchar(pattern)))




    # site_longitude ####
    pattern <- grep(pattern = "site_longitude",
                    x = att,
                    value = T)[1]

    # everything after char 18
    (site_longitude <- substr(x = pattern,
                              start = 19,
                              stop = nchar(pattern)))



    # dataset_project ####
    pattern <- grep(pattern = "dataset_project",
                    x = att,
                    value = T)[1]

    # everything after char 21
    dataset_project <- substr(x = pattern,
                              start = n_dataset_project,
                              stop = nchar(pattern))

    # lab_1_abbr ####
    pattern <- grep(pattern = "lab_1_abbr",
                    x = att,
                    value = T)[1]

    # everything after char 16
    (lab_1_abbr <- substr(x = pattern,
                          start = n_lab,
                          stop = nchar(pattern)))


    # dataset_calibration_scale ####
    pattern <- grep(pattern = "dataset_calibration_scale",
                    x = att,
                    value = T)[1]

    # everything after char 21
    (dataset_calibration_scale <- substr(x = pattern,
                                         start = n_scales,
                                         stop = nchar(pattern)))



    # elevation ####
    pattern <- grep(pattern = " site_elevation",
                    x = att,
                    value = T)[1]

    # everything after char 16
    (site_elevation <- substr(x = pattern,
                              start = n_site_elevation,
                              stop = nchar(pattern)))


    # altitude:comment ####
    pattern <- grep(pattern = " altitude:comment",
                    x = att,
                    value = T)

    # everything after char 16
    (altitude_comment <- substr(x = pattern,
                                start = n_altitude_comment,
                                stop = nchar(pattern)))

    # # altitude:provider_comment ####
    # pattern <- grep(pattern = " altitude:provider_comment",
    #                 x = att,
    #                 value = T)
    #
    # # everything after char 16
    # (altitude_provider_comment <- substr(x = pattern,
    #                                      start = 31,
    #                                      stop = nchar(pattern)))

    # site_utc2lst
    (pattern <- grep(pattern = "site_utc2lst",
                     x = att,
                     value = T)[1])
    # print(pattern)

    # everything after char 16
    (site_utc2lst <- as.numeric(substr(x = pattern,
                                       start = n_utc,
                                       stop = nchar(pattern))))


    names_df <- data.table::fread(x1[i],
                                  skip = nr - 1,
                                  nrows = 1,
                                  h = FALSE)

    err <- tryCatch(data.table::fread(x1[i],
                                      skip = nr - 1),
                    error = function(e) e,
                    warning = function(w) w )

    if(inherits(err, "warning")) {
      message("Found warnings")
      print(class(err))
      dt <- data.table::fread(x1[i],
                              skip = nr - 1,
                              header = FALSE)

      dif_names <- ncol(dt) - length(names_df)

      names(dt) <- c(unlist(names_df),
                     paste0("newcol",1:length(dif_names)))

      if(verbose) message(paste0("\nAdded ",
                                 length(dif_names),
                                 " columns on", x1[i], "\n"))
      print(warnings())
    } else {
      dt <- data.table::fread(x1[i],
                              skip = nr - 1)
    }

    dt$name <- df$name[i]

    dt$sector <- categories
    # dt$id_by_site_code <- 1:nrow(dt)
    dt$site_code <- site_code
    dt$site_name <- site_name
    dt$site_country <- site_country
    dt$site_latitude <- site_latitude
    dt$site_longitude <- site_longitude
    dt$site_elevation <- site_elevation
    dt$dataset_project <- dataset_project
    dt$lab_1_abbr <- lab_1_abbr
    dt$dataset_calibration_scale <- dataset_calibration_scale
    dt$altitude_comment <- rep(altitude_comment, nrow(dt))
    dt$site_utc2lst <- site_utc2lst
    dt$agl <- agl
    df$n <- n
    #if agl is NA, agl = altitude - elevation
    #if elevation is fill_value, NA
    #if altitude - elevation = NA, use altitude => asl

    dt$site_elevation <- ifelse(dt$site_elevation == fill_value,
                                NA,
                                dt$site_elevation)

    agl <- NULL
    site_elevation <- NULL
    altitude <- NULL
    dt[is.na(agl),
       agl :=  altitude - as.numeric(site_elevation)]

    dt$type_altitude <- ifelse(is.na(dt$agl),
                               1, # masl
                               0) # magl

    dt$altitude_final <- ifelse(is.na(dt$agl),
                                dt$altitude,
                                dt$agl)

    dt$type_altitude <- ifelse(is.na(dt$altitude_final),
                               "not available",
                               dt$type_altitude)
    dt$id <- i
    .SD <- . <- name <- altitude_final <- NULL

    if(length(grep("aircraft", categories)) > 0) {
      dt[,
         lapply(.SD, summary, na.rm = TRUE),
         by = .(name,
                sector,
                site_name,
                site_latitude,
                site_longitude,
                site_country,
                site_code,
                type_altitude,
                lab_1_abbr,
                site_utc2lst),
         .SDcols = c("value",
                     "time",
                     "time_decimal",
                     "latitude",
                     "longitude",
                     "altitude_final"
         )]-> dxx

      dxx$time <- as.integer(dxx$time)


      dxx$timeUTC <- as.POSIXct(dxx$time,
                                origin = "1970-01-01",
                                tz = "UTC")

      dxx$stat <- rep(c("min",
                        "q1",
                        "median",
                        "mean",
                        "q3",
                        "max"),
                      nrow(dxx)/6)

    } else {
      dt[, lapply(.SD, summary, na.rm = TRUE),
         by = .(name,
                sector,
                site_name,
                site_latitude,
                site_longitude,
                site_country,
                site_code,
                altitude_final,
                type_altitude,
                lab_1_abbr,
                site_utc2lst),
         .SDcols = c("value",
                     "time",
                     "time_decimal",
                     "latitude",
                     "longitude"
         )]-> dxx

      dxx$time <- as.integer(dxx$time)


      dxx$timeUTC <- as.POSIXct(dxx$time,
                                origin = "1970-01-01",
                                tz = "UTC")

      dxx$stat <- rep(c("min",
                        "q1",
                        "median",
                        "mean",
                        "q3",
                        "max"),
                      nrow(dxx)/6)

    }


    dxx

  }) -> lx


  unames <- unique(unlist(sapply(lx, names)))

  #add names
  for(i in 1:length(lx)) {
    ly <- obs_out(names(lx[[i]]), unames)
    if(length(ly) > 0) {
      for(j in seq_along(ly)) {
        lx[[i]][[ly[j]]] <- NA
      }
    }
  }

  type_altitude <- NULL
  .N <- NULL
  dt <- data.table::rbindlist(lx, use.names = T)
  # if(verbose) cat("Adding id\n")
  # dt$id <- 1:nrow(dt)
  # print(dt[, .N, by = type_altitude])
  return(dt)


}

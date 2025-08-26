#' Helpers Legacy
#'
#'
#' @title obs_id2pos
#' @family helpers legacy
#' @name obs_id2pos
#' @description return ndata.frame based on footprint/receptor id
#' @param id string
#' @param sep string
#' @param asdf Logical, to return as data.frame or not
#' @return data.frame with time and location based on input
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' id <- '2002x08x03x10x45.00Nx090.00Ex00030'
#' (obs_id2pos(id, asdf = TRUE) -> dx)
#' id <- c('2002x08x03x10x00x45.000Nx090.000Ex00030',
#'         '2002x08x03x10x55x45.335Sx179.884Wx00030')
#' (obs_id2pos(id) -> dx)
#' (obs_id2pos(id, asdf = TRUE) -> dx)
#' (obs_id2pos(rep(id, 2)) -> dx)
#' (obs_id2pos(rep(id, 2), asdf = TRUE) -> dx)
#' }
#' }
obs_id2pos <- function(id, sep = "x", asdf = FALSE) {
  # revers of id2pos function to read identifying label for
  # multiple receptors (location&time) returns time as fractional
  # julian day since 1/1/1960 and alt as altitude above ground in
  # meters returns a 2d array for multiple receptors, a vector for
  # a single receptor examples:
  # id2pos('2002x08x03x10x45.00Nx090.00Ex00030') time lat lon alt
  # yr mon day hr min 15555.42 45.00 90.00 30.00 2002 08 03 10 00
  # id2pos(c('2002x08x03x10x00x45.000Nx090.000Ex00030','2002x08x03x10x55x45.335Sx179.884Wx00030'))
  # time lat lon alt yr mon day hr min 15555.42 45.000 90.000 30
  # 2002 08 03 10 00 15555.45 -45.335 -179.884 30 2002 08 03 10 55
  # $Id: id2pos.r,v 1.6 2012/09/24 19:34:37 mellis Exp $
  #---------------------------------------------------------------------------------------------------

  # get format using separator location (assumes same format is
  # used for all elements of id)
  cpos <- as.numeric(gregexpr("x", id[1], fixed = TRUE)[[1]])
  # Detect whether the date contains a minute string Note: this
  # will fail if the date does not include it, but there are
  # extraneous characters after the agl string that contain the
  # separator string

  encode.minutes <- length(cpos) >
    6
  yr4 <- as.numeric(substring(id, 1, cpos[1] - 1))
  mon <- as.numeric(substring(id, cpos[1] + 1, cpos[2] - 1))
  day <- as.numeric(substring(id, cpos[2] + 1, cpos[3] - 1))
  hr <- as.numeric(substring(id, cpos[3] + 1, cpos[4] - 1))
  ipos <- 4
  if (encode.minutes) {
    min <- as.numeric(substring(id, cpos[4] + 1, cpos[5] - 1))
    frac.hr <- hr + min/60
    ipos <- ipos + 1
  } else {
    frac.hr <- hr
    min <- 0
  }
  lat <- as.numeric(substring(id, cpos[ipos] + 1, cpos[ipos + 1] - 2))
  lat <- ifelse(
    substring(id, cpos[ipos + 1] - 1, cpos[ipos + 1] - 1) ==
      "S", -lat, lat
  )
  ipos <- ipos + 1
  lon <- as.numeric(substring(id, cpos[ipos] + 1, cpos[ipos + 1] - 2))
  lon <- ifelse(
    substring(id, cpos[ipos + 1] - 1, cpos[ipos + 1] - 1) ==
      "W", -lon, lon
  )
  ipos <- ipos + 1
  # strip any alpha characters (usually trailing) from agl string:
  alt <- as.numeric(sub("[a-z]+", "", substring(id, cpos[ipos] + 1)))

  time <- obs_julian(mon, day, yr4) +
    frac.hr/24

  pos <- cbind(
    time = time, lat = lat, lon = lon, alt = alt, yr = yr4, mon = mon,
    day = day, hr = hr, min = min
  )


  nx <- dimnames(pos)[[2]]


  xx <- pos[1:dim(pos)[1],
            ,
            drop = TRUE]

  if(asdf) {

    m <- matrix(xx, nrow = length(xx)/length(nx))
    df <- data.frame(m)
    names(df) <- nx
    return(df)

  } else {
    return(xx)
  }
}

#' Helpers Legacy
#'
#'
#' @title obs_julian
#' @family helpers legacy
#' @name obs_julian
#' @description return numeric
#' @param m numeric
#' @param d numeric
#' @param y numeric
#' @param origin. string
#' @return returns day since 1/1/1960
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_julian(1, 2020, 1)
#' }
#' }
obs_julian <- function(m, d, y, origin.){
  # returns day since 1/1/1960
  #
  #  $Id: julian.r,v 1.2 2007/06/27 11:54:03 skoerner Exp $
  #---------------------------------------------------------------------------------------------------

  only.origin <- all(missing(m), missing(d), missing(y))
  if(only.origin)
    m <- d <- y <- NULL
  # return days since origin
  nonnumeric.p <- !c(is.numeric(m), is.numeric(d), is.numeric(y))
  if(any(nonnumeric.p) && !only.origin) {
    badarg <- paste(c("m", "d", "y")[nonnumeric.p], collapse = ", "
    )
    stop(paste("Arguments:", badarg, "are not numeric"))
  }
  if(missing(origin.))
    if(is.null(origin. <- .Options$chron.origin))
      origin. <- c(month = 1, day = 1, year = 1960)
  nms <- names(d)
  max.len <- max(length(m), length(d), length(y))
  #
  # prepend new origin value and rep out to common max. length:
  m <- c(origin.[1], rep(m, length = max.len))
  d <- c(origin.[2], rep(d, length = max.len))
  y <- c(origin.[3], rep(y, length = max.len))
  #
  # code from julian date in the S book (p.269)
  #
  y <- y + ifelse(m > 2, 0, -1)
  m <- m + ifelse(m > 2, -3, 9)
  c <- y %/% 100
  ya <- y - 100 * c
  out <- (146097 * c) %/% 4 + (1461 * ya) %/% 4 + (153 * m + 2) %/% 5 +
    d + 1721119
  #
  # now subtract the new origin from all dates
  #
  if(!only.origin) {
    if(all(origin. == 0))
      out <- out[-1]
    else out <- out[-1] - out[1]
  }
  names(out) <- nms
  out
}


#' @title obs_info2id
#' @family helpers legacy
#' @name obs_info2id
#' @description return footprint/receptor id
#' @param yr  year
#' @param mo  month
#' @param dy day
#' @param hr hour
#' @param mn minute
#' @param lat latitude
#' @param lon longitude
#' @param alt altitude above ground level
#' @param sep character, default"x"
#' @param long Logical, to add minute, and rounded with 2 decimals, instead of 4. default TRUE
#' @return a string with the receptor id
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_info2id(yr = 2002,
#'             mo = 8,
#'             dy = 3,
#'             hr = 10,
#'             mn = 0,
#'             lat = 42,
#'             lon = -90,
#'             alt = 1) [1]
#' }
#' }
obs_info2id <- function(yr,
                        mo,
                        dy,
                        hr,
                        mn = 0,
                        lat,
                        lon,
                        alt,
                        sep = "x",
                        long = T) {
  # function to create identifying label for single receptor
  # (location&time) expects alt as altitude above ground in meters
  # example: info2id(2002,8,3,10,45,-90,0.03) [1]
  # '2002x08x03x10x+45.00x+090.00x00030' Caution: No rounding to
  # nearest hour when long=F Cation: Arguments must be specified
  # when long=F (cannot rely on order when mn is absent).
  #---------------------------------------------------------------------------------------------------

  # 10/21/2024, Sergio
  # just to check missing arguments
  if(missing(yr)) stop("Add year")
  if(missing(mo)) stop("Add month")
  if(missing(dy)) stop("Add day")
  if(missing(hr)) stop("Add hour")
  if(missing(lat)) stop("Add lat")
  if(missing(lon)) stop("Add lon")
  if(missing(alt)) stop("Add alt")



  # need leading zeros
  hr <- substring(100 + hr, 2)  #2 digit hr
  yr4 <- substring(10000 + yr, 2)  #4 digit year
  mon <- substring(100 + mo, 2)  #2 digit mon
  day <- substring(100 + dy, 2)  #2 digit day
  mn <- substring(100 + mn, 2)  #2 digit day


  # location
  if (!long) {
    lat <- round(lat, 2)  #1 km roundoff
    lon <- round(lon, 2)  #1 km roundoff
  } else {
    lat <- round(lat, 4)
    lon <- round(lon, 4)
  }

  alt <- round(alt)  #1 m roundoff
  # no below ground...
  alt[alt < 0] <- 0
  # need leading zeros and sign
  lats <- rep("S", length(lat))
  lons <- rep("W", length(lat))
  lats[lat >= 0] <- "N"
  lons[lon >= 0] <- "E"
  if (!long) {
    lat <- paste(substring(100.001 + abs(lat), 2, 6), lats, sep = "")  #2 dig. before decimal + sign
    lon <- paste(substring(1000.001 + abs(lon), 2, 7), lons, sep = "")  #3 dig. before decimal + sign
  } else {
    lat <- paste(substring(100.00001 + abs(lat), 2, 8), lats, sep = "")  #2 dig. before decimal + sign
    lon <- paste(substring(1000.00001 + abs(lon), 2, 9), lons, sep = "")  #3 dig. before decimal + sign
  }
  alt <- substring(1e+05 + alt, 2)
  out <- paste(yr4, mon, day,   hr,     lat, lon, alt, sep = sep)  #put everything together
  if (long)
    out <- paste(yr4, mon, day, hr, mn, lat, lon, alt, sep = sep)
  return(out)
}


#' @title obs_julian
#' @family helpers legacy
#' @name obs_julian
#' @description return footprint/receptor id
#' @param y  year
#' @param m  month
#' @param d day
#' @param legacy logical, to use legacy code
#' @param verbose logical, to show more messages
#' @return a string with the receptor id
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_julian(y = 2002,
#'             m = 8,
#'             d = 3,
#'             legacy = TRUE)
#' obs_julian(y = 2002,
#'             m = 8,
#'             d = 3,
#'             legacy = FALSE)
#' }
#' }
obs_julian <- function(y,
                       m,
                       d,
                       origin.,
                       legacy = FALSE,
                       verbose = TRUE){

  if(legacy) {
    if(verbose) cat("Using legacy code\n")
    #returns day since 1/1/1960
    #
    #  $Id: julian.r,v 1.2 2007/06/27 11:54:03 skoerner Exp $
    #---------------------------------------------------------------------------------------------------

    only.origin <- all(missing(m), missing(d), missing(y))
    if(only.origin)
      m <- d <- y <- NULL
    # return days since origin
    nonnumeric.p <- !c(is.numeric(m), is.numeric(d), is.numeric(y))
    if(any(nonnumeric.p) && !only.origin) {
      badarg <- paste(c("m", "d", "y")[nonnumeric.p], collapse = ", ")
      stop(paste("Arguments:", badarg, "are not numeric"))
    }
    if(missing(origin.))
      if(is.null(origin. <- .Options$chron.origin))
        origin. <- c(month = 1, day = 1, year = 1960)
    nms <- names(d)
    max.len <- max(length(m), length(d), length(y))
    #
    # prepend new origin value and rep out to common max. length:
    m <- c(origin.[1], rep(m, length = max.len))
    d <- c(origin.[2], rep(d, length = max.len))
    y <- c(origin.[3], rep(y, length = max.len))
    #
    # code from julian date in the S book (p.269)
    #
    y <- y + ifelse(m > 2, 0, -1)
    m <- m + ifelse(m > 2, -3, 9)
    c <- y %/% 100
    ya <- y - 100 * c
    out <- (146097 * c) %/% 4 + (1461 * ya) %/% 4 + (153 * m + 2) %/% 5 +
      d + 1721119
    #
    # now subtract the new origin from all dates
    #
    if(!only.origin) {
      if(all(origin. == 0))
        out <- out[-1]
      else out <- out[-1] - out[1]
    }
    names(out) <- nms
    return(out)

  } else {
    day <- ISOdate(year = y,
                   month = m,
                   day = d,
                   hour = 0,
                   min = 0,
                   sec = 0,
                   tz = "UTC")

    y1960 <- ISOdate(year = 1960,
                     month = 1,
                     day = 1,
                     hour = 0,
                     min = 0,
                     sec = 0,
                     tz = "UTC")
    out <- day - y1960
    return(out)
  }

}

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


#' @title obs_load_ncdf
#' @family helpers legacy
#' @name obs_load_ncdf
#' @description NetCDF list of var and attributes
#' @param ncname pathname of NetCDF4 file
#' @param vars specify variables to retrieve otherwise all are returned
#' (NOTE: '/' indicates only return root group)
#' @param grps groups
#' @param dims return variable dimensions TRUE/FALSE
#' @param useoldfoot  old footprints have dim order: lat,lon,time but new footprints
#' (CF-1.6 convention and on) have dim order: lon,lat,time; this option flips
#' lat/lon dim order of returned footprints for backwards compatibility;
#' TRUE => flips new footprints to old convention;
#' FALSE => flips old footprints to new convention
#' @param attribs array of names of global file attributes to return (NOTE: this is for
#' backwards compatibility - current version automatically returns all
#' global file attributes)
#' @param var.attribs array of names of variable attributes to return (NOTE: this is for
#' backwards compatibility - current version automatically returns all
#' attributes for variables and dimensions)
#' @param gz netCDF file is gzipped. Note that .nc may be already compressed.
#' @param tmpdir temporary directory to unzip netCDF file
#' @param tmpfile temporary unzipped netCDF file name
#' @param clean remove temporary file after processing
#' @param lowercase return all list names in lowercase
#' @param gsubdot substitute '_' with '.' in all list variable names
#' @param verbose logical, to show more messages
#' @importFrom ncdf4 nc_open ncatt_get
#' @return list of NetCDF vars and attributes
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_load_ncdf <- function(ncname,
                          vars = NULL,
                          grps = NULL,
                          dims = TRUE,
                          useoldfoot = FALSE,
                          attribs = 'Conventions',
                          var.attribs = NULL,
                          gz = FALSE,
                          tmpdir = tempdir(),
                          tmpfile = tempfile('load.ncdf4', tmpdir = tmpdir),
                          clean = TRUE,
                          lowercase = FALSE,
                          gsubdot = FALSE,
                          verbose = FALSE) {
  if(verbose) {
    cat(sprintf("obs_load_ncdf Called with ncname = %s,\n",ncname))
    cat(sprintf("obs_load_ncdf and lowercase = %s,\n",lowercase))
    cat(sprintf("obs_load_ncdf and dims = %s,\n",dims))
    if(!is.null(attribs)) {
      cat(sprintf("obs_load_ncdf and attribs = %s,\n",attribs))
    }
    if(!is.null(vars)) {
      cat(sprintf("obs_load_ncdf and vars = %s.\n",vars))
    }
    if(!is.null(var.attribs)) {
      cat(sprintf("obs_load_ncdf and var.attribs = %s.\n",var.attribs))
    }
  }



  if(gz) { # gz not needed since nc may be already compressed
    system(paste("gunzip -c ", ncname, " > ", tmpfile, sep=""))
    ncname <- tmpfile
    system(paste("chmod a+w ", ncname, sep=""))
  }


  retval <- list() # this is the final objhect to return

  nc <- ncdf4::nc_open(ncname)

  # retrieve groups and/or variable names
  origvarpaths <- names(nc$var)

  # retrieve the group names & modify varpath for root variables (when there is no group)
  igrp <- 1
  grpnames <- character(0) # note: length(character(0)) = 0, length("") = 1
  varpaths <- character(0)
  root.only <- TRUE

  for (varpath in origvarpaths) {
    varstruct <- unlist(strsplit(varpath, "/"))

    if (length(varstruct) == 1) { # if only the variable was found then...
      grp <- "/" # ... add a character to identify the root group
      var <- sprintf("/%s", varpath)
    } else {
      grp <- varstruct[1]
      var <- varpath
      root.only <- FALSE
    }

    # skip group if grps specified and grp not requested
    if (!is.null(grps)) {
      if (! grp %in% grps) {
        next
      }
    }
    grpnames[igrp] <- grp
    varpaths[igrp] <- var
    igrp <- igrp + 1
  } # end for varpath

  if (dims) {
    #find dimension names
    dimpaths <- names(nc$dim)
    for (idim in 1:length(dimpaths)) {
      dimstruct <- unlist(strsplit(dimpaths[idim], "/"))
      if (length(dimstruct) == 1) { # if only the dimension was found then...
        dimpaths[idim] <- sprintf("/%s", dimpaths[idim])
      }
    }#end for idim
  }


  # read group(s)
  grpnames <- unique(grpnames)
  igrp <- 1

  for (grpname in grpnames) {

    values <- list()

    if (verbose) cat(sprintf("obs_load_ncdf Reading group '%s'\n", grpname))

    varnames <- grep(sprintf("^%s", grpname), varpaths, value=TRUE)
    ivar <- 1
    allnames <- varnames
    if (dims) {
      dimennames <- grep(sprintf("^%s", grpname), dimpaths, value=TRUE)
      allnames <- c(allnames,dimennames)
      if (grpname == '/') {dimennames <- substring(dimennames, 2, nchar(dimennames)) }
    } else {
      dimennames <- c()
    }


    #loop over variables and optionally dimensions
    for(varname in allnames) {
      # format variable name
      basename <- unlist(strsplit(varname, "/"))[-1]
      if (grepl("^/", varname)) { varname <- substring(varname, 2, nchar(varname)) }

      if (varname %in% dimennames) {
        # retrieve dimension values
        if (verbose) { cat(sprintf("obs_load_ncdf Reading dimension  '%s'\n", basename)) }
        values[[ivar]] <- nc$dim[[varname]]$vals
      } else {
        # skip variable if vars specified and basename not requested
        if (!is.null(vars)) {
          if (! basename %in% vars) {
            next
          }
        }
        # retrieve variable values
        if (verbose) { cat(sprintf("obs_load_ncdf Reading variable '%s'\n", basename)) }
        values[[ivar]] <- ncdf4::ncvar_get(nc, varname)
      }
      names(values)[ivar] <- basename


      # retrieve variable/dimension attributes
      attnames <- ncdf4::ncatt_get(nc,varname)

      # the following attribute names are reserved in R:
      #   ‘class’, ‘comment’, ‘dim’, ‘dimnames’, ‘names’, ‘row.names’, ‘tsp’
      if (length(attnames) > 0) {
        for (iii in 1:length(attnames)) {
          attributes(values[[ivar]])[[names(attnames)[iii]]] <- attnames[[iii]]

          # convert date/time information to POSIX time
          time.checked <- obs_interpret_udunits_time(vals=values[[ivar]],
                                                     unitstring=attnames[[iii]])
          if (time.checked$is.time) {
            values[[ivar]] <- time.checked$vals
            attributes(values[[iii]])$tzone <- time.checked$tz
          }
        }#end for iii
      }

      #include to keep backwards compatible
      if (gsubdot) {
        names(values)[ivar] <- gsub('_','.',names(values)[ivar])
      }
      ivar <- ivar + 1
    }#end for varname


    if (length(values) == 0) { next }

    # concatenate variables to the user's list
    if (grpname == "/") {
      retval <- values
    } else {
      retval[[igrp]] <- values
      names(retval)[igrp] <- grpname
    }
    igrp <- igrp + 1
  }#end for grpname


  #global file attributes: return entire list
  global.att.list <- ncdf4::ncatt_get(nc,varid=0)
  retval$global.att.list <- global.att.list
  # For backwards compatibility: also return individual specified attributes
  for (att in attribs) {
    target.name <- att
    if(gsubdot) {
      target.name <- gsub('_','.',target.name)
    }
    # Some attribute names are reserved and we should
    # not overwrite them with values from the netCDF
    # file.  An example is "names", which occurs in
    # column3d output.  Tack on a "2" to such names.
    if (target.name %in% names(attributes(retval))) {
      target.name <- paste(target.name,"2",sep='')
    }
    if (is.null(global.att.list[[att]])) {
      sprintf("obs_load_ncdf Cannot find attribute \"%s\" in file \"%s\".",att,nc$filename)
    } else {
      attributes(retval)[[target.name]] <- global.att.list[[att]]
    }
  }#end for att


  #check if dimensions of any footprint need to be flipped
  if (root.only) {
    #no groups (only root group)
    footnames <- grep('^foot[[:alnum:]]*[0-9]$',names(retval),value=TRUE)
    footcount <- length(footnames)
  } else {
    #footprints in groups
    footnames <- list()
    footcount <- 0
    for (name in names(retval)) {
      footnames[[name]] <- grep('^foot[[:alnum:]]*[0-9]$',names(retval[[name]]),value=TRUE)
      footcount <- footcount + length(footnames[[name]])
    }
  }
  footflip <- FALSE

  if (is.null(attributes(retval)[['Conventions']]) &
      !useoldfoot & footcount > 0) {

    print('Flipping old footprint(s) dimensions to match new convention: lon,lat,date')
    footflip <- TRUE
  } else if (!is.null(attributes(retval)[['Conventions']]) &
             useoldfoot &
             footcount > 0) {
    print('Flipping new footprint(s) dimensions to match old convention: lat,lon,date')
    footflip <- TRUE
  }

  if (footflip) {
    if (root.only) {
      #no groups (only root group)
      for (footname in footnames) {
        retval[[footname]] <- aperm(retval[[footname]],c(2,1,3))
      }
    } else {
      #footprints in groups
      for (name in names(retval)) {
        for (footname in footnames[[name]]) {
          retval[[name]][[footname]] <- aperm(retval[[name]][[footname]],c(2,1,3))
        }
      }
    }
  }


  # for backwards compatibility retrieve attributes for specified variables
  for (var in var.attribs) {
    att.val <- ncatt_get(nc,var)  #get all attributes for variable var
    retval[[paste('attrib.',var,sep='')]] <- att.val
  }

  nc <- nc_close(nc)

  if(gz && clean) {
    system(paste("rm -f",ncname))
  }
  if(lowercase) {
    names(retval) <- tolower(names(retval))
    if (!root.only) {
      for (name in names(retval)) {
        names(retval[[name]]) <- tolower(names(retval[[name]]))
      }
    }
  }
  return(retval)

} # end




#' @title obs_interpret_udunits_time
#' @family helpers legacy
#' @name obs_interpret_udunits_time
#' @description return list of time vars
#' @param vals  time array
#' @param unitstring  month
#' @param tz  time zone, "UTC" default
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_interpret_udunits_time <- function(vals,
                                       unitstring,
                                       tz="UTC") {

  retval <- list()
  retval$is.time <- FALSE

  if(length(grep("^DAYS since",
                 unitstring,
                 ignore.case = TRUE)) == 1) {
    retval$is.time <- TRUE
    # length of string is a bad parsing heuristic, but works
    # for everything so far.
    #     21 chars if "days since 1900-01-01"
    #     19 chars if "days since 1900-1-1"
    retval$vals <- as.POSIXct(substr(unitstring,
                                     11,
                                     nchar(unitstring)),
                              tz=tz) + vals*86400
    retval$tz <- tz # UTC tzone is a presumption
  }
  if(length(grep("^SECONDS since",
                 unitstring,
                 ignore.case = TRUE)) == 1) {
    retval$is.time <- TRUE
    retval$vals <- as.POSIXct(substr(unitstring,
                                     15,
                                     nchar(unitstring)),
                              tz=tz) + vals
    retval$tz <- tz # UTC tzone is a presumption
  }
  if(length(grep("^HOURS since",
                 unitstring,
                 ignore.case = TRUE)) == 1) {
    retval$is.time <- TRUE
    retval$vals <- as.POSIXct(substr(unitstring,
                                     12,
                                     nchar(unitstring)),
                              tz=tz) + vals*3600
    retval$tz <- tz # UTC tzone is a presumption
  }
  if(length(grep("^decimal date",
                 unitstring,
                 ignore.case = TRUE)) == 1) {
    retval$is.time <- TRUE
    retval$vals <- obs_decimal_to_POSIX(vals)
    retval$tz <- tz # UTC tzone is a presumption
  }
  return(retval)
}



#' @title obs_decimal_to_POSIX
#' @family helpers legacy
#' @name obs_decimal_to_POSIX
#' @description return list of time vars
#' @param decimal.dates  vecot with format YYYY.yyyy
#' @param tz  time zone, "UTC" default
#' @param legacy logical, to use legacy code
#' @importFrom lubridate decimal_date
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_decimal_to_POSIX <- function(decimal.dates,
                                 tz = "UTC",
                                 legacy = TRUE) {

  # converts a vector of YYYY.yyyy decimal dates
  # such as those used by Arlyn into POSIXt dates.

  retval <- NULL

  if(legacy) {
    for (dd in decimal.dates) {
      year <- trunc(dd)
      y0 <- ISOdatetime(year,1,1,0,0,0,tz="UTC")
      y1 <- as.numeric(ISOdatetime(year+1,1,1,0,0,0,tz="UTC"))
      sec.in.year <- y1 - as.numeric(y0)
      pd <- as.POSIXct(y0 + (dd-year)*sec.in.year,tz=tz)
      if(is.null(retval)) {
        retval <- pd
      } else {
        retval <- c(retval,pd)
      }
    }
    attributes(retval)$tzone <- tz

    return(retval)

  } else {
    return(lubridate::decimal_date(decimal.dates, tz = tz))
  }
}



#' @title obs_idate_to_POSIX
#' @family helpers legacy time
#' @name obs_idate_to_POSIX
#' @description return list of time vars
#' @param idate  vector of integers representing ONE date or date time
#' @param tz  time zone, "UTC" default
#' @importFrom lubridate decimal_date
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_idate_to_POSIX(c(2000, 1, 3))
#' }}
obs_idate_to_POSIX <- function(idate,
                               tz = "UTC") {
  # takes a vector of integers representing ONE date
  # cannot now handle multiple dates
  # year month day required (00:00:00 assumed)
  # if idate has 6 elements, will use 4,5,6 as HH::MM::SS UTC
  if(length(idate) == 3) {
    return(ISOdatetime(idate[1],idate[2],idate[3],0,0,0,tz=tz))
  }
  if(length(idate) == 6) {
    return(ISOdatetime(idate[1],idate[2],idate[3],idate[4],idate[5],idate[6],tz=tz))
  }
  return(idate)
  stop(sprintf("Was expecting 3 or 6 elements in the idate numeric vector, got %d instead.",length(idate)))

}

#' @title obs_POSIX_to_idate
#' @family helpers legacy time
#' @name obs_POSIX_to_idate
#' @description return list of time vars
#' @param dates  vector of integers representing ONE date or date time
#' @param tz  time zone, "UTC" default
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_POSIX_to_idate(c(2000, 1, 3))
#' }}
obs_POSIX_to_idate <- function(dates,
                               tz = "UTC") {
  lt <- as.POSIXlt(dates)

  return(data.frame(year=lt$year+1900,
                    month=lt$mon+1,
                    day=lt$mday,
                    hour=lt$hour,
                    minute=lt$min,
                    second=lt$sec))
}

#' @title obs_itau_to_POSIX
#' @family helpers legacy time
#' @name obs_itau_to_POSIX
#' @description return time vars
#' @param itaus  vector of integers representing ONE date or date time
#' @param year0  default 1999
#' @aliases itau2date
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_itau_to_POSIX(1)
#' }}
obs_itau_to_POSIX <- function(itaus,
                              year0 = 1999) {
  if(is.na(year0)) {
    year0 <- 1999
  }
  return(ISOdatetime(year0,1,1,0,0,0,tz="UTC")+itaus)
}

#' @title obs_is_leap
#' @family helpers legacy time
#' @name obs_is_leap
#' @description return logical
#' @param year  integer year
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_is_leap(1)
#' }}
obs_is_leap <- function(year) {
  return(year %% 4 == 0 &
           (year %% 100 != 0 | year %% 400 == 0))
}


#' @title obs_days_in_year
#' @family helpers legacy time
#' @name obs_days_in_year
#' @description return days
#' @param years  integer year
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_days_in_year(1)
#' }}
obs_days_in_year <- function(years) {
  retval <- numeric(0)
  for (yr in years) {
    if(obs_is_leap(yr)) {
      retval <- c(retval,366)
    } else {
      retval <- c(retval,365)
    }
  }
  return(retval)
}


#' @title obs_days_in_month
#' @family helpers legacy time
#' @name obs_days_in_month
#' @description return days
#' @param year  integer year
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_days_in_month(1)
#' }}
obs_days_in_month <- function(year) {
  retval <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(!is.null(year)) {
    if(obs_is_leap(year)) {
      retval[2] <- 29
    }
  }
  return(retval)
}

#' @title obs_seconds_in_year
#' @family helpers legacy time
#' @name obs_seconds_in_year
#' @description return seconds
#' @param year  integer year
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_seconds_in_year(1)
#' }}
obs_seconds_in_year <- function(year) {
  return(86400*obs_days_in_year(year))
}


#' @title obs_length_POSIXlt
#' @family helpers legacy time
#' @name obs_length_POSIXlt
#' @description return formatted time
#' @param x  time
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_length_POSIXlt(1)
#' }}
obs_length_POSIXlt <- function(x) {
  return(length(x$wday))
}



#' @title obs_format_et
#' @family helpers legacy time
#' @name obs_format_et
#' @description return logical
#' @param elapsed.seconds  seconds
#' @param verbose  logical, to show more info
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_format_et(1)
#' }}
obs_format_et <- function(elapsed.seconds,
                          verbose = FALSE) {

  tvec <- character(0)
  years <- 0
  weeks <- 0
  days <- 0
  hours <- 0
  minutes <- 0
  seconds <- 0

  # 365-day years
  while(elapsed.seconds > 86400*365) {
    elapsed.seconds <- elapsed.seconds - 86400*365
    years <- years + 1
  }

  # weeks
  while(elapsed.seconds > 86400*7) {
    elapsed.seconds <- elapsed.seconds - 86400*7
    weeks <- weeks + 1
  }

  # days
  while(elapsed.seconds > 86400) {
    elapsed.seconds <- elapsed.seconds - 86400
    days <- days + 1
  }

  # hours
  while(elapsed.seconds > 3600) {
    elapsed.seconds <- elapsed.seconds - 3600
    hours <- hours + 1
  }

  # minutes
  while(elapsed.seconds > 60) {
    elapsed.seconds <- elapsed.seconds - 60
    minutes <- minutes + 1
  }

  # seconds <- elapsed.seconds
  if(verbose) {
    if(years>0) {
      tvec <- c(tvec,sprintf("%d year%s",years,ifelse(years>1,"s","")))
    }
    if(weeks>0) {
      tvec <- c(tvec,sprintf("%d week%s",weeks,ifelse(weeks>1,"s","")))
    }
    if(days>0) {
      tvec <- c(tvec,sprintf("%d day%s",days,ifelse(days>1,"s","")))
    }
    if(hours>0) {
      tvec <- c(tvec,sprintf("%d hour%s",hours,ifelse(hours>1,"s","")))
    }
    if(minutes>0) {
      tvec <- c(tvec,sprintf("%d minute%s",minutes,ifelse(minutes>1,"s","")))
    }
    if(elapsed.seconds>0) {
      tvec <- c(tvec,sprintf("%.0f second%s",
                             elapsed.seconds,
                             ifelse(elapsed.seconds > 1,"s","")))
    }
  } else {
    if(years>0) {
      tvec <- c(tvec,sprintf("%dy",years))
    }
    if(weeks>0) {
      tvec <- c(tvec,sprintf("%dw",weeks))
    }
    if(days>0) {
      tvec <- c(tvec,sprintf("%dd",days))
    }
    if(hours>0) {
      tvec <- c(tvec,sprintf("%02dh",hours))
    }
    if(minutes>0) {
      tvec <- c(tvec,sprintf("%02dm",minutes))
    }
    if(elapsed.seconds>0) {
      tvec <- c(tvec,sprintf("%02.0fs", elapsed.seconds))
    }
  }
  return(paste(tvec,
               collapse='-'))
}



#' @title obs_POSIX_to_decimal
#' @family helpers legacy time
#' @name obs_POSIX_to_decimal
#' @description return time decimal
#' @param posix.dates  dates+
#' @param tz  time zone
#' @param legacy logical, to use legacy or not
#' @importFrom lubridate decimal_date
#' @return return list of time vars
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_POSIX_to_decimal(1)
#' }}
obs_POSIX_to_decimal <- function(posix.dates,
                                 tz = "UTC",
                                 legacy = TRUE) {

  # convert a vector of POSIXt dates into YYYY.yyyy decimal dates
  # (such as those used by Arlyn).  The .yyyy is the fractional part
  # of the year YYYY.  (Four decimal places shown for convenience;
  # internally this is kept as a standard numeric quantity, generally
  # R*8.)

  if(legacy) {

    if(!any(class(posix.dates)=="POSIXt")) {
      stop("obs_POSIX_to_decimal Expecting one of class(posix.dates) == 'POSIXt'")
    }

    #  retval <- numeric(0)
    #  ndates <- length(posix.dates)

    #  for (idate in 1:ndates) {
    #    year <- as.numeric(format(posix.dates[idate],format="%Y"))
    #    y0 <- as.numeric(ISOdatetime(year,1,1,0,0,0,tz=tz))
    #    y1 <- as.numeric(ISOdatetime(year+1,1,1,0,0,0,tz=tz))
    #    retval <- c(retval,year+(as.numeric(posix.dates[idate])-y0)/(y1-y0))
    #  }

    # alternative code as of 3 Aug 2009:  much faster, as there is no looping.

    #   There's a potential problem here in that I'm not sure how the R
    #   date-time classes handle a vector of times with different time
    #   zone attributes among the members.  It would appear that the
    #   attribute belonging to the vector overrides any such attribute
    #   assigned to a member.  This is probably because it is the vector
    #   object which can have attributes, not the member.  This code
    #   assumes that all members of the vector have the same time zone
    #   attribute.


    lt <- as.POSIXlt(posix.dates)
    y0 <- as.numeric(ISOdatetime(lt$year+1900,1,1,0,0,0,tz=attributes(lt)$tzone))
    y1 <- as.numeric(ISOdatetime(lt$year+1901,1,1,0,0,0,tz=attributes(lt)$tzone))
    return(lt$year+1900+(as.numeric(posix.dates)-y0)/(y1-y0))
  } else {
    return(lubridate::decimal_date(posix.dates,
                                   tz = tz))
  }
}


#' @title obs_extract_dates
#' @family helpers legacy time
#' @name obs_extract_dates
#' @description return time
#' @param ncfile  string to ncfile
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_extract_dates(1)
#' }}
obs_extract_dates <- function(ncfile) {
  z <- obs_load_ncdf(ncfile,
                     vars = c("decimal_date",
                              "idate"))
  return(z)

}

#' @title obs_epoch_seconds_to_POSIX
#' @family helpers legacy time
#' @name obs_epoch_seconds_to_POSIX
#' @description return seconds
#' @param sec  seconds
#' @param tz  time zone
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_epoch_seconds_to_POSIX(1)
#' }}
obs_epoch_seconds_to_POSIX <- function(sec,
                                       tz = "UTC") {
  class(sec) <- c("POSIXt","POSIXct")
  attributes(sec)$tzone <- tz
  return(sec)
}

#' @title obs_POSIX_to_epoch_seconds
#' @family helpers legacy time
#' @name obs_POSIX_to_epoch_seconds
#' @description return number
#' @param pt  time
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs_POSIX_to_epoch_seconds(1)
#' }}
obs_POSIX_to_epoch_seconds <- function(pt) {
  return(as.numeric(pt))
}


#' @title obs_seq_midmon
#' @family helpers legacy time
#' @name obs_seq_midmon
#' @description return time
#' @param year0  year start
#' @param month0  month start
#' @param year1  year end
#' @param month1  month end
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_seq_midmon <- function(year0,
                           month0,
                           year1,
                           month1) {

  retval <- NULL
  y <- year0
  m <- month0

  while(TRUE) {
    if(is.null(retval)) {
      retval <- obs_month_middle(y,m)
    } else {
      retval <- c(retval, obs_month_middle(y,m))
    }
    if(y == year1 & m == month1) {
      break
    }
    m <- m+1
    if(m>12) {
      y <- y+1
      m <- 1
    }
  }
  attributes(retval)$tzone <- "UTC"
  return(retval)
}


#' @title obs_month_middle
#' @family helpers legacy time
#' @name obs_seq_midmon
#' @description return time
#' @param year  year
#' @param month  month (1 to 12)
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_month_middle <- function(year,
                             month) {
  d.i.m <- obs_days_in_month(year)[month]  # can have leap years
  if((year == 1969) &
     (month == 12)) {
    retval <- (mean(c(ISOdatetime(year, month,1,0,0,0,tz="UTC"),
                      obs_epoch_seconds_to_POSIX(-1))))
  } else {
    retval <- (mean(c(ISOdatetime(year,month,1,0,0,0,tz="UTC"),
                      ISOdatetime(year,month,d.i.m,23,59,59,tz="UTC"))))
  }
  attributes(retval)$tzone <- "UTC"
  return(retval)
}



#' @title obs_seconds_in_month
#' @family helpers legacy time
#' @name obs_seconds_in_month
#' @description return time
#' @param year  year
#' @param month  month (1 to 12)
#' @return return dates
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_seconds_in_month <- function(year,
                                 month) {
  return(86400*obs_days_in_month(year)[month])  # can have leap years
}


#' @title obs_julian_to_POSIX
#' @family helpers legacy time
#' @name obs_julian_to_POSIX
#' @description return time
#' @param julians  year
#' @param epoch  default 0001-01-01
#' @return return dates
#' @export
#' @note Convention is that the Julian date of the epoch is 1.0 (not 0!)
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_julian_to_POSIX <- function(julians,
                                epoch = ISOdatetime(0001, 1, 1, 0, 0, 0,
                                                    tz = "UTC")) {
  # Convention is that the Julian date of the epoch is 1.0 (not 0!)
  retval <- epoch+86400*(julians-1.0)
  attributes(retval)$tzone <- attributes(epoch)$tzone
  return(retval)
}



#' @title obs_POSIX_to_julian
#' @family helpers legacy time
#' @name obs_POSIX_to_julian
#' @description return time
#' @param times  time
#' @param epoch  default 0001-01-01
#' @return return dates
#' @export
#' @note Convention is that the Julian date of the epoch is 1.0 (not 0!)
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_POSIX_to_julian <- function(times,
                                epoch = ISOdatetime(0001,1,1,0,0,0,
                                                    tz = "UTC")) {
  # Convention is that the Julian date of the epoch is 1.0 (not 0!)
  return(as.numeric(difftime(times,
                             epoch,
                             units="days") + 1.0))
}


#' @title obs_make_hysplit_nc4
#' @family helpers legacy time
#' @name obs_make_hysplit_nc4
#' @description makes nc
#' @param ident  identifier
#' @param ncname  netcdf name file format hysplitid.nc
#' @param origident logical, default FALSE, include most accurate receptor agl,lat,lon, utctime fields,  before rounding
#' @param part logical, default FALSE, include particle array
#' @param foot logical, default FALSE, include foot
#' @param horizconv logical, default FALSE, include results from horizontally convolving footprints with
#' fluxes for each particle and summing particle over time
#' @param avghorizconv logical, default FALSE, include results from horizontally convolving footprints with
#' fluxes for each particle and summing particle over time, avg particles
#' @param dvmr logical, default FALSE, include delta volume mixing ratio results from convolve-fast.r
#'  (replaces horizconv and avghorizconv functionality)
#' @param dvmrmean logical, default FALSE, include climatological mean of delta volume mixing ratio results
#' @param dvmrstd logical, default FALSE, include climatological standard dev of volume mixing ratio results
#' @param runinfo logical, default FALSE, text summary of run parameters
#' @param check logical, default FALSE, output from Trajeccheck
#' @param emit logical, default FALSE, emission window description
#' @param init logical, default FALSE, include init mixing ratio values
#' @param avginit logical, default FALSE, include avginit mixing ratio value
#' @param partdatname string, default 'part', name of particle table, with the following convention:
#' part       => whole particle table  (default)
#' partfoot   => particle table thinned to retain rows where foot > 0
#' part3d     => particle table thinned to retain rows every nhrs
#' endpts     => particle table thinned to retain rows with trajectory endpts
#' @param footname default NULL, name of footprint to distinguish among multiple footprints -e.g. foot1, foot2, etc
#' @return return dates
#' @note Convention is that the Julian date of the epoch is 1.0 (not 0!)
#' @examples {
#' \dontrun{
#' # Do not run
#' }}
obs_make_hysplit_nc4 <- function(ident,
                                 ncname = paste("hysplit",
                                                ident,
                                                ".nc",
                                                sep = ""),
                                 origident = FALSE,
                                 part = FALSE,
                                 foot = FALSE,
                                 horizconv = FALSE,
                                 avghorizconv = FALSE,
                                 dvmr = FALSE,
                                 dvmrmean = FALSE,
                                 dvmrstd = FALSE,
                                 runinfo = FALSE,
                                 check = FALSE,
                                 emit = FALSE,
                                 init = FALSE,
                                 avginit = FALSE,
                                 partdatname = 'part',
                                 footname = NULL,
                                 horizconvname=NULL,                                   # name of horizontal convolve mixing ratio values (should reflect species
                                 # and source - e.g. co2horizconvCT, co2horizconvvul, etc)
                                 avghorizconvname=NULL,                                # name of avg horizontal convolve mixing ratio (should reflect species
                                 # and source - e.g. co2avghorizconvCT, co2avghorizconvvul, etc)
                                 dvmrname=NULL,                                        # name of delta volume mixing ratio results (should reflect species
                                 # and source - e.g. co2dvmrCT, co2dvmrvul, etc)
                                 dvmrmeanname=NULL,                                    # name of climatological mean of delta volume mixing ratio results (should
                                 # reflect species and source - e.g. co2dvmrmeanCT, co2dvmrmeanvul, etc)
                                 dvmrstdname=NULL,                                     # name of climatological std of delta volume mixing ratio results (should
                                 # reflect species and source - e.g. co2dvmrstdCT, co2dvmrstdvul, etc)
                                 initname=NULL,                                        # name of init mixing ratio values (should reflect species and source)
                                 # (e.g. co2initCT, co2initGV, coinitGV, etc)
                                 avginitname=NULL,                                     # name of average init mixing ratio (should reflect species and source)
                                 # (e.g. co2avginitCT, co2avginitGV, coavginitGV, etc)
                                 horizconvdatunits=NULL,                               # string indicating assigned units to horizconvdat
                                 avghorizconvdatunits=NULL,                            # string indicating assigned units to avghorizconvdat
                                 dvmrdatunits=NULL,                                    # string indicating assigned units to dvmrdat
                                 dvmrmeandatunits=NULL,                                # string indicating assigned units to dvmrmeandat
                                 dvmrstddatunits=NULL,                                 # string indicating assigned units to dvmrstddat
                                 initdatunits=NULL,                                    # string indicating assigned units to initdat
                                 avginitdatunits=NULL,                                 # string indicating assigned units to avginitdat
                                 origidentdat=NULL,                                    # list of original receptor agl, lat, lon, utctime fields
                                 origutctime.format=NULL,                              # string format of origutctime from receptor list
                                 # e.g. '%Y-%m-%dT%H:%M:%S'
                                 partdat=NULL,                                         # passed in R object (use getr by default)
                                 footdat=NULL,                                         # passed in R object (use getr by default)
                                 horizconvdat=NULL,                                    # passed in R object (use getr by default)
                                 avghorizconvdat=NULL,                                 # passed in R object (use getr by default)
                                 dvmrdat=NULL,                                         # matrix of dvmr results
                                 dvmrmeandat=NULL,                                     # matrix of dvmrmean results
                                 dvmrstddat=NULL,                                      # matrix of dvmrstd results
                                 runinfodat=NULL,                                      # passed in R object (use getr by default)
                                 checkdat=NULL,                                        # passed in R object (use getr by default)
                                 emitdat=NULL,                                         # data frame/2D array
                                 initdat=NULL,                                         # passed in R object (use getr by default)
                                 avginitdat=NULL,                                      # passed in R object (use getr by default)
                                 partpath=NULL,                                        # location of particle file
                                 checkpath=partpath,                                   # location of check file
                                 footpath=paste(partpath, "footprints/", sep=""),      # location of footprint file
                                 horizconvpath=paste(partpath, "convolve/", sep=""),   # location of horizontal convolve summarized results file
                                 avghorizconvpath=paste(partpath, "convolve/", sep=""),# location of horizontal convolve summarized results file
                                 initpath=paste(partpath, "boundary/", sep=""),        # location of init file
                                 avginitpath=paste(partpath, "boundary/", sep=""),     # location of avginit file
                                 runinfopath=NULL,                                     # location of runinfo file
                                 targetdir,                                            # destination directory
                                 errorlog=paste("ncerrors",ident,".txt",sep=""),       # name of errorlog file; use NA to suppress
                                 appendnc=FALSE,                                       # TRUE=>append an existing netCDF file,
                                 # FALSE=>create new netCDF file
                                 plotfoot=F,
                                 plotmeantrajec=F,
                                 sourcepath=NULL,
                                 charmissval=NULL,                            # missing value for character variables
                                 # (ncvar_def requires NA or NULL depending on the version of ncdf4)
                                 global.att.list=NULL, # list of global attributes (only used if !appendnc)
                                 global.att.files=NULL, # list of filenames whose contents are turned into global attributes
                                 #(only used if !appendnc and global.att.list is empty)
                                 ident.start=NULL) { #time related to hysplit start time (may different from nearest hour to ident if partperhymodelc was turned on)
  #source required functions
  require("ncdf4")
  # Sergio, I suspect that this is not needed anymore
  if(!exists('getr',mode='function') || !exists('id2pos',mode='function')) {
    if (is.null(sourcepath)) {
      if (file.exists("stiltR"))
        sourcepath <- paste(getwd(), "/stiltR/", sep="")
      else if (file.exists("Rsc"))
        sourcepath <- paste(getwd(), "/Rsc/", sep="")
      else {
        stop('sourceall.r needs to be sourced, no sourcepath was provided as an argument,',
             ' and no stiltR or Rsc directory was found.')
      }
    }
    source(paste(sourcepath,"sourceall.r",sep=""))
  }

  #Non Fatal Errors
  warns<-list()

  #Check whether files exist and warn if not.

  # modified: M. Trudeau, 26 Mar 2010
  if (part &&
      partdatname == 'part' &&
      is.null(partdat) &&
      !existsr(ident,partpath)) {
    print(paste(".RData",ident, " not found. Exiting...",sep=""))
    return()
  }

  if (foot &&
      is.null(footdat) &&
      !existsr(paste("footr",
                     ident,
                     sep = ""),
               footpath)) {
    warns$footnofile<-"NO FOOTPRINT FILE FOUND"
    foot <- F
  }

  if (runinfo &&
      is.null(runinfodat) &&
      !existsr(paste("run.info",
                     ident,
                     sep = ""),
               runinfopath)) {
    warns$runinfonofile<-"NO RUNINFO FILE FOUND"
    runinfo <- F
  }

  if (check &&
      is.null(checkdat) &&
      !existsr(paste("check",
                     ident,
                     sep=""),
               checkpath)) {
    warns$checknofile<-"NO CHECK FILE FOUND"
    check <- F
  }

  if (part &&
      partdatname == 'endpts' &&
      is.null(partdat) &&
      !existsr(paste("end",
                     ident,
                     sep=""),
               partpath))  {
    warns$partnofile <- "NO ENDPOINTS FILE FOUND"
    part <- F
  }

  if(!file.exists(targetdir)) {
    cat(sprintf("  Creating output path \"%s\".\n", targetdir))
    dir.create(targetdir,recursive=TRUE)
  }

  if (!is.null(ident.start)) {
    #calculate STILT start time for this ident
    #NOTES:
    #1) STILT start time is the ident time rounded to top of the hour
    #   e.g. for backward runs: ident start time = 17:01 relates to a
    #                           STILT start time = 18:00
    #2) STILT start time is the time at which particle table btime/time column = 0
    #3) particles are released at ident time, which is relative to STILT start time
    info<-id2pos(ident.start)
    inittime<-ISOdatetime(info["yr"],info["mon"], info["day"],info["hr"],info["min"],0,tz="UTC")
    stilt.start <- round(inittime,'hour')
    if (stilt.start < inittime) {
      stilt.start <- stilt.start + 3600
    }
    epoch <- ISOdatetime(2000,1,1,0,0,0,"UTC")
  }

  #DEFINE NCDF NCHAR DIMENTION FOR STRING VECTORS

  dimnchar   <- ncdim_def("nchar",   "", 1:500 )

  #CREATE EMPTY LIST TO HOLD NCDF VARS

  vars<-list()
  outs<-list()
  atts<-list()


  nident.dim <-ncdim_def("nident","",vals=1:1,create_dimvar=F)
  vars$var.ident <- ncvar_def(name="ident",units="string",dim=list(dimnchar,nident.dim),
                              missval=charmissval,longname="identifier string",prec="char",compression=9)

  outs$ident<-ident

  if (origident) {
    if(!is.null(origidentdat)){
      norigident.dim<-ncdim_def("norig","",vals=1:length(origidentdat[['agl']]),create_dimvar=F)

      vars$var.origagl<-ncvar_def(name="origagl",units="m",dim=list(norigident.dim),
                                  missval=-1e34,longname="original receptor height above ground",compression=9)
      atts$varid <- c(atts$varid,'origagl','origagl')
      atts$name  <- c(atts$name,'standard_name','description')
      atts$val   <- c(atts$val,'height',"before rounding")

      vars$var.origlat<-ncvar_def(name="origlat",units="degrees_north",dim=list(norigident.dim),
                                  missval=-1e34,longname="original receptor latitude",compression=9)
      atts$varid <- c(atts$varid,'origlat','origlat')
      atts$name  <- c(atts$name,'standard_name','description')
      atts$val   <- c(atts$val,'latitude',"before rounding")

      vars$var.origlon<-ncvar_def(name="origlon",units="degrees_east",dim=list(norigident.dim),
                                  missval=-1e34,longname="original receptor longitude",compression=9)
      atts$varid <- c(atts$varid,'origlon','origlon')
      atts$name  <- c(atts$name,'standard_name','description')
      atts$val   <- c(atts$val,'longitude',"before rounding")

      vars$var.origutctime<-ncvar_def(name="origutctime",units="UTC",dim=list(dimnchar,norigident.dim),
                                      missval=charmissval,longname="original receptor time",
                                      prec="char",compression=9)
      atts$varid <- c(atts$varid,'origutctime')
      atts$name  <- c(atts$name,'description')
      atts$val   <- c(atts$val,"in UTC, before rounding")

      vars$var.origutctimeformat<-ncvar_def(name="origutctimeformat",units="none",dim=list(dimnchar,norigident.dim),
                                            missval=charmissval,longname="original receptor time (origutctime) format",
                                            prec="char",compression=9)

      outs$origagl <- as.numeric(origidentdat[['agl']])
      outs$origlat <- as.numeric(origidentdat[['lat']])
      outs$origlon <- as.numeric(origidentdat[['lon']])
      outs$origutctime <- as.character(origidentdat[['utctime']])
      outs$origutctimeformat <- as.character(origutctime.format)
    }  else  warns$origidentnull<- "ORIGIDENTDAT IS EMPTY OR MISSING"
  }#end if origident


  if(part) {
    #writes particle tables with partdatname:
    #part       => stilt particle location array
    #partfoot   => stilt particle location array thinned to retain rows where foot > 0
    #part3d     => stilt particle location array thinned to retain rows approximately every so many hours
    #endpts     => stilt particle location array thinned to retain rows containing trajectory endpts

    if(partdatname == 'part') {
      if(is.null(partdat) && existsr(ident, partpath)) partdat <-  getr(ident, partpath)
      part.string <- 'particle location array'

    } else if (partdatname == 'endpts') {
      if(is.null(partdat)) partdat <- data.matrix(getr(paste("end",ident,sep=""), partpath))
      part.string <- 'particle location array thinned to retain rows containing trajectory endpts'

    } else if (partdatname == 'partfoot') {
      part.string <- 'particle location array thinned to retain rows where foot > 0'

    } else if (partdatname == 'part3d') {
      part.string <- 'particle location array thinned to retain rows approximately every so many hours'
    }

    if(!is.null(partdat)){

      partnames <-dimnames(partdat)[[2]]
      parttime <- NULL
      if ("time" %in% partnames) {
        #particle time provided in negative minutes
        parttime <- stilt.start + (partdat[,"time"]*60)
      }
      if ("btime" %in% partnames) {
        #particle time provided in positive hours (negative implied)
        parttime <- stilt.start - (partdat[,"btime"]*3600)
      }
      if (is.null(parttime)) {
        parttime <- stilt.start + 1:dim(partdat)[1]
        warns$parttimenull <-
          "PARTICLE object does not have a (b)time column, assigning dummy values to parttime"
      }

      npartnames.dim <- ncdim_def(paste('n',partdatname,'names',sep=''),"",vals=1:ncol(partdat),create_dimvar=F)
      vars[[paste('var.',partdatname,'names',sep='')]] <- ncvar_def(name=paste(partdatname,'names',sep=''),units="various",
                                                                    dim=list(dimnchar,npartnames.dim),missval=charmissval,
                                                                    longname="column names for particle array",prec="char",compression=9)
      partdate.vals <- as.numeric(difftime(parttime,epoch,units="days")) # subtract the epoch to make days-since
      partdate.dim <- ncdim_def(paste(partdatname,'date',sep=''),"days since 2000-01-01 00:00:00 UTC",vals=partdate.vals)

      vars[[paste('var.',partdatname,sep='')]] <- ncvar_def(name=partdatname,units="various",dim=list(partdate.dim,npartnames.dim),
                                                            missval=-1e34,longname=part.string,compression=9)

      outs[[paste(partdatname,'names',sep='')]] <- partnames
      outs[[partdatname]] <- partdat
    } else warns$partdatnull<- "PARTICLE FILE IS EMPTY"
  }#end if part

  if(foot){
    if(is.null(footdat)) footdat <-  getr(paste("footr",ident,sep=""), footpath)
    if(!is.null(footdat)){
      #for netCDF standard maintain order of dimensions as lon, lat, time; displayed in ncdump as time, lat, lon
      footdat <- aperm(footdat,c(2,1,3))

      footlon<-as.numeric(dimnames(footdat)[[1]])
      footlat<-as.numeric(dimnames(footdat)[[2]])
      foothr<-as.numeric(dimnames(footdat)[[3]])
      foottime<-stilt.start-foothr*3600

      footlon.dim <- ncdim_def(paste(footname,'lon',sep=''),units="degrees_east",vals=footlon)
      atts$varid <- c(atts$varid,paste(footname,'lon',sep=''),paste(footname,'lon',sep=''))
      atts$name  <- c(atts$name,'standard_name','description')
      atts$val   <- c(atts$val,'longitude',"degrees longitude of center of grid boxes")

      footlat.dim <- ncdim_def(paste(footname,'lat',sep=''),units="degrees_north",vals=footlat)
      atts$varid <- c(atts$varid,paste(footname,'lat',sep=''),paste(footname,'lat',sep=''))
      atts$name  <- c(atts$name,'standard_name','description')
      atts$val   <- c(atts$val,'latitude',"degrees latitude of center of grid boxes")

      footdate.vals <- as.numeric(difftime(foottime,epoch,units="days")) # subtract the epoch to make days-since
      footdate.dim <- ncdim_def(paste(footname,'date',sep=''),units="days since 2000-01-01 00:00:00 UTC",vals=footdate.vals)

      vars[[paste('var.',footname,sep='')]] <- ncvar_def(name=footname,units="ppm per (micromol m-2 s-1)",dim=list(footlon.dim,footlat.dim,footdate.dim),
                                                         missval=-1e34,longname="gridded footprint",compression=9)
      atts$varid <- c(atts$varid,footname)
      atts$name  <- c(atts$name,'description')
      atts$val   <- c(atts$val,paste("aggregates particle footprints on lon/lat/time grid ",
                                     "starting at hysplit start time (see foothr description), (i.e. foot[,,1] is a lat/lon grid of footprints ",
                                     "aggregated in grid boxes and in time prior to the hysplit start time up to and including footdate[1], which ",
                                     "is foothr[1] hours prior to the hysplit start time; foot[,,2] is a lat/lon grid of footprints aggregated ",
                                     "in grid boxes and in time prior to footdate[1] up to and including footdate[2], which is foothr[2] hours prior ",
                                     "to the hysplit start time; etc)",sep=""))

      vars[[paste('var.',footname,'hr',sep='')]] <- ncvar_def(name=paste(footname,'hr',sep=''),units="hours",dim=list(footdate.dim),
                                                              missval=-1e34,longname="footprint hours back from hysplit start time",compression=9)
      atts$varid <- c(atts$varid,paste(footname,'hr',sep=''))
      atts$name  <- c(atts$name,'description')
      atts$val   <- c(atts$val,paste('hysplit start time => can be found in the netCDF file global attribute: CONTROL. The first 4 numbers ',
                                     'indicate the 2 digit year, month, day and hour',sep=''))

      outs[[footname]] <- footdat
      outs[[paste(footname,'hr',sep='')]] <- foothr
      if(plotfoot){
        library(fields)
        library(maps)

        print(paste("Saving... ", targetdir, "foot", ident, ".pdf", sep=""))

        pdf(file=paste(targetdir,"foot",ident,".pdf",sep=""))
        xfoot<-apply(footdat, MARGIN=c(1,2), FUN=sum)
        temp<-log10(xfoot)
        temp[!is.finite(temp)]<-NA
        image.plot(x=footlon ,y=footlat,z=t(temp), col=c("#FFFFFF",tim.colors(64)),xlab="",ylab="",  zlim=c(-10,0), xlim=c(-180,-40),ylim=c(25,80))
        box()
        map("state", add=T)
        map("world",add=T)
        title(ident)
        dev.off()
      }
    } else warns$footdatnull<- "FOOTPRINT FILE IS EMPTY"

  }#foot
  if(horizconv){
    if (is.null(horizconvname)) {
      horizconvname <- 'horizconv'
      warns$horizconvnamenull <- paste("HORIZCONV object does not have an assigned name ",
                                       "designating unique species and source, ",
                                       "assigning dummy name to horizconvname",sep='')
    }
    if (is.null(horizconvdatunits)) {
      horizconvdatunits <- 'unknown'
      warns$horizconvdatunitsnull <- paste("HORIZCONV object does not have assigned units ",
                                           "assigning dummy units to horizconvdatunits",sep='')
    }
    if(!is.null(horizconvdat)){
      horizconvdatnames <-dimnames(horizconvdat)[[2]]

      ncol.dim<-ncdim_def(paste("ncol",horizconvname,sep=""),"",vals=1:ncol(horizconvdat),create_dimvar=F)
      nrow.dim<-ncdim_def(paste("nrow",horizconvname,sep=""),"",vals=1:nrow(horizconvdat),create_dimvar=F)

      vars[[paste('var.',horizconvname,sep='')]] <- ncvar_def(name=horizconvname,units=horizconvdatunits,dim=list(nrow.dim,ncol.dim),
                                                              missval=-1e34,longname="horizontally convolved mixing ratios per particle",compression=9)
      atts$varid <- c(atts$varid,horizconvname)
      atts$name  <- c(atts$name,'description')
      atts$val   <- c(atts$val,"matrix includes associated particle index values")

      vars[[paste('var.',horizconvname,'names',sep='')]] <- ncvar_def(name=paste(horizconvname,'names',sep=''),units="none",dim=list(dimnchar,ncol.dim),
                                                                      missval=charmissval,longname=paste('column names for ',horizconvname,sep=''),prec="char",compression=9)

      outs[[horizconvname]] <- horizconvdat
      outs[[paste(horizconvname,'names',sep='')]] <- horizconvdatnames
    } else warns$horizconvdatnull<- "HORIZONTAL CONVOLVE SUMMARY RESULTS NOT FOUND"
  }#horizconv

  if(avghorizconv){
    if (is.null(avghorizconvname)) {
      avghorizconvname <- 'avghorizconv'
      warns$avghorizconvnamenull <- paste("AVGHORIZCONV object does not have an assigned name ",
                                          "designating unique species and source, ",
                                          "assigning dummy name to avghorizconvname",sep='')
    }
    if (is.null(avghorizconvdatunits)) {
      avghorizconvdatunits <- 'unknown'
      warns$avghorizconvdatunitsnull <- paste("AVGHORIZCONV object does not have assigned units ",
                                              "assigning dummy units to avghorizconvdatunits",sep='')
    }
    if(!is.null(avghorizconvdat)){
      n.dim<-ncdim_def(paste("n",avghorizconvname,sep=""),"",vals=1:length(avghorizconvdat),create_dimvar=F)

      vars[[paste('var.',avghorizconvname,sep='')]] <- ncvar_def(name=avghorizconvname,units=avghorizconvdatunits,dim=list(n.dim),
                                                                 missval=-1e34,longname="average horizontally convolved mixing ratio of all particles",compression=9)

      outs[[avghorizconvname]] <- avghorizconvdat
    } else warns$avghorizconvdatnull<- "AVGHORIZONTAL CONVOLVE SUMMARY RESULTS NOT FOUND"
  }#avghorizconv

  if (dvmr) {
    if (is.null(dvmrname)) {
      dvmrname <- 'dvmr'
      warns$dvmrnamenull <- paste("DVMR object does not have an assigned name ",
                                  "designating unique species and source, ",
                                  "assigning dummy name to dvmrname",sep='')
    }
    if (is.null(dvmrdatunits)) {
      dvmrdatunits <- 'unknown'
      warns$dvmrdatunitsnull <- paste("DVMRDAT object does not have assigned units ",
                                      "assigning dummy units to dvmrdatunits",sep='')
    }
    if(!is.null(dvmrdat)){
      ndvmrnames.dim<-ncdim_def(paste('n',dvmrname,'names',sep=''),"",vals=1:ncol(dvmrdat),create_dimvar=F)
      vars[[paste('var.',dvmrname,'names',sep='')]] <- ncvar_def(name=paste(dvmrname,'names',sep=''),units="none",
                                                                 dim=list(dimnchar,ndvmrnames.dim),missval=charmissval,
                                                                 longname=paste("column names for ",dvmrname,sep=""),prec="char",compression=9)

      dvmryear <- as.numeric(dimnames(dvmrdat)[[1]])
      dvmryear.dim <- ncdim_def(paste(dvmrname,'year',sep=''),"flux data year",vals=dvmryear)

      vars[[paste('var.',dvmrname,sep='')]] <- ncvar_def(name=dvmrname,units=dvmrdatunits,dim=list(dvmryear.dim,ndvmrnames.dim),
                                                         missval=-1e34,longname="delta volume mixing ratio and uncertainty estimates",compression=9)
      atts$varid <- c(atts$varid,dvmrname)
      atts$name  <- c(atts$name,'description')
      atts$val   <- c(atts$val,"results from horizontally convolving footprints and fluxes")

      outs[[paste(dvmrname,'names',sep='')]] <- dimnames(dvmrdat)[[2]]
      outs[[dvmrname]] <- dvmrdat
    } else warns$dvmrdatnull<- "DVMR RESULTS NOT FOUND"
  }#dvmr

  if (dvmrmean) {
    ndvmrmean.dim<-ncdim_def("ndvmrmean","",vals=1:length(dvmrmeandat),create_dimvar=F)

    vars[[paste('var.',dvmrmeanname,sep='')]] <- ncvar_def(name=dvmrmeanname,units=dvmrmeandatunits,dim=list(ndvmrmean.dim),
                                                           missval=-1e34,longname="climatological mean delta volume mixing ratio components and uncertainties",compression=9)

    vars[[paste('var.',dvmrmeanname,'names',sep='')]] <- ncvar_def(name=paste(dvmrmeanname,'names',sep=''),units="various",dim=list(dimnchar,ndvmrmean.dim),
                                                                   missval=charmissval,longname=paste('names for ',dvmrmeanname,' 1D array',sep=''), prec="char",compression=9)
    outs[[dvmrmeanname]] <- dvmrmeandat
    outs[[paste(dvmrmeanname,'names',sep='')]] <- names(dvmrmeandat)
  }#dvmrmean

  if (dvmrstd) {
    ndvmrstd.dim<-ncdim_def("ndvmrstd","",vals=1:length(dvmrstddat),create_dimvar=F)

    vars[[paste('var.',dvmrstdname,sep='')]] <- ncvar_def(name=dvmrstdname,units=dvmrstddatunits,dim=list(ndvmrstd.dim),
                                                          missval=-1e34,longname=paste("climatological standard deviation for delta volume mixing ratio components ",
                                                                                       "and uncertainties",sep=""),compression=9)

    vars[[paste('var.',dvmrstdname,'names',sep='')]] <- ncvar_def(name=paste(dvmrstdname,'names',sep=''),units="various",dim=list(dimnchar,ndvmrstd.dim),
                                                                  missval=charmissval,longname=paste('names for ',dvmrstdname,' 1D array',sep=''), prec="char",compression=9)
    outs[[dvmrstdname]] <- dvmrstddat
    outs[[paste(dvmrstdname,'names',sep='')]] <- names(dvmrstddat)
  }#dvmrstd

  if(check) {
    if(is.null(checkdat)) checkdat <-  getr(paste("check",ident,sep=""), checkpath)
    if(!is.null(checkdat)){
      checkbasic<-checkdat$basic
      checkbasicnames<-names(checkdat$basic)

      if(!is.null(checkbasic) & !is.null(checkbasicnames)){
        ncheckbasic.dim<-ncdim_def("ncheckbasic","",vals=1:length(checkbasic),create_dimvar=F)

        vars$var.checkbasic <- ncvar_def(name="checkbasic",units="various",dim=list(ncheckbasic.dim),
                                         missval=-1e34,longname="basic output from Trajeccheck()",compression=9)

        vars$var.checkbasicnames<-ncvar_def(name="checkbasicnames",units="various",dim=list(dimnchar,ncheckbasic.dim),
                                            missval=charmissval,longname="names for checkbasic 1D array", prec="char",compression=9)
        outs$checkbasic<-checkbasic
        outs$checkbasicnames<-checkbasicnames
      } else  warns$checkbasicnull<- "CHECKDAT$BASIC IS EMPTY OR NAMES ARE MISSING"

      checksum<-checkdat$summary
      checksumnames<-dimnames(checkdat$summary)[[2]]

      if(!is.null(checksum) & !is.null(checksumnames)){

        checksumtime<-stilt.start+checksum[,"time"]*60

        checksumdate.vals <- as.numeric(difftime(checksumtime,epoch,units="days")) # subtract the epoch to make days-since
        checksumdate.dim <- ncdim_def("checksumdate","days since 2000-01-01 00:00:00 UTC",vals=checksumdate.vals)
        ncolchecksum.dim<-ncdim_def("ncolchecksum","",vals=1:ncol(checksum),create_dimvar=F)

        vars$var.checksumnames<-ncvar_def(name="checksumnames",units="various",dim=list(dimnchar,ncolchecksum.dim),
                                          missval=charmissval,longname="column names for checksum array",prec="char",compression=9)

        vars$var.checksum <- ncvar_def(name="checksum",units="various",dim=list(checksumdate.dim,ncolchecksum.dim),
                                       missval=-1e34,longname="checksum array",compression=9)
        outs$checksum<-checksum
        outs$checksumnames<-checksumnames
        if(plotmeantrajec){
          errorbar<-function(xx,yy,xunc=NULL,yunc=NULL,color=1,ltype=1,lwidth=1,xbar=NULL,ybar=NULL){

            if (!is.null(xunc)) segments(xx+xunc,yy,xx-xunc,yy, col=color,lty=ltype,lwd=lwidth)
            if (!is.null(yunc)) segments(xx,yy+yunc,xx,yy-yunc, col=color,lty=ltype,lwd=lwidth)
            if(!is.null(xbar)&!is.null(xunc)){
              segments(xx-xunc,yy-xbar,xx-xunc,yy+xbar, col=color,lty=ltype,lwd=lwidth)
              segments(xx+xunc,yy-xbar,xx+xunc,yy+xbar, col=color,lty=ltype,lwd=lwidth)
            }
            if(!is.null(ybar)&!is.null(yunc)){
              segments(xx-ybar,yy-yunc,xx+ybar,yy-yunc, col=color,lty=ltype,lwd=lwidth)
              segments(xx-ybar,yy+yunc,xx+ybar,yy+yunc, col=color,lty=ltype,lwd=lwidth)
            }
          } #errorbar

          print(paste("Saving... ", targetdir, "meantrajec", ident, ".pdf", sep=""))

          pdf(file=paste(targetdir,"meantrajec",ident,".pdf",sep=""))
          library(maps)
          plot(checksum[,"mlon"],checksum[,"mlat"],xlim=c(-180,-50),ylim=c(10,70),xlab="",ylab="", type="n")
          errorbar(checksum[,"mlon"],checksum[,"mlat"],xunc=checksum[,"slon"],yunc=checksum[,"slat"],col=8,xbar=0,ybar=0)
          points(checksum[,"mlon"],checksum[,"mlat"], type="l",lwd=3)
          map(add=T)
          map("state", add=T)
          title(ident)
          dev.off()
        } #if plotmeantrajec
      } else  warns$checksumnull<- "CHECKDAT$SUM IS EMPTY OR NAMES ARE MISSING"


      if ('speed' %in% names(checkdat)) {
        #speed is no longer a required field in checkdat
        checkspeed<-checkdat$speed
        checkspeednames<-names(checkdat$speed)

        if(!is.null(checkspeed) & !is.null(checkspeednames)){

          ncheckspeed.dim<-ncdim_def("ncheckspeed","",vals=1:length(checkspeed),create_dimvar=F)

          vars$var.checkspeed <- ncvar_def(name="checkspeed",units="various",dim=list(ncheckspeed.dim),
                                           missval=-1e34,longname="speed output from Trajeccheck()",compression=9)

          vars$var.checkspeednames<-ncvar_def(name="checkspeednames",units="various",dim=list(dimnchar,ncheckspeed.dim),
                                              missval=charmissval,longname="names for checkspeed 1D array", prec="char",compression=9)

          outs$checkspeed<-checkspeed
          outs$checkspeednames<-checkspeednames

        } else  warns$checkspeednull<- "CHECKDAT$SPEED IS EMPTY OR NAMES ARE MISSING"
      }#end if speed in checkdat

    } else warns$checkdatnull<- "CHECKDAT IS EMPTY"

  }#check

  if(runinfo){
    if(is.null(runinfodat)) runinfodat <- getr(paste("run.info",ident,sep=""), runinfopath)
    if(!is.null(runinfodat)){

      runinfonames<-dimnames(runinfodat)[[2]]


      nruninfo.dim<-ncdim_def("nruninfo","",vals=1:(dim(runinfodat)[2]),create_dimvar=T)
      vars$var.runinfonames<-ncvar_def(name="runinfonames",units="various",dim=list(dimnchar,nruninfo.dim),
                                       missval=charmissval,longname="names for runinfo array",prec="char",compression=9)
      vars$var.runinfo<-ncvar_def(name="runinfo",units="various",dim=list(dimnchar,nruninfo.dim),
                                  missval=charmissval,longname="runinfo array",prec="char",compression=9)

      outs$runinfonames<-runinfonames
      outs$runinfo<-runinfodat
    }  else  warns$runinfonull<- "RUNINFO IS EMPTY OR NAMES ARE MISSING"
  }#runinfo

  if (init) {
    if (is.null(initname)) {
      initname <- 'init'
      warns$initnamenull <- paste("INIT object does not have an assigned name ",
                                  "designating unique species and source, ",
                                  "assigning dummy name to initname",sep='')
    }
    if (is.null(initdatunits)) {
      initdatunits <- 'unknown'
      warns$initdatunitsnull <- paste("INIT object does not have assigned units ",
                                      "assigning dummy units to initdatunits",sep='')
    }
    if (!is.null(initdat)) {
      initdatnames <- dimnames(initdat)[[2]]
      nrow.dim <- ncdim_def(paste("nrow",initname,sep=""),"",vals=1:nrow(initdat),create_dimvar=F)
      ncol.dim <- ncdim_def(paste("ncol",initname,sep=""),"",vals=1:ncol(initdat),create_dimvar=F)

      vars[[paste('var.',initname,sep='')]] <- ncvar_def(name=initname,units=initdatunits,dim=list(nrow.dim,ncol.dim),
                                                         missval=-1e34,longname="matrix containing initial mixing ratio values of particles and associated particle index value",compression=9)

      vars[[paste('var.',initname,'names',sep='')]] <- ncvar_def(name=paste(initname,'names',sep=''),units='none',dim=list(dimnchar,ncol.dim),
                                                                 missval=charmissval,longname="column names of associated init data matrix",prec="char",compression=9)

      outs[[initname]] <- initdat
      outs[[paste(initname,'names',sep='')]] <- initdatnames

    }else{warns$initdatnull<- "INITDAT IS EMPTY OR NAMES ARE MISSING"}
  }#init

  if (avginit) {
    if (is.null(avginitname)) {
      avginitname <- 'avginit'
      warns$avginitnamenull <- paste("AVGINIT object does not have an assigned name ",
                                     "designating unique species and source, ",
                                     "assigning dummy name to avginitname",sep='')
    }
    if (is.null(avginitdatunits)) {
      avginitdatunits <- 'unknown'
      warns$avginitdatunitsnull <- paste("AVGINIT object does not have assigned units ",
                                         "assigning dummy units to avginitdatunits",sep='')
    }
    if (!is.null(avginitdat)) {
      n.dim <- ncdim_def(paste("n",avginitname,sep=""),"",vals=1:length(avginitdat),create_dimvar=F)

      vars[[paste('var.',avginitname,sep='')]] <- ncvar_def(name=avginitname,units=avginitdatunits,dim=list(n.dim),
                                                            missval=-1e34,longname="average initial mixing ratio value of all particles",compression=9)

      outs[[avginitname]] <- avginitdat

    }else{warns$avginitdatnull<- "AVGINITDAT IS EMPTY OR NAMES ARE MISSING"}
  }#avginit

  if (emit) {
    if (!is.null(emitdat)) {
      n.dim <- ncdim_def("nemitwindow","",vals=1:dim(emitdat)[[2]],create_dimvar=F)

      vars$var.emitwindow <- ncvar_def(name='emitwindow',units='hours, x grid lengths, y grid lengths, z grid lengths',
                                       dim=list(n.dim),missval=-1e34,
                                       longname="emission time and space window of particle releases at receptor",compression=9)
      atts$varid <- c(atts$varid,'emitwindow')
      atts$name  <- c(atts$name,'description')
      atts$val   <- c(atts$val,paste('emithrs: length (in hours) of emission time ',
                                     'period for time-average receptors (default: 1/3600, for instantaneous ',
                                     'releases);   emitdx/emitdy/emitdz: if > 0, specifies randomized horizontal ',
                                     'and vertical release locations for this receptor (xreceptor +/- dx, yreceptor',
                                     ' +/- dy, zreceptor +/- dz instead of xreceptor,yreceptor,zreceptor)',sep=''))

      vars$var.emitwindownames <- ncvar_def(name="emitwindownames",units="various",
                                            dim=list(dimnchar,n.dim),missval=charmissval,
                                            longname="names for emitwindow 1D array", prec="char",compression=9)
      outs$emitwindow <- array(emitdat)
      outs$emitwindownames <- colnames(emitdat)
    }else{warns$emitdatnull<- "EMITDAT IS EMPTY OR NAMES ARE MISSING"}
  }#emit

  ncnameout<-paste(targetdir,ncname,sep="")

  varnames<-names(vars)
  outnames<-matrix(unlist(strsplit(names(vars),"var.")),ncol=2, byrow=T)[,2]


  if (appendnc && file.exists(ncnameout)) {
    #Append existing NCDF output file and write variable to file.

    #retrieve data from existing netCDF file before its overwritten
    filenc <- nc_open(ncnameout, write=TRUE)
    oldnames <- names(filenc$var)
    cat('make.hysplit.nc4.r: netCDF variable names in existing file = ',oldnames,'\n')

    #find variables that are missing in newest version of file
    for (i in 1:length(outnames)) {
      if (any(oldnames == outnames[i])) {
        if (outnames[i] == 'ident') {
          cat(' Skipping: variable = ', outnames[i],'is already in netCDF file\n')
        }else{
          stop('Do not overwrite: Variable = ', outnames[i],'is already in netCDF file\n')
        }
      }else{
        #variable is not in file, append to file
        cat('Attempt to append file with variable = ', outnames[i],'\n')

        #modify file handle to contain new ncvar4 variable (data is junk)
        filenc <- ncvar_add(filenc, vars[[varnames[i]]])

        #close file handle to save ncvar4 changes and reopen to add data
        #(ncvar_put won't work without closing and reopening)
        nc_close(filenc)
        filenc <- nc_open(ncnameout, write=TRUE)
        ncvar_put(filenc, vars[[varnames[i]]],outs[[outnames[i]]])
      }
    }#end for i

    #attributes
    if (length(atts) > 0) {
      for (nn in 1:length(atts$varid)) {
        if (!any(oldnames == atts$varid[[nn]])) {
          ncatt_put(filenc,atts$varid[[nn]],atts$name[[nn]],atts$val[[nn]])
        }
      }
    }

    #close file
    nc_close(filenc)

  }else{
    #Create new NCDF output file and write variable to file.
    ncf <- nc_create(ncnameout,vars=vars)

    #global attributes
    if (length(global.att.list) == 0) {
      # None specified in arg list as global.att.list, use the default:
      ncatt_put(ncf,varid=0,'Title','Output from HYSPLIT')
      ncatt_put(ncf,varid=0,'Conventions','CF-1.6')
      # plus file contents from specified global.att.files:
      if (length(global.att.files) > 0) {
        if (is.null(names(global.att.files))) names(global.att.files) <- global.att.files
        for (igatt in 1:length(global.att.files)) {
          gatt.name <- names(global.att.files)[igatt]
          #          gatt.fname <- global.att.files[igatt]
          gatt.fname <- global.att.files[[gatt.name]]   # made this change - kwt
          gatt.str <- NULL
          if (file.exists(gatt.fname)) gatt.str <- scan(gatt.fname,what="",sep="\n",
                                                        quiet = TRUE, blank.lines.skip = FALSE)
          if (length(gatt.str) > 0) {
            gatt.str <- paste(paste(gatt.str,collapse="\n"),"\n",sep="")
            ncatt_put(ncf,varid=0,gatt.name,gatt.str)
          }
        }
      }
    } else {
      # Global attributes specified from arg.list
      # (These could have been obtained via "gatt.list <- ncatt_get(filenc,varid=0)")
      for (gatt.name in names(global.att.list))
        ncatt_put(ncf,varid=0,gatt.name,global.att.list[[gatt.name]])
    }
    for (nn in 1:length(varnames)) {
      cat('Attempting to write netCDF data for variable = ',varnames[nn],'\n')
      ncvar_put(ncf, vars[[varnames[nn]]],outs[[outnames[nn]]])
    }

    #attributes
    if (length(atts) > 0) {
      for (nn in 1:length(atts$varid)) {
        ncatt_put(ncf,atts$varid[[nn]],atts$name[[nn]],atts$val[[nn]])
      }
    }

    nc_close(ncf)
  }#end if appendnc

  errorlogout<-paste(targetdir,errorlog,sep="")
  if(!is.null(names(warns)) && !is.na(errorlog)) {
    do.append=FALSE
    for(ww in names(warns)) {
      cat(paste(ww,': ',warns[[ww]],'\n',sep=''),file=errorlogout, append=do.append)
      do.append=TRUE
    }
  } #if warnings

  warns

} #end function


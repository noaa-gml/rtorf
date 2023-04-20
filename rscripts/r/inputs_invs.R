
library(colorout)
library(data.table)
library(robspackfilter)

# dir.create("invfiles")

# aircraft insitu ####
ma1 <- paste0("master/master_aircraft_insitu_", 2017:2021, ".csv")
na <- gsub(pattern = ".csv", replacement = "", x = ma1)
na <- gsub(pattern = "master/master_", replacement = "", x = na)

lx <- list()

for(i in seq_along(ma1)) {
  if(!file.exists(ma1[i])) {
    cat("skiiping ", ma1[i], "\n")
    next
  }
  dx <- fread(ma1[i])
  dx$file <- na[i]
  print(dim(dx))
  lx[[i]] <- dx
}
master <- obs_list.dt(lx)
# filtering between dec 2017 and jan 2021

master <- master[timeUTC > ISOdatetime(2017,12,1,0,0,0, tz = "UTC") &
                   timeUTC < ISOdatetime(2021,2,1,0,0,0, tz = "UTC")]
#  footprints_hera_hysplit.txt
master$nc <- obs_footname(year = master$year,
                          month = master$month,
                          day = master$day,
                          hour = master$hour,
                          minute = master$minute,
                          lat = master$latitude,
                          lon = master$longitude,
                          alt = master$altitude_final)

path <-  "/work/noaa/co2/footprint/hysplit/CPO2021/hysplit4/aircraft/flask/" # param

master$fnc <- paste0(path, master$nc)

master$is_fnc <- file.exists(master$fnc)

table(master$is_fnc)

master <- master[is_fnc == TRUE]

master <- master[!is.na(value)]

master <- master[altitude_final > 10]

dfaircraft <- obs_invfiles(master = master,
                           path = path,
                           Type = "continuous",
                           SubType = "aircraft")
plot(dfaircraft)

obs_plot(dfsurface$master,
         time = "timeUTC",
         y = "value",
         colu = "site_code",
         n = c("BRA", "EGB", "EST", "ETL", "LLB"),
         yfactor = 1e9,
         cex = 0.5,
         type = "l",
         ylim = c(1800, 3000)
)


# Found  760 missing `value` on master
# It is possible that this script must be run again to explude missing value


# tower insitu ####
ma1 <- paste0("master/master_tower_insitu_", 2017:2021, ".csv")
na <- gsub(pattern = ".csv", replacement = "", x = ma1)
na <- gsub(pattern = "master/master_", replacement = "", x = na)

lx <- list()

for(i in seq_along(ma1)) {
  if(!file.exists(ma1[i])) {
    cat("skiiping ", ma1[i], "\n")
    next
  }
  dx <- fread(ma1[i])
  dx$file <- "tower-insitu"
  lx[[i]] <- dx
}
master <- obs_list.dt(lx)
master <- unique(master[altitude_final == max_altitude])

# filtering between dec 2017 and jan 2021
master <- master[timeUTC > ISOdatetime(2017,12,1,0,0,0, tz = "UTC") &
                   timeUTC < ISOdatetime(2021,2,1,0,0,0, tz = "UTC")]

# footprints_hera_hysplit.txt
master$nc <- obs_footname(year = master$year_end,
                          month = master$month_end,
                          day = master$day_end,
                          hour = master$hour_end,
                          minute = master$minute_end,
                          lat = master$latitude,
                          lon = master$longitude,
                          alt = master$max_altitude)

path <-  "/work/noaa/co2/footprint/hysplit/CPO2021/hysplit4/tower/continuous/" # param

master$fnc <- paste0(path, master$nc)

master$is_fnc <- file.exists(master$fnc)

table(master$is_fnc)

master <- master[is_fnc == TRUE]

master <- master[!is.na(value)]

master <- master[altitude_final > 10]

dftower <- obs_invfiles(master = master,
                        path = path,
                        Type = "continuous",
                        SubType = "ground",
                        Surface_Elev = master$max_altitude)

plot(dftower)


lx <- pbapply::pblapply(seq_along(dftower$footprints_hera_hysplit),
                        function(i){
                          nc <- ncdf4::nc_open(dftower$footprints_hera_hysplit[i])
                          f <- ncdf4::ncvar_get(nc, "foot1")
                          ncdf4::nc_close(nc)
                          sum(f)
                        })

dftower$master$foot <- unlist(lx)
dftower$master$foot <- units::set_units(dftower$master$foot, "ppm/micromol/m^2/s")


# surface insitu ####
ma1 <- paste0("master/master_surface_insitu_", 2017:2021, ".csv")
na <- gsub(pattern = ".csv", replacement = "", x = ma1)
na <- gsub(pattern = "master/master_", replacement = "", x = na)

lx <- list()

for(i in seq_along(ma1)) {
  if(!file.exists(ma1[i])) {
    cat("skiiping ", ma1[i], "\n")
    next
  }
  dx <- fread(ma1[i])
  print(dim(dx))
  dx$file <- "surface-insitu"
  lx[[i]] <- dx
}
master <- obs_list.dt(lx)
master <- unique(master[altitude_final == max_altitude])

# filtering between dec 2017 and jan 2021
master <- master[timeUTC > ISOdatetime(2017,12,1,0,0,0, tz = "UTC") &
                   timeUTC < ISOdatetime(2021,2,1,0,0,0, tz = "UTC")]

# footprints_hera_hysplit.txt
master$nc <- obs_footname(year = master$year_end,
                          month = master$month_end,
                          day = master$day_end,
                          hour = master$hour_end,
                          minute = master$minute_end,
                          lat = master$latitude,
                          lon = master$longitude,
                          alt = master$max_altitude)

path <-  "/work/noaa/co2/footprint/hysplit/CPO2021/hysplit4/tower/continuous/" # param

master$fnc <- paste0(path, master$nc)

master$is_fnc <- file.exists(master$fnc)

table(master$is_fnc)

master <- master[is_fnc == TRUE]

master <- master[!is.na(value)]

master <- master[altitude_final > 10]

dfsurface <- obs_invfiles(master = master,
                          path = path,
                          Type = "continuous",
                          SubType = "ground",
                          Surface_Elev = master$max_altitude)
plot(dfsurface)


# flask non noaa ####
ma1 <- paste0("master/master_flask_no_noaa_", 2017:2021, ".csv")
na <- gsub(pattern = ".csv", replacement = "", x = ma1)
na <- gsub(pattern = "master/master_", replacement = "", x = na)

lx <- list()

for(i in seq_along(ma1)) {
  if(!file.exists(ma1[i])) {
    cat("skiiping ", ma1[i], "\n")
    next
  }
  dx <- fread(ma1[i])
  dx$file <- na[i]
  print(dim(dx))
  lx[[i]] <- dx
}
master <- obs_list.dt(lx)

# filtering between dec 2017 and jan 2021

master <- master[timeUTC > ISOdatetime(2017,12,1,0,0,0, tz = "UTC") &
                   timeUTC < ISOdatetime(2021,2,1,0,0,0, tz = "UTC")]
#  footprints_hera_hysplit.txt
master$nc <- obs_footname(year = master$year,
                          month = master$month,
                          day = master$day,
                          hour = master$hour,
                          minute = master$minute,
                          lat = master$latitude,
                          lon = master$longitude,
                          alt = master$altitude_final)

path <-  "/work/noaa/co2/footprint/hysplit/CPO2021/hysplit4/flask_non_noaa/flask/"

master$fnc <- paste0(path, master$nc)

master$is_fnc <- file.exists(master$fnc)

table(master$is_fnc)

master <- master[is_fnc == TRUE]

master <- master[!is.na(value)]

master <- master[altitude_final > 10]

dfflask_nn <- obs_invfiles(master = master,
                           path = path,
                           Type = "flask",
                           SubType = "ground")

plot(dfflask_nn)


# merge files and create inputs ####

# footprints_hera_hysplit ####

hela <- c(dfaircraft$footprints_hera_hysplit,
          dftower$footprints_hera_hysplit,
          dfsurface$footprints_hera_hysplit,
          dfflask_nn$footprints_hera_hysplit)


fwrite(x = as.data.table(hela),
       file = "invfiles/footprints_hera_hysplit.txt",
       quote = FALSE,
       row.names = FALSE,
       col.names = FALSE,
       showProgress = TRUE)

# obs_hysplit ####
obsh <- c(dfaircraft$obs_hysplit,
          dftower$obs_hysplit,
          dfsurface$obs_hysplit,
          dfflask_nn$obs_hysplit)


fwrite(x = as.data.table(obsh),
       file = "invfiles/obs_hysplit.txt",
       quote = FALSE,
       row.names = FALSE,
       col.names = FALSE,
       showProgress = TRUE)


# receptor_info_hysplit ####
#original file has a header?
reih <- rbind(dfaircraft$receptor_info_hysplit,
              dftower$receptor_info_hysplit,
              dfsurface$receptor_info_hysplit,
              dfflask_nn$receptor_info_hysplit)


fwrite(x = reih,
       file = "invfiles/receptor_info_hysplit.txt",
       quote = FALSE,
       sep = "\t",
       row.names = FALSE,
       col.names = TRUE,
       showProgress = TRUE)


reihnos <- reih
reihnos$Scale <- NULL

fwrite(x = reihnos,
       file = "invfiles/receptor_info_hysplit_noscale.txt",
       quote = FALSE,
       sep = "\t",
       row.names = FALSE,
       col.names = TRUE,
       showProgress = TRUE)


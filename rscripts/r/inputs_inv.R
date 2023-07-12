
library(colorout)
library(data.table)
library(robspack)

dir.create("invfiles")

# aircraft insitu ####
ma1 <- paste0("/path/master/master_aircraft_insitu_", 2017:2021, ".csv")
na <- gsub(pattern = ".csv", replacement = "", x = ma1)
na <- gsub(pattern = "master/master_", replacement = "", x = na)

lx <- list()

for(i in seq_along(ma1)) {
  if(!file.exists(ma1[i])) {
    cat("skipping ", ma1[i], "\n")
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

master$nc <- obs_footname(year = master$year,
                          month = master$month,
                          day = master$day,
                          hour = master$hour,
                          minute = master$minute,
                          lat = master$latitude,
                          lon = master$longitude,
                          alt = master$altitude_final)

path <-  "/path/aircraft/flask/" # param

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
obs_plot(dfaircraft$master,
         time = "timeUTC",
         y = "value",
         colu = "site_code",
         yfactor = 1e9,
         cex = 0.5,
         type = "l"
)


# tower insitu ####
ma1 <- paste0("master/master_tower_insitu_", 2017:2021, ".csv")
na <- gsub(pattern = ".csv", replacement = "", x = ma1)
na <- gsub(pattern = "master/master_", replacement = "", x = na)

lx <- list()

for(i in seq_along(ma1)) {
  if(!file.exists(ma1[i])) {
    cat("skipping ", ma1[i], "\n")
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

master$nc <- obs_footname(year = master$year_end,
                          month = master$month_end,
                          day = master$day_end,
                          hour = master$hour_end,
                          minute = master$minute_end,
                          lat = master$latitude,
                          lon = master$longitude,
                          alt = master$max_altitude)

path <-  "/path/tower/continuous/" # param

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

obs_plot(dftower$master,
         time = "timeUTC",
         y = "value",
         colu = "site_code",
         yfactor = 1e9,
         cex = 0.5,
         type = "l"
)

# surface insitu ####
ma1 <- paste0("master/master_surface_insitu_", 2017:2021, ".csv")
na <- gsub(pattern = ".csv", replacement = "", x = ma1)
na <- gsub(pattern = "master/master_", replacement = "", x = na)

lx <- list()

for(i in seq_along(ma1)) {
  if(!file.exists(ma1[i])) {
    cat("skipping ", ma1[i], "\n")
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

master$nc <- obs_footname(year = master$year_end,
                          month = master$month_end,
                          day = master$day_end,
                          hour = master$hour_end,
                          minute = master$minute_end,
                          lat = master$latitude,
                          lon = master$longitude,
                          alt = master$max_altitude)

path <-  "/path/tower/continuous/" # param

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
obs_plot(dfsurface$master,
         time = "timeUTC",
         y = "value",
         colu = "site_code",
         yfactor = 1e9,
         cex = 0.5,
         type = "l"
)


# flask non noaa ####
ma1 <- paste0("master/master_flask_no_noaa_", 2017:2021, ".csv")
na <- gsub(pattern = ".csv", replacement = "", x = ma1)
na <- gsub(pattern = "master/master_", replacement = "", x = na)

lx <- list()

for(i in seq_along(ma1)) {
  if(!file.exists(ma1[i])) {
    cat("skipping ", ma1[i], "\n")
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

master$nc <- obs_footname(year = master$year,
                          month = master$month,
                          day = master$day,
                          hour = master$hour,
                          minute = master$minute,
                          lat = master$latitude,
                          lon = master$longitude,
                          alt = master$altitude_final)

path <-  "/path/flask_non_noaa/flask/"

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

obs_plot(dfflask_nn$master,
         time = "timeUTC",
         y = "value",
         colu = "site_code",
         yfactor = 1e9,
         cex = 0.5,
         type = "l"
)



# merge files and create inputs ####

# footprints_hera_hysplit ####
hela <- c(dfaircraft$footprints_hera_hysplit,
          dftower$footprints_hera_hysplit,
          dfsurface$footprints_hera_hysplit,
          dfflask_nn$footprints_hera_hysplit)


fwrite(x = as.data.table(hela),
       file = "receptors/footprints_hera_hysplit.txt",
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
       file = "receptors/obs_hysplit.txt",
       quote = FALSE,
       row.names = FALSE,
       col.names = FALSE,
       showProgress = TRUE)


# receptor_info_hysplit ####
reih <- rbind(dfaircraft$receptor_info_hysplit,
              dftower$receptor_info_hysplit,
              dfsurface$receptor_info_hysplit,
              dfflask_nn$receptor_info_hysplit)


fwrite(x = reih,
       file = "receptors/receptor_info_hysplit.txt",
       quote = FALSE,
       sep = "\t",
       row.names = FALSE,
       col.names = TRUE,
       showProgress = TRUE)


reihnos <- reih
reihnos$Scale <- NULL

fwrite(x = reihnos,
       file = "receptors/receptor_info_hysplit_noscale.txt",
       quote = FALSE,
       sep = "\t",
       row.names = FALSE,
       col.names = TRUE,
       showProgress = TRUE)


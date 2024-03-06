#example Rscript rscripts/tower_year.R 2020 obs/obs_tower-insitu.csv

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args) != 2) {
  stop("At least one argument must be supplied (year).n and obs_index",
       call.=FALSE)
} else {
  yy <-  as.numeric(args[1] )
  print(yy)

  ind  <- as.character(args[2])
  print(ind)

}
out <- paste0("master/master_tower_insitu_", yy)

message("All missing values are filled with 999999")

n01 <- ("                  _     rtorffilter              \n")
n02 <- ("               (`  ).                  _           \n")
n03 <- ("              (     ).             .:(`  )`.       \n")
n04 <- (")           _(       '`.          :(   .    )      \n")
n05 <- ("        .=(`(      .   )     .--  `.  (    ) )      \n")
n06 <- ("       ((    (..__.:'-'   .+(   )   ` _`  ) )                 \n")
n07 <- ("`.     `(       ) )       (   .  )     (   )  ._   \n")
n08 <- ("  )      ` __.:'   )     (   (   ))     `-'.-(`  ) \n")
n09 <- (")  )  ( )       --'       `- __.'         :(      )) \n")
n10 <- (".-'  (_.'          .')                    `(    )  ))\n")
n11 <- ("                  (_  )                     ` __.:'          \n")
n12 <- ("\n")
n13 <- ("--..,___.--,--'`,---..-.--+--.,,-,,..._.--..-._.-a:f--.\n")
cat(c(n01, n02,n03, n04, n05,
      n06, n07, n08, n09, n10,
      n11, n12, n13))

library(data.table)
library(rtorf)

# 0 filters ####
north <- 80
south <- 10
west <- -170
east <- -50
max_altitude <- 8000
evening <- 14:15

# 1 Index ####
#cat("Running obs_index(obs)\n")
#index <- obs_index(obs = obs)

# 2 Read ####
#cat("Running obs_read(index = index,
#               categories = 'tower-insitu',
#               verbose = T)\n")
# df <- obs_read(index = index,
#               categories = "tower-insitu",
#               verbose = T)
df <- fread(ind)

# 3 Filtering space and time ####
cat("Filterinig\n")
df <- rbind(df[year == yy - 1 & month == 12],
            df[year == yy],
            df[year == yy + 1 & month == 1])

df <- df[altitude_final < max_altitude &
           latitude < north &
           latitude > south &
           longitude < east &
           longitude > west]

names(df)


# 4 Max altitude by site ####
dfa <- df[,
          max(altitude_final),
          by = site_code] |> unique()

names(dfa)[2] <- "max_altitude"


# 5 Adding time columns ####
cat("Running obs_addtime(df)\n")
df2 <- obs_addtime(df)

# 6 Add local time ####
df3 <- obs_addltime(df2)
timeUTC_new <- cut(x = df3$timeUTC+3600,
                   breaks = "2 hour") |>
  as.character() |>
  as.POSIXct(tz = "UTC")

# check!!!!!
cat("please, check the following time:\n")
data.table(timeUTC_old = df3$timeUTC,
           timeUTC_new           )


df3$timeUTC <- cut(x = df3$timeUTC+3600,
                   breaks = "2 hour") |>
  as.character() |>
  as.POSIXct(tz = "UTC")


# 7 Add local time ####
df3 <- obs_addltime(df3)
df3 <- df3[lh %in% evening] # in this case will only select 15

# 8 Aggregating values every 2 hours ####
cat("Calculating averagaes every 2 hours\n")
df4 <- obs_agg(dt = df3,
               gby = "mean",
               cols = c("value",
                        "latitude",
                        "longitude",
                        "type_altitude",
                        "dif_time",
                        "year_end",
                        "site_utc2lst"),
               verbose = T,
               byalt = T
)
df4[site_code == "CRV" & altitude_final == 17]
df5 <- obs_addltime(df4)

# 9 Highest altitude by site ###
df5 <- merge(x = df5,
             y = dfa,
             by = "site_code",
             all.x = T)

#check
df5[,
    c("site_code",
      "altitude_final",
      "max_altitude")] |> unique()


# 10 Save in master ####
cat("Saving in master/ \n")
master <- df5
#master[is.na(master)] <- 999999
master$timeUTC <- as.character(master$timeUTC)
master$timeUTC_end <- as.character(master$timeUTC_end)
master$local_time <- as.character(master$local_time)
master$latitude <- round(master$latitude, 4)
master$longitude <- round(master$longitude, 4)

fwrite(master,
       paste0(out, ".txt"),
       sep = " ")

fwrite(master,
       paste0(out, ".csv"),
       sep = ",")

# 11 Save csvy in master ####
cat(paste0("Saving csvy in master/ \n"))
notes <- c("sector: tower",
           "missing_values: = 999999",
           "timespan: Observations between Dec year - 1 and Jan year + 1",
           "spatial_limits: north = 80, south = 10, east = -50, west = -170",
           "data: Data averaged between 14 aand 15 local hours",
           "data: For instance, the average between midpoints 10:30 and 11:30 is 11:00",
           "altitude: < 8000",
           "altitudes: There are data for every altitude per site",
           "altitudes: includes column `max_altitude",
           "local_time: if var `site_utc2lst` is not available, calculated as",
           "longitude/15*60*60 (John Miller)")

obs_write_csvy(dt = master,
               notes = notes,
               out = paste0(out, ".csvy"))

# 11 Receptors ####
cat("Generating receptors \n")
message("Change Python/Bash to generate footprints")

receptor <- master[altitude_final == max_altitude,
                   c("site_code",
                     "year",
                     "month",
                     "day",
                     "hour",
                     "minute",
                     "second",
                     "latitude",
                     "longitude",
                     "altitude_final",
                     "type_altitude",
                     "year_end",
                     "month_end",
                     "day_end",
                     "hour_end",
                     "minute_end",
                     "second_end")]
receptor$altitude_final <- round(receptor$altitude_final)
receptor <- obs_format(receptor)

cat("Saving receptors in receptors/ \n")

receptor_agl <- receptor[type_altitude == 0]
receptor_asl <- receptor[type_altitude == 1]

if(nrow(receptor_agl) > 0) {
  cat(paste0(gsub("master",
                  "receptors",
                  out), "_AGL.txt"),
      "\n")

  fwrite(x = receptor_agl,
         file = paste0(gsub("master",
                            "receptors",
                            out), "_AGL.txt"),
         sep = " ")

}

if(nrow(receptor_asl) > 0) {
  cat(paste0(gsub("master",
                  "receptors",
                  out), "_ASL.txt"),
      "\n")

  fwrite(x = receptor_asl,
         file = paste0(gsub("master",
                            "receptors",
                            out), "_ASL.txt"),
         sep = " ")

}


message("Coordinates are round with 4 decimals  to match bash script")


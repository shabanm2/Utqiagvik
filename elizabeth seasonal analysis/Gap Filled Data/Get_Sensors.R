# Track all the sensors we have data for

library(dplyr)

# Get files from Transpose_Data

# change filepath to your directory - folder where Transpose_Data.Rmd exports data
filepath = "/Users/emvanmetre/Desktop/Arctic/Gap_Filled_Data/Transposed/"
exportpath = "/Users/emvanmetre/Desktop/Arctic/SENSOR_TRACKING_DATA/"


# filename scheme: [var]_gap_filled_[SITE].csv

# vars:
  # temp
  # vwc
  # solar
  # wind

# SITEs:
  # TNHA
  # SSMH
  # BEO

# temp

temp_tnha <- read.csv(paste0(filepath, "temp_gap_filled_TNHA.csv"))
temp_sensors_tnha <- temp_tnha$sensor %>% unique()
temp_sensors_tnha <- data.frame(temp_sensors_tnha, "TNHA")
colnames(temp_sensors_tnha) <- c("sensor", "site")

temp_ssmh <- read.csv(paste0(filepath, "temp_gap_filled_SSMH.csv"))
temp_sensors_ssmh <- temp_ssmh$sensor %>% unique()
temp_sensors_ssmh <- data.frame(temp_sensors_ssmh, "SSMH")
colnames(temp_sensors_ssmh) <- c("sensor", "site")

temp_beo <- read.csv(paste0(filepath, "temp_gap_filled_BEO.csv"))
temp_sensors_beo <- temp_beo$sensor %>% unique()
temp_sensors_beo <- data.frame(temp_sensors_beo, "BEO")
colnames(temp_sensors_beo) <- c("sensor", "site")

temp_sensors <- rbind(temp_sensors_tnha, temp_sensors_ssmh, temp_sensors_beo)
write.csv(temp_sensors, paste0(exportpath, "all_temp_sensors.csv"))

# vwc

vwc_tnha <- read.csv(paste0(filepath, "vwc_gap_filled_TNHA.csv"))
vwc_sensors_tnha <- vwc_tnha$sensor %>% unique()
vwc_sensors_tnha <- data.frame(vwc_sensors_tnha, "TNHA")
colnames(vwc_sensors_tnha) <- c("sensor", "site")

vwc_ssmh <- read.csv(paste0(filepath, "vwc_gap_filled_SSMH.csv"))
vwc_sensors_ssmh <- vwc_ssmh$sensor %>% unique()
vwc_sensors_ssmh <- data.frame(vwc_sensors_ssmh, "SSMH")
colnames(vwc_sensors_ssmh) <- c("sensor", "site")

vwc_beo <- read.csv(paste0(filepath, "vwc_gap_filled_BEO.csv"))
vwc_sensors_beo <- vwc_beo$sensor %>% unique()
vwc_sensors_beo <- data.frame(vwc_sensors_beo, "BEO")
colnames(vwc_sensors_beo) <- c("sensor", "site")

vwc_sensors <- rbind(vwc_sensors_tnha, vwc_sensors_ssmh, vwc_sensors_beo)
write.csv(vwc_sensors, paste0(exportpath, "all_vwc_sensors.csv"))

# solar

solar_tnha <- read.csv(paste0(filepath, "solar_gap_filled_TNHA.csv"))
solar_sensors_tnha <- solar_tnha$sensor %>% unique()
solar_sensors_tnha <- data.frame(solar_sensors_tnha, "TNHA")
colnames(solar_sensors_tnha) <- c("sensor", "site")

solar_ssmh <- read.csv(paste0(filepath, "solar_gap_filled_SSMH.csv"))
solar_sensors_ssmh <- solar_ssmh$sensor %>% unique()
solar_sensors_ssmh <- data.frame(solar_sensors_ssmh, "SSMH")
colnames(solar_sensors_ssmh) <- c("sensor", "site")

solar_beo <- read.csv(paste0(filepath, "solar_gap_filled_BEO.csv"))
solar_sensors_beo <- solar_beo$sensor %>% unique()
solar_sensors_beo <- data.frame(solar_sensors_beo, "BEO")
colnames(solar_sensors_beo) <- c("sensor", "site")

solar_sensors <- rbind(solar_sensors_tnha, solar_sensors_ssmh, solar_sensors_beo)
write.csv(solar_sensors, paste0(exportpath, "all_solar_sensors.csv"))

# wind

wind_tnha <- read.csv(paste0(filepath, "wind_gap_filled_TNHA.csv"))
wind_sensors_tnha <- wind_tnha$sensor %>% unique()
wind_sensors_tnha <- data.frame(wind_sensors_tnha, "TNHA")
colnames(wind_sensors_tnha) <- c("sensor", "site")

wind_ssmh <- read.csv(paste0(filepath, "wind_gap_filled_SSMH.csv"))
wind_sensors_ssmh <- wind_ssmh$sensor %>% unique()
wind_sensors_ssmh <- data.frame(wind_sensors_ssmh, "SSMH")
colnames(wind_sensors_ssmh) <- c("sensor", "site")

wind_beo <- read.csv(paste0(filepath, "wind_gap_filled_BEO.csv"))
wind_sensors_beo <- wind_beo$sensor %>% unique()
wind_sensors_beo <- data.frame(wind_sensors_beo, "BEO")
colnames(wind_sensors_beo) <- c("sensor", "site")

wind_sensors <- rbind(wind_sensors_tnha, wind_sensors_ssmh, wind_sensors_beo)
write.csv(wind_sensors, paste0(exportpath, "all_wind_sensors.csv"))
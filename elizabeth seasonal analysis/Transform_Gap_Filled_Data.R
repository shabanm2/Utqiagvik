# link to file path with gap filled data
# the files are too big to read from github (too many columns)
#
path <- "/Users/emvanmetre/Desktop/Arctic/Gap_Filled_Data"

tempfile <- "Temp_data_CLEAN_TNHA_from_2022_07_15_to_2024_08_14.csv"
moistfile <- "Moist_data_CLEAN_TNHA_from_2022_07_15_to_2024_08_14.csv"

# filenames
# Temperature
# Temp_data_CLEAN_TNHA_from_2022_05_17_to_2024_08_14_no_gps.csv
# Temp_data_CLEAN_SSMH_from_2022_06_08_to_2024_08_28_no_gps.csv
# Temp_data_CLEAN_BEO_from_2022_06_13_to_2024_08_31_no_gps.csv

# VWC
# Moist_data_CLEAN_TNHA_from_2022_05_17_to_2024_08_14_no_gps.csv
# Moist_data_CLEAN_SSMH_from_2022_06_08_to_2024_08_28_no_gps.csv
# Moist_data_CLEAN_BEO_from_2022_06_13_to_2024_08_31_no_gps.csv

temp <- read.csv(paste0(path, tempfile))
moist <- read.csv(paste0(path, moistfile))

# ======================================
# turn into long dataframes
# ======================================
# -----------------------------
# libraries
# -----------------------------
library(dplyr)
library(tidyr)
library(splitstackshape)

# -----------------------------
# ground temperature
# -----------------------------
# rename sensors and columns first
colnames(temp)[1:4] <- c("sensor", "depthcm", "latitude", "longitude")

temp_long <- gather(temp, date, value, 5:ncol(temp))
temp_long <- cSplit(temp_long, 'sensor', sep="-", direction = "wide")
temp_long <- select(temp_long, date, sensor_1, sensor_2, depthcm, longitude, latitude, value)
colnames(temp_long)[2:3] <- c("sensor", "sensordepth")

# get rid of leading X
temp_long$date <- gsub('X', '', temp_long$date)

# --delete--
# separate date and time by a space
# rename_dates <- paste0(substring(temp_long$date, 1, 10), ' ', substring(temp_long$date, 12, 19))
# ----------

# format date and time
dates <- as.POSIXct(temp_long$date,format="%Y.%m.%d.%H.%M.%S",tz="UTC")
# -----------------------------

# -----------------------------
# volumetric water content
# -----------------------------
# rename sensors and columns first
colnames(moist)[1:4] <- c("sensor", "depthcm", "latitude", "longitude")

moist_long <- gather(moist, date, value, 5:ncol(moist))
moist_long <- cSplit(moist_long, 'sensor', sep="-", direction = "wide")
moist_long <- select(moist_long, date, sensor_1, sensor_2, depthcm, longitude, latitude, value)
colnames(moist_long)[2:3] <- c("sensor", "sensordepth")

# get rid of leading X
moist_long$date <- gsub('X', '', moist_long$date)

# format date and time
dates <- as.POSIXct(moist_long$date,format="%Y.%m.%d.%H.%M.%S",tz="UTC")
moist_long$date <- dates
# -----------------------------

# ======================================
# rename sensors
# ======================================
# -----------------------------
# volumetric water content
# -----------------------------
vwcsensors <- c('21393048'='BEO-OLD','21398591'='BEO-OLD','21398585'='BUECI-BASE',
                '21398590'='BUECI-SA','21398583'='BUECI-SB','21393042'='BUECI-SC',
                '21398584'='BUECI-SD','21398579'='BUECI-SE','21398578'='BUECI-SF01',
                '21398598'='BUECI-SF02','21398576'='REMOVE','21398588'='SSMH-BASE',
                '21398599'='SSMH-SA','21393044'='SSMH-SB','21393049'='SSMH-SD',
                '21398594'='SSMH-SE01','21393043'='SSMH-SE02','21398586'='SSMH-SF01',
                '21398580'='SSMH-SF02','21398581'='SSMH-SG','21206939'='TNHA-BASE',
                '21398593'='TNHA-SA','21398587'='TNHA-SB','21393047'='TNHA-SC',
                '21398601'='TNHA-SD','21398577'='TNHA-SE')

# name sensors by ID from metadata
moist_long$fullname <- recode(moist_long$sensor, !!!vwcsensors)

# output any sensor IDs that are missing a name
missing_sensors <- moist_long %>% select(sensor, fullname) %>% filter(is.na(fullname)) %>% unique()
missing_sensors

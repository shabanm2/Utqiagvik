#Daily Averages for All Seasons


#All data is stored and goes to the files listed in lines 6,7,8

filepath = "/Users/shabangin/Desktop/UVA/RESEARCH/Barrow/RECENT_CSV_DOWNLOADS/"
export_to = "/Users/shabangin/Desktop/UVA/RESEARCH/Barrow/ALL_COMBINED/" #Directory to export clean data sets
large_file_export = "/Users/shabangin/Desktop/UVA/RESEARCH/Barrow/ALL_COMBINED/Large_Files/"

years <- c(2022, 2023, 2024, 2025) # the years covered by the data #edited to add 2025 on 9/6/25
seasons <- c("Summer","Fall","Winter","Spring") # don't need to change this

# Date ranges (seasons) of data
# For File Naming - names the total combined file
sznstart <- "Summer2022"
sznend <- "Summer2025" #edited from Fall2024 on 9/6/25

# Find Files

all_files = list.files(filepath, pattern="*.csv", full.names=F)

vwcgroundsensors <- c('21393048'='BEO-B06','21398591'='BEO-B05','21393045'='BEO-B07','21398585'='BUECI-BASE','21398590'='BUECI-SA','21398583'='BUECI-SB','21393042'='BUECI-SC','21398584'='BUECI-SD','21398579'='BUECI-SE','21398578'='BUECI-SF01','21398598'='BUECI-SF02','21398588'='SSMH-BASE','21398599'='SSMH-SA','21393044'='SSMH-SB','21393049'='SSMH-SD','21398594'='SSMH-SE','21393043'='SSMH-SF','21398586'='SSMH-SG','21398580'='SSMH-SH','21398581'='SSMH-SI','21206939'='TNHA-BASE','21398593'='TNHA-SA','21398587'='TNHA-SB','21393047'='TNHA-SC','21398601'='TNHA-SD','21398577'='TNHA-SE', '21398597' ='TNHA-SE','21398576'='BEO-B08','21393046'='TNHA-SC','21166008'='SSMH-SC','21212510'='REMOVE')
airtempsensors <- c('21398585'='BUECI-BASE','21398659'='BUECI-__','21398661'='BUECI-__','21390851'='BUECI-BASE','21397542'='BUECI-__','21398671'='BUECI-SA','21362254'='BUECI-SB','21398668'='BUECI-SC','21362256'='BUECI-SD','21187245'='BUECI-SE','21390849'='SSMH-BASE','21398670'='SSMH-SA','21398665'='SSMH-SB','21398667'='SSMH-SI','21212510'='TNHA-SF','21218018'='TNHA-BASE','21390850'='TNHA-BASE','21380919'='TNHA-SA','21398676'='TNHA-SB','21398664'='TNHA-SB','21398674'='TNHA-SC','21398666'='TNHA-SD','21981318'='BEO-BASE','21187247'='REMOVE','21397541'='REMOVE')
solarsensors <- c('21390415'='BEO-BASE','21362314'='BUECI-?','21398617'='BUECI-??','21398621'='BUECI-???','21398623'='BUECI-????','21390411'='BUECI-BASE','21390414'='BUECI-?????','21398618'='BUECI-SA','21398624'='BUECI-SB','21362313'='BUECI-SC','21362316'='BUECI-SD','21362320'='BUECI-SE','21390413'='SSMH-BASE','21398622'='SSMH-SA','21362319'='SSMH-SB','21398619'='SSMH-SI','21362318'='TNHA-SB','21176526'='TNHA-BASE','21398620'='TNHA-SA','21398616'='TNHA-SB','21362317'='TNHA-SC','21362315'='TNHA-SD','21390412'='REMOVE')

windsensors <- c('21350915'='BEO-BASE','21350894'='BUECI-BASE','21398590'='BUECI-SA','21398729'='BUECI-SB','21398724'='BUECI-SC','21398660'='BUECI-SD','21398719'='BUECI-SE','21350901'='SSMH-BASE','21350915'='SSMH-BASE','21206911'='SSMH-SA','21206912'='SSMH-SB','21398711'='SSMH-SI','21176861'='TNHA-BASE','21398709'='TNHA-SA','21181033'='TNHA-SB','21398715'='TNHA-SB','21206909'='TNHA-SC','21398712'='TNHA-SD','21390849'='REMOVE','21398665'='REMOVE','21398667'='REMOVE','21398670'='REMOVE','21981320'='REMOVE','21350819'='REMOVE','21350855'='BEO-BASE','21350820'='SSMH-BASE', '21350910' = 'SSMH-BASE','21167037'='TNHA-BASE','21398716'='TNHA-SD')

winddir_ids <- c('21350855','21350894','21398590','21398729','21398724','21398660','21398719','21350820','21206911','21206912','21398711','21181033','21167037','21398709','21398715','21206909','21398712','21398716')

windspeed_ids <- c('21350915','21350894','21398590','21398729','21398724','21398660','21398719','21350901','21350910','21206911','21206912','21398711','21176861','21398709','21181033','21398715','21206909','21398712','21398716')


# select ground temperature files
gtf = grepl("grnd", all_files, ignore.case=T)
gotf = grepl("ground", all_files, ignore.case=T)
gtfiles = all_files[gtf | gotf]

# ADD THIS LINE HERE - Exclude previously merged/combined files
gtfiles = gtfiles[!grepl("MERGED|^ALL_|COMBINED", gtfiles, ignore.case=T)]

# select air temperature files
airf = grepl("air", all_files, ignore.case=T)
airfiles = all_files[airf]

# Exclude previously merged/combined files
airfiles = airfiles[!grepl("MERGED|^ALL_|COMBINED", airfiles, ignore.case=T)]

# select solar radiation files
solf = grepl("solar", all_files, ignore.case=T)
solarfiles = all_files[solf]

solarfiles = solarfiles[!grepl("MERGED|^ALL_|COMBINED", solarfiles, ignore.case=T)]

# select vwc files
vwcf = grepl("vwc", all_files, ignore.case=T)
vwcfiles = all_files[vwcf]

vwcfiles = vwcfiles[!grepl("MERGED|^ALL_|COMBINED", vwcfiles, ignore.case=T)]

# select wind files (windspeed or winddirection)
windf = grepl("wind", all_files, ignore.case=T)
windfiles = all_files[windf]

windfiles = windfiles[!grepl("MERGED|^ALL_|COMBINED", windfiles, ignore.case=T)]


#============================================#
# Packages
#============================================#


#library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(gridExtra)
library(forcats)
library(splitstackshape)
library(naniar)
library(stringr)



#============================================#
# Ground Temperature
#============================================#

file = "Summer_22_grndtmp_2023_11_14_20_50_46_UTC_1.csv"

groundtemp <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(groundtemp) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in gtfiles){
  temp <- read.csv(paste0(filepath, file))
  
  # Save original dates BEFORE any transformations
  original_dates <- temp$Date
  
  # Clean column names - old patterns
  names(temp) <- sub("....C..RX3000_BRW1", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW4", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW5", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW6", "", names(temp))
  names(temp) <- sub("RX3000_BRW6", "", names(temp))
  names(temp) <- sub("RX3000_BRW4", "", names(temp))
  names(temp) <- sub("RX3000_BRW1", "", names(temp))
  names(temp) <- sub("RX3000_BRW2", "", names(temp))
  names(temp) <- sub("RX3000_BRW3", "", names(temp))
  names(temp) <- sub("RX3000_BRW5", "", names(temp))
  names(temp) <- sub(".RXW.GP6.", "", names(temp))
  
  # Clean column names - NEW patterns for 2024+ files
  names(temp) <- sub("\\.\\.+C\\.RX3000_BRW[0-9].*", "", names(temp))
  names(temp) <- sub("_SSMH$", "", names(temp))
  names(temp) <- sub("_BEO$", "", names(temp))
  names(temp) <- sub("_TNHA$", "", names(temp))
  names(temp) <- sub("_BUECI$", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  
  # REMOVE DUPLICATES AFTER ALL NAME TRANSFORMATIONS
  temp <- temp[, !duplicated(names(temp))]
  
  # ========== DATE PARSING ==========
  has_tz_offset <- grepl("-[0-9]{4}$", original_dates[1])
  
  if(has_tz_offset){
    clean_dates <- gsub(" -[0-9]{4}$", "", original_dates)
    temp$Date <- parse_date_time(clean_dates, 
                                 orders = c("mdy HMS", "mdy HM"),
                                 tz = "UTC")
  } else {
    temp$Date <- parse_date_time(original_dates, 
                                 orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS"),
                                 tz = "UTC")
  }
  
  if(all(is.na(temp$Date))){
    warning(paste("Could not parse dates in file:", file))
    next
  }
  
  # Some dupes for name change
  dates <- substr(temp$Date, 1, 10)
  if(max(dates, na.rm = TRUE) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  temp <- select(temp, !contains(".RXW"))
  temp <- select(temp, !contains(".S"))
  
  temp <- temp[, 2:ncol(temp)]
  
  # Remove zeroes
  colstart <- 2
  while(colstart < ncol(temp)){
    colend <- colstart + 10
    if(colend > ncol(temp)){
      colend <- ncol(temp)
    }
    valsums <- data.frame(rowSums(temp[, colstart:colend], na.rm = F))
    colnames(valsums) <- "sum"
    valsums <- cbind(x = rownames(valsums), valsums)
    zeroes <- valsums %>% filter(sum == 0)
    temp[zeroes$x, c(colstart:colend)] <- NA
    colstart <- colend + 1
  }
  
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp <- cSplit(temp, 'variable', sep = ".", direction = "wide")
  
  # ========== HANDLE DIFFERENT COLUMN COUNTS ==========
  if(ncol(temp) == 6){
    colnames(temp) <- c("Date", "value", "variable", "site", "fullname", "depth")
    
  } else if(ncol(temp) == 9){
    # NEW FORMAT: 9 columns from ...C. remnants
    temp <- temp %>%
      select(Date, response, variable_1, variable_2, variable_3, variable_4) %>%
      rename(value = response, 
             variable = variable_1, 
             site = variable_2, 
             fullname = variable_3, 
             depth = variable_4)
    
  } else if(ncol(temp) == 10){
    temp <- temp %>%
      select(Date, response, variable_1, variable_2, variable_4, variable_8) %>%
      rename(value = response, 
             variable = variable_1, 
             site = variable_2, 
             fullname = variable_4, 
             depth = variable_8)
    
  } else {
    warning(paste("Unexpected column count:", ncol(temp), "in file:", file))
    next
  }
  
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI",
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!vwcgroundsensors)
  
  groundtemp <- rbind(groundtemp, temp)
}

# Check results
cat("\n========== RESULTS ==========\n")
cat("Date range:\n")
cat("Min:", as.character(min(groundtemp$Date, na.rm = TRUE)), "\n")
cat("Max:", as.character(max(groundtemp$Date, na.rm = TRUE)), "\n")
cat("Total rows:", nrow(groundtemp), "\n")

#============================================#
# Air Temperature
#============================================#

airtemp <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(airtemp) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in airfiles){
  temp <- read.csv(paste0(filepath, file))
  
  # Save original dates BEFORE any transformations
  original_dates <- temp$Date
  
  # Change sensor serial numbers to names - NEW regex patterns FIRST
  names(temp) <- sub("\\.+RXW\\.GP6\\.", ".", names(temp))
  names(temp) <- sub("\\.+RXW\\.THC\\.", ".", names(temp))
  names(temp) <- sub("\\.+RXW\\.TMB\\.", ".", names(temp))
  names(temp) <- sub("\\.+S\\.THC\\.", ".", names(temp))
  names(temp) <- sub("\\.\\.+C\\.", "", names(temp))
  names(temp) <- sub("\\.\\.+C$", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW1", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW4", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW5", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW6", "", names(temp))
  names(temp) <- sub("RX3000_BRW6", "", names(temp))
  names(temp) <- sub("RX3000_BRW4", "", names(temp))
  names(temp) <- sub("RX3000_BRW1", "", names(temp))
  names(temp) <- sub("RX3000_BRW2", "", names(temp))
  names(temp) <- sub("RX3000_BRW3", "", names(temp))
  names(temp) <- sub("RX3000_BRW5", "", names(temp))
  names(temp) <- sub("\\.\\.+C\\.RX3000_BRW[0-9].*", "", names(temp))
  names(temp) <- sub("_SSMH$", "", names(temp))
  names(temp) <- sub("_BEO$", "", names(temp))
  names(temp) <- sub("_TNHA$", "", names(temp))
  names(temp) <- sub("_BUECI$", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  
  # REMOVE DUPLICATES AFTER ALL NAME TRANSFORMATIONS
  temp <- temp[, !duplicated(names(temp))]
  
  # ========== DATE PARSING ==========
  has_tz_offset <- grepl("-[0-9]{4}$", original_dates[1])
  
  if(has_tz_offset){
    clean_dates <- gsub(" -[0-9]{4}$", "", original_dates)
    temp$Date <- parse_date_time(clean_dates, 
                                 orders = c("mdy HMS", "mdy HM"),
                                 tz = "UTC")
  } else {
    temp$Date <- parse_date_time(original_dates, 
                                 orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS"),
                                 tz = "UTC")
  }
  
  if(all(is.na(temp$Date))){
    warning(paste("Could not parse dates in file:", file))
    next
  }
  
  # Some dupes for name change
  dates <- substr(temp$Date, 1, 10)
  if(max(dates, na.rm = TRUE) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  # Has "line" as first column
  temp <- temp[, 2:ncol(temp)]
  
  # Turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  # Skip if no data remains after filtering NAs
  if(nrow(temp) == 0){
    cat("Skipping file (no valid data):", file, "\n")
    next
  }
  
  temp <- cSplit(temp, 'variable', sep = ".", direction = "wide")
  
  # ========== HANDLE DIFFERENT COLUMN COUNTS ==========
  if(ncol(temp) == 6){
    colnames(temp) <- c("Date", "value", "variable", "site", "fullname", "depth")
    
  } else if(ncol(temp) == 9){
    temp <- temp %>%
      select(Date, response, variable_1, variable_2, variable_3, variable_4) %>%
      rename(value = response, 
             variable = variable_1, 
             site = variable_2, 
             fullname = variable_3, 
             depth = variable_4)
    
  } else if(ncol(temp) == 10){
    temp <- temp %>%
      select(Date, response, variable_1, variable_2, variable_4, variable_8) %>%
      rename(value = response, 
             variable = variable_1, 
             site = variable_2, 
             fullname = variable_4, 
             depth = variable_8)
    
  } else {
    warning(paste("Unexpected column count:", ncol(temp), "in file:", file))
    next
  }
  
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI", 
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  # Recode values from sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!airtempsensors)
  
  # Ignore sensor 21380922 (only a few random days in December 2023)
  temp <- temp %>% filter(fullname != "21380922")
  
  airtemp <- rbind(airtemp, temp)
}

# Check results
cat("\n========== AIR TEMP RESULTS ==========\n")
cat("Date range:\n")
cat("Min:", as.character(min(airtemp$Date, na.rm = TRUE)), "\n")
cat("Max:", as.character(max(airtemp$Date, na.rm = TRUE)), "\n")
cat("Total rows:", nrow(airtemp), "\n")

#============================================#
# VWC (Volumetric Water Content)
#============================================#

vwc <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(vwc) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in vwcfiles){
  temp <- read.csv(paste0(filepath, file))
  
  # Save original dates BEFORE any transformations
  original_dates <- temp$Date
  
  # Change sensor serial numbers to names
  names(temp) <- sub("\\.+RXW\\.SMC\\.", ".", names(temp))
  names(temp) <- sub("\\.+m\\.+m\\.+RX3000_BRW[0-9].*", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW1", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW4", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW5", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW6", "", names(temp))
  names(temp) <- sub("..RXW.GP6.", "", names(temp))
  names(temp) <- sub("Water.Content", "VWC", names(temp))
  names(temp) <- sub("_TNHA$", "", names(temp))
  names(temp) <- sub("_SSMH$", "", names(temp))
  names(temp) <- sub("_BUECI$", "", names(temp))
  names(temp) <- sub("_BEO$", "", names(temp))
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  # FIX: Remove consecutive dots (.. or ... become single .)
  names(temp) <- gsub("\\.+", ".", names(temp))
  
  # FIX: Ensure dot after VWC if followed by number
  names(temp) <- sub("VWC([0-9])", "VWC.\\1", names(temp))
  
  # REMOVE DUPLICATES AFTER ALL NAME TRANSFORMATIONS
  temp <- temp[, !duplicated(names(temp))]
  
  # ========== DATE PARSING ==========
  has_tz_offset <- grepl("-[0-9]{4}$", original_dates[1])
  
  if(has_tz_offset){
    clean_dates <- gsub(" -[0-9]{4}$", "", original_dates)
    temp$Date <- parse_date_time(clean_dates, 
                                 orders = c("mdy HMS", "mdy HM"),
                                 tz = "UTC")
  } else {
    temp$Date <- parse_date_time(original_dates, 
                                 orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS"),
                                 tz = "UTC")
  }
  
  if(all(is.na(temp$Date))){
    warning(paste("Could not parse dates in file:", file))
    next
  }
  
  # Some dupes for name change
  dates <- substr(temp$Date, 1, 10)
  if(max(dates, na.rm = TRUE) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  # There may be wind and air variables in the df
  temp <- select(temp, !contains(".RXW"))
  temp <- select(temp, !contains(".S."))
  
  # Has "line" as first column
  temp <- temp[, 2:ncol(temp)]
  
  # Remove zeroes if zero at all depths (1-6)
  colstart <- 2
  while(colstart < ncol(temp)){
    colend <- colstart + 5
    if(colend > ncol(temp)){
      colend <- ncol(temp)
    }
    
    valsums <- data.frame(rowSums(temp[, colstart:colend], na.rm = F))
    colnames(valsums) <- "sum"
    valsums <- cbind(x = rownames(valsums), valsums)
    
    zeroes <- valsums %>% filter(sum == 0)
    temp[zeroes$x, c(colstart:colend)] <- NA
    
    colstart <- colend + 1
  }
  
  # Turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  # Skip if no data remains after filtering NAs
  if(nrow(temp) == 0){
    cat("Skipping file (no valid data):", file, "\n")
    next
  }
  
  temp <- cSplit(temp, 'variable', sep = ".", direction = "wide")
  
  # ========== HANDLE DIFFERENT COLUMN COUNTS ==========
  if(ncol(temp) == 6){
    colnames(temp) <- c("Date", "value", "variable", "site", "fullname", "depth")
    
  } else {
    warning(paste("Unexpected column count:", ncol(temp), "in file:", file))
    cat("Column names:", paste(names(temp), collapse = ", "), "\n")
    print(head(temp, 2))
    next
  }
  
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI", 
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  # Recode values from sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!vwcgroundsensors)
  
  temp <- temp %>% filter(fullname != "REMOVE")
  
  vwc <- rbind(vwc, temp)
}

# Check results
cat("\n========== VWC RESULTS ==========\n")
cat("Date range:\n")
cat("Min:", as.character(min(vwc$Date, na.rm = TRUE)), "\n")
cat("Max:", as.character(max(vwc$Date, na.rm = TRUE)), "\n")
cat("Total rows:", nrow(vwc), "\n")

# Check for NA depths
cat("NA depths:", sum(is.na(vwc$depth)), "\n")

#============================================#
# Solar Radiation
#============================================#

solar <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(solar) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in solarfiles){
  temp <- read.csv(paste0(filepath, file))
  
  # Save original dates BEFORE any transformations
  original_dates <- temp$Date
  
  # Change sensor serial numbers to names
  names(temp) <- sub("\\.+RXW\\.LIB\\.", ".", names(temp))
  names(temp) <- sub("\\.+S\\.LIB\\.", ".", names(temp))
  names(temp) <- sub("..RXW.LIB", "", names(temp))
  names(temp) <- sub("..S.LIB", "", names(temp))
  names(temp) <- sub("Solar.Radiation.", "Solar.", names(temp))
  names(temp) <- sub("Solar.Radiation", "Solar", names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW1", "", names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW4", "", names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW5", "", names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW6", "", names(temp))
  names(temp) <- sub("\\.+W\\.m\\.2\\.+RX3000_BRW[0-9].*", "", names(temp))
  names(temp) <- sub("\\.W\\.m\\.RX3000_BRW[0-9].*", "", names(temp))
  names(temp) <- sub("\\.W\\.m\\.2\\.RX3000_BRW[0-9].*", "", names(temp))
  names(temp) <- sub("_TNHA$", "", names(temp))
  names(temp) <- sub("_SSMH$", "", names(temp))
  names(temp) <- sub("_BUECI$", "", names(temp))
  names(temp) <- sub("_BEO$", "", names(temp))
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  # FIX: Remove consecutive dots
  names(temp) <- gsub("\\.+", ".", names(temp))
  
  # FIX: Ensure dot after Solar if followed by number
  names(temp) <- sub("Solar([0-9])", "Solar.\\1", names(temp))
  
  # Keep only Solar columns (exclude Temperature, etc.)
  solar_cols <- c(1, 2, grep("^Solar\\.", names(temp)))
  if(length(solar_cols) <= 2){
    cat("Skipping file (no Solar columns found):", file, "\n")
    next
  }
  temp <- temp[, solar_cols]
  
  # REMOVE DUPLICATES AFTER ALL NAME TRANSFORMATIONS
  temp <- temp[, !duplicated(names(temp))]
  
  # ========== DATE PARSING ==========
  has_tz_offset <- grepl("-[0-9]{4}$", original_dates[1])
  
  if(has_tz_offset){
    clean_dates <- gsub(" -[0-9]{4}$", "", original_dates)
    temp$Date <- parse_date_time(clean_dates, 
                                 orders = c("mdy HMS", "mdy HM"),
                                 tz = "UTC")
  } else {
    temp$Date <- parse_date_time(original_dates, 
                                 orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS"),
                                 tz = "UTC")
  }
  
  if(all(is.na(temp$Date))){
    warning(paste("Could not parse dates in file:", file))
    next
  }
  
  # Some dupes for name change
  dates <- substr(temp$Date, 1, 10)
  if(max(dates, na.rm = TRUE) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  # Has "line" as first column
  temp <- temp[, 2:ncol(temp)]
  
  # Turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  # Skip if no data remains after filtering NAs
  if(nrow(temp) == 0){
    cat("Skipping file (no valid data):", file, "\n")
    next
  }
  
  temp <- cSplit(temp, 'variable', sep = ".", direction = "wide")
  
  # ========== HANDLE DIFFERENT COLUMN COUNTS ==========
  if(ncol(temp) == 6){
    colnames(temp) <- c("Date", "value", "variable", "site", "fullname", "depth")
    
  } else if(ncol(temp) == 7){
    # Format with extra NA column from double dots
    # variable_1=Solar, variable_2=site, variable_3=sensor, variable_4=depth, variable_5=NA
    temp <- temp %>%
      select(Date, response, variable_1, variable_2, variable_3, variable_4) %>%
      rename(value = response, 
             variable = variable_1, 
             site = variable_2, 
             fullname = variable_3, 
             depth = variable_4)
    
  } else if(ncol(temp) == 9){
    # Format with .W.m.RX3000_BRW remnants
    # variable_1=Solar, variable_2=site, variable_3=sensor, variable_4=depth, variable_5-7=junk
    temp <- temp %>%
      select(Date, response, variable_1, variable_2, variable_3, variable_4) %>%
      rename(value = response, 
             variable = variable_1, 
             site = variable_2, 
             fullname = variable_3, 
             depth = variable_4)
    
  } else {
    warning(paste("Unexpected column count:", ncol(temp), "in file:", file))
    cat("Column names:", paste(names(temp), collapse = ", "), "\n")
    print(head(temp, 2))
    next
  }
  
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI",
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  # Recode values from sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!solarsensors)
  temp <- temp %>% filter(site != "BUECI")
  
  solar <- rbind(solar, temp)
}

# Check results
cat("\n========== SOLAR RESULTS ==========\n")
cat("Date range:\n")
cat("Min:", as.character(min(solar$Date, na.rm = TRUE)), "\n")
cat("Max:", as.character(max(solar$Date, na.rm = TRUE)), "\n")
cat("Total rows:", nrow(solar), "\n")


#============================================#
# Wind Speed and Wind Direction
#============================================#

windspeed <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(windspeed) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")
winddir <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(winddir) <- c("Date", "value", "variable", "site", "fullname", "sensor", "depth")

for(file in windfiles){
  temp <- read.csv(paste0(filepath, file))
  
  # Save original dates BEFORE any transformations
  original_dates <- temp$Date
  
  # Change sensor serial numbers to names
  names(temp) <- sub("Wind.Speed.", "WindSpeed.", names(temp))
  names(temp) <- sub("Wind.Direction.", "WindDirection.", names(temp))
  names(temp) <- sub("\\.+RXW\\.WCF\\.", ".", names(temp))
  names(temp) <- sub("\\.+RXW\\.WDA\\.", ".", names(temp))
  names(temp) <- sub("\\.+S\\.WSB\\.", ".", names(temp))
  names(temp) <- sub("\\.+S\\.WDA\\.", ".", names(temp))
  names(temp) <- sub(".S.WSB.", "", names(temp))
  names(temp) <- sub(".RXW.WCF.", "", names(temp))
  names(temp) <- sub(".S.WDA.", "", names(temp))
  names(temp) <- sub("...m.s..RX3000", "", names(temp))
  names(temp) <- sub("......RX3000", "", names(temp))
  names(temp) <- sub("\\.+m\\.s\\.+RX3000.*", "", names(temp))
  names(temp) <- sub("\\.+RX3000.*", "", names(temp))
  names(temp) <- sub("_BRW1", "", names(temp))
  names(temp) <- sub("_BRW4", "", names(temp))
  names(temp) <- sub("_BRW5", "", names(temp))
  names(temp) <- sub("_BRW6", "", names(temp))
  names(temp) <- sub("_TNHA$", "", names(temp))
  names(temp) <- sub("_SSMH$", "", names(temp))
  names(temp) <- sub("_BUECI$", "", names(temp))
  names(temp) <- sub("_BEO$", "", names(temp))
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  names(temp) <- sub("X", "", names(temp))
  
  # FIX: Remove consecutive dots
  names(temp) <- gsub("\\.+", ".", names(temp))
  
  # FIX: Ensure dot after WindSpeed/WindDirection if followed by number
  names(temp) <- sub("WindSpeed([0-9])", "WindSpeed.\\1", names(temp))
  names(temp) <- sub("WindDirection([0-9])", "WindDirection.\\1", names(temp))
  
  # Get rid of Gust columns
  gust_cols = grepl("Gust", colnames(temp), ignore.case=T)
  if (sum(gust_cols) > 0) {
    temp = temp[, !gust_cols]
  }
  
  # REMOVE DUPLICATES AFTER ALL NAME TRANSFORMATIONS
  temp <- temp[, !duplicated(names(temp))]
  
  # ========== DATE PARSING ==========
  # Check for timezone offset (either + or -)
  # ========== DATE PARSING ==========
  # Check for timezone offset (either + or -)
  has_tz_offset <- grepl("[+-][0-9]{4}$", original_dates[1])
  
  if(has_tz_offset){
    clean_dates <- gsub(" [+-][0-9]{4}$", "", original_dates)
    temp$Date <- parse_date_time(clean_dates, 
                                 orders = c("mdy HMS", "mdy HM", "ymd HMS"),
                                 tz = "UTC")
  } else {
    # Check if date looks like ISO format (starts with 4-digit year)
    if(grepl("^[0-9]{4}-", original_dates[1])){
      temp$Date <- parse_date_time(original_dates, 
                                   orders = c("ymd HMS", "ymd HM"),
                                   tz = "UTC")
    } else {
      temp$Date <- parse_date_time(original_dates, 
                                   orders = c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS"),
                                   tz = "UTC")
    }
  }
  
  if(all(is.na(temp$Date))){
    warning(paste("Could not parse dates in file:", file))
    cat("Sample date:", original_dates[1], "\n")
    next
  }
  
  # Handle pre/post March 2023 sensor name changes
  dates <- substr(temp$Date, 1, 10)
  if(max(dates, na.rm = TRUE) < "2023-03-05"){
    temp <- select(temp, !contains("_"))
  }
  
  # Remove "Line." column if it exists (NOT all files have it)
  if(names(temp)[1] == "Line." || names(temp)[1] == "Line"){
    temp <- temp[, 2:ncol(temp)]
  }
  
  # Check if columns need WindSpeed/WindDirection prefix added
  # (some files from newer export format don't have the prefix)
  data_cols <- names(temp)[names(temp) != "Date"]
  has_prefix <- any(grepl("^Wind", data_cols))
  
  if(!has_prefix && length(data_cols) > 0){
    # Determine prefix from filename
    if(grepl("wind_sp|Windspeed|WindSpeed", file, ignore.case = TRUE)){
      prefix <- "WindSpeed"
    } else if(grepl("wind_dir|Winddir|WindDirection", file, ignore.case = TRUE)){
      prefix <- "WindDirection"
    } else {
      cat("Skipping file (can't determine wind type):", file, "\n")
      next
    }
    
    # Extract site serial number from filename
    site_from_filename <- NA
    if(grepl("^SSMH", file)) site_from_filename <- "21401801"
    if(grepl("^TNHA", file)) site_from_filename <- "21198259"
    if(grepl("^BEO", file)) site_from_filename <- "21401803"
    if(grepl("^BUECI", file)) site_from_filename <- "21401800"
    
    if(is.na(site_from_filename)){
      cat("Skipping file (can't determine site from filename):", file, "\n")
      next
    }
    
    # Add prefix AND site to data columns
    # Format: WindSpeed.SITE.SENSOR.DEPTH
    for(i in seq_along(names(temp))){
      if(names(temp)[i] != "Date"){
        names(temp)[i] <- paste0(prefix, ".", site_from_filename, ".", names(temp)[i])
      }
    }
    cat("Added prefix:", prefix, "with site:", site_from_filename, "to file:", file, "\n")
  }
  
  # Turn into long df (gets rid of NAs)
  temp <- gather(temp, variable, response, 2:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  # Skip if no data remains after filtering NAs
  if(nrow(temp) == 0){
    cat("Skipping file (no valid data):", file, "\n")
    next
  }
  
  temp <- cSplit(temp, 'variable', sep = ".", direction = "wide")
  
  # ========== HANDLE DIFFERENT COLUMN COUNTS ==========
  if(ncol(temp) == 6){
    # Standard format: Date, response, variable, site, sensor, depth
    colnames(temp) <- c("Date", "value", "variable", "site", "fullname", "depth")
    
  } else if(ncol(temp) == 5){
    # WindDirection without depth: Date, response, variable, site, sensor
    colnames(temp) <- c("Date", "value", "variable", "site", "fullname")
    temp$depth <- NA
    
  } else if(ncol(temp) == 7){
    # Extra NA column from double dots
    temp <- temp %>%
      select(Date, response, variable_1, variable_2, variable_3, variable_4) %>%
      rename(value = response, 
             variable = variable_1, 
             site = variable_2, 
             fullname = variable_3, 
             depth = variable_4)
    
  } else if(ncol(temp) == 9){
    temp <- temp %>%
      select(Date, response, variable_1, variable_2, variable_3, variable_4) %>%
      rename(value = response, 
             variable = variable_1, 
             site = variable_2, 
             fullname = variable_3, 
             depth = variable_4)
    
  } else {
    warning(paste("Unexpected column count:", ncol(temp), "in file:", file))
    cat("Column names:", paste(names(temp), collapse = ", "), "\n")
    print(head(temp, 2))
    next
  }
  
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$fullname <- as.factor(as.character(temp$fullname))
  temp$site <- as.factor(temp$site)
  
  temp$site <- recode(temp$site, "21198259" = "TNHA", "21401800" = "BUECI",
                      "21401801" = "SSMH", "21401803" = "BEO")
  
  # Recode values from sensor names
  temp$sensor <- temp$fullname
  temp$fullname <- recode(temp$fullname, !!!windsensors)
  
  # Filter out invalid values
  temp <- temp %>% filter(value != -888.88)
  
  # Separate windspeed and winddirection
  ws <- temp %>% filter(variable == "WindSpeed")
  wd <- temp %>% filter(variable == "WindDirection")
  
  if(nrow(ws) > 0) windspeed <- rbind(windspeed, ws)
  if(nrow(wd) > 0) winddir <- rbind(winddir, wd)
}

# Check results
cat("\n========== WIND SPEED RESULTS ==========\n")
cat("Date range:\n")
cat("Min:", as.character(min(windspeed$Date, na.rm = TRUE)), "\n")
cat("Max:", as.character(max(windspeed$Date, na.rm = TRUE)), "\n")
cat("Total rows:", nrow(windspeed), "\n")

cat("\n========== WIND DIRECTION RESULTS ==========\n")
cat("Date range:\n")
cat("Min:", as.character(min(winddir$Date, na.rm = TRUE)), "\n")
cat("Max:", as.character(max(winddir$Date, na.rm = TRUE)), "\n")
cat("Total rows:", nrow(winddir), "\n")
####

# recode depth values
groundtemp$depth <- recode(groundtemp$depth, "7" = "3.5cm", "8" = "10cm", "9" = "20cm", 
                           "10" = "30cm", "11" = "40cm", "12" = "50cm", "13" = "55cm", 
                           "14" = "65cm", "15" = "75cm", "16" = "85cm", "17" = "90cm")

groundtemp$depth <- factor(groundtemp$depth, c("3.5cm","10cm", "20cm", "30cm", "40cm", 
                                               "50cm", "55cm", "65cm", "75cm", "85cm", "90cm"))

vwc$depth <- recode(vwc$depth, "1" = "0-15cm", "2" = "15-30cm", "3" = "30-45cm",
                    "4" = "45-60cm", "5" = "60-75cm", "6" = "75-90cm")

vwc$depth <- factor(vwc$depth, c("0-15cm", "15-30cm", "30-45cm","45-60cm", 
                                 "60-75cm","75-90cm"))

# remove bad data
groundtemp <- groundtemp %>% filter(!value == -888.88)
airtemp <- airtemp %>% filter(!value == -888.88)
airtemp <- airtemp %>% filter(!value == 128.83)

# COLUMN NAMING CONVENTION
# [1] Date [2] fullname [3] site [4] station [5] depth [6] value [7] metadata

groundtemp <- groundtemp %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor, depth)

airtemp <- airtemp %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor)

solar <- solar %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor)

vwc <- vwc %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor, depth)

windspeed <- windspeed %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor)

winddir <- winddir %>% mutate(station = str_extract(fullname, '\\b\\w+$')) %>%
  select(Date, value, fullname, site, station, sensor)


#============================================#
# CREATE METADATA COLUMN - FIXED FOR BEO GROUND SENSORS
#============================================#

# For GROUND TEMP - preserve BEO-B05, B06, B07, B08 names
groundtemp <- groundtemp %>% mutate(metadata = case_when(
  fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
  fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
  site=="BEO"~station, 
  T~NA))
groundtemp$fullname = as.character(groundtemp$fullname)
groundtemp$site = as.character(groundtemp$site)
# DO NOT overwrite fullname for BEO ground temp - keep B05, B06, B07, B08
# Only set station to BASE for consistency in grouping
groundtemp$station[groundtemp$site == "BEO"] = "BASE"
# NOTE: We keep fullname as BEO-B05, BEO-B06, etc. - DO NOT change to BEO-BASE


# For AIR TEMP - these ARE at the base station, so BEO-BASE is correct
airtemp <- airtemp %>% mutate(metadata = case_when(
  fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
  fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
  site=="BEO"~station, 
  T~NA))
airtemp$fullname = as.character(airtemp$fullname)
airtemp$site = as.character(airtemp$site)
airtemp$station[airtemp$site == "BEO"] = "BASE"
airtemp$fullname[airtemp$site == "BEO"] = "BEO-BASE"


# For SOLAR - these ARE at the base station, so BEO-BASE is correct
solar <- solar %>% mutate(metadata = case_when(
  fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
  fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
  site=="BEO"~station, 
  T~NA))
solar$fullname = as.character(solar$fullname)
solar$site = as.character(solar$site)
solar$station[solar$site == "BEO"] = "BASE"
solar$fullname[solar$site == "BEO"] = "BEO-BASE"


# For VWC - preserve BEO-B05, B06, B07, B08 names (same as ground temp)
vwc <- vwc %>% mutate(metadata = case_when(
  fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
  fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
  site=="BEO"~station, 
  T~NA))
vwc$fullname = as.character(vwc$fullname)
vwc$site = as.character(vwc$site)
# DO NOT overwrite fullname for BEO VWC - keep B05, B06, B07, B08
vwc$station[vwc$site == "BEO"] = "BASE"
# NOTE: We keep fullname as BEO-B05, BEO-B06, etc. - DO NOT change to BEO-BASE


# For WIND SPEED - these ARE at the base station, so BEO-BASE is correct
windspeed <- windspeed %>% mutate(metadata = case_when(
  fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
  fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
  site=="BEO"~station, 
  T~NA))
windspeed$fullname = as.character(windspeed$fullname)
windspeed$site = as.character(windspeed$site)
windspeed$station[windspeed$site == "BEO"] = "BASE"
windspeed$fullname[windspeed$site == "BEO"] = "BEO-BASE"


# For WIND DIRECTION - these ARE at the base station, so BEO-BASE is correct
winddir <- winddir %>% mutate(metadata = case_when(
  fullname=="TNHA-SA"|fullname=="SSMH-SA"~"South",
  fullname=="TNHA-SC"|fullname=="SSMH-SB"~"North",
  site=="BEO"~station, 
  T~NA))
winddir$fullname = as.character(winddir$fullname)
winddir$site = as.character(winddir$site)
winddir$station[winddir$site == "BEO"] = "BASE"
winddir$fullname[winddir$site == "BEO"] = "BEO-BASE"



write.csv(groundtemp, file=paste(large_file_export, "Raw/", "GroundTemperature_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(airtemp, file=paste(large_file_export, "Raw/", "AirTemperature_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(solar, file=paste(large_file_export, "Raw/", "Solar_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(vwc, file=paste(large_file_export, "Raw/", "VWC_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(windspeed, file=paste(large_file_export, "Raw/", "WindSpeed_", sznstart, "_", sznend,"_RAW", ".csv", sep=""))
write.csv(winddir, file=paste(large_file_export, "Raw/", "WindDirection_", sznstart, "_", sznend, "_RAW",".csv", sep=""))



#============================================#
# EXPORT RAW DATA BY SEASON
#============================================#

# Go By Each Season
# Export each variable

for(yr in years){
  for(szn in seasons){
    
    if(szn == "Summer"){
      datemin <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      
    } else if(szn=="Fall"){
      datemin <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      
    }else if(szn=="Winter"){
      datemin <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("03/01/",yr+1),format="%m/%d/%Y")
      
    } else{
      datemin <- as.Date(paste0("03/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
    }
    
    #Ground temp
    tmp <- groundtemp %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "GroundTemperature_", szn, yr, "_RAW", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- airtemp %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "AirTemperature_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "VWC_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "Solar_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- windspeed %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "WindSpeed_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
    tmp <- winddir %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(large_file_export, "Raw/", "WindDirection_", szn, yr,"_RAW", ".csv", sep=""))
    }
    
  }
}



#============================================#
# HOURLY
#============================================#

gt_hourly <- groundtemp %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
gt_hourly$hour[gt_hourly$hour==''] <- "00"
gt_hourly <- gt_hourly %>% group_by(site, fullname, station, sensor, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
gt_hourly <- gt_hourly %>% select(Date, day, hour, fullname, site, station, sensor, depth, min, max, avg, amp)


at_hourly <- airtemp %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
at_hourly$hour[at_hourly$hour==''] <- "00"
at_hourly <- at_hourly %>% group_by(site, fullname, station, sensor, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
at_hourly <- at_hourly %>% select(Date, day, hour, fullname, site, station, sensor, min, max, avg, amp)


solar_hourly <- solar %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
solar_hourly$hour[solar_hourly$hour==''] <- "00"
solar_hourly <- solar_hourly %>% group_by(site, fullname, station, sensor, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
solar_hourly <- solar_hourly %>% select(Date, day, hour, fullname, site, station, sensor, min, max, avg, amp)


vwc_hourly <- vwc %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
vwc_hourly$hour[vwc_hourly$hour==''] <- "00"
vwc_hourly <- vwc_hourly %>% group_by(site, fullname, station, sensor, depth, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
vwc_hourly <- vwc_hourly %>% select(Date, day, hour, fullname, site, station, sensor, depth, min, max, avg, amp)


ws_hourly <- windspeed %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
ws_hourly$hour[ws_hourly$hour==''] <- "00"
ws_hourly <- ws_hourly %>% group_by(site, fullname, station, sensor, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
ws_hourly <- ws_hourly %>% select(Date, day, hour, fullname, site, station, sensor, min, max, avg, amp)


wd_hourly <- winddir %>% mutate(hour = substr(Date, 12, 13), day = substr(Date, 1, 10)) 
wd_hourly$hour[wd_hourly$hour==''] <- "00"
wd_hourly <- wd_hourly %>% group_by(site, fullname, station, sensor, day, hour) %>%
  summarize(max=max(value), min=min(value)) %>% mutate(avg = (max+min)/2, amp = max-min, 
                                                       Date= as.POSIXct(paste0(day," ", hour, ":00:00"),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
wd_hourly <- wd_hourly %>% select(Date, day, hour, fullname, site, station, sensor, min, max, avg, amp)


#============================================#
# EXPORT HOURLY BY SEASON
#============================================#

# Go By Each Season
# Export each variable

for(yr in years){
  for(szn in seasons){
    
    if(szn == "Summer"){
      datemin <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      
    } else if(szn=="Fall"){
      datemin <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      
    }else if(szn=="Winter"){
      datemin <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("03/01/",yr+1),format="%m/%d/%Y")
      
    } else{
      datemin <- as.Date(paste0("03/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
    }
    
    #Ground temp
    tmp <- gt_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "GroundTemperature_", szn, yr, "_HOURLY", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- at_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "AirTemperature_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "VWC_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "Solar_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- ws_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "WindSpeed_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
    tmp <- wd_hourly %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Hourly/", "WindDirection_", szn, yr,"_HOURLY", ".csv", sep=""))
    }
    
  }
}


#============================================#
# DAILY
#============================================#

gt_daily <- gt_hourly %>% group_by(as.Date(day), fullname, site, station, sensor, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(gt_daily)[1] <- "Date"

at_daily <- at_hourly %>% group_by(as.Date(day), fullname, site, station, sensor) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(at_daily)[1] <- "Date"

solar_daily <- solar_hourly %>% group_by(as.Date(day), fullname, site, station, sensor) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(solar_daily)[1] <- "Date"

vwc_daily <- vwc_hourly %>% group_by(as.Date(day), fullname, site, station, sensor, depth) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(vwc_daily)[1] <- "Date"

ws_daily <- ws_hourly %>% group_by(as.Date(day), fullname, site, station, sensor) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(ws_daily)[1] <- "Date"

wd_daily <- wd_hourly %>% group_by(as.Date(day), fullname, site, station, sensor) %>% 
  summarize(max = max(max), min = min(min)) %>% mutate(avg = (max+min)/2, amp = max-min)
colnames(wd_daily)[1] <- "Date"



#============================================#
# EXPORT DAILY BY SEASON
#============================================#

# Go By Each Season
# Export each variable for daily data

for(yr in years){
  for(szn in seasons){
    
    if(szn == "Summer"){
      datemin <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      
    } else if(szn=="Fall"){
      datemin <- as.Date(paste0("09/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      
    }else if(szn=="Winter"){
      datemin <- as.Date(paste0("12/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("03/01/",yr+1),format="%m/%d/%Y")
      
    } else{
      datemin <- as.Date(paste0("03/01/",yr),format="%m/%d/%Y")
      datemax <- as.Date(paste0("06/01/",yr),format="%m/%d/%Y")
    }
    
    #Ground temp
    tmp <- gt_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "GroundTemperature_", szn, yr, "_DAILY", ".csv", sep=""))
    }
    
    #Air Temp
    tmp <- at_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "AirTemperature_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #VWC
    tmp <- vwc_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "VWC_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #Solar Radiation
    tmp <- solar_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "Solar_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    #Wind Speed
    tmp <- ws_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "WindSpeed_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
    tmp <- wd_daily %>% filter(as.Date(Date, format="%Y-%m-%d") >= datemin) %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < datemax) %>% filter(station != "BUECI")
    if(nrow(tmp) > 0){
      write.csv(tmp, file=paste(export_to, "Daily/", "WindDirection_", szn, yr,"_DAILY", ".csv", sep=""))
    }
    
  }
}


#============================================#
# COMBINE HOURLY - WITH BEO EXPANSION
#============================================#

# Rename depth columns
colnames(gt_hourly)[colnames(gt_hourly) == "depth"] <- "grounddepth"
colnames(vwc_hourly)[colnames(vwc_hourly) == "depth"] <- "vwcdepth"

# Apply vwcdepth mapping to gt_hourly
gt_hourly <- gt_hourly %>% mutate(vwcdepth = case_when(
  grounddepth == "3.5cm" ~ "0-15cm",
  grounddepth == "10cm" ~ "0-15cm",
  grounddepth == "20cm" ~ "15-30cm",
  grounddepth == "30cm" ~ "30-45cm",
  grounddepth == "40cm" ~ "30-45cm",
  grounddepth == "50cm" ~ "45-60cm",
  grounddepth == "55cm" ~ "45-60cm",
  grounddepth == "65cm" ~ "60-75cm",
  grounddepth == "75cm" ~ "75-90cm",
  grounddepth == "85cm" ~ "75-90cm",
  grounddepth == "90cm" ~ "75-90cm"
))

# Rename avg columns
colnames(gt_hourly)[colnames(gt_hourly) == "avg"] <- "groundtemp"
colnames(vwc_hourly)[colnames(vwc_hourly) == "avg"] <- "vwc"
colnames(at_hourly)[colnames(at_hourly) == "avg"] <- "airtemp"
colnames(solar_hourly)[colnames(solar_hourly) == "avg"] <- "solar"
colnames(ws_hourly)[colnames(ws_hourly) == "avg"] <- "windspeed"
colnames(wd_hourly)[colnames(wd_hourly) == "avg"] <- "winddirection"

# Create sensor_name column for joining
gt_hourly$fullname <- gt_hourly$fullname
vwc_hourly$fullname <- vwc_hourly$fullname
at_hourly$fullname <- at_hourly$fullname
solar_hourly$fullname <- solar_hourly$fullname
ws_hourly$fullname <- ws_hourly$fullname
wd_hourly$fullname <- wd_hourly$fullname

#####
beo_ground_sensors <- c('BEO-B05', 'BEO-B06', 'BEO-B07', 'BEO-B08')

expand_beo_base <- function(df) {
  beo_base_rows <- df %>% filter(fullname == "BEO-BASE")
  non_beo_rows <- df %>% filter(fullname != "BEO-BASE")
  
  if(nrow(beo_base_rows) == 0){
    return(df)
  }
  
  beo_expanded <- beo_base_rows %>%
    crossing(new_sensor = beo_ground_sensors) %>%
    mutate(fullname = new_sensor) %>%
    select(-new_sensor)
  
  bind_rows(non_beo_rows, beo_expanded)
}
#####

# Expand BEO-BASE for air, solar, wind ONLY
at_hourly <- at_hourly %>% expand_beo_base()
solar_hourly <- solar_hourly %>% expand_beo_base()
ws_hourly <- ws_hourly %>% expand_beo_base()
wd_hourly <- wd_hourly %>% expand_beo_base()

# Join using fullname
join <- full_join(gt_hourly, vwc_hourly, by = c("Date", "fullname", "vwcdepth"), relationship = "many-to-many") %>%
  select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc) %>% 
  unique() %>%
  full_join(at_hourly, by = c("Date", "fullname"), relationship = "many-to-many") %>%
  select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp) %>% 
  unique() %>%
  full_join(solar_hourly, by = c("Date", "fullname"), relationship = "many-to-many") %>%
  select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar) %>% 
  unique() %>%
  full_join(ws_hourly, by = c("Date", "fullname"), relationship = "many-to-many") %>%
  select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed) %>% 
  unique() %>%
  full_join(wd_hourly, by = c("Date", "fullname"), relationship = "many-to-many") %>%
  select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed, winddirection) %>% 
  unique()


# Check results
cat("\n========== JOIN RESULTS ==========\n")
cat("Date range:\n")
cat("Min:", as.character(min(join$Date, na.rm = TRUE)), "\n")
cat("Max:", as.character(max(join$Date, na.rm = TRUE)), "\n")
cat("Total rows:", nrow(join), "\n")

write.csv(join, file=paste(large_file_export, "All_Variables_", sznstart, "_", sznend, "_HOURLY", ".csv", sep=""))


#============================================#
# COMBINE Daily - WITH BEO EXPANSION (FIXED)
#============================================#

# Step 1: Rename depth columns
colnames(gt_daily)[colnames(gt_daily) == "depth"] <- "grounddepth"
colnames(vwc_daily)[colnames(vwc_daily) == "depth"] <- "vwcdepth"

# Step 2: Recode vwcdepth in vwc_daily (if not already done)
vwc_daily$vwcdepth <- recode(as.character(vwc_daily$vwcdepth), 
                             "1" = "0-15cm", 
                             "2" = "15-30cm", 
                             "3" = "30-45cm",
                             "4" = "45-60cm", 
                             "5" = "60-75cm", 
                             "6" = "75-90cm",
                             .default = as.character(vwc_daily$vwcdepth))

# Step 3: Apply vwcdepth mapping to gt_daily
gt_daily <- gt_daily %>% 
  mutate(vwcdepth = case_when(
    grounddepth == "3.5cm" ~ "0-15cm",
    grounddepth == "10cm" ~ "0-15cm",
    grounddepth == "20cm" ~ "15-30cm",
    grounddepth == "30cm" ~ "30-45cm",
    grounddepth == "40cm" ~ "30-45cm",
    grounddepth == "50cm" ~ "45-60cm",
    grounddepth == "55cm" ~ "45-60cm",
    grounddepth == "65cm" ~ "60-75cm",
    grounddepth == "75cm" ~ "75-90cm",
    grounddepth == "85cm" ~ "75-90cm",
    grounddepth == "90cm" ~ "75-90cm"
  ))

# Step 4: Rename avg columns
colnames(gt_daily)[colnames(gt_daily) == "avg"] <- "groundtemp"
colnames(vwc_daily)[colnames(vwc_daily) == "avg"] <- "vwc"
colnames(at_daily)[colnames(at_daily) == "avg"] <- "airtemp"
colnames(solar_daily)[colnames(solar_daily) == "avg"] <- "solar"
colnames(ws_daily)[colnames(ws_daily) == "avg"] <- "windspeed"
colnames(wd_daily)[colnames(wd_daily) == "avg"] <- "winddirection"

# Step 5: FIX BEO sensor names using sensor serial numbers
# Convert sensor to character first!

gt_daily$sensor <- as.character(gt_daily$sensor)
vwc_daily$sensor <- as.character(vwc_daily$sensor)

# Create fullname by mapping BEO sensor serial numbers
gt_daily <- gt_daily %>%
  mutate(fullname = case_when(
    site == "BEO" & sensor == "21398591" ~ "BEO-B05",
    site == "BEO" & sensor == "21393048" ~ "BEO-B06",
    site == "BEO" & sensor == "21393045" ~ "BEO-B07",
    site == "BEO" & sensor == "21398576" ~ "BEO-B08",
    site == "BEO" ~ "BEO-BASE",
    TRUE ~ fullname
  ))

vwc_daily <- vwc_daily %>%
  mutate(fullname = case_when(
    site == "BEO" & sensor == "21398591" ~ "BEO-B05",
    site == "BEO" & sensor == "21393048" ~ "BEO-B06",
    site == "BEO" & sensor == "21393045" ~ "BEO-B07",
    site == "BEO" & sensor == "21398576" ~ "BEO-B08",
    site == "BEO" ~ "BEO-BASE",
    TRUE ~ fullname
  ))

# For other variables, use fullname
at_daily$fullname <- at_daily$fullname
solar_daily$fullname <- solar_daily$fullname
ws_daily$fullname <- ws_daily$fullname
wd_daily$fullname <- wd_daily$fullname

# Verify the fix worked
cat("\n=== AFTER MAPPING: BEO fullname values ===\n")
cat("\ngt_daily BEO fullnames:\n")
print(unique(gt_daily$fullname[grepl("BEO", gt_daily$fullname)]))
cat("\nvwc_daily BEO fullnames:\n")
print(unique(vwc_daily$fullname[grepl("BEO", vwc_daily$fullname)]))

# # Step 6: Expand BEO-BASE for air, solar, wind to match B05, B06, B07, B08
# beo_ground_sensors <- c('BEO-B05', 'BEO-B06', 'BEO-B07', 'BEO-B08')
# 
# expand_beo_base <- function(df) {
#   beo_base_rows <- df %>% filter(fullname == "BEO-BASE")
#   non_beo_rows <- df %>% filter(fullname != "BEO-BASE")
#   
#   if(nrow(beo_base_rows) == 0){
#     return(df)
#   }
#   
#   beo_expanded <- beo_base_rows %>%
#     crossing(new_sensor = beo_ground_sensors) %>%
#     mutate(fullname = new_sensor) %>%
#     select(-new_sensor)
#   
#   bind_rows(non_beo_rows, beo_expanded)
# }


###### NEW ADDITION expand_beo below
# Step 3: Expand BEO-BASE for air, solar, wind ONLY
beo_ground_sensors <- c('BEO-B05', 'BEO-B06', 'BEO-B07', 'BEO-B08')

expand_beo_base <- function(df) {
  beo_base_rows <- df %>% filter(fullname == "BEO-BASE")
  non_beo_rows <- df %>% filter(fullname != "BEO-BASE")
  
  beo_expanded <- beo_base_rows %>%
    crossing(new_sensor = beo_ground_sensors) %>%
    mutate(fullname = new_sensor) %>%
    select(-new_sensor)
  
  bind_rows(non_beo_rows, beo_expanded)
}





####END ADDITION

at_daily <- at_daily %>% expand_beo_base()
solar_daily <- solar_daily %>% expand_beo_base()
ws_daily <- ws_daily %>% expand_beo_base()
wd_daily <- wd_daily %>% expand_beo_base()

# Step 7: Verify ALL sensor names
cat("\n=== BEO Sensor Names After Expansion ===\n")
cat("Ground Temp BEO:\n")
print(unique(gt_daily$fullname[grepl("BEO", gt_daily$fullname)]))
cat("\nVWC BEO:\n")
print(unique(vwc_daily$fullname[grepl("BEO", vwc_daily$fullname)]))
cat("\nAir Temp BEO:\n")
print(unique(at_daily$fullname[grepl("BEO", at_daily$fullname)]))
cat("\nSolar BEO:\n")
print(unique(solar_daily$fullname[grepl("BEO", solar_daily$fullname)]))
cat("\nWind Speed BEO:\n")
print(unique(ws_daily$fullname[grepl("BEO", ws_daily$fullname)]))


###BELOW, OLD JOIN FUNCTION COMMENTED OUT. CURRENT JOIN FUNCTION IS ADOPTED FROM THE GAP_FILL_TRANSPOSE_AND_JOIN.RMD
# # Step 8: Join using sensor_name
# join <- full_join(gt_daily, vwc_daily, by = c("Date", "fullname", "vwcdepth"), relationship = "many-to-many") %>%
#   select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc) %>% 
#   unique() %>%
#   full_join(at_daily, by = c("Date", "fullname"), relationship = "many-to-many") %>%
#   select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp) %>% 
#   unique() %>%
#   full_join(solar_daily, by = c("Date", "fullname"), relationship = "many-to-many") %>%
#   select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar) %>% 
#   unique() %>%
#   full_join(ws_daily, by = c("Date", "fullname"), relationship = "many-to-many") %>%
#   select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed) %>% 
#   unique() %>%
#   full_join(wd_daily, by = c("Date", "fullname"), relationship = "many-to-many") %>%
#   select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed, winddirection) %>% 
#   unique()


# Try without the intermediate unique() calls
join_test <- full_join(gt_daily, vwc_daily, by = c("Date", "fullname", "vwcdepth"), relationship = "many-to-many") %>%
  full_join(at_daily, by = c("Date", "fullname"), relationship = "many-to-many") %>%
  full_join(solar_daily, by = c("Date", "fullname"), relationship = "many-to-many") %>%
  full_join(ws_daily, by = c("Date", "fullname"), relationship = "many-to-many") %>%
  full_join(wd_daily, by = c("Date", "fullname"), relationship = "many-to-many") %>%
  select(Date, fullname, grounddepth, vwcdepth, groundtemp, vwc, airtemp, solar, windspeed, winddirection) %>%
  distinct()  # Use distinct() instead of unique()

# Final verification
cat("\n=== Final Join BEO Check ===\n")
print(join_test %>% filter(grepl("BEO", fullname)) %>% 
        filter(!is.na(airtemp)) %>% 
        head(20))

write.csv(join, file=paste(large_file_export, "All_Variables_", sznstart, "_", sznend, "_HOURLY", ".csv", sep=""))

write.csv(join_test, file=paste(export_to, "Daily/All_Variables_", sznstart, "_", sznend, "_DAILY", ".csv", sep=""))


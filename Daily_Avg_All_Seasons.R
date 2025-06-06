#Daily Averages for All Seasons


filepath = "~/Desktop/Arctic/X-Y_plots/data/raw/Meteorological_Seasons_Data/" #raw data filepath
export_to = "~/Desktop/Arctic/X-Y_Plots/data/processed/Daily_Averages/" #Directory to export clean data sets

library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(gridExtra)
library(forcats)
library(splitstackshape)
library(naniar)
library(stringr)

#Ground Temperature
gtfiles = c("Summer_grndtmp_2023_04_06_12_21_30_UTC_1.csv", 
            "Fall_grndtmp_2023_04_06_12_19_52_UTC_1.csv",
            "Winter_grndtmp_2023_04_06_12_18_43_UTC_1.csv",
            "Spring_grndtmp_2023_09_14_19_38_13_UTC_1.csv")

#Air Temperature
airfiles = c("Summer_airtemp_2023_04_06_11_59_55_UTC_1.csv",
             "Fall_airtemp_2023_04_06_12_01_15_UTC_1.csv",
             "Winter_airtemp_2023_04_06_12_02_30_UTC_1.csv",
             "Spring_airtemp_2023_09_14_19_34_54_UTC_1.csv")

#Solar Radiation
solarfiles = c("Summer_solar_2023_04_06_13_16_22_UTC_1.csv",
               "Fall_solar_2023_04_06_13_09_45_UTC_1.csv",
               "Winter_solar_2023_04_06_12_55_01_UTC_1.csv",
               "Spring_solar_2023_09_14_19_35_21_UTC_1.csv")

#VWC
vwcfiles = c("Summer_VWC_2023_04_06_12_33_54_UTC_1.csv",
             "Fall_VWC_2023_04_06_12_35_08_UTC_1.csv",
             "Winter_VWC_2023_04_06_12_36_52_UTC_1.csv",
             "Spring_VWC_2023_09_14_19_31_00_UTC_1.csv")


#Wind Speed
wspeedfiles = c("Summer22_Summer23_Wind_Speed_2023_09_07_19_43_04_UTC_1.csv")

#Wind Direction
wdirfiles = ("Summer22_Summer23_Wind_Dir_2023_09_07_19_32_22_UTC_1.csv")



#Ground Temperature Average Calculations
groundtemp_daily = data.frame(matrix(nrow = 0, ncol = 5))
colnames(groundtemp_daily) = c("date", "station", "sensor", "depth", "gtavg")

for(file in gtfiles){
  
  #read file into temporary storage df
  temp = read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("....C..RX3000_BRW1", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW4", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW5", "", names(temp))
  names(temp) <- sub("....C..RX3000_BRW6", "", names(temp))
  names(temp) <- sub(".RXW.GP6.", "", names(temp))
  names(temp) <- sub("Temperature.", "", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #some dupes for name change
  dates = substr(temp$Date, 1, 10)
  if(max(dates) < "2023-03-05"){
    temp = select(temp, !contains("_"))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  
  #there may be wind and air variables in the df
  temp = select(temp, !contains(".RXW"))
  temp = select(temp, !contains(".S"))
 
  #turn into long df
  temp = gather(temp, variable, response, 3:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[3:6] <- c("value", "station", "sensor","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$sensor <- as.factor(as.character(temp$sensor))
  temp$station <- as.factor(temp$station)
  
  temp$station <- revalue(temp$station, c("21198259" = "TNHA", "21401800" = "BUECI", "21401801" = "SSMH", "21401803" = "BEO"))
  temp$sensor <- revalue(temp$sensor, c("21398585" = "BUECI-BASE", "21398590" = "BUECI-SA", "21398583" = "BUECI-SB", "21393042" = "BUECI-SC",
                                        "21398584" = "BUECI-SD", "21398579" = "BUECI-SE", "21398578" = "BUECI-SF.01", "21398598" = "BUECI-SF.02", 
                                        "21398588" ="SSMH-BASE","21393049" = "SSMH-SD", "21398599" = "SSMH-SA", "21393044" = "SSMH-SB", "21393049" = "SSMH-SD" ,
                                        "21206939" = "TNHA-BASE", "21398593" = "TNHA-SA", "21398576"="TNHA-SB","21398601" = "TNHA-SD","21398587"="TNHA-SD", "21393047" = "TNHA-SC",
                                        "21393048" = "BEO-B06", "21398591" = "BEO-B05"))
  
  temp <- temp %>% mutate(day = substr(Date, 1, 10))
  #tempavg = data.frame(tapply(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), mean))
  tempavg = aggregate(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), FUN=mean)
  colnames(tempavg) = c("date", "station", "sensor", "depth", "gtavg")
  
  groundtemp_daily = rbind(groundtemp_daily, tempavg)
  
}



#Air Temperature Average Calculations
airtemp_daily = data.frame(matrix(nrow = 0, ncol = 5))
colnames(airtemp_daily) = c("date", "station", "sensor", "depth", "airavg")

file = "Spring_airtemp_2023_09_14_19_34_54_UTC_1.csv"

for(file in airfiles){
  
  #read file into temporary storage df
  temp = read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("Temperature..RXW.THC.", "" ,names(temp))
  names(temp) <- sub("Temperature..S.THC.", "" ,names(temp))
  names(temp) <- sub("S.", "" ,names(temp))
  names(temp) <- sub("....C..RX3000_BRW1", "" ,names(temp))
  names(temp) <- sub("....C..RX3000_BRW4", "" ,names(temp))
  names(temp) <- sub("....C..RX3000_BRW5", "" ,names(temp))
  names(temp) <- sub("....C..RX3000_BRW6", "" ,names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #some dupes for name change
  dates = substr(temp$Date, 1, 10)
  if(max(dates) < "2023-03-05"){
    temp = select(temp, !contains("_"))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  names(temp) <- sub("_MH", "", names(temp))
  
  
  #turn into long df
  temp = gather(temp, variable, response, 3:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[3:6] <- c("value", "station", "sensor","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$sensor <- as.factor(as.character(temp$sensor))
  temp$station <- as.factor(temp$station)
  
  temp$station <- revalue(temp$station, c("21198259" = "TNHA", "21401800" = "BUECI", "21401801" = "SSMH", "21401803" = "BEO"))
  temp$sensor <- revalue(temp$sensor, c("21390851" = "BUECI-BASE", "21398671" = "BUECI-SA","21398659"="BUECI-SA", "21362254" = "BUECI-SB", "21398668" = "BUECI-SC","21397542"="BUECI-SC","21398661" = "BUECI-SD", "21362256" = "BUECI-SD", "21187245" = "BUECI-SE", "21390849" ="SSMH-BASE", "21398670" = "SSMH-SA", "21398665" = "SSMH-SB", "21218018" = "TNHA-BASE", "21380919" = "TNHA-SA", "21398676"="TNHA-SB","21398666" = "TNHA-SD","21398664"="TNHA-SD", "21398674" = "TNHA-SC", "21397541" = "BEO-B02"))
  
  #there is wind speed for BUECI
  #need to remove
  temp = temp %>% filter(sensor != "21398660")
  
  temp <- temp %>% mutate(day = substr(Date, 1, 10))
  #tempavg = data.frame(tapply(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), mean))
  tempavg = aggregate(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), FUN=mean)
  colnames(tempavg) = c("date", "station", "sensor", "depth", "airavg")
  
  airtemp_daily = rbind(airtemp_daily, tempavg)
  
}




#Solar Radiation Daily Average Calculations
solar_daily = data.frame(matrix(nrow = 0, ncol = 5))
colnames(solar_daily) = c("date", "station", "sensor", "depth", "solaravg")

for(file in solarfiles){
  
  #read file into temporary storage df
  temp = read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("olar.Radiation..RXW.LIB", "" ,names(temp))
  names(temp) <- sub("olar.Radiation..S.LIB", "" ,names(temp))
  names(temp) <- sub("S.", "" ,names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW1", "" ,names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW4", "" ,names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW5", "" ,names(temp))
  names(temp) <- sub("...W.m.2..RX3000_BRW6", "" ,names(temp))
  temp$Date <- as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #some dupes for name change
  dates = substr(temp$Date, 1, 10)
  if(max(dates) < "2023-03-05"){
    temp = select(temp, !contains("_"))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  
  #turn into long df
  temp = gather(temp, variable, response, 3:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[3:6] <- c("value", "station", "sensor","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$sensor <- as.factor(as.character(temp$sensor))
  temp$station <- as.factor(temp$station)
  
  temp$station <- revalue(temp$station, c("21198259" = "TNHA", "21401800" = "BUECI", "21401801" = "SSMH", "21401803" = "BEO"))
  temp$sensor <- revalue(temp$sensor, c("21390411" = "BUECI-BASE","21390414"="BUECI-BASE","21398621"="BUECI-SB", "21398618" = "BUECI-SA","21398617"="BUECI-SA", "21398624" = "BUECI-SB", "21362313" = "BUECI-SC","21362314"="BUECI-SC", "21362316" = "BUECI-SD", "21362320" = "BUECI-SE", "21398578" = "BUECI-SF.01", "21398598" = "BUECI-SF.02", 
                                        "21390413" ="SSMH-BASE", "21398622" = "SSMH-SA", "21362319" = "SSMH-SB", "21166008" = "SSMH-SC", "21393049" = "SSMH-SD" ,
                                        "21176526" = "TNHA-BASE", "21398620" = "TNHA-SA", "21398616"="TNHA-SB","21362315" = "TNHA-SD", "21362317" = "TNHA-SC", "21362318"="TNHA-SC",
                                        "21390415" = "BEO-BASE"))
  
  #Not a Solar Sensor
  temp = temp %>% filter(sensor != "21398623")
  
  temp <- temp %>% mutate(day = substr(Date, 1, 10))
  #tempavg = data.frame(tapply(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), mean))
  tempavg = aggregate(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), FUN=mean)
  colnames(tempavg) = c("date", "station", "sensor", "depth", "solaravg")
  
  solar_daily = rbind(solar_daily, tempavg)
  
}




#VWC Daily Average Calculations
vwc_daily = data.frame(matrix(nrow = 0, ncol = 5))
colnames(vwc_daily) = c("date", "station", "sensor", "depth", "vwcavg")

for(file in vwcfiles){
  
  #read file into temporary storage df
  temp = read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("...m.3.m.3..RX3000_BRW1", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW4", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW5", "", names(temp))
  names(temp) <- sub("...m.3.m.3..RX3000_BRW6", "", names(temp))
  names(temp) <- sub("..RXW.GP6.", "", names(temp))
  names(temp) <- sub("Water.Content", "", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  
  #some dupes for name change
  dates = substr(temp$Date, 1, 10)
  if(max(dates) < "2023-03-05"){
    temp = select(temp, !contains("_"))
  }
  
  names(temp) <- sub("_TNHA", "", names(temp))
  names(temp) <- sub("_SSMH", "", names(temp))
  names(temp) <- sub("_BUECI", "", names(temp))
  names(temp) <- sub("_BEO", "", names(temp))
  
  
  #turn into long df
  temp = gather(temp, variable, response, 3:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[3:6] <- c("value", "station", "sensor","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$sensor <- as.factor(as.character(temp$sensor))
  temp$station <- as.factor(temp$station)
  
  temp$station <- revalue(temp$station, c("21198259" = "TNHA", "21401800" = "BUECI", "21401801" = "SSMH", "21401803" = "BEO"))
  temp$sensor <- revalue(temp$sensor, c("21398585" = "BUECI-BASE", "21398590" = "BUECI-SA", "21398583" = "BUECI-SB", "21393042" = "BUECI-SC", "21398584" = "BUECI-SD", "21398579" = "BUECI-SE", "21398578" = "BUECI-SF.01", "21398598" = "BUECI-SF.02", "21398588" ="SSMH-BASE","21393049" = "SSMH-SD", "21398599" = "SSMH-SA", "21393044" = "SSMH-SB", "21393049" = "SSMH-SD" ,
                                        "21206939" = "TNHA-BASE", "21398593" = "TNHA-SA", "21398576"="TNHA-SB","21398587"="TNHA-SB","21398601" = "TNHA-SD", "21393047" = "TNHA-SC", "21393048" = "BEO-B06", "21398591" = "BEO-B05"))
  
  temp <- temp %>% mutate(day = substr(Date, 1, 10))
  #tempavg = data.frame(tapply(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), mean))
  tempavg = aggregate(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), FUN=mean)
  colnames(tempavg) = c("date", "station", "sensor", "depth", "vwcavg")
  
  vwc_daily = rbind(vwc_daily, tempavg)
  
}


#Wind Speed Daily Average Calculations
windspeed_daily = data.frame(matrix(nrow = 0, ncol = 5))
colnames(windspeed_daily) = c("date", "station", "sensor", "depth", "wspeedavg")

for(file in wspeedfiles){
  
  #read file into temporary storage df
  temp = read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("Wind.Speed.", "", names(temp))
  names(temp) <- sub(".S.WSB.", "", names(temp))
  names(temp) <- sub(".RXW.WCF.", "", names(temp))
  names(temp) <- sub("...m.s..RX3000", "", names(temp))
  names(temp) <- sub("_BRW1", "", names(temp))
  names(temp) <- sub("_BRW4", "", names(temp))
  names(temp) <- sub("_BRW6", "", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #The sensor names switched on March 5, 2023 to include "_SITE"
  dates = substr(temp$Date, 1, 10)
  switch = match("2023-03-05", dates) #Find first index of date of switch
  pre_mar = temp[c(1:switch-1),] #select data from before switch
  post_mar = temp[c(switch:nrow(temp)),]#select data from after switch
  
  pre_mar = select(pre_mar, !contains("_")) #columns that don't contain "_SITE"
  post_mar = select(post_mar, contains("_")) #columns that do contain "_SITE"
  
  #make col names match
  names(post_mar) <- sub("_TNHA", "", names(post_mar))
  names(post_mar) <- sub("_BEO", "", names(post_mar))
  names(post_mar) <- sub("_SSMH", "", names(post_mar))
  names(post_mar) <- sub("_BUECI", "", names(post_mar))
  
  temp = temp[,c(1:2)]
  for(c in colnames(pre_mar)[c(3:ncol(pre_mar))]){
    if(c %in% colnames(post_mar)){
      pre = pre_mar %>% select(c)
      post = post_mar %>% select (c)
      long = rbind(pre, post)
      temp = data.frame(temp, long)
    }
    else{
      pre = pre_mar %>% select(c)
      post = data.frame(rep(NA, nrow(temp)-nrow(pre)))
      colnames(post) = c
      long = rbind(pre, post)
      temp = data.frame(temp, long)
    }
  }
  
  names(temp) <- sub("X", "", names(temp))
  
  #turn into long df
  temp = gather(temp, variable, response, 3:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))
  
  temp = filter(temp, variable != "21198259.21181033.1")
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[3:6] <- c("value", "station", "sensor","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$sensor <- as.factor(as.character(temp$sensor))
  temp$station <- as.factor(temp$station)
  
  temp$station <- revalue(temp$station, c("21198259" = "TNHA", "21401800" = "BUECI", "21401801" = "SSMH", "21401803" = "BEO"))
  temp$sensor <- revalue(temp$sensor, c("21350894" = "BUECI-BASE", "21398733" = "BUECI-SA", "21398729" = "BUECI-SB", "21398724" = "BUECI-SC", "21398660" = "BUECI-SD", "21398719" = "BUECI-SE", "21398578" = "BUECI-SF.01", "21398598" = "BUECI-SF.02",
                                        "21350910" ="SSMH-BASE","21393049" = "SSMH-SD", "21206911" = "SSMH-SA", "21206912" = "SSMH-SB", "21393049" = "SSMH-SD" ,
                                        "21176861" = "TNHA-BASE", "21398709" = "TNHA-SA", "21398715"="TNHA-SB","21398712" = "TNHA-SD", "21206909" = "TNHA-SC",
                                        "21350915" = "BEO-BASE"))
  
  temp <- temp %>% mutate(day = substr(Date, 1, 10))
  #tempavg = data.frame(tapply(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), mean))
  tempavg = aggregate(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), FUN=mean)
  colnames(tempavg) = c("date", "station", "sensor", "depth", "wspeedavg")
  
  windspeed_daily = rbind(windspeed_daily, tempavg)
  
}



#Wind Direction Daily Average Calculations
winddir_daily = data.frame(matrix(nrow = 0, ncol = 5))
colnames(winddir_daily) = c("date", "station", "sensor", "depth", "wdiravg")

for(file in wdirfiles){
  
  #read file into temporary storage df
  temp = read.csv(paste0(filepath, file))
  
  #change sensor serial numbers to names
  names(temp) <- sub("Wind.Direction.", "", names(temp))
  names(temp) <- sub(".S.WDA.", "", names(temp))
  names(temp) <- sub(".RXW.WCF.", "", names(temp))
  names(temp) <- sub("......RX3000", "", names(temp))
  names(temp) <- sub("_BRW1", "", names(temp))
  names(temp) <- sub("_BRW4", "", names(temp))
  names(temp) <- sub("_BRW6", "", names(temp))
  temp$Date <-as.POSIXct(temp$Date,format="%m/%d/%y %H:%M",tz="UTC")
  
  #The sensor names switched on March 5, 2023 to include "_SITE"
  dates = substr(temp$Date, 1, 10)
  switch = match("2023-03-05", dates) #Find first index of date of switch
  pre_mar = temp[c(1:switch-1),] #select data from before switch
  post_mar = temp[c(switch:nrow(temp)),]#select data from after switch
  
  pre_mar = select(pre_mar, !contains("_")) #columns that don't contain "_SITE"
  post_mar = select(post_mar, contains("_")) #columns that do contain "_SITE"
  
  #make col names match
  names(post_mar) <- sub("_TNHA", "", names(post_mar))
  names(post_mar) <- sub("_BEO", "", names(post_mar))
  names(post_mar) <- sub("_SSMH", "", names(post_mar))
  names(post_mar) <- sub("_BUECI", "", names(post_mar))
  
  temp = temp[,c(1:2)]
  for(c in colnames(pre_mar)[c(3:ncol(pre_mar))]){
    if(c %in% colnames(post_mar)){
      pre = pre_mar %>% select(c)
      post = post_mar %>% select (c)
      long = rbind(pre, post)
      temp = data.frame(temp, long)
    }
    else{
      pre = pre_mar %>% select(c)
      post = data.frame(rep(NA, nrow(temp)-nrow(pre)))
      colnames(post) = c
      long = rbind(pre, post)
      temp = data.frame(temp, long)
    }
  }
  
  names(temp) <- sub("X", "", names(temp))
  
  
  #turn into long df
  temp = gather(temp, variable, response, 3:ncol(temp)) %>%
    filter(!is.na(response)) %>%
    mutate(variable = fct_inorder(variable))

  
  temp = filter(temp, variable != "21198259.21181033.3")
  
  temp= cSplit(temp, 'variable', sep=".", direction = "wide") # texttocols, 
  colnames(temp)[3:6] <- c("value", "station", "sensor","depth")
  temp$value <- as.numeric(as.character(temp$value)) 
  temp$sensor <- as.factor(as.character(temp$sensor))
  temp$station <- as.factor(temp$station)

  
  temp$station <- revalue(temp$station, c("21198259" = "TNHA", "21401800" = "BUECI", "21401801" = "SSMH", "21401803" = "BEO"))
  temp$sensor <- revalue(temp$sensor, c("21350894" = "BUECI-BASE", "21398733" = "BUECI-SA", "21398729" = "BUECI-SB", "21398724" = "BUECI-SC", "21398660" = "BUECI-SD", "21398719" = "BUECI-SE", "21398578" = "BUECI-SF.01", "21398598" = "BUECI-SF.02",
                                        "21350820" ="SSMH-BASE","21393049" = "SSMH-SD", "21206911" = "SSMH-SA", "21206912" = "SSMH-SB", "21393049" = "SSMH-SD" ,
                                        "21167037" = "TNHA-BASE", "21398709" = "TNHA-SA", "21398715"="TNHA-SB","21398712" = "TNHA-SD", "21206909" = "TNHA-SC",
                                        "21350855" = "BEO-BASE"))
  
  #TNHA Base, SSMH SB, TNHASA, TNHASB, TNHASD, SSMHBase, SSMHSA, BEO BASE
  
  temp <- temp %>% mutate(day = substr(Date, 1, 10))
  #tempavg = data.frame(tapply(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), mean))
  tempavg = aggregate(temp$value, list(temp$day,temp$station,temp$sensor,temp$depth), FUN=mean)
  colnames(tempavg) = c("date", "station", "sensor", "depth", "wdiravg")
  
  winddir_daily = rbind(winddir_daily, tempavg)
  
}


write.csv(groundtemp_daily, file=paste(export_to, "grndtemp_daily.csv", sep=""))
write.csv(airtemp_daily, file=paste(export_to, "airtemp_daily.csv", sep=""))
write.csv(solar_daily, file=paste(export_to, "solar_daily.csv", sep=""))
write.csv(vwc_daily, file=paste(export_to, "vwc_daily.csv", sep=""))
write.csv(windspeed_daily, file=paste(export_to, "windspeed_daily.csv", sep=""))
write.csv(winddir_daily, file=paste(export_to, "winddir_daily.csv", sep=""))
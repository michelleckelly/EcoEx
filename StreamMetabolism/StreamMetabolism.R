# Script description:             Stream metabolism calculations with
#                                 automatic data pulls from USGS & NEON
# Script written by:              Michelle Catherine Kelly
#                                 github: michelleckelly
# Last modified date:             20 November 2017

# Set up environment
rm(list=ls())
library(dataRetrieval) #USGS pull packages
library(httr) #NEON pull packages
library(jsonlite)
library(dplyr)
library(streamMetabolizer) #required for streamMetabolizer
library(tidyr)
library(ggplot2) #data vis

# FOR REFERENCE: list of USGS parameter code numerics is available at
# https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%

# list of NEON product code numerics is available at
# http://data.neonscience.org/data-product-catalog 

# Data to be pulled from USGS
siteNo <- "06892350" # Kansas R at Desoto, KS
startDate <- "2016-08-08" # YYYY-MM-DD
endDate <- "2016-08-15"
pCode <- c("00010","00060","00065","00300","00301","00400","32318","63680","99133") 
# Parameter codes: water temp, discharge, gage height, DO, DO % sat, pH, chlorophyll, NO3+NO2

# justification for using gage height to approximate stream depth https://water.usgs.gov/edu/measureflow.html

# Pull from USGS
kansas <- readNWISuv(siteNumbers = siteNo, parameterCd = pCode,
                     startDate = startDate, endDate = endDate,
                     tz = "America/Chicago")
# Rename columns to paramater names
kansas <- renameNWISColumns(kansas)

# Fix remaining names that are still parameter codes
names(kansas)[names(kansas)=="00301_Inst"] <- "DO_percentsat"
names(kansas)[names(kansas)=="00301_Inst_cd"] <- "DO_percentsat_cd"
names(kansas)[names(kansas)=="32318_Inst"] <- "chlorophyll"
names(kansas)[names(kansas)=="32318_Inst_cd"] <- "chlorophyll_cd"
names(kansas)[names(kansas)=="99133_Inst"] <- "nitrate"
names(kansas)[names(kansas)=="99133_Inst_cd"] <- "nitrate_cd"

# Dataframe of parameter names, codes, description, & units
parameterInfo <- attr(kansas, "variableInfo")
parameterInfo$variableDescription #gives variable and units
siteInfo <- attr(kansas, "siteInfo")

# Timeseries plot of nitrate
nitrateplot <- ggplot(data = kansas, aes(dateTime, nitrate))+geom_line()
nitrateplot <- nitrateplot + xlab("") + ylab(parameterInfo$variableDescription[8]) +
  ggtitle(siteInfo$station_nm)
nitrateplot

# Data pull from NEON
# NOTES FOR EDITING: either change input to just month or figure out how to loop input

NEON_PARpull <- function(endpoint, productCode, NEONsite, year, month){
  baseURL <- "http://data.neonscience.org/api/v0" # the base for all calls to the API
  getURL <- paste(baseURL, endpoint, productCode, sep = "/") #concatenates URL
  
  dataPull <- GET(url = getURL) # actual data pull from NEON api
  
  pull_text <- content(dataPull, as = "text") # make output JSON readable
  pull_avail <- fromJSON(pull_text, simplifyDataFrame = TRUE, flatten = TRUE) # flatten data frame to see available data
  pull_urls <- unlist(pull_avail$data$siteCodes$availableDataUrls) #returns list of all available data by URL
  
  # generate URL snips to be used with grep
  URLsnip <- c()
  for (i in 1:length(month)){
    if (month < 10){
      monthA <- paste(0, month, sep = "")
      yearmonth <- paste(year, monthA, sep = "-")
      URLsnip[i] <- paste(NEONsite, yearmonth, sep = "/")
      i <- i + 1
    }
    if (month >= 10){
      yearmonth <- paste(year, month, sep = "-")
      URLsnip[i] <- paste(NEONsite, yearmonth, sep = "/")
    }
  }
  
  # add an apply function in here to grep across months 1:12
  data_yearmonth1 <- GET(pull_urls[grep(URLsnip[1], pull_urls)]) #get data availability for time frame
  data_files_yearmonth1 <- fromJSON(content(data_yearmonth1, as = "text"))
  
  # grab available data files for this month with PAR measured at 30 min intervals and the basic file
  data_yearmonth1_30min <- data_files_yearmonth1$data$files$url[intersect(grep("PARPAR_30min", data_files_yearmonth1$data$files$name), 
                                                                  grep("basic", data_files_yearmonth1$data$files$name))]
  
  data_yearmonth1_30min #all available data that meet this criteria (6 diff files - not sure what the difference is between them!!)
  data_yearmonth1_30min[1] #for now, just pull the first one in the list
  
  #note: pull from neon is time sensitive, server will be expecting read.delim to be in same time period
  PAR_2016.01_30min_data <- read.delim(data_yearmonth1_30min[1], sep = ",")
  return(PAR_2016.01_30min_data)
}

PAR_2016.01_30min <- NEON_PARpull(endpoint = "products", productCode = "DP1.00024.001", 
                                  NEONsite = "UKFS", year = "2016", month = 1) #jan
PAR_2016.08_30min <- NEON_PARpull(endpoint = "products", productCode = "DP1.00024.001", 
                                  NEONsite = "UKFS", year = "2016", month = 8) #aug

# "DP1.00024.001" code for PAR
# "UKFS" KU field station

#can probably hide this within function
PAR_Date_start <- sub("(\\d\\d\\d\\d-\\d\\d-\\d\\d)\\w\\d\\d:\\d\\d:\\d\\d\\w", "\\1", PAR_2016.08_30min$startDateTime, perl = TRUE) 
PAR_time_start <- sub("\\d\\d\\d\\d-\\d\\d-\\d\\d\\w(\\d\\d:\\d\\d:\\d\\d)\\w", "\\1", PAR_2016.08_30min$startDateTime, perl = TRUE) 
PAR_Date_end <- sub("(\\d\\d\\d\\d-\\d\\d-\\d\\d)\\w\\d\\d:\\d\\d:\\d\\d\\w", "\\1", PAR_2016.08_30min$endDateTime, perl = TRUE) 
PAR_time_end <- sub("\\d\\d\\d\\d-\\d\\d-\\d\\d\\w(\\d\\d:\\d\\d:\\d\\d)\\w", "\\1", PAR_2016.08_30min$endDateTime, perl = TRUE) 

PAR_2016.08_30min$startDateTime <- as.POSIXct(paste(PAR_Date_start, PAR_time_start, sep = " "), tz = "America/Chicago")
PAR_2016.08_30min$endDateTime <- as.POSIXct(paste(PAR_Date_end, PAR_time_end, sep = " "), tz = "America/Chicago")

head(PAR_2016.08_30min) #ready!!!!!!!!!

PAR_Aug <- data.frame(PAR_2016.08_30min$startDateTime, PAR_2016.08_30min$PARMean)
names(PAR_Aug) <- c("DateTime", "PAR")
head(PAR_Aug)

#--------- stream metabolism calcs

metab_inputs("mle", "data") #lists required data and units for max likelyhood estimation model

# converting both Datetimes to solar time
siteInfo$dec_lon_va # returns longitude of station

lubridate::tz(kansas$dateTime) # check that both data sets are in same TZ
lubridate::tz(PAR_Aug$DateTime)

kansas$solarTime <- streamMetabolizer::calc_solar_time(kansas$dateTime, longitude = siteInfo$dec_lon_va)
PAR_Aug$solarTime <- streamMetabolizer::calc_solar_time(PAR_Aug$DateTime, longitude = siteInfo$dec_lon_va) #approximating longitude of KU field station -- edit

# Convert discharge, gage ht to SI units ----- clean this up
ft3s_m3s <- function(ft3s){
  m3s <- ft3s*0.0283168
  return(m3s)
}
kansas$Flow_Inst <- ft3s_m3s(kansas$Flow_Inst)
parameterInfo$variableName[2] <- "Streamflow, m3/s"
parameterInfo$variableDescription[2] <- "Discharge, cubic meters per second"
parameterInfo$unit[2] <- "m3/s"

ft_m <- function(ft){
  m <- 0.3048*ft
  return(m)
}
kansas$GH_Inst <- ft_m(kansas$GH_Inst)
parameterInfo$variableName[3] <- "Gage height, m"
parameterInfo$variableDescription[3] <- "Gage height, meters"
parameterInfo$unit[3] <- "m"

# compile data frame of merged solar datetime, 
# PAR, discharge, o2, o2 sat, water temp, gage ht

#note:Kansas_streammet_inputs covers WHOLE YR, for example limit to a couple days
kansas_streammet_inputs <- data.frame(kansas$solarTime, kansas$DO_Inst, kansas$DO_percentsat, kansas$GH_Inst, kansas$Wtemp_Inst, kansas$Flow_Inst)
kansas_PAR_inputs <- data.frame(PAR_Aug$solarTime, PAR_Aug$PAR)

names(kansas_streammet_inputs) <- c("solarTime", "DO_Inst", "DO_percentsat", 
                                    "GH_Inst", "Wtemp_Inst", "Flow_Inst")
names(kansas_PAR_inputs) <- c("solarTime", "PAR")
# head(merge(kansas_streammet_inputs, kansas_PAR_inputs))


head(kansas_streammet_inputs)
head(kansas_PAR_inputs)
kansas_met <- merge(kansas_streammet_inputs, kansas_PAR_inputs, by = "solarTime")
head(kansas_met)
names(kansas_met) <- c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "discharge", "light")
kansas_met <- kansas_met[c(1,2,3,4,5,7,6)] # reorder data frame

#model fitting

mle_name <- mm_name(type = "mle")
mle_specs <- specs(mle_name) #view specifications of model, change with specs(mle_name, init.GPP.daily = 2, ...)
mle_fit <- metab(mle_specs, data = kansas_met, info = c(site = "Kansas R at Desoto", source = "USGS"))

#output of graphs onto pdf
pdf(file = "Streammet_vis.pdf", width = 11, height = 8.5)

discharge_plot <- ggplot(data = kansas, aes(dateTime, Flow_Inst)) + geom_point()
discharge_plot <- ts + xlab("") + ylab(parameterInfo$variableDescription[2]) + 
  ggtitle(siteInfo$station_nm) + theme_classic()
discharge_plot

GPP_ER_plot <- plot_metab_preds(mle_fit, style = c("ggplot2"))
GPP_ER_plot

DO_predict_plot <- plot_DO_preds(mle_fit, style = c("ggplot2"))
DO_predict_plot

dev.off()

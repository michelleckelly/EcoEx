# Script description:             Stream metabolism calculations with
#                                 automatic data pulls from USGS & NEON
# Script written by:              Michelle Catherine Kelly
#                                 github: michelleckelly

# Set up environment
rm(list=ls())
library(ggplot2)
library(dataRetrieval) #USGS API package
library(httr) #NEON API associated packages
library(jsonlite)
library(dplyr)
library(streamMetabolizer) #streamMetabolizer and associated packages
library(tidyr)
library(beepr) # Makes a beep when the script is done running :)

# FOR REFERENCE: list of USGS parameter code numerics is available at
# https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%

# list of NEON product code numerics is available at
# http://data.neonscience.org/data-product-catalog 

# Set up data pull from USGS API
siteNo <- "06892350" # Kansas R at Desoto, KS
startDate <- "2016-08-9" # YYYY-MM-DD
endDate <- "2016-08-11"
pCode <- c("00010","00060","00065","00300","00301") # Parameter codes: water temp, discharge, gage height, DO, DO % saturation

# Justification for using gage height to approximate stream depth https://water.usgs.gov/edu/measureflow.html

# Pull from USGS
kansas <- readNWISuv(siteNumbers = siteNo, parameterCd = pCode, startDate = startDate, endDate = endDate, tz = "America/Chicago")

# Pull cleanup
kansas <- renameNWISColumns(kansas) # Rename columns from codes to parameter names
names(kansas)[names(kansas)=="00301_Inst"] <- "DO_percentsat" # Fix remaining names that are still parameter codes
names(kansas)[names(kansas)=="00301_Inst_cd"] <- "DO_percentsat_cd"

# Assemble meta-dataframes of parameter names, codes, description, & units
parameterInfo <- attr(kansas, "variableInfo") # Parameter information dataframe
parameterInfo$variableDescription # Outputs parameter descriptions and units
siteInfo <- attr(kansas, "siteInfo") # Monitoring station information dataframe
siteInfo$station_nm # Outputs name and general location of station

# Convert from American to SI units
ft3s_m3s <- function(ft3s){
  m3s <- ft3s*0.0283168
  return(m3s)
}
ft_m <- function(ft){
  m <- 0.3048*ft
  return(m)
}
kansas$Flow_Inst <- ft3s_m3s(kansas$Flow_Inst)
kansas$GH_Inst <- ft_m(kansas$GH_Inst)

parameterInfo$variableName[grep(pattern = "ft3/s", x = parameterInfo$unit)] <- "Streamflow, m3/s" # Rename descriptions
parameterInfo$variableDescription[grep(pattern = "ft3/s", x = parameterInfo$unit)] <- "Discharge, cubic meters per second"
parameterInfo$unit[grep(pattern = "ft3/s", x = parameterInfo$unit)] <- "m3/s"

parameterInfo$variableName[grep(pattern = "ft$", x = parameterInfo$unit, perl = TRUE)] <- "Gage height, m"
parameterInfo$variableDescription[grep(pattern = "ft$", x = parameterInfo$unit, perl = TRUE)] <- "Gage height, meters"
parameterInfo$unit[grep(pattern = "ft$", x = parameterInfo$unit, perl = TRUE)] <- "m"

# Data pull from NEON

NEON_PARpull <- function(endpoint, productCode, NEONsite, year, month, tz){
  baseURL <- "http://data.neonscience.org/api/v0" # The base URL for all calls to the API
  getURL <- paste(baseURL, endpoint, productCode, sep = "/") # Concatenates inputs and URL
  dataPull <- GET(url = getURL) # Communication with NEON API
  
  pull_text <- content(dataPull, as = "text") # Make output JSON readable
  pull_avail <- fromJSON(pull_text, simplifyDataFrame = TRUE, flatten = TRUE) # Flatten data frame to see available data
  pull_urls <- unlist(pull_avail$data$siteCodes$availableDataUrls) #Returns list of all available data by URL
  
  # Generate URL snips from month input to be used with grep
  if (month < 10){
      monthA <- paste(0, month, sep = "")
      yearmonth <- paste(year, monthA, sep = "-")
      URLsnip <- paste(NEONsite, yearmonth, sep = "/")
  }
  if (month >= 10) {
      yearmonth <- paste(year, month, sep = "-")
      URLsnip <- paste(NEONsite, yearmonth, sep = "/")
  }
  
  data_yearmonth <- GET(pull_urls[grep(URLsnip, pull_urls)]) #get data availability for time frame
  data_files_yearmonth <- fromJSON(content(data_yearmonth, as = "text"))
   
  # grab available data files for this month with PAR measured at 30 min intervals
  data_yearmonth_30min <- data_files_yearmonth$data$files$url[intersect(grep("PARPAR_30min", data_files_yearmonth$data$files$name),
                                                                        grep("basic", data_files_yearmonth$data$files$name))]
  
  PAR_yearmonth_30min_data <- read.delim(data_yearmonth_30min[1], sep = ",") #there are multiple potential data sets per selected year and month. I'm not sure what the difference is, need to check. For now, pull set 1
  
  #Cleaning up time formats, making sure they're POSIX readable
  PAR_Date_start <- sub("(\\d\\d\\d\\d-\\d\\d-\\d\\d)\\w\\d\\d:\\d\\d:\\d\\d\\w", "\\1", PAR_yearmonth_30min_data$startDateTime, perl = TRUE) 
  PAR_time_start <- sub("\\d\\d\\d\\d-\\d\\d-\\d\\d\\w(\\d\\d:\\d\\d:\\d\\d)\\w", "\\1", PAR_yearmonth_30min_data$startDateTime, perl = TRUE) 
  PAR_Date_end <- sub("(\\d\\d\\d\\d-\\d\\d-\\d\\d)\\w\\d\\d:\\d\\d:\\d\\d\\w", "\\1", PAR_yearmonth_30min_data$endDateTime, perl = TRUE) 
  PAR_time_end <- sub("\\d\\d\\d\\d-\\d\\d-\\d\\d\\w(\\d\\d:\\d\\d:\\d\\d)\\w", "\\1", PAR_yearmonth_30min_data$endDateTime, perl = TRUE) 
  PAR_yearmonth_30min_data$startDateTime <- as.POSIXct(paste(PAR_Date_start, PAR_time_start, sep = " "), tz = tz)
  PAR_yearmonth_30min_data$endDateTime <- as.POSIXct(paste(PAR_Date_end, PAR_time_end, sep = " "), tz = tz)
  
  #subsetting data frame of just times and PAR means
  PAR_outputdata <- data.frame(PAR_yearmonth_30min_data$startDateTime, PAR_yearmonth_30min_data$PARMean)
  names(PAR_outputdata) <- c("DateTime", "PAR")
  return(PAR_outputdata)
}

# "DP1.00024.001" code for PAR
# "UKFS" KU field station
# month must be numeric for logic argument

PAR_2016.08 <- NEON_PARpull(endpoint = "products", productCode = "DP1.00024.001", NEONsite = "UKFS", year = "2016", month = 8, tz = "America/Chicago")

# Stream Metabolism Calculations

# List of required data and units for maximum likelyhood estimation (MLE) model
metab_inputs("mle", "data")

# Convert both datetimes to solar time
lubridate::tz(kansas$dateTime) == lubridate::tz(PAR_2016.08$DateTime) # check that both data sets are in same tz

kansas$solarTime <- streamMetabolizer::calc_solar_time(kansas$dateTime, longitude = siteInfo$dec_lon_va)
PAR_2016.08$solarTime <- streamMetabolizer::calc_solar_time(PAR_2016.08$DateTime, longitude = siteInfo$dec_lon_va)

# Compile data frame of MLE-required data

kansas_streammet_inputs <- data.frame(kansas$solarTime, kansas$DO_Inst, kansas$DO_percentsat, kansas$GH_Inst, kansas$Wtemp_Inst, kansas$Flow_Inst)
kansas_PAR_inputs <- data.frame(PAR_2016.08$solarTime, PAR_2016.08$PAR)
# Rename columns for clarity
names(kansas_streammet_inputs) <- c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "discharge")
names(kansas_PAR_inputs) <- c("solar.time", "light")
# Merge seperate data frames by solar.time
kansas_met <- merge(kansas_streammet_inputs, kansas_PAR_inputs, by = "solar.time")

# Model fitting, as detailed in StreamMetabolizer package documentation
mle_name <- mm_name(type = "mle")
mle_specs <- specs(mle_name) #view specifications of model, change with specs(mle_name, init.GPP.daily = 2, ...)
mle_fit <- metab(mle_specs, data = kansas_met, info = c(site = "Kansas R at Desoto", source = "USGS"))

#output of graphs onto pdf
pdf(file = "Streammet_vis.pdf", width = 9, height = 5, paper = "letter")

# Plot of discharge
discharge_plot <- ggplot(data = kansas, aes(dateTime, Flow_Inst)) + geom_point()
discharge_plot <- discharge_plot + xlab("") + ylab(parameterInfo$variableDescription[2]) + 
  ggtitle(siteInfo$station_nm) + theme_classic()
discharge_plot
# Plot of predicted GPP and ER
GPP_ER_plot <- plot_metab_preds(mle_fit, style = c("ggplot2"))
GPP_ER_plot
# Plot of predicted DO
DO_predict_plot <- plot_DO_preds(mle_fit, style = c("ggplot2"))
DO_predict_plot

dev.off()
beepr::beep("treasure")

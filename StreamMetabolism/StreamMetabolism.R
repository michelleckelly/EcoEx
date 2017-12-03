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

# FOR REFERENCE: list of USGS parameter code numerics is available at
# https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%

# list of NEON product code numerics is available at
# http://data.neonscience.org/data-product-catalog 

# Set up data pull from USGS API
siteNo <- "06892350" # Kansas R at Desoto, KS
startDate <- "2016-08-08" # YYYY-MM-DD
endDate <- "2016-08-15"
pCode <- c("00010","00060","00065","00300","00301") # Parameter codes: water temp, discharge, gage height, DO, DO % saturation

# justification for using gage height to approximate stream depth https://water.usgs.gov/edu/measureflow.html

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

# Data visualization example: time series plot of discharge
dischargeplot <- ggplot(data = kansas, aes(dateTime, Flow_Inst))+geom_line()
dischargeplot <- dischargeplot + xlab("") + 
  ylab(parameterInfo$variableDescription[2]) +
  ggtitle(siteInfo$station_nm)
dischargeplot

# Data pull from NEON
# NOTES FOR EDITING: either change input to just month or figure out how to loop input

NEON_PARpull <- function(endpoint, productCode, NEONsite, year, months){
  baseURL <- "http://data.neonscience.org/api/v0" # The base URL for all calls to the API
  getURL <- paste(baseURL, endpoint, productCode, sep = "/") # Concatenates inputs and URL
  dataPull <- GET(url = getURL) # Communication with NEON API
  
  pull_text <- content(dataPull, as = "text") # Make output JSON readable
  pull_avail <- fromJSON(pull_text, simplifyDataFrame = TRUE, flatten = TRUE) # Flatten data frame to see available data
  pull_urls <- unlist(pull_avail$data$siteCodes$availableDataUrls) #Returns list of all available data by URL
  
  # Generate URL snips from month input to be used with grep
  URLsnip <- c()
  for (i in 1:length(month)){
    if (month[i] < 10){
      monthA <- paste(0, month[i], sep = "")
      yearmonth <- paste(year, monthA, sep = "-")
      URLsnip[i] <- paste(NEONsite, yearmonth, sep = "/")
      i <- i + 1
    }
    else {
      yearmonth <- paste(year, month[i], sep = "-")
      URLsnip[i] <- paste(NEONsite, yearmonth, sep = "/")
    }
  }
  URLsnip
  
  # Getting data availability across all selected time frames (URL snips)
  data_yearmonth <- c()
  data_yearmonth <- lapply(X = URLsnip, FUN = function(x){GET(pull_urls[grep(x, pull_urls)])})
  data_yearmonth  #works
  
  data_files_yearmonth <- c()
  data_files_yearmonth <- lapply(X = data_yearmonth, FUN = function(x){fromJSON(content(x, as = "text"))})
  data_files_yearmonth[[1]]$data$files$url  #gives urls for first month in list
  
  # Data files for month, PAR measured at 30 min intervals
  # Clumsy formatting here, to be fixed ASAP
  data_yearmonth_30min <- c()
  length(month)
  data_yearmonth_30min_1 <- lapply(X = data_files_yearmonth[[1]]$data$files$url, FUN = function(x){x[intersect(grep("PARPAR_30min", x), grep("basic", x))]})
  data_yearmonth_30min_2 <- lapply(X = data_files_yearmonth[[2]]$data$files$url, FUN = function(x){x[intersect(grep("PARPAR_30min", x), grep("basic", x))]})
  data_yearmonth_30min_3 <- lapply(X = data_files_yearmonth[[3]]$data$files$url, FUN = function(x){x[intersect(grep("PARPAR_30min", x), grep("basic", x))]})
  data_yearmonth_30min_4 <- lapply(X = data_files_yearmonth[[4]]$data$files$url, FUN = function(x){x[intersect(grep("PARPAR_30min", x), grep("basic", x))]})
  data_yearmonth_30min_5 <- lapply(X = data_files_yearmonth[[5]]$data$files$url, FUN = function(x){x[intersect(grep("PARPAR_30min", x), grep("basic", x))]})
  
  data_yearmonth_30min_1 <- data_yearmonth_30min_1[grep("https", data_yearmonth_30min_1)]
  data_yearmonth_30min_2 <- data_yearmonth_30min_2[grep("https", data_yearmonth_30min_2)]
  data_yearmonth_30min_3 <- data_yearmonth_30min_3[grep("https", data_yearmonth_30min_3)]
  data_yearmonth_30min_4 <- data_yearmonth_30min_4[grep("https", data_yearmonth_30min_4)]
  data_yearmonth_30min_5 <- data_yearmonth_30min_5[grep("https", data_yearmonth_30min_5)]
  
  #note: pull from neon is time sensitive, server will be expecting read.delim to be executed in same time period
  PAR_outputdata_1 <- read.delim(data_yearmonth_30min_1[[1]], sep = ",")
  PAR_outputdata_2 <- read.delim(data_yearmonth_30min_2[[1]], sep = ",")
  PAR_outputdata_3 <- read.delim(data_yearmonth_30min_3[[1]], sep = ",")
  PAR_outputdata_4 <- read.delim(data_yearmonth_30min_4[[1]], sep = ",")
  PAR_outputdata_5 <- read.delim(data_yearmonth_30min_5[[1]], sep = ",")
  
  PAR_outputdata <- list(PAR_outputdata_1, PAR_outputdata_2, PAR_outputdata_3, PAR_outputdata_4, PAR_outputdata_5)
  return(PAR_outputdata)
}

PAR_2016.08_2016.12_30min <- NEON_PARpull(endpoint = "products", productCode = "DP1.00024.001", NEONsite = "UKFS", year = "2016", month = 8:12) #aug thru dec

str(PAR_2016.08_2016.12_30min)
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

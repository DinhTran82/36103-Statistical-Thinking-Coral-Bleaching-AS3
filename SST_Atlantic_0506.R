library(httr)
library(tidyverse)
library(jsonlite)

library(lubridate) # Useful functions for dealing with dates
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing



# The information for the NOAA OISST data
rerddap::info(datasetid = "NOAA_DHW_monthly", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

NOAA_datasetid <-"NOAA_DHW_monthly"   #NOAA global coral bleaching monitoring
NOAA_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"

# This function downloads and prepares data based on user provided start and end dates, longitutde and latitude
NOAA_sub_dl <- function(time_start, time_end, xlon, ylat) {
  NOAA_dat <- griddap(
    x = NOAA_datasetid,
    url = NOAA_url,
    time = c(time_start, time_end),
    latitude = c(ylat-0.01, ylat+0.05),
    longitude = c(xlon-0.01, xlon+0.05 ),
    fields = c(
      "sea_surface_temperature",
      "sea_surface_temperature_anomaly"
    )
  )$data %>%
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
    rename(t = time, SST =  sea_surface_temperature, SSTA =  sea_surface_temperature_anomaly) %>%
    select(t, lon, lat, SST, SSTA) %>%
    na.omit()
}

start_date <- as.Date(c("2005-01-01"))
 end_date<- as.Date(c("2006-12-31"))

 
 location <-read_csv("Loc0506.csv")
 
location
 n_rows <- nrow(location)
 n_rows
 

 
 desired_length <- n_rows # or whatever length you want
 SST <- vector(mode = "list", length = desired_length)
 
 
 system.time(for (i in 1:desired_length) {
   #Skip data that we have already downloaded OR NaN values
   if (!is.null(SST[[i]])) {
     next
   }
   tryCatch({
     SST[[i]] <- NOAA_sub_dl(start_date,
                                  end_date,
                                  location$Longitude_coarse[i],
                                  location$Latitude_coarse[i])
     Sys.sleep(.1)
   }
   , error = function(e) {
     
   })
   
   if (i %% 10 == 0) {
     print(sprintf("Downloading line %i", i))
   }
   
   filename <- sprintf("C:\\Users\\Dinh\\Dropbox\\UTS\\LearnR\\AS3\\datadownload1\\data__%003i_%f_%f.csv", i , location$Longitude_coarse[i], location$Latitude_coarse[i] )
   #  print(paste("Writing to file", filename))
   write.table(SST[i], filename)
   
 }
 
 #TO DO save the FINAL DOWNLOAD DATA 5801-5855
 
 )
 
 
 location$SSTdata <- SST
write.csv(SSTtable,"temp_SST_table.csv")

write_csv(location, "Location0506_SST.csv")


#write_json(location,"temp.json")
#temp_location <- read_json("temp.json")

View(temp_location)

SST_Pelican <- NOAA_sub_dl(time_start, time_end, X_lon, Y_lat)
SST_Pelican 

write_csv(SST_Pelican, "Pelican_island.csv")


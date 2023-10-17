library(stringr)
library(janitor)
library(jsonlite)
library(tidyr)
library(dplyr)
library(grf)
rm(list = ls())

setwd("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk")

#################################################################################################
#Get postnummer data with geolocation at https://www.erikbolstad.no/postnummer-koordinatar/txt/#
#################################################################################################

#First we only do some data handling 

#Loading postnummer from Erik Bolstad and extracting coordinates (after some name editing)
postnummer <- read.table('C:/Users/christian.thorjussen/Project Nortura/postnummer.csv', header=TRUE, sep="\t")
postnummer <- postnummer %>% 
  clean_names()
#postnummer$poststad <- str_to_title(str_to_lower(postnummer$poststad))
postnummer <- subset(postnummer, select = c('postnr', 'lat', 'lon'))
colnames(postnummer) <- c('postnummer', 'lat', 'lon')

#Leverandoer.Rdata has changed encoding and stored as .csv. This is not necessary, the original Leverandoer.Rdata can be used
lever_data <- load("Leverandoer.Rdata")
lever_data <- Leverandoer
leverandoer <- subset(lever_data, select = c('LeverandoerNr', 'Postnr'))
colnames(leverandoer) <- c('id_producer', 'postnummer')
rm(lever_data)

#Merge leverandoer ID and location data
locations <- merge(leverandoer, postnummer, by='postnummer')

####################################################################################
# Then get all the station IDs closes to the postnummer geolocation
####################################################################################
#First you need a Frost client ID, go to Frost web side to register
# Insert your own client ID here
client_id <- '188729ee-d228-4e9e-8a3b-b57101f6aee3'

my_list <- unique(as.integer(locations$id_producer))
my_list <- sort(my_list)
results <- list()

#Loop through all leverandor ID's extract the postnummer coordinates 
#and use those to get nearest weather stations IDs

for (i in 1:length(my_list)) {
  # Get the lat and lon from locations table
  current_df <-  filter(locations, id_producer == my_list[i])
  lon <- current_df$lon[1]
  lat <- current_df$lat[1]
  
  # Build URL 
  endpoint <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld")
  geometry <- paste0('nearest(POINT(',lon,' ',lat,'))')
  nearestmaxcount <- 6 #Set the number of stations you want 
  fields <- 'id, masl' #Set the fields you want, we only want the station ID and meter above sea level (we also get distance as default)

  url <- paste0(
    endpoint, "?types=SensorSystem&",
    "geometry=", geometry,
    "&nearestmaxcount=", nearestmaxcount,
   "&fields=", fields
  )
  
  #url #Check if the url looks correct
  
  print(paste0('Results from producer: ',i,''))
  # Extract JSON data
  xs <- try(fromJSON(URLencode(url),flatten=T))
  
  # Check if the request worked and if it works, collect the data
  if (class(xs) != 'try-error') {
    df <- unnest(xs$data, cols = c('id', ))
    #Storing the results
    df$id_producer <- my_list[i]
    results <- append(results, df[c('id_producer', 'id', 'distance', 'masl')])
    
    print("Data retrieved from frost.met.no!")
  } else {
    print("Error: the data retrieval was not successful!")
  }

}
rm(current_df)
#Creating a dataframe called station_data 
station_data <- data.frame(matrix(results, ncol = 4, byrow = T))
station_data <- data.frame(cbind(unlist(station_data[,1]), 
                                 unlist(station_data[,2]), 
                                 unlist(station_data[,3]),
                                 unlist(station_data[,4])))
colnames(station_data) <- c('id_producer', 'id_station', 'distance', 'masl')
save(station_data, file = "station_data.Rdata")
######################################################
#__________Next part gets temperature data____________
######################################################
#In case you start here:
load('station_data.Rdata')

#Connect id_producer and id_station to id_batch (innsettID) 
load("Innsett.Rdata")
innsett_to_producer <- subset(Innsett, select =  c('LeverandoerNr', 'PK_Innsett_Dim')) 
colnames(innsett_to_producer) <- c('id_producer', 'id_batch')


#Getting dates
load("Produksjonsdata.Rdata")
dates <- subset(Produksjonsdata, select = c('FK_Innsett_Dim', 'Dato'))
colnames(dates) <- c( 'id_batch', 'prod_date')

#Merging
farms_loc <- merge(innsett_to_producer, station_data, by = 'id_producer')
data <- merge(dates, farms_loc, by = 'id_batch')

head(data)
# The data frame data is very large since it has several weather stations for each production day per innett ID. This is becaues not all 
# weather stations have the data we need, therefor we need to search more than one to increase our chance to get data. 
# In the next step we are gonna loop all rows in this data frame to get meteorological data
# the loop takes some time.

#Shaving off a few dates 
data <- subset(data, as.Date(prod_date) <= as.Date("2023-08-01"))

#The procedure is pretty much the same as above
# Insert your own client ID here
client_id <- '188729ee-d228-4e9e-8a3b-b57101f6aee3'

endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld") #Specifying observations
my_list <- unique(as.integer(data$id_batch)) #Get all batch IDs
# my_list <- c(1,2,3)
my_list <- sort(my_list)
results <- list() 
#Loop through all batches
for (i in 1:length(my_list)) {
  batch_data <-  filter(data, id_batch == my_list[i])
  
  #Letting us know how far we are on the process
  print(paste0('Feching data for batch ', i, ' out of a total of ', length(my_list),' batches'))
  
  batch_data$prod_date <- as.Date(batch_data$prod_date)
  referenceTime <- paste0('',min(batch_data$prod_date),'/',max(batch_data$prod_date),'')
  sources <- paste(unique(batch_data$id_station), collapse = ", ")
  elements <- 'mean(air_temperature P1D)'
  
  url <- paste0(
  endpoint, "?",
  "sources=", sources,
  "&referencetime=", referenceTime,
  "&elements=", elements 
  )  
    
  xs <- suppressWarnings(try(fromJSON(URLencode(url),flatten=T), silent = T))
  
  #Unpack the data from FROST and print out some text to see how things are going    
  if (class(xs) != 'try-error') {
    df <- unnest(xs$data, cols = c('observations'))
    df <- df[, c('sourceId', 'referenceTime', 'value', 'timeOffset', 'qualityCode')]
    df$id_batch <- my_list[i]
    results <- append(results, df)
    print("Data retrieved from frost.met.no!")
    } else {
    print(paste0('Error: station IDs ', sources, ' did not retrieved temperature data in interval ', referenceTime,''))
    }
  }


#Unpacking the data
matrix_data <- matrix(results, ncol = 6, byrow = T)
temperature_data <- data.frame(cbind(unlist(matrix_data[,6]), 
                                 unlist(matrix_data[,1]), 
                                 unlist(matrix_data[,2]),
                                 unlist(matrix_data[,3]),
                                 unlist(matrix_data[,4]),
                                 unlist(matrix_data[,5])))
colnames(temperature_data) <- c('id_batch', 'id_station', 'date', 'temperature', 'timeOffset', 'qualityCode')
temperature_data$date <- as.Date(temperature_data$date, origin = "1970-01-01")
#Looks ok
head(temperature_data)
#Saving the results
write.csv(temperature_data, "temperature_data.csv", row.names = FALSE)
save(temperature_data, file = "Temperature_update.Rdata")
rm(results)
rm(xs)

#Then we do the same for humidity
# Insert your own client ID here
client_id <- '188729ee-d228-4e9e-8a3b-b57101f6aee3'

endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld") 
my_list <- unique(as.integer(data$id_batch)) 
my_list <- sort(my_list)
results <- list() 

for (i in 1:length(my_list)) {
  batch_data <-  filter(data, id_batch == my_list[i])
  print(paste0('Feching data for batch ', i, ' out of a total of ', length(my_list),''))
  batch_data$prod_date <- as.Date(batch_data$prod_date)
  referenceTime <- paste0('',min(batch_data$prod_date),'/',max(batch_data$prod_date),'')
  sources <- paste(unique(batch_data$id_station), collapse = ", ")
  elements <- 'mean(relative_humidity P1D)' #<- This is the only line of code we change, 
  #I recommend collecting one weather 'element' at the time. 
  #If you have more than one element, the API-call will fail if the weather station lacks any of the elements. 
  url <- paste0(
    endpoint, "?",
    "sources=", sources,
    "&referencetime=", referenceTime,
    "&elements=", elements 
  )
  url
  xs <- suppressWarnings(try(fromJSON(URLencode(url),flatten=T), silent = F))
  
  if (class(xs) != 'try-error') {
    df <- unnest(xs$data, cols = c('observations'))
    df <- df[, c('sourceId', 'referenceTime', 'elementId', 'value', "timeOffset", "qualityCode")]
    df$id_batch <- my_list[i]
    results <- append(results, df)
    print("Data retrieved from frost.met.no!")
  } else {
    print(paste0('Error: station IDs ', sources, ' did not retrieved humidity data in interval ', referenceTime,''))
  }
}

matrix_data <- matrix(results, ncol = 7, byrow = T)
humidity_data <- data.frame(cbind(unlist(matrix_data[,1]), 
                                     unlist(matrix_data[,2]), 
                                     unlist(matrix_data[,3]),
                                     unlist(matrix_data[,4]),
                                     unlist(matrix_data[,5]),
                                     unlist(matrix_data[,6]),
                                     unlist(matrix_data[,7])))

colnames(humidity_data) <- c('id_station', 'date', 'element', 'value', "timeOffset", "qualityCode", 'id_batch')
humidity_data$date <- as.Date(humidity_data$date, origin = "1970-01-01")
#Looks ok?
head(humidity_data)
#Saving the results
save(humidity_data, file = "Humidity_update.Rdata")

load("Humidity.Rdata")



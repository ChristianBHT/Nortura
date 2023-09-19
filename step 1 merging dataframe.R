install.packages("usethis")
usethis::use_git_config(user.name="Christian Thorjussen", user.email="christianbern@gmail.com")
usethis::create_github_token()
gitcreds::gitcreds_set()



#This script builds the data frame for Ascites modeling
library(lubridate)
library(dplyr)
library(naniar)
setwd("C:/Users/christian.thorjussen/Project Nortura/")
load("raw data/Produksjonsdata.Rdata")
load('Temperature.Rdata')
load("Humidity.Rdata")
load("raw data/DaggamleKyllinger.Rdata")
table(DaggamleKyllinger$Hybrid)
day_data <- data.frame(Produksjonsdata$PK_Produksjonsdata_Fak,
                   Produksjonsdata$FK_Innsett_Dim,
                   Produksjonsdata$FK_Forfirma_Dim,
                   Produksjonsdata$FK_Slakteri_Dim,
                   Produksjonsdata$FK_Rugeri_Dim,
                   Produksjonsdata$FK_TypeProduksjon_Dim,
                   Produksjonsdata$InnsatteKyllinger,
                   ymd(Produksjonsdata$Dato), 
                   Produksjonsdata$Alder, 
                   Produksjonsdata$VektDyr,
                   Produksjonsdata$AntallSelvdoede, 
                   Produksjonsdata$AvlivedeBen, 
                   Produksjonsdata$AvlivedeMisvekst, 
                   Produksjonsdata$AvlivedeSkaderHakking, 
                   Produksjonsdata$AvlivedeAndre, 
                   Produksjonsdata$Doede,
                   Produksjonsdata$Vannforbruk, 
                   Produksjonsdata$Forforbruk,
                   Produksjonsdata$TempMin, 
                   Produksjonsdata$TempMaks, 
                   Produksjonsdata$LuftfuktighetMin, 
                   Produksjonsdata$LuftfuktighetMaks, 
                   Produksjonsdata$Lysstyrke, 
                   Produksjonsdata$Lystimer,
                   Produksjonsdata$Vektvariasjon,
                   Produksjonsdata$DagligTilvekst,
                   Produksjonsdata$VannDeltPaaFor)  

colnames(day_data) <- c('id_farmday', 
                        'id_batch', 
                        'id_feedfirm', 
                        'id_slaughterhouse', 
                        'id_hatchery', 
                        'prod_type', 
                        'N_of_chicken',
                        'date',
                        'age',
                        'weight',
                        'dead_self',
                        'killed_legs',
                        'killed_stunted',
                        'killed_pecking',
                        'killed_other',
                        'total_dead',
                        'water_consump',
                        'feed_consump',
                        'temp_min',
                        'temp_max',
                        'humidity_min',
                        'humidity_max',
                        'light_strength',
                        'light_hours',
                        'weight_var',
                        'day_growth',
                        'water_by_food')


temperature_data$date <- as.character(temperature_data$date)

#Find duplications and select those with best quality (temperature)
temp_data_selected <- temperature_data %>%
  group_by(id_batch, date) %>% # Group by id_batch and date
  slice_min(qualityCode) # Filter to keep only the best quality

dups <- duplicated(temperature_data[, c("id_batch", "date")]) | 
        duplicated(temperature_data[, c("id_batch", "date")], fromLast = TRUE)
non_dups <- !dups
uniqe_temp_data <- temperature_data[non_dups, ]
temp_df <- data.frame(rbind(temp_data_selected, uniqe_temp_data))
temp_df$id_batch <- as.numeric(temp_df$id_batch)
temp_df$date <- as.Date(temp_df$date)
temp_df$temperature <- as.numeric(temp_df$temperature)
temp_df <- subset(temp_df, select = c('id_batch', 'date', 'temperature'))
colnames(temp_df) <-  c('id_batch', 'date', 'out_temp')

#Find duplications and select those with best quality (humiditiy)
humi_data_selected <- humidity_data %>%
  group_by(id_batch, date) %>% # Group by id_batch and date
  slice_min(qualityCode) # Filter to keep only the best quality

dups <- duplicated(humidity_data[, c("id_batch", "date")]) | 
        duplicated(humidity_data[, c("id_batch", "date")], fromLast = TRUE)

non_dups <- !dups
uniqe_humi_data <- humidity_data[non_dups, ]

humi_df <- data.frame(rbind(humi_data_selected, uniqe_humi_data))
humi_df$id_batch <- as.numeric(humi_df$id_batch)
humi_df$date <- as.Date(humi_df$date)
humi_df$value <- as.numeric(humi_df$value)
humi_df <- subset(humi_df, select = c('id_batch', 'date', 'value'))
colnames(humi_df) <-  c('id_batch', 'date', 'out_humidity')
#I am using merge in such a way that we are only keeping observations with observations in both data set Y and X!
analysis_df <- merge(day_data, temp_df, by = c('id_batch', 'date'))
analysis_df <- merge(analysis_df, humi_df, by = c('id_batch', 'date'))

rm(dups, non_dups, 
   day_data, 
   temp_df, 
   Produksjonsdata, 
   temperature_data, 
   uniqe_temp_data, 
   temp_data_selected, 
   humi_data_selected, 
   humi_df,
   humidity_data)

load("raw data/Innsett.Rdata")
batch_df <- subset(Innsett, select = c('PK_Innsett_Dim', 'Areal',  'Aceties', 'FK_TypeProduksjon_Dim', 'ForforbrukTotalt', 'LeverandoerNr'))
colnames(batch_df) <- c('id_batch', 'area', 'aceties', 'type_of_prod', 'total_food_used', 'LeverandoerNr')
analysis_df <- merge(analysis_df, batch_df, by = 'id_batch')

feed_data <- read.csv(file = "vekstfor_data.csv", sep = ';')
feed_data <- subset(feed_data, select = c('innsett_id', 'vekstfortype1'))
colnames(feed_data) <- c('id_batch', 'growth_feed')
analysis_df <- merge(analysis_df, feed_data, by = 'id_batch')
analysis_df$age <- as.numeric(analysis_df$age)
# Remove duplicated rows
analysis_df <- analysis_df[!duplicated(analysis_df), ]


hybrid <- subset(DaggamleKyllinger, select = c("FK_Innsett_Dim", "Hybrid"))
colnames(hybrid) <- c("id_batch", "hybrid")

analysis_df <- merge(analysis_df, hybrid, by = 'id_batch')

save(analysis_df,file="analysis_df.Rda")


# All data managment in one file
#This script builds the data frame for Ascites modeling
library(dplyr)
library(lubridate)
library(naniar)
library(tidyverse)
library(expss)
library(dagitty)
library(ggplot2)
library(reshape2)
library(janitor)
library(lavaan)
library(gridExtra)
library(tidyr)
library(broom)
rm(list = ls())
setwd("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/")
load("Produksjonsdata.Rdata")
load('Temperature.Rdata')
load("Humidity.Rdata")
load("DaggamleKyllinger.Rdata")
# table(DaggamleKyllinger$Hybrid)
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

df_filtered <- df[df$Date >= as.Date("2022-10-01"), ]
temperature_data$date <- as.character(temperature_data$date)

temperature_data <- subset(temperature_data, timeOffset == 'PT0H')

#Find duplications and select those with best quality (temperature)
temp_data_selected <- temperature_data %>%
  group_by(id_batch, date) %>% # Group by id_batch and date
  slice_min(qualityCode) # Filter to keep only the best quality
uniqe_temp_data <- distinct(temp_data_selected, id_batch, date, .keep_all = TRUE) #Removing duplicated entries  (if equal quality)
temp_df <- uniqe_temp_data
temp_df$id_batch <- as.numeric(temp_df$id_batch)
temp_df$date <- as.Date(temp_df$date)
temp_df$temperature <- as.numeric(temp_df$temperature)
temp_df <- subset(temp_df, select = c('id_batch', 'date', 'temperature'))
colnames(temp_df) <-  c('id_batch', 'date', 'out_temp')

analysis_df <- merge(day_data, temp_df, by = c('id_batch', 'date'))

load("Innsett.Rdata")
batch_df <- subset(Innsett, select = c('PK_Innsett_Dim', 'Areal',  'Aceties', 'FK_TypeProduksjon_Dim', 'ForforbrukTotalt', 'LeverandoerNr'))
colnames(batch_df) <- c('id_batch', 'area', 'aceties', 'type_of_prod', 'total_food_used', 'LeverandoerNr')
analysis_df <- merge(analysis_df, batch_df, by = 'id_batch')

load("Handling.Rdata")
feed_data <- subset(Handling, select = c('FK_Innsett_Dim', 'FK_Fortype_Dim', 'FK_Forblanding_Dim', 'Mengde'))
colnames(feed_data) <- c('id_batch', 'feed_type', 'feed_mix', 'mengde')
load('Forblanding.Rdata')
forblanding <- subset(Forblanding, select = c('PK_Forblanding_Dim', 'Forblanding'))
colnames(forblanding) <- c('feed_mix', 'feed_name')
feed_data <- merge(feed_data, forblanding, by = 'feed_mix')
# Manual fixing name in Notepad ++
# write.csv(feed_data, file = 'feed_data.csv')
feed_data <- read.csv(file = 'feed_data.csv')
feed_data <- subset(feed_data, select = -X)
feed_data <- subset(feed_data, feed_type == 2)

result <- feed_data %>%
  group_by(id_batch, feed_mix, feed_name) %>%
  summarise(total_amount = sum(mengde))

unique_groups <- result %>%
  group_by(id_batch) %>%
  filter(n() == 1)

feed <- unique_groups

analysis_df <- merge(analysis_df, feed, by = 'id_batch')

analysis_df$age <- as.numeric(analysis_df$age)

# Remove duplicated rows
analysis_df <- analysis_df[!duplicated(analysis_df), ]


hybrid <- subset(DaggamleKyllinger, select = c("FK_Innsett_Dim", "Hybrid"))
colnames(hybrid) <- c("id_batch", "hybrid")

analysis_df <- merge(analysis_df, hybrid, by = 'id_batch')
max(analysis_df$date)
# 2022-12-01
save(analysis_df,file="analysis_df.Rda")

#--------------step one complete now to more cleaning tasks----------------------
rm(list = ls())
setwd("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/")

load("analysis_df.Rda")
df <- analysis_df
data <- df[order(df$id_batch, df$age),]
table(data$prod_type)
data$chicken_type <-  ifelse(data$prod_type == 4, "Processed", 
                             ifelse(data$prod_type == 6, "Grill", 
                                    ifelse(data$prod_type == 8, "Land",
                                           ifelse(data$prod_type == 12, "Organic",
                                                  ifelse(data$prod_type == 14, "Liveche",
                                                         ifelse(data$prod_type == 17, "McDonalds",
                                                                ifelse(data$prod_type == 21, "Hubbard",
                                                                       ifelse(data$prod_type == 22, "Kyllinggaarden",
                                                                              ifelse(data$prod_type == 29, "Unknown", NA)))))))))
table(data$chicken_type)                            

#####################################
###Cleaning data for Grill chicken###
#####################################
data$weight[data$weight < 1000 & data$age > 30 & data$chicken_type == "Grill"] <- NA
data$weight[data$weight < 1000 & data$age > 30 & data$chicken_type == "Processed"] <- NA
data$weight[data$weight < 750 & data$age > 25 & data$chicken_type == "McDonalds"] <- NA
data$weight[data$weight < 100 & data$age > 19 & data$chicken_type == "Kyllinggaarden"] <- NA

grill_data <- subset(data, chicken_type == "Grill")
grill_data <- subset(grill_data, age <= 35)
grill_data$weight[grill_data$weight >= 100 & grill_data$age == 0] <- NA
grill_data$weight[grill_data$weight <= 34 & grill_data$age == 0] <- NA

for (i in 0:35){
  grill_data$weight[grill_data$weight == 0 & grill_data$age == i]  <- NA
  mean <- mean(grill_data$weight[grill_data$age == i], na.rm = T)
  outlier <- 4*sd(grill_data$weight[grill_data$age == i], na.rm = T)
  grill_data$weight[(grill_data$weight >= (mean + outlier) | grill_data$weight <= (mean - outlier) ) & grill_data$age == i]  <- NA
}

##########################################
###Cleaning data for Processes chicken###
##########################################
proces_data <- subset(data, chicken_type == "Processed")
proces_data <- subset(proces_data, age <= 35) 
proces_data$weight[proces_data$weight >= 100 & proces_data$age == 0] <- NA
proces_data$weight[proces_data$weight <= 25 & proces_data$age == 0] <- NA

for (i in 0:35){
  proces_data$weight[proces_data$weight == 0 & proces_data$age == i]  <- NA
  mean <- mean(proces_data$weight[proces_data$age == i], na.rm = T)
  outlier <- 4*sd(proces_data$weight[proces_data$age == i], na.rm = T)
  proces_data$weight[(proces_data$weight >= (mean + outlier) | proces_data$weight <= (mean - outlier) ) & proces_data$age == i]  <- NA
}

##########################################
###Cleaning data for land chicken###
##########################################
land_data <- subset(data, chicken_type == "Land")
land_data <- subset(land_data, age <= 58)
land_data$weight[land_data$weight >= 100 & land_data$age == 0] <- NA
land_data$weight[land_data$weight <= 25 & land_data$age == 0] <- NA

for (i in 0:58){
  land_data$weight[land_data$weight == 0 & land_data$age == i]  <- NA
  mean <- mean(land_data$weight[land_data$age == i], na.rm = T)
  outlier <- 4*sd(land_data$weight[land_data$age == i], na.rm = T)
  land_data$weight[(land_data$weight >= (mean + outlier) | land_data$weight <= (mean - outlier) ) & land_data$age == i]  <- NA
}

########################################
###Cleaning data for organic chicken###
########################################
organic_data <- subset(data, chicken_type == "Organic")
organic_data <- subset(organic_data, age <= 77)
organic_data$weight[organic_data$weight >= 100 & organic_data$age == 0] <- NA
organic_data$weight[organic_data$weight <= 25 & organic_data$age == 0] <- NA

for (i in 0:77){
  organic_data$weight[organic_data$weight == 0 & organic_data$age == i]  <- NA
  mean <- mean(organic_data$weight[organic_data$age == i], na.rm = T)
  outlier <- 4*sd(organic_data$weight[organic_data$age == i], na.rm = T)
  organic_data$weight[(organic_data$weight >= (mean + outlier) | organic_data$weight <= (mean - outlier) ) & organic_data$age == i]  <- NA
}


########################################
###Cleaning data for liveche chicken###
########################################
liveche_data <- subset(data, chicken_type == "Liveche")
liveche_data <- subset(liveche_data, age <= 58)
liveche_data$weight[liveche_data$weight >= 100 & liveche_data$age == 0] <- NA
liveche_data$weight[liveche_data$weight <= 25 & liveche_data$age == 0] <- NA

for (i in 0:58){
  liveche_data$weight[liveche_data$weight == 0 & liveche_data$age == i]  <- NA
  mean <- mean(liveche_data$weight[liveche_data$age == i], na.rm = T)
  outlier <- 4*sd(liveche_data$weight[liveche_data$age == i], na.rm = T)
  liveche_data$weight[(liveche_data$weight >= (mean + outlier) | liveche_data$weight <= (mean - outlier) ) & liveche_data$age == i]  <- NA
}

#########################################
###Cleaning data for McDonalds chicken###
#########################################
mcdonald_data <- subset(data, chicken_type == "McDonalds")
mcdonald_data$weight[mcdonald_data$weight >= 100 & mcdonald_data$age == 0] <- NA
mcdonald_data$weight[mcdonald_data$weight <= 25 & mcdonald_data$age == 0] <- NA

for (i in 0:34){
  mcdonald_data$weight[mcdonald_data$weight == 0 & mcdonald_data$age == i]  <- NA
  mean <- mean(mcdonald_data$weight[mcdonald_data$age == i], na.rm = T)
  outlier <- 4*sd(mcdonald_data$weight[mcdonald_data$age == i], na.rm = T)
  mcdonald_data$weight[(mcdonald_data$weight >= (mean + outlier) | mcdonald_data$weight <= (mean - outlier) ) & mcdonald_data$age == i]  <- NA
}

#######################################
###Cleaning data for Hubbard chicken###
#######################################
hubbard_data <- subset(data, chicken_type == "Hubbard")
hubbard_data <- subset(hubbard_data, age <= 47)
hubbard_data$weight[hubbard_data$weight >= 100 & hubbard_data$age == 0] <- NA
hubbard_data$weight[hubbard_data$weight <= 25 & hubbard_data$age == 0] <- NA

for (i in 0:47){
  hubbard_data$weight[hubbard_data$weight == 0 & hubbard_data$age == i]  <- NA
  mean <- mean(hubbard_data$weight[hubbard_data$age == i], na.rm = T)
  outlier <- 4*sd(hubbard_data$weight[hubbard_data$age == i], na.rm = T)
  hubbard_data$weight[(hubbard_data$weight >= (mean + outlier) | hubbard_data$weight <= (mean - outlier) ) & hubbard_data$age == i]  <- NA
}

unknown_data <- subset(data, chicken_type == "Unknown")
unknown_data <- subset(unknown_data, age <= 33)
unknown_data$weight[unknown_data$weight >= 100 & unknown_data$age == 0] <- NA
unknown_data$weight[unknown_data$weight <= 25 & unknown_data$age == 0] <- NA

for (i in 0:33){
  unknown_data$weight[unknown_data$weight == 0 & unknown_data$age == i]  <- NA
  mean <- mean(unknown_data$weight[unknown_data$age == i], na.rm = T)
  outlier <- 4*sd(unknown_data$weight[unknown_data$age == i], na.rm = T)
  unknown_data$weight[(unknown_data$weight >= (mean + outlier) | unknown_data$weight <= (mean - outlier) ) & unknown_data$age == i]  <- NA
}


##############################################
###Cleaning data for Kyllinggaarden chicken###
##############################################
kyllinggaarden_data <- subset(data, chicken_type == "Kyllinggaarden")

kyllinggaarden_data$weight[kyllinggaarden_data$weight >= 100 & kyllinggaarden_data$age == 0] <- NA
kyllinggaarden_data$weight[kyllinggaarden_data$weight <= 25 & kyllinggaarden_data$age == 0] <- NA

for (i in 0:34){
  kyllinggaarden_data$weight[kyllinggaarden_data$weight == 0 & kyllinggaarden_data$age == i]  <- NA
  mean <- mean(kyllinggaarden_data$weight[kyllinggaarden_data$age == i], na.rm = T)
  outlier <- 4*sd(kyllinggaarden_data$weight[kyllinggaarden_data$age == i], na.rm = T)
  kyllinggaarden_data$weight[(kyllinggaarden_data$weight >= (mean + outlier) | kyllinggaarden_data$weight <= (mean - outlier) ) & kyllinggaarden_data$age == i]  <- NA
}

#Data cleaning line by line
data2 <- rbind(grill_data, proces_data, land_data, organic_data, liveche_data, mcdonald_data, hubbard_data, kyllinggaarden_data, unknown_data)

data2$temp_max[data2$temp_max == -99.0] <- NA
data2$temp_max[data2$temp_max == 99.0] <- NA
data2$temp_max[data2$temp_min == -99.0] <- NA
data2$temp_max[data2$temp_min == 99.0] <- NA
data2$temp_min[data2$temp_min >= 80] <- NA
data2$temp_max[data2$temp_max >= 80] <- NA
data2$humidity_min[data2$humidity_min > 100] <- NA
data2$humidity_max[data2$humidity_max > 100] <- NA
data2$humidity_min[data2$humidity_min < 1] <- NA
data2$humidity_max[data2$humidity_max < 1] <- NA


library(janitor)
data2 <- data2 %>% 
  clean_names()

save(data2,file="analysis_df_cleaned.Rda")

#-------------------Finish part 2, now on to part 3---------------------

setwd("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/")
rm(list = ls())
load(file="analysis_df_cleaned.Rda") #142182 observations
data <- data2
#This subscript estimates a latent growth model, where the aim is to use the latent variables as measures of growth rates and variation of growth
#Create data set with id age and weight of chicken
long_df <-  subset(data, select = c(id_batch, age, weight))
long_data <- data
#Create data set with chicken types
type <- subset(data, select = c(id_batch, chicken_type))
#Reshape data
wide_df <- reshape(long_df, idvar = "id_batch", timevar = "age", direction = "wide")
#Remove duplicates from type data
type <- distinct(type)
#Merge the two dataset to have a wide data with a chicken type as grouping variable
data <- merge(wide_df, type, by = "id_batch")
data <- data %>%
  clean_names()
table(data$chicken_type)

# Create a list of variable names
variable_names <- paste0("weight_", 1:77)

# Loop through each variable and replace 1 with NA
data[, variable_names] <- lapply(data[, variable_names], function(x) ifelse(x == 1, NA, x))


#We start by modeling for the Foredlingskylling (Processed), the model conceptual the same for all chicken types, the only thing we change is 
#how many days we use for modelling.
data_proc <- data[data$chicken_type == "Processed",]

data_proc <- select(data_proc, c('id_batch', "weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                                 "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                                 "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                                 "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27"))

#Removing rows with missing values, i.e. we are only modeling on those with a complete set of weight observations
data_proc <- na.omit(data_proc)
#Specifying a growth model
model <- ' intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27 
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26  
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27'

fit <- growth(model, data=data_proc)
summary(fit)
# Predict the latent variables using the fitted model and the observed data
parameters <- predict(fit, data_proc)
data_proc <- cbind(data_proc, parameters)
data_proc$id <- seq(nrow(data_proc))
growth_data <- data_proc %>% select(id_batch, id, intercept, growth, sqr_growth)

#Reshape to long data for plotting
long_df <- reshape(data_proc,
                   idvar = "id",
                   timevar = "day",
                   varying = c("weight_0","weight_1", "weight_2", "weight_3", "weight_4","weight_5", "weight_6", "weight_7","weight_8",
                               "weight_9", "weight_10","weight_11", "weight_12", "weight_13", "weight_14","weight_15", "weight_16",
                               "weight_17", "weight_18","weight_19", "weight_20", "weight_21", "weight_22","weight_23", "weight_24",
                               "weight_25", "weight_26","weight_27"),
                   v.names = c("weight"),
                   direction = "long")
long_df <- select(long_df, c('id', 'id_batch', 'day', 'weight'))

plot_data <- merge(long_df, growth_data, by = "id")
plot_data <- select(plot_data, c("id", "id_batch.y", "day", "weight", "intercept", "growth", "sqr_growth"))
plot_data$age <- plot_data$day-1
plot_data$estimate_weight <- plot_data$intercept + plot_data$growth*plot_data$age + plot_data$sqr_growth*plot_data$age^2
data_proc_r2 <- plot_data %>% group_by(id_batch.y) %>% summarize(correlation = cor(weight, estimate_weight))
data_proc_r2$r2 <- data_proc_r2$correlation^2
head(data_proc_r2)
colnames(data_proc_r2) <- c('id_batch', 'cor', 'r2')  
data_proc <- merge(data_proc, data_proc_r2, by = 'id_batch')

#plot 
i <- sample(plot_data$id_batch.y, 1)
obs_df <- filter(plot_data, id_batch.y == i)
obs_df <- obs_df[order(obs_df$day), ]
obs_df$age <- obs_df$day - 1 
rows <- nrow(obs_df)
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2

#plot 1 with ggplot2
rows <- nrow(obs_df)
# Compute x and y values 
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2

p1 <- ggplot(data = obs_df, aes(x = age, y = weight)) + 
  geom_line(lwd = 1, color = "black", linetype = "dashed") +
  geom_point(shape = 21, fill = "white", size = 3) +
  ylim(0, 2000) +
  xlab("Age") + 
  ylab("Weight")

p1 <- p1 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ", i, ". Foredlingskylling"))
p1


#Then on to grill chicken
data_grill <- data[data$chicken_type == "Grill",]
#28 days looks like a good cut off 
data_grill <- select(data_grill, c('id_batch', "weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                                   "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                                   "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                                   "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27"))
data_grill <- na.omit(data_grill)
#Specifying a growth model
model <- ' intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27  
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27  
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27' 

fit <- growth(model, data=data_grill)
summary(fit)

# Predict the latent variables using the fitted model and the observed data
parameters <- predict(fit, data_grill)
data_grill <- cbind(data_grill, parameters)
data_grill$id <- seq(nrow(data_grill))
growth_data <- data_grill %>% select(id_batch, id, intercept, growth, sqr_growth)

#Reshape to long data for plotting
long_df <- reshape(data_grill,
                   idvar = "id",
                   timevar = "day",
                   varying = c("weight_0","weight_1", "weight_2", "weight_3", "weight_4","weight_5", "weight_6", "weight_7","weight_8",
                               "weight_9", "weight_10","weight_11", "weight_12", "weight_13", "weight_14","weight_15", "weight_16",
                               "weight_17", "weight_18","weight_19", "weight_20", "weight_21", "weight_22","weight_23", "weight_24",
                               "weight_25", "weight_26","weight_27"),
                   v.names = c("weight"),
                   direction = "long")

long_df <- select(long_df, c('id', 'id_batch', 'day', 'weight'))
plot_data <- merge(long_df, growth_data, by = "id")
plot_data <- select(plot_data, c("id", "id_batch.y", "day", "weight", "intercept", "growth", "sqr_growth"))

plot_data$age <- plot_data$day-1
plot_data$estimate_weight <- plot_data$intercept + plot_data$growth*plot_data$age + plot_data$sqr_growth*plot_data$age^2
data_grill_r2 <- plot_data %>% group_by(id_batch.y) %>% summarize(correlation = cor(weight, estimate_weight))
data_grill_r2$r2 <- data_grill_r2$correlation^2
head(data_grill_r2)
colnames(data_grill_r2) <- c('id_batch', 'cor', 'r2')  
data_grill <- merge(data_grill, data_grill_r2, by = 'id_batch')

#plot 
i <- sample(plot_data$id_batch.y, 1)
obs_df <- filter(plot_data, id_batch.y == i)
obs_df <- obs_df[order(obs_df$day), ]
obs_df$age <- obs_df$day - 1 
rows <- nrow(obs_df)
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2

# #plot 1 
p2 <- ggplot(data = obs_df, aes(x = age, y = weight)) + 
  geom_line(lwd = 1, color = "black", linetype = "dashed") +
  geom_point(shape = 21, fill = "white", size = 3) +
  ylim(0, 2000) +
  xlab("Age") + 
  ylab("Weight")
# 
p2 <- p2 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ", i, ". Grillkylling"))
p2

##############################################
####Estimating growth from Kyllinggaarden####
#############################################

data_gaarden <- data[data$chicken_type == "Kyllinggaarden",]
data_gaarden <- select(data_gaarden, c('id_batch', "weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                                       "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                                       "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                                       "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27"))
data_gaarden <- na.omit(data_gaarden)
#Specifying a growth model
model <- 'intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27  
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27  
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27' 

fit <- growth(model, data=data_gaarden)
summary(fit)
# Predict the latent variables using the fitted model and the observed data
parameters <- predict(fit, data_gaarden)
data_gaarden <- cbind(data_gaarden, parameters)
head(data_gaarden)
data_gaarden$id <- seq(nrow(data_gaarden))
growth_data <- data_gaarden %>% select(id_batch, id, intercept, growth, sqr_growth)

#Reshape to long data for plotting
long_df <- reshape(data_gaarden,
                   idvar = "id",
                   timevar = "day",
                   varying = c("weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                               "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                               "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                               "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27"),
                   v.names = c("weight"),
                   direction = "long")
long_df <- select(long_df, c('id', 'id_batch', 'day', 'weight'))
plot_data <- merge(long_df, growth_data, by = "id")
plot_data <- select(plot_data, c("id", "id_batch.y", "day", "weight", "intercept", "growth", "sqr_growth"))
plot_data$age <- plot_data$day-1
plot_data$estimate_weight <- plot_data$intercept + plot_data$growth*plot_data$age + plot_data$sqr_growth*plot_data$age^2
data_gaarden_r2 <- plot_data %>% group_by(id_batch.y) %>% summarize(correlation = cor(weight, estimate_weight))
data_gaarden_r2$r2 <- data_gaarden_r2$correlation^2
head(data_gaarden_r2)
colnames(data_gaarden_r2) <- c('id_batch', 'cor', 'r2')  
data_gaarden <- merge(data_gaarden, data_gaarden_r2, by = 'id_batch')

i <- sample(plot_data$id_batch.y, 1)
obs_df <- filter(plot_data, id_batch.y == i)
obs_df <- obs_df[order(obs_df$day), ]
obs_df$age <- obs_df$day - 1 
rows <- nrow(obs_df)
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2
# 
p3 <- ggplot(data = obs_df, aes(x = age, y = weight)) + 
  geom_line(lwd = 1, color = "black", linetype = "dashed") +
  geom_point(shape = 21, fill = "white", size = 3) +
  ylim(0, 2000) +
  xlab("Age") + 
  ylab("Weight") 
# 
p3 <- p3 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ", i, ". Kyllinggården"))
p3

#Hubbard Chicken
data_hubbard <- data[data$chicken_type == "Hubbard",]
data_hubbard <- select(data_hubbard, c('id_batch', "weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                                       "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                                       "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                                       "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                                       "weight_28", "weight_29", "weight_30", "weight_31", "weight_32", "weight_33", "weight_34",
                                       "weight_35"))
data_hubbard <- na.omit(data_hubbard)
#Specifying a growth model
model <- 'intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27 + 1*weight_28 + 1*weight_29 + 1*weight_30 + 1*weight_31 + 1*weight_32 + 1*weight_33 + 1*weight_34 + 1*weight_35   
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27 + 28*weight_28 + 29*weight_29  + 30*weight_30  + 31*weight_31 + 32*weight_32 + 33*weight_33 + 34*weight_34 + 35*weight_35 
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27 + 784*weight_28 + + 841*weight_29  + 900*weight_30  + 961*weight_31  + 1024*weight_32 + 1089*weight_33 + 1156*weight_34 + 1225*weight_35' 

fit <- growth(model, data=data_hubbard)
summary(fit)
# Predict the latent variables using the fitted model and the observed data
parameters <- predict(fit, data_hubbard)
data_hubbard <- cbind(data_hubbard, parameters)

data_hubbard$id <- seq(nrow(data_hubbard))
growth_data <- data_hubbard %>% select(id_batch, id, intercept, growth, sqr_growth)

#Reshape to long data for plotting
long_df <- reshape(data_hubbard,
                   idvar = "id",
                   timevar = "day",
                   varying = c("weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                               "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                               "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                               "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                               "weight_28", "weight_29", "weight_30", "weight_31", "weight_32", "weight_33", "weight_34",
                               "weight_35"),
                   v.names = c("weight"),
                   direction = "long")
long_df <- select(long_df, c('id', 'id_batch', 'day', 'weight'))
plot_data <- merge(long_df, growth_data, by = "id")
plot_data <- select(plot_data, c("id", "id_batch.y", "day", "weight", "intercept", "growth", "sqr_growth"))

plot_data$age <- plot_data$day-1
plot_data$estimate_weight <- plot_data$intercept + plot_data$growth*plot_data$age + plot_data$sqr_growth*plot_data$age^2
data_hubbard_r2 <- plot_data %>% group_by(id_batch.y) %>% summarize(correlation = cor(weight, estimate_weight))
data_hubbard_r2$r2 <- data_hubbard_r2$correlation^2

colnames(data_hubbard_r2) <- c('id_batch', 'cor', 'r2')  
data_hubbard <- merge(data_hubbard, data_hubbard_r2, by = 'id_batch')
head(data_hubbard)

i <- sample(plot_data$id_batch.y, 1)
obs_df <- filter(plot_data, id_batch.y == i)
obs_df <- obs_df[order(obs_df$day), ]
obs_df$age <- obs_df$day - 1 
rows <- nrow(obs_df)
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2
# 
p4 <- ggplot(data = obs_df, aes(x = age, y = weight)) + 
  geom_line(lwd = 1, color = "black", linetype = "dashed") +
  geom_point(shape = 21, fill = "white", size = 3) +
  ylim(0, 2000) +
  xlab("Age") + 
  ylab("Weight") 
# 
p4 <- p4 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ", i, ". Hubbard"))
p4
# 

#McDonald's Chicken 
data_mcdon <- data[data$chicken_type == "McDonalds",]
data_mcdon <- select(data_mcdon, c('id_batch', "weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                                   "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                                   "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                                   "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                                   "weight_28", "weight_29", "weight_30"))
data_mcdon <- na.omit(data_mcdon)
#Specifying a growth model
model <- 'intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27 + 1*weight_28 + 1*weight_29 + 1*weight_30   
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27 + 28*weight_28 + 29*weight_29  + 30*weight_30   
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27 + 784*weight_28 + 841*weight_29  + 900*weight_30' 

fit <- growth(model, data=data_mcdon)
summary(fit)

# Predict the latent variables using the fitted model and the observed data
parameters <- predict(fit, data_mcdon)
data_mcdon <- cbind(data_mcdon, parameters)

data_mcdon$id <- seq(nrow(data_mcdon))
growth_data <- data_mcdon %>% select(id_batch, id, intercept, growth, sqr_growth)

#Reshape to long data for plotting
long_df <- reshape(data_mcdon,
                   idvar = "id",
                   timevar = "day",
                   varying = c("weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                               "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                               "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                               "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                               "weight_28", "weight_29", "weight_30"),
                   v.names = c("weight"),
                   direction = "long")
long_df <- select(long_df, c('id', 'id_batch', 'day', 'weight'))
plot_data <- merge(long_df, growth_data, by = "id")
plot_data <- select(plot_data, c("id", "id_batch.y", "day", "weight", "intercept", "growth", "sqr_growth"))

plot_data$age <- plot_data$day-1
plot_data$estimate_weight <- plot_data$intercept + plot_data$growth*plot_data$age + plot_data$sqr_growth*plot_data$age^2
data_mcdon_r2 <- plot_data %>% group_by(id_batch.y) %>% summarize(correlation = cor(weight, estimate_weight))
data_mcdon_r2$r2 <- data_mcdon_r2$correlation^2
head(data_mcdon_r2)
colnames(data_mcdon_r2) <- c('id_batch', 'cor', 'r2')  
data_mcdon <- merge(data_mcdon, data_mcdon_r2, by = 'id_batch')
head(data_mcdon)

i <- sample(plot_data$id_batch.y, 1)
obs_df <- filter(plot_data, id_batch.y == i)
obs_df <- obs_df[order(obs_df$day), ]
obs_df$age <- obs_df$day - 1 
rows <- nrow(obs_df)
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2

p5 <- ggplot(data = obs_df, aes(x = age, y = weight)) + 
  geom_line(lwd = 1, color = "black", linetype = "dashed") +
  geom_point(shape = 21, fill = "white", size = 3) +
  ylim(0, 2000) +
  xlab("Age") + 
  ylab("Weight") 
# 
p5 <- p5 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ", i, ". McDonalds-kylling"))
p5

#Unknown Chicken 
data_u <- data[data$chicken_type == "Unknown",]
data_u <- select(data_u, c('id_batch', "weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                           "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                           "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                           "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27"))
data_u <- na.omit(data_u)
#Specifying a growth model
model <- 'intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27     
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27      
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27' 

fit <- growth(model, data=data_u)
summary(fit)

# Predict the latent variables using the fitted model and the observed data
parameters <- predict(fit, data_u)
data_u <- cbind(data_u, parameters)

data_u$id <- seq(nrow(data_u))
growth_data <- data_u %>% select(id_batch, id, intercept, growth, sqr_growth)

#Reshape to long data for plotting
long_df <- reshape(data_u,
                   idvar = "id",
                   timevar = "day",
                   varying = c("weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                               "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                               "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                               "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27"),
                   v.names = c("weight"),
                   direction = "long")
long_df <- select(long_df, c('id', 'id_batch', 'day', 'weight'))
plot_data <- merge(long_df, growth_data, by = "id")
plot_data <- select(plot_data, c("id", "id_batch.y", "day", "weight", "intercept", "growth", "sqr_growth"))

plot_data$age <- plot_data$day-1
plot_data$estimate_weight <- plot_data$intercept + plot_data$growth*plot_data$age + plot_data$sqr_growth*plot_data$age^2
data_u_r2 <- plot_data %>% group_by(id_batch.y) %>% summarize(correlation = cor(weight, estimate_weight))
data_u_r2$r2 <- data_u_r2$correlation^2
head(data_u_r2)
colnames(data_u_r2) <- c('id_batch', 'cor', 'r2')  
data_u <- merge(data_u, data_u_r2, by = 'id_batch')
head(data_u)

i <- sample(plot_data$id_batch.y, 1)
obs_df <- filter(plot_data, id_batch.y == i)
obs_df <- obs_df[order(obs_df$day), ]
obs_df$age <- obs_df$day - 1 
rows <- nrow(obs_df)
obs_df$x <- seq(0, rows, length.out = rows)
obs_df$y <- obs_df$intercept[1] + obs_df$growth[1]*obs_df$x + obs_df$sqr_growth[1]*obs_df$x^2

p6 <- ggplot(data = obs_df, aes(x = age, y = weight)) + 
  geom_line(lwd = 1, color = "black", linetype = "dashed") +
  geom_point(shape = 21, fill = "white", size = 3) +
  ylim(0, 2000) +
  xlab("Age") + 
  ylab("Weight") 
# 
p6 <- p6 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ", i, ". McDonalds-kylling"))
p6


growth_data <- rbind(select(data_proc, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')), 
                     select(data_grill, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')), 
                     select(data_gaarden, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')), 
                     select(data_mcdon, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')),
                     select(data_hubbard, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')),
                     select(data_u, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')))

data <- merge(long_data, growth_data, by = "id_batch")

save(data,file="analysis_df.Rda")

#---------------------------
setwd("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/")
rm(list = ls())
load(file="analysis_df.Rda") 

# setwd("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk/Nytt datauttrekk")
# load('analysis_df_newdata.Rda')
# load('HusInfo.Rdata')
# husdata <- subset(HusInfo, select = c('FK_Innsett_Dim', 'LeverandoerNr', 'Areal'))
# colnames(husdata) <- c('id_batch', 'leverandoer_nr', 'areal')
# data <- merge(data, husdata, by = 'id_batch')
#Extracting the variables we need

analytic_data <- data
#Calculating accumulated dead 
cumu_dead <- analytic_data %>%
  arrange(id_batch, age) %>%
  group_by(id_batch) %>%
  reframe(accum_dead = cumsum(total_dead))
analytic_data <- analytic_data %>%
  arrange(id_batch, age)
analytic_data <- cbind(analytic_data, cumu_dead)

analytic_data$alive <- analytic_data$n_of_chicken - analytic_data$accum_dead
analytic_data$birds_p_m_sqr <- analytic_data$alive/analytic_data$area
analytic_data$weight_kg <- analytic_data$weight/1000
analytic_data$kg_birds <- (analytic_data$alive*analytic_data$weight_kg)
analytic_data$kg_per_sqr <- analytic_data$kg_birds/analytic_data$area

#merging with our exsiting data 
analytic_data <- analytic_data[, !duplicated(names(analytic_data))]
table(analytic_data$feed_name)
#Calculating daily water and food consumption per bird 
analytic_data$water_per_chick <- analytic_data$water_consump/analytic_data$alive
analytic_data$food_per_chick <- analytic_data$feed_consump/analytic_data$alive

i = sample(analytic_data$id_batch, 1)
obs_df <- filter(analytic_data, id_batch == i)
obs_df <- obs_df[order(obs_df$age), ]
obs_df <- subset(obs_df, age < 30)

ggplot(data = obs_df, aes(x = age, y = water_per_chick)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = T, color = "red") +
  xlab("Age") +
  ylab("Water per chick")

analytic_data$out_temp <- as.numeric(analytic_data$out_temp)

climate_stats <- analytic_data %>%
  group_by(id_batch) %>%
  summarise(climate_mean_temp = mean(out_temp), 
            climate_min_temp = min(out_temp),
            climate_max_temp = max(out_temp))
analytic_data <- left_join(analytic_data, climate_stats, by = 'id_batch')

indoor_stats <- data %>%
  group_by(id_batch) %>%
  summarise(indoor_mean_maxtemp = mean(temp_max),
            indoor_mean_mintemp = mean(temp_min),
            indoor_sd_maxtemp = sd(temp_max),
            indoor_sd_mintemp = sd(temp_min),
            indoor_min_maxtemp = min(temp_max),
            indoor_min_mintemp = min(temp_min),
            indoor_max_maxtemp = max(temp_max),
            indoor_max_mintemp = max(temp_min))

analytic_data <- left_join(analytic_data, indoor_stats, by = 'id_batch')

#Making a month variable
analytic_data$month <- month(analytic_data$date)

#Get the most frequent month as the month appearing in final data
freq_month <- analytic_data %>% 
  group_by(id_batch) %>%
  summarise(frequent_month = names(which.max(table(month))))

freq_month$frequent_month <- as.numeric(freq_month$frequent_month)
analytic_data <- merge(analytic_data, freq_month, by = 'id_batch')

#remove som variables we do not need anymore
water <- analytic_data %>%
  group_by(id_batch) %>%
  filter(age > 1 & age < 30) %>%
  summarise(average_water = mean(water_per_chick),
            sd_water = sd(water_per_chick))

analytic_data <- left_join(analytic_data, water, by = 'id_batch')

#Then we get some alternative food descriptive statistics
food_stats <- analytic_data %>%
  group_by(id_batch) %>%
  filter(age > 1 & age < 30) %>%
  summarise(average_food = mean(food_per_chick),
            sd_food = sd(food_per_chick))


analytic_data <- left_join(analytic_data, food_stats, by = 'id_batch')

#Alternative bird data
birds <-  analytic_data %>%
  group_by(id_batch) %>%
  summarise(birds_m_sqr = mean(birds_p_m_sqr),
            kg_m_sqr =  mean(kg_per_sqr))

analytic_data <- left_join(analytic_data, birds, by = 'id_batch')

# Getting slaugther age as the max age
slaughter_age <- analytic_data %>%
  group_by(id_batch) %>%
  summarise(slaughter_age = max(age))
analytic_data <- left_join(analytic_data, slaughter_age, by = 'id_batch')
save(analytic_data,file="long_data.Rda")

start_weight <- analytic_data %>%
  group_by(id_batch) %>%
  summarise(start_weight = min(weight))

analytic_data <- left_join(analytic_data, start_weight, by = 'id_batch')

start_weight$start_weight[start_weight$start_weight <= 20] <- NA #Removing implausible values of low weight
analytic_data$average_food[analytic_data$average_food > 1] <- NA
analytic_data$average_food[analytic_data$average_food == 0] <- NA
load('Produksjonsdata.Rdata')
# Keeping the variables we need 
load('DaggamleKyllinger.Rdata')
hybrid <- subset(DaggamleKyllinger, select = c('FK_Innsett_Dim', 'Hybrid'))
colnames(hybrid) <- c('id_batch', 'hybrid')
analytic_data <- merge(analytic_data, hybrid, by = 'id_batch')

long_data <- subset(analytic_data, select = c('id_batch',
                                              'age',              
                                              'feed_name', #Treatment
                                              'aceties', #Outcome
                                              'prod_type', #Chicken type
                                              'start_weight', #start_weight
                                              'growth', #Growth_linear
                                              'sqr_growth', #Growth_sqr
                                              'indoor_mean_maxtemp', #Indoor Temperature
                                              'frequent_month', #Month
                                              'climate_mean_temp', #Outdoor temperature
                                              'id_slaughterhouse', #Slaughterhouse
                                              'average_food', #Food consumption
                                              'average_water', #Water consumption
                                              'birds_p_m_sqr', #Birds p m-sqr
                                              'kg_per_sqr', #Kg per m-sqr
                                              'n_of_chicken', #Number of chickens
                                              'slaughter_age',
                                              'leverandoer_nr',
                                              'hybrid'))  
save(long_data,file="long_data_for_analysis.Rda")


# Reshape from long to wide 
wide_data <-  distinct(long_data, id_batch, .keep_all = TRUE)

save(wide_data,file="wide_data_for_analysis.Rda")

library(tidyverse)
library(expss)
rm(list = ls())
setwd("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk")

load("analysis_df_newdata.Rda")
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
# data$weight[data$weight < 1000 & data$age > 30 & data$chicken_type == "Grill"] <- NA
# data$weight[data$weight < 1000 & data$age > 30 & data$chicken_type == "Processed"] <- NA
# data$weight[data$weight < 750 & data$age > 25 & data$chicken_type == "McDonalds"] <- NA
# data$weight[data$weight < 100 & data$age > 19 & data$chicken_type == "Kyllinggaarden"] <- NA

#Remove observations 
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

save(data2,file="analysis_df_new_cleaned.Rda")



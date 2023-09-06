#This script builds the data frame for Ascites modeling
library(lubridate)
library(dplyr)
library(naniar)

setwd("C:/Users/christian.thorjussen/Project Nortura/")
load("Nytt datauttrekk/Produksjonsdata.Rdata")
Produksjonsdata$Dato <- as.Date(Produksjonsdata$Dato)
target_date <- as.Date("2021-06-10")
Produksjonsdata <- subset(Produksjonsdata, Dato > target_date)
target_date <- as.Date("2024-04-17")
Produksjonsdata <- subset(Produksjonsdata, Dato < target_date)

load('Nytt datauttrekk/Temperature.Rdata')
load("Humidity.Rdata")

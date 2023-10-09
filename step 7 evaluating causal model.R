# In sample global validation
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(ipred)
library(forcats)
library(boot)
library(xgboost)

setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())

load("wide_data_for_analysis.Rda")
wide_data$feed_name <- str_replace(wide_data$feed_name, "�", "aa")
wide_data$feed_name <- str_replace(wide_data$feed_name, "�", "o")
wide_data$feed_name <- str_replace(wide_data$feed_name, "o?=", "aa")
wide_data <- subset(wide_data, hybrid == "Ross 308")
data <- wide_data

data$feed_name <- as.factor(data$feed_name)
data$prod_type <- as.factor(data$prod_type)
data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$leverandoer_nr <- data$leverandoer_nr
data$ascites_prev <- 1000*data$ascites/data$n_of_chicken
data$frequent_month <- as.factor(data$frequent_month)

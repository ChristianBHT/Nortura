library(dplyr)
library(dagitty)
library(ggplot2)
library(reshape2)
library(janitor)
library(lavaan)
library(tidyverse)
library(gridExtra)

setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())
load(file="data_cleaned.Rda")

table(data$chicken_type)

#List number of batches in data
n_distinct(data$id_batch)

#This subscript estimates a latent growth model, where the aim is to use the latent variables as measures of growth rates and variation of growth
#Create data set with id age and weight of chicken
long_df <-  subset(data, select = c(id_batch, age, weight))
#Create data set with chicken types
type <- subset(data, select = c(id_batch, chicken_type))
#Reshape data
long_df
wide_df <- reshape(long_df, idvar = "id_batch", timevar = "age", direction = "wide")
#Remove duplicates from type data
type <- distinct(type)
#Merge the two dataset to have a wide data with a chicken type as grouping variable
data <- merge(wide_df, type, by = "id_batch")
data <- data %>%
  clean_names()
table(data$chicken_type)
#We start by modeling for the Foredlingskylling (Processed), the model conceptual the same for all chicken types, the only thing we change is 
#how many days we use for modelling.
data_proc <- data[data$chicken_type == "Processed",]

data_proc <- select(data_proc, c('id_batch', "weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                                 "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                                 "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                                 "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                                 "weight_28", "weight_29", "weight_30"))

#Removing rows with missing values, i.e. we are only modeling on those with a complete set of weight observations
data_proc <- na.omit(data_proc)
#Specifying a growth model
model <- ' intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27 + 1*weight_28 + 1*weight_29 + 1*weight_30
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27 + 28*weight_28 + 29*weight_29 + 30*weight_30
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27 + 784*weight_28 + 841*weight_29 + 900*weight_30'

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
                               "weight_25", "weight_26","weight_27", "weight_28", "weight_29", "weight_30"),
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
                                 "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                                 "weight_28"))
data_grill <- na.omit(data_grill)
#Specifying a growth model
model <- ' intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27 + 1*weight_28 
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27 + 28*weight_28 
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27 + 784*weight_28' 

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
                               "weight_25", "weight_26","weight_27", "weight_28"),
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
                                 "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                                 "weight_28", "weight_29", "weight_30", "weight_31"))
data_gaarden <- na.omit(data_gaarden)
#Specifying a growth model
model <- 'intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27 + 1*weight_28  + 1*weight_29  + 1*weight_30  + 1*weight_31 
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27 + 28*weight_28 + 29*weight_29  + 30*weight_30  + 31*weight_31 
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27 + 784*weight_28 + + 841*weight_29  + 900*weight_30  + 961*weight_31' 

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
                               "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                               "weight_28", "weight_29", "weight_30", "weight_31"),
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
 p3 <- p3 + geom_line(data=obs_df, aes(x,y)) + ggtitle(paste0("Batch id = ", i, ". Kyllingg??rden"))
 p3
 
#Hubbard Chicken
data_hubbard <- data[data$chicken_type == "Hubbard",]
data_hubbard <- select(data_hubbard, c('id_batch', "weight_0", "weight_1", "weight_2", "weight_3", "weight_4", "weight_5", "weight_6", 
                                       "weight_7", "weight_8", "weight_9", "weight_10", "weight_11", "weight_12", "weight_13", 
                                       "weight_14", "weight_15", "weight_16", "weight_17", "weight_18", "weight_19", "weight_20", 
                                       "weight_21", "weight_22", "weight_23", "weight_24", "weight_25", "weight_26", "weight_27", 
                                       "weight_28", "weight_29", "weight_30", "weight_31", "weight_32", "weight_33", "weight_34",
                                       "weight_35", "weight_36", "weight_37", "weight_38", "weight_39", "weight_40"))
data_hubbard <- na.omit(data_hubbard)
#Specifying a growth model
model <- 'intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27 + 1*weight_28 + 1*weight_29 + 1*weight_30 + 1*weight_31 + 1*weight_32 + 1*weight_33 + 1*weight_34 + 1*weight_35 + 1*weight_36 + 1*weight_37  + 1*weight_38 + 1*weight_39 + 1*weight_40  
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27 + 28*weight_28 + 29*weight_29  + 30*weight_30  + 31*weight_31 + 32*weight_32 + 33*weight_33 + 34*weight_34 + 35*weight_35 + 36*weight_36 + 37*weight_37 + 38*weight_38 + 39*weight_39 + 40*weight_40
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27 + 784*weight_28 + + 841*weight_29  + 900*weight_30  + 961*weight_31  + 1024*weight_32 + 1089*weight_33 + 1156*weight_34 + 1225*weight_35 + 1296*weight_36 + 1369*weight_37 + 1444*weight_38 + 1521*weight_39 + 1600*weight_40' 

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
                               "weight_35", "weight_36", "weight_37", "weight_38", "weight_39", "weight_40"),
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
                                       "weight_28", "weight_29", "weight_30", "weight_31"))
data_mcdon <- na.omit(data_mcdon)
#Specifying a growth model
model <- 'intercept =~  1*weight_0 + 1*weight_1 + 1*weight_2 + 1*weight_3 + 1*weight_4 + 1*weight_5 + 1*weight_6 + 1*weight_7 + 1*weight_8 + 1*weight_9 + 1*weight_10 + 1*weight_11 + 1*weight_12 + 1*weight_13 + 1*weight_14 + 1*weight_15 + 1*weight_16 + 1*weight_17 + 1*weight_18 + 1*weight_19 + 1*weight_20 + 1*weight_21 + 1*weight_22 + 1*weight_23 +  1*weight_24 + 1*weight_25 + 1*weight_26 + 1*weight_27 + 1*weight_28 + 1*weight_29 + 1*weight_30 + 1*weight_31  
           growth =~  0*weight_0 + 1*weight_1 + 2*weight_2 + 3*weight_3 + 4*weight_4 + 5*weight_5 + 6*weight_6 + 7*weight_7 + 8*weight_8 + 9*weight_9 + 10*weight_10 + 11*weight_11 + 12*weight_12 + 13*weight_13 + 14*weight_14 + 15*weight_15 + 16*weight_16 + 17*weight_17 + 18*weight_18 + 19*weight_19 + 20*weight_20 + 21*weight_21 + 22*weight_22 + 23*weight_23 +24*weight_24 + 25*weight_25 + 26*weight_26 + 27*weight_27 + 28*weight_28 + 29*weight_29  + 30*weight_30  + 31*weight_31 
            sqr_growth =~  0*weight_0 + 1*weight_1 + 4*weight_2 + 9*weight_3 + 16*weight_4 + 25*weight_5 + 36*weight_6 + 49*weight_7 + 64*weight_8 + 81*weight_9 + 100*weight_10 + 121*weight_11 + 144*weight_12 + 169*weight_13 + 196*weight_14 + 225*weight_15 + 256*weight_16 + 289*weight_17 + 324*weight_18 + 361*weight_19 + 400*weight_20 + 441*weight_21 + 484*weight_22 + 529*weight_23 +  576*weight_24 + 625*weight_25 + 676*weight_26 + 729*weight_27 + 784*weight_28 + + 841*weight_29  + 900*weight_30  + 961*weight_31' 

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
                               "weight_28", "weight_29", "weight_30", "weight_31"),
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

load(file="data_cleaned.Rda")
growth_data <- rbind(select(data_proc, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')), 
                     select(data_grill, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')), 
                     select(data_gaarden, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')), 
                     select(data_mcdon, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')),
                     select(data_hubbard, c("id_batch", "intercept", "growth", "sqr_growth", 'cor', 'r2')))

data <- merge(data, growth_data, by = "id_batch")

save(data,file="data_nortura_for_analysis.Rda")








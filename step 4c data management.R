library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(lubridate)
library(dagitty)
setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())
load('data_nortura_for_analysis.Rda')
#load('data_nortura_for_analysis_2.Rda')

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

#Calculate density of chicken per sqr meter and kilo per square meter
analytic_data$alive <- analytic_data$n_of_chicken - analytic_data$accum_dead
analytic_data$birds_p_m_sqr <- analytic_data$alive/analytic_data$area
analytic_data$weight_kg <- analytic_data$weight/1000
analytic_data$kg_birds <- (analytic_data$alive*analytic_data$weight_kg)
analytic_data$kg_per_sqr <- analytic_data$kg_birds/analytic_data$area

#Getting feed names from the forblanding table
feed_name <- read.csv('raw data/Forblanding.csv', sep = ';')
feed_name <- subset(feed_name, select = c('PK_Forblanding_Dim', 'Forblanding', 'FK_Forfirma_Dim'))
colnames(feed_name) <- c('growth_feed', 'feed_name', 'FK_feed_firm')
#merging with our exsiting data 
analytic_data <- merge(analytic_data, feed_name, by = 'growth_feed')
analytic_data <- analytic_data[, !duplicated(names(analytic_data))]

#Calculating daily water and food consumption per bird 
analytic_data$water_per_chick <- analytic_data$water_consump/analytic_data$alive
analytic_data$food_per_chick <- analytic_data$feed_consump/analytic_data$alive

## Plot of how water consumption develops in a random batch 
# i = sample(analytic_data$id_batch, 1)
# obs_df <- filter(analytic_data, id_batch == i)
# obs_df <- obs_df[order(obs_df$age), ]
# obs_df <- subset(obs_df, age < 30)
# 
# ggplot(data = obs_df, aes(x = age, y = water_per_chick)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = T, color = "red") +
#   xlab("Age") +
#   ylab("Water per chick")
# 
# 



#Since the final data set are going to have one row per batch (because of ascites), we need to extract as much information as possible from the production day data

#Here we want to extract as much information as possible regarding water consumption. 
#We do this by fitting a qudratic linear regression function for each batch and extract the regression parameters and R2 
water_stats <- analytic_data %>%
  group_by(id_batch) %>%
  filter(!any(is.na(water_per_chick))) %>%
  do({
    if (nrow(.) > 4) {
      model <- lm(water_per_chick ~ age + I(age^2), data = .)
      estimates <- tidy(model)
      rsquared <- summary(model)$r.squared
    } else {
      estimates <- tibble()
      rsquared <- NA
    }
    tibble(estimates, rsquared)
  })


water_data <-  data.frame(water_stats$id_batch, water_stats$term, water_stats$estimate, water_stats$rsquared)
colnames(water_data) <- c('id_batch', 'term', 'estimate', 'rsquared')
water_stats <- pivot_wider(water_data, names_from = term, values_from = estimate)
colnames(water_stats) <- c('id_batch', 'water_r2', 'water_cons', 'water_slope', 'water_slope2')
# Merge data
analytic_data <- left_join(analytic_data, water_stats, by = "id_batch")
# 
# i = sample(analytic_data$id_batch, 1)
# obs_df <- filter(analytic_data, id_batch == i)
# obs_df <- obs_df[order(obs_df$age), ]
# obs_df <- subset(obs_df, age < 30)
# 
# ggplot(data = obs_df, aes(x = age, y = birds_p_m_sqr)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = T, color = "red") +
#   xlab("Age") +
#   ylab("birds_p_m_sqr")


birds_meter <- analytic_data %>%
  group_by(id_batch) %>%
  filter(!any(is.na(birds_p_m_sqr))) %>%
  do({
    if (nrow(.) > 4) {
      model <- lm(birds_p_m_sqr ~ age + I(age^2), data = .)
      estimates <- tidy(model)
      rsquared <- summary(model)$r.squared
    } else {
      estimates <- tibble()
      rsquared <- NA
    }
    tibble(estimates, rsquared)
  }) 
bird_data <- data.frame(birds_meter$id_batch, birds_meter$term, birds_meter$estimate, birds_meter$rsquared)
colnames(bird_data) <- c('id_batch', 'term', 'estimate', 'rsquared')
bird_data <- pivot_wider(bird_data, names_from = term, values_from = estimate)
colnames(bird_data) <- c('id_batch', 'bird_r2', 'bird_cons', 'bird_slope', 'bird_slope2')
 
analytic_data <- left_join(analytic_data, bird_data, by = "id_batch")

#This chunck of code extracts outdoor climate stats; mean, standard deviation, min and max
climate_stats <- data %>%
              group_by(id_batch) %>%
              summarise(climate_mean_temp = mean(out_temp), 
                        climate_sd_temp = sd(out_temp), 
                        climate_min_temp = min(out_temp),
                        climate_max_temp = max(out_temp),
                        climate_mean_hum = mean(out_humidity),
                        climate_sd_hum = sd(out_humidity),
                        climate_min_hum = min(out_humidity),
                        climate_max_hum = max(out_humidity))
analytic_data <- left_join(analytic_data, climate_stats, by = 'id_batch')

#And we do the same for indoor temp and humidity
indoor_stats <- data %>%
                group_by(id_batch) %>%
                summarise(indoor_mean_maxtemp = mean(temp_max),
                          indoor_mean_mintemp = mean(temp_min),
                          indoor_sd_maxtemp = sd(temp_max),
                          indoor_sd_mintemp = sd(temp_min),
                          indoor_min_maxtemp = min(temp_max),
                          indoor_min_mintemp = min(temp_min),
                          indoor_max_maxtemp = max(temp_max),
                          indoor_max_mintemp = max(temp_min),
                          indoor_mean_maxhumidity = mean(humidity_max),
                          indoor_mean_minhumidity = mean(humidity_min),
                          indoor_sd_maxhumidity = sd(humidity_max),
                          indoor_sd_minhumidity = sd(humidity_min),
                          indoor_min_maxhumidity = min(humidity_max),
                          indoor_min_minhumidity = min(humidity_min),
                          indoor_max_maxhumidity = max(humidity_max),
                          indoor_max_minhumidity = max(humidity_min))

analytic_data <- left_join(analytic_data, indoor_stats, by = 'id_batch')


# We do the same for food consumption as for water it has more missing values than water

food_stats <- analytic_data %>%
  group_by(id_batch) %>%
  filter(!any(is.na(food_per_chick))) %>%
  do({
    if (nrow(.) > 8) {
      model <- lm(food_per_chick ~ age + I(age^2), data = .)
      estimates <- tidy(model)
      rsquared <- summary(model)$r.squared
    } else {
      estimates <- tibble()
      rsquared <- NA
    }
    tibble(estimates, rsquared)
  }) 

food_data <- data.frame(food_stats$id_batch, food_stats$term, food_stats$estimate, food_stats$rsquared)
colnames(food_data) <- c('id_batch', 'term', 'estimate', 'rsquared')
food_data <- pivot_wider(food_data, names_from = term, values_from = estimate)
colnames(food_data) <- c('id_batch', 'food_r2', 'food_cons', 'food_slope', 'food_slope2')

colnames(food_data) <- c('id_batch', 'R2_food', 'food_inter', 'food_age2', 'food_age')

analytic_data <- left_join(analytic_data, food_data, by = 'id_batch')

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
  summarise(birds_m_sqr = mean(birds_p_m_sqr))

analytic_data <- left_join(analytic_data, birds, by = 'id_batch')

# Getting slaugther age as the max age
slaughter_age <- analytic_data %>%
  group_by(id_batch) %>%
  summarise(slaughter_age = max(age))
analytic_data <- left_join(analytic_data, slaughter_age, by = 'id_batch')

# Keeping the variables we need 
# long_data <- subset(analytic_data, select = c('age','id_batch','aceties', 'prod_type', 'leverandoer_nr', 'feed_name','id_slaughterhouse', 
#                                               'climate_mean_temp','climate_sd_temp', 'climate_min_temp','climate_max_temp', 'asymptote', 'displacement', 'growth_rate', 'growth_r2',
#                                               'climate_max_hum', 'indoor_mean_maxtemp','indoor_mean_mintemp','indoor_sd_maxtemp','indoor_sd_mintemp',
#                                               'indoor_min_mintemp','indoor_min_maxtemp','indoor_max_mintemp','indoor_max_maxtemp',
#                                               'R2_food','food_inter','food_age2','food_age','frequent_month','average_food','sd_food','birds_m_sqr',
#                                               'bird_r2','bird_cons','bird_slope','bird_slope2','water_r2','water_cons','water_slope','water_slope2',
#                                               'slaughter_age','n_of_chicken'))

long_data <- subset(analytic_data, select = c('age','id_batch','id_feedfirm', 'id_hatchery','aceties', 'prod_type', 'leverandoer_nr', 'feed_name','id_slaughterhouse', 
                                              'climate_mean_temp','climate_sd_temp', 'climate_min_temp','climate_max_temp', 'intercept', 'growth', 'sqr_growth', 'r2',
                                              'climate_max_hum', 'indoor_mean_maxtemp','indoor_mean_mintemp','indoor_sd_maxtemp','indoor_sd_mintemp',
                                              'indoor_min_mintemp','indoor_min_maxtemp','indoor_max_mintemp','indoor_max_maxtemp',
                                              'R2_food','food_inter','food_age2','food_age','frequent_month','average_food','sd_food','birds_m_sqr',
                                              'bird_r2','bird_cons','bird_slope','bird_slope2','water_r2','water_cons','water_slope','water_slope2',
                                              'slaughter_age','n_of_chicken', 'average_water', 'sd_water', 'hybrid'))


# Reshape from long to wide 
wide_data <-  distinct(long_data, id_batch, .keep_all = TRUE)
wide_data <- subset(wide_data, select = -age)

save(wide_data,file="wide_data_for_analysis.Rda")

load(file="wide_data_for_analysis.Rda")














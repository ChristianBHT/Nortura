library(GeneralisedCovarianceMeasure)
library(dagitty)
library(caret)
library(ipred)
library(dplyr)
library(mgcv)
setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())

DAGmodell <- dagitty('dag {
bb="0,0,1,1"
"Farm effects" [pos="0.030,0.174"]
"Food amount" [pos="0.142,0.020"]
"Food type" [exposure,pos="0.030,0.047"]
"Speed of growth" [pos="0.330,0.024"]
"Water consumption" [pos="0.141,0.076"]
Ascites [outcome,pos="0.330,0.226"]
Humidity [pos="0.143,0.321"]
Temperature [pos="0.125,0.235"]
"Farm effects" -> "Food type"
"Farm effects" -> "Speed of growth"
"Farm effects" -> Ascites
"Farm effects" -> Humidity
"Farm effects" -> Temperature
"Food amount" -> "Speed of growth"
"Food amount" -> Ascites
"Food type" -> "Food amount"
"Food type" -> "Speed of growth"
"Food type" -> "Water consumption"
"Food type" -> Ascites
"Speed of growth" -> Ascites
"Water consumption" -> "Speed of growth"
"Water consumption" -> Ascites
Humidity -> Ascites
Temperature -> "Speed of growth"
Temperature -> Ascites
Temperature -> Humidity
Temperature -> "Water consumption"
}')
#Control for Farm effects, Food amount, Speed of growth, Temperature, Water consumption
growth_feed <- read.csv(file = "vekstfor_data.csv", sep = ';')
f_data <- data.frame(growth_feed$innsett_id, 
                     growth_feed$vekstfortype1)

colnames(f_data) <- c('id_batch', 
                      'feed')

plot(DAGmodell)
impliedConditionalIndependencies(DAGmodell)
#Food type ⊥ Humidity | Farm effects
#Food type _||_ CO2 |  Farm effects

load(file="nortura_analysis.Rda")

#Thining the data a little bit
data <- data[data$age <= 48, ]
data <- data[, !names(data) %in% c("primary_feed_growth", "amount_primary_growth")]
data <- merge(data, f_data, by = 'id_batch')
#Wtrc _||_ CO2 | Frme, Tmpr
data$id_batch <- as.integer(data$id_batch)
data$temp_max[data$temp_max == 0] <- NA
data$co2_max[data$co2_max == 0] <- NA
data$co2_min[data$co2_min == 0] <- NA
data$humidity_max[data$humidity_max == 0] <- NA
data$humidity_min[data$humidity_min == 0] <- NA

#Calculate mean max temp for batches with more than 10 observed values
mean_max_temp <- data %>% 
  group_by(id_batch) %>%
  filter(sum(!is.na(temp_max)) > 10 & age < 30) %>%
  summarize(mean_max_temp = mean(temp_max, na.rm = TRUE))

my_list <- unique(as.integer(data$id_batch))
my_list <- sort(my_list)
results <- list()

for (i in 1:length(my_list)) {
    obs_df <- filter(data, id_batch == my_list[i])
    if (sum(!is.na(obs_df$temp_max)) > 15) {
      model <- gam(temp_max ~ s(age), data = obs_df)
      results[[i]] <- list(id_batch = my_list[i], obs = summary(model)$n, rsqr = summary(model)$r.sq, criterion = summary(model)$sp.criterion)
      
    } else {
      message(paste0("No success for ID batch ", my_list[i], ", too many NA's"))
      }
}
 
matrix <- matrix(unlist(results), ncol = 4, byrow = TRUE)

# convert the matrix to a data frame
temp_metrics <- as.data.frame(matrix)
names(temp_metrics) <- c('id_batch', 'obs', 'r_squared', 'gcv')
compare_data <- data[, c('id_batch', 'age', 'temp_max')]
temp_metrics$id_batch <- as.factor(temp_metrics$id_batch)

temp_metrics$index <- temp_metrics$r_squared*100/temp_metrics$gcv

temp_metrics <- temp_metrics %>% filter(r_squared >= 0)

data$alive <- data$n_of_chicken - data$total_dead
data$water_per_chick <- data$water_consump/data$alive

mean_water_cons  <- data %>% 
  group_by(id_batch) %>%
  filter(sum(!is.na(water_per_chick)) > 10) %>%
  summarize(mean_water_cons = mean(water_per_chick, na.rm = TRUE))


#Plotting a water consumption plot
i = sample(data$id_batch, 1)
obs_df <- filter(data, id_batch == i)
obs_df <- obs_df[order(obs_df$age), ]
obs_df <- subset(obs_df, age < 30)

ggplot(data = obs_df, aes(x = age, y = water_per_chick)) + 
  geom_point() + 
  geom_smooth(method = "gam", se = FALSE, color = "red") +
  xlab("Age") + 
  ylab("Water per chick")

data$food_per_chick <- data$feed_consump/data$alive

#Plotting food consumption plot
i = sample(data$id_batch, 1)
obs_df <- filter(data, id_batch == i)
obs_df <- obs_df[order(obs_df$age), ]
obs_df <- subset(obs_df, age < 30)

ggplot(data = obs_df, aes(x = age, y = food_per_chick)) + 
  geom_point() + 
  geom_smooth(method = "gam", se = FALSE, color = "red") +
  xlab("Age") + 
  ylab("food per chick")

mean_food_cons  <- data %>% 
  group_by(id_batch) %>%
  filter(sum(!is.na(food_per_chick)) > 10) %>%
  summarize(mean_food_cons = mean(food_per_chick, na.rm = TRUE))

mini_temp <- data %>%
  group_by(id_batch) %>%
  summarise(mini_temp = min(temp_max, na.rm = TRUE))

mini_temp <- mini_temp %>% filter(mini_temp != Inf)

#Keep Farm effects, Food amount, Speed of growth, Temperature, Water consumption, chicken type 
long_data <- data[, c('id_batch', 'n_of_chicken', 'prod_type', 'id_producer', 'intercept', 'growth', 'sqr_growth', 'condition_aceties', 'feed')]

#reshape data into wide format
wide_data <- unique(long_data) 

wide_data <- merge(wide_data, mean_food_cons, by = 'id_batch', all = T)
wide_data <- merge(wide_data, mean_water_cons, by = 'id_batch', all = T)
wide_data <- merge(wide_data, temp_metrics, by = 'id_batch', all = T)
wide_data <- merge(wide_data, mini_temp, by = 'id_batch', all = T)

# Convert data to a panel data frame
panel_data <- pdata.frame(wide_data, index=c("id_producer"))
panel_data$feed <- ifelse(panel_data$feed %in% c(234, 233, 226, 223, 348, 357, 365, 383, 413, 438, 501, 523), 523, panel_data$feed)
panel_data$feed <- ifelse(panel_data$feed %in% c(11, 35, 150, 154, 172, 178, 192, 193, 350, 354, 355, 359, 362, 369, 375, 
                                                 385, 394, 395, 397, 415, 420, 421, 423, 428, 430, 432, 435, 436, 456, 458, 
                                                 462, 486, 497, 498, 499, 500, 528, 529), 999,  panel_data$feed)
table(panel_data$feed)

panel_data$prod_type <- as.factor(panel_data$prod_type)
panel_data$feed <- as.factor(panel_data$feed)
#Some estimations of causal effects
#Fixed effect estimate
library(plm)
library(margins)
# Estimate fixed effects model
hist(panel_data$condition_aceties)
panel_data$ascite_perc <- 100*panel_data$condition_aceties / panel_data$n_of_chicken
hist(panel_data$ascite_perc)

model_fe <- plm(ascite_perc ~ feed + prod_type + growth + sqr_growth + mean_food_cons + mean_water_cons, data = panel_data, model = "within")
summary(model_fe)



library(caret)
library(neuralnet)
library(randomForest)
library(ipred)
model_data <- panel_data[, c('prod_type', 'id_producer', 'intercept', 'growth', 'sqr_growth', 'condition_aceties', 'feed', 'ascite_perc')]
inTraining <- createDataPartition(na.omit(model_data$condition_aceties), p = 0.8, list = FALSE)
training <- model_data[inTraining,]
testing  <- model_data[-inTraining,]

rf_model <- randomForest(condition_aceties ~ feed + prod_type + growth + sqr_growth, 
                         data = training, 
                         ntree = 500, 
                         na.action=na.omit)

test.features <- subset(testing, select=c(feed, prod_type,  growth, sqr_growth))
test.target <- subset(testing, select=condition_aceties)[,1]
predictions <- predict(rf_model, newdata = test.features)
rf_R2 <- cor(test.target, predictions, use = "pairwise.complete.obs") ^ 2
rf_R2
table(panel_data$feed)

panel_data <- panel_data %>% rename(feedT = feed)
panal_data$feed <- 8 
predict_data <- panel_data[, c(condition_aceties, feed, prod_type, growth , sqr_growth)]
treat1 <- predict(rf_model, newdata = predict_data)
treat2 <- 
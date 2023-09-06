library(GeneralisedCovarianceMeasure)
library(dagitty)
library(caret)
library(ipred)
library(dplyr)
library(mgcv)

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

plot(DAGmodell)
impliedConditionalIndependencies(DAGmodell)
#Food type ⊥ Humidity | Farm effects
#Food type _||_ CO2 |  Farm effects
setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())
load(file="nortura_analysis.Rda")
#Thining the data a little bit
data <- data[data$age <= 48, ]

#Wtrc _||_ CO2 | Frme, Tmpr
data$id_producer <- as.factor(data$id_producer)
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

model <- test_data %>% 
  group_by(id_batch) %>% 
  summarize(model = gam(temp_max ~ s(age)))

test_data <- data %>% select(c("id_batch", "age", "temp_max"))

model <- gam(temp_max ~ s(age), data = obs_df) 
summary(model)

my_list <- unique(data$id_batch)
my_list <- sort(my_list)
# Using a forloop
results <- list()
for (i in 1:length(my_list)) {
  id <- my_list[[i]]
  obs_df <- filter(test_data, id_batch == my_list[[i]])
  obs_df <- obs_df[order(obs_df$age), ]
  obs_df <- subset(obs_df, age < 30)
  if (sum(!is.na(obs_df$temp_max)) > 15) {
    model <- gam(temp_max ~ s(age), data = obs_df, na.action = na.omit)
    results <- append(results, c(id,  summary(model)$n, summary(model)$r.sq, summary(model)$sp.criterion))
  } else {
    message(paste0("Skipping ID batch ", id, " due to insufficient non-NA data."))
  }
}
matrix <- matrix(unlist(results), ncol = 4, byrow = TRUE)

# convert the matrix to a data frame
temp_metrics <- as.data.frame(matrix)
names(temp_metrics) <- c('id_batch', 'obs', 'r_squared', 'gcv')
temp_metrics$index <- temp_metrics$r_squared*100/temp_metrics$gcv
temp_metrics <- temp_metrics %>% filter(r_squared >= 0)

data$alive <- data$n_of_chicken - data$total_dead
data$water_per_chick <- data$water_consump/data$alive

mean_water_cons  <- data %>% 
  group_by(id_batch) %>%
  filter(sum(!is.na(water_per_chick)) > 10) %>%
  summarize(mean_water_cons = mean(water_per_chick, na.rm = TRUE))

#Plotting some water consumption plots
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

#Plotting some food consumption plots
i = sample(data$id_batch, 1)
obs_df <- filter(data, id_batch == i)
obs_df <- obs_df[order(obs_df$age), ]
obs_df <- subset(obs_df, age < 30)

ggplot(data = obs_df, aes(x = age, y = food_per_chick)) + 
  geom_point() + 
  geom_smooth(method = "gam", se = FALSE, color = "red") +
  xlab("Age") + 
  ylab("food per chick")

data <- data %>% filter(food_per_chick != 0)



inTraining <- createDataPartition(data$st_TempDiff, p = 0.7, list = FALSE)
training <- data[inTraining,]
testing  <- data[-inTraining,]

rf <- randomForest(st_TempDiff ~ st_Pressure + st_AddedWater + st_Fat, 
                   data=training,
                   ntree=500)

test.features <- subset(testing, select=c(st_Pressure, st_AddedWater, st_Fat))
test.target <- subset(testing, select=st_TempDiff)[,1]
predictions <- predict(rf, newdata = test.features)
rf_R2 <- cor(test.target, predictions) ^ 2

nn <- neuralnet(st_TempDiff ~ st_Pressure + st_AddedWater + st_Fat,
                data = training,
                hidden = 5)

test.features <- subset(testing, select=c(st_Pressure, st_AddedWater, st_Fat))
test.target <- subset(testing, select=st_TempDiff)[,1]
predictions <- predict(nn, newdata = test.features)
nn_R2 <- cor(test.target, predictions) ^ 2

bt <- bagging(
  st_TempDiff ~ st_Pressure + st_AddedWater + st_Fat,
  data = training,
  coob = TRUE
)

test.features <- subset(testing, select=c(st_Pressure, st_AddedWater, st_Fat))
test.target <- subset(testing, select=st_TempDiff)[,1]
predictions <- predict(bt, newdata = test.features)
bt_R2 <- cor(test.target, predictions) ^ 2
print(paste0("RF: ", rf_R2, ", NN: ", nn_R2, ",BT: ", bt_R2))




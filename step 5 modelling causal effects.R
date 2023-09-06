library(GeneralisedCovarianceMeasure)
library(dagitty)
library(caret)
library(ipred)
library(dplyr)
library(mgcv)
library(plm)
library(margins)
library(boot)
library(caret)
library(neuralnet)
library(randomForest)
library(ipred)
setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())
load('data_nortura_for_analysis.Rda')

DAGmodell <- dagitty('dag {
bb="-4.723,-5.908,5.144,4.316"
"Birds p m-sqr" [pos="-2.761,-1.840"]
"Chicken type" [pos="3.292,3.308"]
"Feed firm" [pos="1.446,-4.051"]
"Feed mix" [exposure,pos="1.460,3.388"]
"Food consumption" [pos="-3.112,1.610"]
"Indoor Temperature" [pos="-2.217,3.750"]
"Indoor humidity" [pos="-2.754,3.048"]
"Kg per m-sqr" [pos="-3.298,-1.091"]
"Outdoor humidity" [pos="2.968,-3.496"]
"Outdoor temperature" [pos="3.547,-2.885"]
"Slaughter age" [pos="-2.375,-2.520"]
"Water consumption" [pos="-2.926,2.335"]
Ascites [outcome,pos="0.489,-2.058"]
CO2 [latent,pos="-3.291,0.863"]
Farm [pos="3.574,2.086"]
Growth [pos="-1.921,-3.100"]
Month [pos="4.035,-2.296"]
"Birds p m-sqr" -> "Kg per m-sqr"
"Birds p m-sqr" -> Ascites
"Chicken type" -> "Birds p m-sqr"
"Chicken type" -> "Feed mix"
"Chicken type" -> "Kg per m-sqr"
"Chicken type" -> "Slaughter age"
"Chicken type" -> Ascites
"Chicken type" -> Growth
"Feed firm" -> "Feed mix"
"Feed mix" -> "Food consumption"
"Feed mix" -> "Water consumption"
"Feed mix" -> Ascites
"Feed mix" -> Growth
"Food consumption" -> Ascites
"Food consumption" -> Growth
"Indoor Temperature" -> "Indoor humidity"
"Indoor Temperature" -> "Water consumption"
"Indoor Temperature" -> Ascites
"Indoor Temperature" -> Growth
"Indoor humidity" -> Ascites
"Kg per m-sqr" -> Ascites
"Outdoor humidity" -> "Indoor humidity"
"Outdoor humidity" -> Ascites
"Outdoor temperature" -> "Indoor Temperature"
"Outdoor temperature" -> "Indoor humidity"
"Outdoor temperature" -> "Outdoor humidity"
"Outdoor temperature" -> "Water consumption"
"Outdoor temperature" -> Ascites
"Outdoor temperature" -> CO2
"Slaughter age" -> Ascites
"Water consumption" -> Ascites
"Water consumption" -> Growth
CO2 -> Ascites
Farm -> "Birds p m-sqr"
Farm -> "Feed mix"
Farm -> "Indoor Temperature"
Farm -> "Indoor humidity"
Farm -> Ascites
Farm -> Growth
Growth -> "Slaughter age"
Growth -> Ascites
Month -> "Feed mix"
Month -> "Indoor humidity"
Month -> Ascites
Month -> CO2
Month -> Growth
}')
#Control for Farm effects, Food amount, Speed of growth, Temperature, Water consumption

plot(DAGmodell)
impliedConditionalIndependencies(DAGmodell)
#Food type ⊥ Humidity | Farm effects
#Food type _||_ CO2 |  Farm effects

load(file="nortura_analysis.Rda")
# Randomly select 300 id_batches
selected_id_batches <- sample(unique(data$id_batch), 300, replace = FALSE)
data_test <- data[data$id_batch %in% selected_id_batches,]
data <- data[!data$id_batch %in% selected_id_batches,]

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

mean_water_cons  <- data %>% 
  group_by(id_batch) %>%
  filter(sum(!is.na(water_per_chick)) > 10) %>%
  summarize(mean_water_cons = mean(water_per_chick, na.rm = TRUE))


data$food_per_chick <- data$feed_consump/data$alive

#Plotting temp plot
i = sample(data$id_batch, 1)
obs_df <- filter(data, id_batch == i)
obs_df <- obs_df[order(obs_df$age), ]
obs_df <- subset(obs_df, age < 30)

ggplot(data = obs_df, aes(x = age, y = temp_max)) + 
  geom_point() + 
  geom_smooth(method = "gam", se = FALSE, color = "red") +
  xlab("Age") + 
  ylab("Temp")

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

panel_data$prod_type <- as.factor(panel_data$prod_type)
#panel_data$feed <- as.factor(panel_data$feed)

{
feed_mix <- data.frame(feed = c(1, 3, 4, 8, 10, 11, 14, 16, 33, 35, 99, 101, 135, 136, 137, 138, 140, 144, 145, 146, 147, 148, 149, 150, 151, 152, 154, 160, 172, 178, 179, 183, 184, 185, 186, 188, 191, 192, 193, 194, 195, 196, 197, 198, 199, 216, 217, 218, 223, 226, 233, 234, 285, 338, 339, 347, 348, 349, 350, 353, 354, 355, 357, 358, 359, 360, 361, 362, 364, 365, 366, 367, 369, 374, 375, 379, 383, 385, 387, 388, 389, 392, 393, 394, 395, 397, 398, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 437, 438, 456, 457, 458, 459, 460, 461, 462, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 497, 498, 499, 500, 501, 502, 503, 504, 505, 508, 509, 510, 511, 512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529, 999), 
                      feed_mix = c("Kromat Kylling 1 Hog  u/k", 
                                      "Kromat Kylling 2 Laag m/k", 
                                      "Kromat Kylling 2 Laag u/k", 
                                      "Kromat Kylling 2 Enkel u/k", 
                                      "Toppkylling 1", 
                                      "Toppkylling 2", 
                                      "Kromat Kylling 1 uten narasin", 
                                      "Kromat Kylling 2 Laag uten narasin", 
                                      "Harmoni kylling start u koks", 
                                      "Harmoni kylling Vekst u koks", 
                                      "Kromat Kylling 1 Hog  u/k", 
                                      "Kromat Kylling 2 Laag u/k", 
                                      "Kromat Kylling prestart", 
                                      "Harmoni Kylling Start", 
                                      "Stangekylling start (Landkylling)", 
                                      "Toppkylling 1", 
                                      "Harmoni Kylling Start (Mc Donalds)", 
                                      "Slaktekylling Start u/narasin", 
                                      "Kromat Kylling prestart u/k", 
                                      "Kromat Kylling prestart u/k", 
                                      "Kromat McKylling 1 u/k", 
                                      "Kromat Kylling 2 Hog uten narasin", 
                                      "FK Kylling Tarmhelse", 
                                      "Harmoni Kylling Vekst", 
                                      "Harmoni Kylling Ressurs", 
                                      "Harmoni Kylling Spurt", 
                                      "Harmoni kylling Ressurs", 
                                      "Natura Kylling 1", 
                                      "Harmoni Kylling Ressurs Mc Donalds m/ Clostat", 
                                      "Harmoni Kylling Vekst (Mc Donalds)", 
                                      "Harmoni Kylling Ressurs", 
                                      "Kromat Kylling 2 Enkel u/k", 
                                      "Kromat Kylling 2 Hog u/k", 
                                      "Kromat McKylling 2 Hog u/k", 
                                      "Kromat McKylling 2 Laag u/k", 
                                      "Toppkylling Netto", 
                                      "Kromat Kylling 2 Hog u/k", 
                                      "Toppkylling 2", 
                                      "Toppkylling 3", 
                                      "Toppkylling Netto", 
                                      "Toppkylling Lett", 
                                      "Toppkylling 3", 
                                      "Harmoni Kylling Margin", 
                                      "Stangekylling Vekst 1 (Landkylling)", 
                                      "Stangekylling Vekst 2 (Landkylling)", 
                                      "Toppkylling 1 (Mc Donald)", 
                                      "Toppkylling Netto", 
                                      "Toppkylling Pluss (Mc Donald)", 
                                      "Testfor", 
                                      "Testfor", 
                                      "Testfor", 
                                      "Testfor", 
                                      "Kromat Kylling 2", 
                                      "Toppkylling Lett", 
                                      "Kylling Liveche (Gourmet)", 
                                      "Kromat Kylling 2 Laag m/k", 
                                      "Testfor", 
                                      "Natura Kylling Start", 
                                      "Natura Kylling 2", 
                                      "Toppkylling Pluss", 
                                      "Toppkylling Premium", 
                                      "Toppkylling Hubbard", 
                                      "Testfor", 
                                      "Toppkylling Narasin", 
                                      "Testfor", 
                                      "Harmoni kylling Vekst Sterk", 
                                      "Harmoni kylling Vekst ", 
                                      "Harmoni kylling Brems", 
                                      "Harmoni kylling Ressurs m/koks", 
                                      "Testfor", 
                                      "Harmoni kylling Start", 
                                      "Harmoni kylling Vekst", 
                                      "Harmoni kylling Spurt", 
                                      "Harmoni Kylling Spurt (Mc Donalds)", 
                                      "Harmoni Kylling Ressurs margin (Mc Donalds)", 
                                      "Harmoni Kylling Brems (Mc Donalds)", 
                                      "Testfor", 
                                      "Harmoni Kylling Ressurs u koks", 
                                      "Slaktekylling Koks", 
                                      "Toppkylling Brems", 
                                      "Kylling Liveche (Rapid)", 
                                      "Kromat Kylling 2 Tarmhelse", 
                                      "Kromat Kylling prestart u/k", 
                                      "Kromat Kylling 2 Laag u/k", 
                                      "Kromat Kylling 2 Hog u/k", 
                                      "Harmoni Kylling Ressurs", 
                                      "Harmoni Kylling Ressurs Mc Donalds m/Clostat", 
                                      "Harmoni Kylling Ressurs", 
                                      "Stangekylling start (Landkylling)", 
                                      "Harmoni Kylling Start", 
                                      "Stangekylling Vekst 1 (Landkylling)", 
                                      "Stangekylling Vekst 2 (Landkylling)", 
                                      "Harmoni Kylling Vekst ", 
                                      "Harmoni Kylling Spurt", 
                                      "Harmoni Kylling  Ressurs", 
                                      "Harmoni Kylling Vekst m/koks", 
                                      "Testfor", 
                                      "Harmoni Kylling Start", 
                                      "Harmoni Kylling Vekst ", 
                                      "Harmoni Kylling Spurt", 
                                      "Kromat McKylling 2 Tarmhelse ", 
                                      "Kromat Solvinge 3", 
                                      "Kromat Solvinge 1", 
                                      "Kromat Solvinge 2", 
                                      "Kromat Kylling 2 Tarmhelse", 
                                      "Stangekylling start Rangr", 
                                      "Stangekylling Vekst 1 Ranger", 
                                      "Stangekylling Vekst 2 (Landkylling", 
                                      "Harmoni Kylling Ressurs", 
                                      "Harmoni Kylling Margin", 
                                      "Kromat Moderat Solvinge", 
                                      "Kromat Moderat Solvinge Pluss", 
                                      "Harmoni Kylling Margin (Mc Donalds)", 
                                      "Kromat Moderat Solvinge Start", 
                                      "Kromat Kylling 2 Enkel u/narasin", 
                                      "Miljofor", 
                                      "Kromat Moderat Solvinge Minus", 
                                      "Natura Kylling 2 Soyafri ", 
                                      "Testfor", 
                                      "Toppkylling Netto Mc Donald", 
                                      "Toppkylling 2 (Mc Donald)", 
                                      "Harmoni Kylling Margin", 
                                      "Kromat Kylling 1 u/k", 
                                      "Kromat Kylling 3 Hog u/k", 
                                      "Kromat Kylling 3 Enkel u/k", 
                                      "Kromat McKylling 3 Hog u/k", 
                                      "Kromat Kylling 1 u/k", 
                                      "Kromat Kylling 1 PN", 
                                      "Kromat Kylling 1 u/k", 
                                      "Kromat Kylling 3 Enkel u/k", 
                                      "Kromat Kylling 2 Enkel u/k", 
                                      "Harmoni Kylling Ressurs", 
                                      "Harmoni Kylling Brems", 
                                      "Harmoni Kylling Margin", 
                                      "Testfor", 
                                      "Harmoni Kylling Spurt", 
                                      "Kromat Kylling 2 PN", 
                                      "Kromat Kylling 3 PN", 
                                      "Kromat Kylling 3 Enkel u/k", 
                                      "Kromat Kylling 3 Hog u/k", 
                                      "Testfor", 
                                      "Kromat Kylling 2 m/k", 
                                      "Harmoni Kylling Vekst m/koks", 
                                      "Toppkylling Narasin", 
                                      "Harmoni Kylling Ressurs m/koks", 
                                      "Toppkylling Netto m/koks", 
                                      "Toppkylling Pluss (Mc Donald) m/koks", 
                                      "Harmoni Kylling Start (Mc Donalds)", 
                                      "Harmoni Kylling Vekst  (Mc Donalds)", 
                                      "Harmoni Kylling Ressurs", 
                                      "Harmoni Kylling Spurt (Mc Donalds)", 
                                      "Harmoni Kylling Margin (Mc Donalds)", 
                                      "Harmoni Kylling Vekst (Mc Donalds)", 
                                      "Harmoni Kylling Ressurs Mc Donalds", 
                                      "Harmoni Kylling Ressurs Mc Donalds m/Clostat", 
                                      "Harmoni Kylling Spurt Mc Donalds", 
                                      "Harmoni Kylling Brems (Mc Donalds)", 
                                      "Harmoni Kylling Ressurs Mc Donalds okonomi", 
                                      "Harmoni Kylling Ressurs m/koks", 
                                      "Harmoni Kylling Ressurs Mc Donalds m/koks", 
                                      "Testfor", 
                                      "Toppkylling 1", 
                                      "Toppkylling 2", 
                                      "Toppkylling Netto", 
                                      "Toppkylling 3", 
                                      "Toppkylling Lett", 
                                      "Kromat Kylling 3 Hog u/k", 
                                      "Other"))

}


panel_data <- merge(panel_data, feed_mix, by = 'feed')
table(panel_data$feed_mix)
#Some estimations of causal effects
panel_data$ascite_perc <- 100*panel_data$condition_aceties / panel_data$n_of_chicken

model_data <- panel_data[, c('prod_type', 'growth', 'sqr_growth', 'feed_mix', 'ascite_perc')]
model_data$feed_mix <- as.factor(model_data$feed_mix)

causal_food <-  function(data, index){
  resample <- data[index, ]
  
  inTraining <- createDataPartition(na.omit(resample$ascite_perc), p = 0.8, list = FALSE)
  training <- resample[inTraining,]
  testing  <- resample[-inTraining,]

  rf_model <- randomForest(ascite_perc ~ feed_mix + prod_type + growth + sqr_growth, 
                         data = training, 
                         ntree = 500, 
                         na.action=na.omit)

  test.features <- subset(testing, select=c(feed_mix, prod_type,  growth, sqr_growth))
  test.target <- subset(testing, select=ascite_perc)[,1]
  predictions <- predict(rf_model, newdata = test.features)
  rf_R2 <- cor(test.target, predictions, use = "pairwise.complete.obs") ^ 2

  levels <- levels(model_data$feed_mix)
  # Create a new data frame with all values of feed equal to 8
  new_data <- data.frame(feed_mix = rep("Toppkylling Netto", nrow(model_data)),
                       prod_type = model_data$prod_type,
                       growth = model_data$growth,
                       sqr_growth = model_data$sqr_growth)

  new_data$feed_mix <- factor(new_data$feed_mix, levels = levels)

  # Use the predict() function to make predictions using rf_model
  treat1 <- predict(rf_model, new_data)

  new_data <- data.frame(feed_mix = rep("Kromat Kylling 2 Enkel u/k", nrow(model_data)),
                       prod_type = model_data$prod_type,
                       growth = model_data$growth,
                       sqr_growth = model_data$sqr_growth)
  new_data$feed_mix <- factor(new_data$feed_mix, levels = levels)

  treat2 <- predict(rf_model, new_data)

  new_data <- data.frame(feed_mix = rep("Kromat Kylling 2 Laag u/k", nrow(model_data)),
                       prod_type = model_data$prod_type,
                       growth = model_data$growth,
                       sqr_growth = model_data$sqr_growth)
  new_data$feed_mix <- factor(new_data$feed_mix, levels = levels)

  treat3 <- predict(rf_model, new_data)

  new_data <- data.frame(feed_mix = rep("Toppkylling Lett", nrow(model_data)),
                       prod_type = model_data$prod_type,
                       growth = model_data$growth,
                       sqr_growth = model_data$sqr_growth)
  new_data$feed_mix <- factor(new_data$feed_mix, levels = levels)

  treat4 <- predict(rf_model, new_data)

  return(c(rf_R2, treat1, treat2, treat3, treat4))

}
N <- nrow(model_data) #number of obeservations in dataset
R = 25 #Number of bootstrap replica

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) #Creating dirichelet weights for Bayesian bootstrap
dirichlet_w <- dirichlet / rowSums(dirichlet)

causal_boot <- boot(data=model_data, statistic = causal_food, weights = dirichlet_w, R=rep(1,R))
hist(causal_boot$t0)

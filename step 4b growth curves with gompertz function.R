library(dplyr)
library(dagitty)
library(ggplot2)
library(reshape2)
library(janitor)
library(lavaan)
library(tidyverse)
library(gridExtra)
library(easynls)

setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())
load(file="data_cleaned.Rda")

table(data$chicken_type)

#List number of batches in data
n_distinct(data$id_batch)

long_df <-  subset(data, select = c(id_batch, age, weight))
select <- long_df[long_df$id_batch == 25036,]
select <- subset(select, select = c(age, weight))


plot(select$age, select$weight)
select <- na.omit(select)
#fit data to the gompertz model
model = nlsfit(select, model = 10, start = c(a = 3000, b = 0.5, c = 1))
nlsplot(select, model = 10, start = c(a = 4000, b = 6, c = 0.8), xlab = "Days" , ylab = "Chicken Weight", position = 1)

# growth_results <- list()
# growth_results[[as.character(batch_id)]] <- model$Parameters$weight



start_values <- list(
  c(a = 3000, b = 0.5, c = 1),
  c(a = 3000, b = 1, c = 1),
  c(a = 2000, b = 3, c = 0.2),
  c(a = 3000, b = 5, c = 0.5),
  c(a = 4000, b = 6, c = 0.8)
)
growth_results <- list()

# Loop over unique batch IDs
for (batch_id in unique(long_df$id_batch)) {
  # Subset data for the current batch ID
  select <- subset(long_df, id_batch == batch_id, select = c(age, weight))
  select <- na.omit(select)
  if (nrow(select) > 7) {
    # Try to fit Gompertz model
    for (start in start_values) {
      model <- try(nlsfit(select, model = 10, start = start), silent = TRUE)
      if (!inherits(model, "try-error")) {
        growth_results[[as.character(batch_id)]] <- model$Parameters$weight
      }
      else {
      }
    }
  }
  else {
    
    
  }
}
gompertz_results <- data.frame(id_batch = names(growth_results), numbers = I(growth_results))
gompertz_results <- unnest_wider(gompertz_results, numbers, names_sep = "_")
gompertz_results <- subset(gompertz_results, select = c('id_batch', 'numbers_1', 'numbers_2', 'numbers_3', 'numbers_7'))
gompertz_results$id_batch <- as.numeric(gompertz_results$id_batch)
colnames(gompertz_results) <- c('id_batch', 'asymptote', 'displacement', 'growth_rate', 'growth_r2')
gompertz_results
load(file = "data_cleaned.Rda")

data <- merge(data, gompertz_results, by = 'id_batch')

save(data,file="data_nortura_for_analysis_2.Rda")








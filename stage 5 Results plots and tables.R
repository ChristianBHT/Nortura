library(ggplot2)
library(BSDA)
load("C:/broiler_acites/ascites_case/Results/T_total_boot_feed1.Rda")
load("C:/broiler_acites/ascites_case/Results/T_total_boot_feed2.Rda")
load("C:/broiler_acites/ascites_case/Results/T_total_boot_feed3.Rda")
colnames(T_total_boot_feed1) <- c('CATE', 'mu_0', 'mu_1', 'r2_t', 'r2_c')
treatment <- cbind(T_total_boot_feed1[,1], T_total_boot_feed2[,1], T_total_boot_feed3[,1])
colMeans(treatment)
sd <- apply(treatment, 2, sd)
summary(treatment)
quantile(T_total_boot_feed1[,1], probs = 0.025)
quantile(T_total_boot_feed1[,1], probs = 0.975)
quantile(T_total_boot_feed2[,1], probs = 0.025)
quantile(T_total_boot_feed2[,1], probs = 0.975)
quantile(T_total_boot_feed3[,1], probs = 0.025)
quantile(T_total_boot_feed3[,1], probs = 0.975)

mean(T_total_boot_feed1[,1])
sd(T_total_boot_feed1[,1])
hist(T_total_boot_feed1[,1])
treat1 <- data.frame(T_cate_boot_feed1[,1])
treat2 <- data.frame(T_cate_boot_feed2[,1])
treat3 <- data.frame(T_cate_boot_feed3[,1])
treat1$feed <- "Feed 1"
colnames(treat1) <- c('values', 'feed')
treat2$feed <- "Feed 2"
colnames(treat2) <- c('values', 'feed')
treat3$feed <- "Feed 3"
colnames(treat3) <- c('values', 'feed')
df <- rbind(treat1, treat2, treat3)

plot_total <- ggplot(data = df, aes(x = feed, y = values)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle("T-learner CATE Direct effect")
plot_total

png("C:/broiler_acites/ascites_case/Results/plot_total_Tl.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot_total, col = "lightblue", main = " ", xlab = "Difference in R-Squared", freq = F)
dev.off()
plot_total
hist(T_cate_boot_feed1[,5])
summary(lm(formula = ascites_prev ~  prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight,
           data = data))


#------------------------------------------------------------------
load("C:/broiler_acites/ascites_case/Results/T_cate_boot_feed1.Rda")
load("C:/broiler_acites/ascites_case/Results/T_cate_boot_feed2.Rda")
load("C:/broiler_acites/ascites_case/Results/T_cate_boot_feed3.Rda")
colnames(T_cate_boot_feed1) <- c('CATE', 'mu_0', 'mu_1', 'r2_t', 'r2_c')
summary(T_cate_boot_feed1)
treat1 <- data.frame(T_cate_boot_feed1[,1])
treat2 <- data.frame(T_cate_boot_feed2[,1])
treat3 <- data.frame(T_cate_boot_feed3[,1])
treat1$feed <- "Feed 1"
colnames(treat1) <- c('values', 'feed')
treat2$feed <- "Feed 2"
colnames(treat2) <- c('values', 'feed')
treat3$feed <- "Feed 3"
colnames(treat3) <- c('values', 'feed')
df <- rbind(treat1, treat2, treat3)

plot_total <- ggplot(data = df, aes(x = feed, y = values)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle("T-learner CATE Direct effect")
plot_total

png("C:/broiler_acites/ascites_case/Results/plot_total_Tl.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot_total, col = "lightblue", main = " ", xlab = "Difference in R-Squared", freq = F)
dev.off()
plot_total
hist(T_cate_boot_feed1[,5])
summary(lm(formula = ascites_prev ~  prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight,
           data = data))

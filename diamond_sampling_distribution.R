install.packages("cowplot")
library(cowplot)
library(tidyverse)
library(tidymodels)
library(ggplot2)

#Load diamonds dataset (save as tibble)
diamond_data <- as_tibble(diamonds)

#Selecting for just price variable
diamond_price <- select(diamond_data, price)

#Histogram - distribtion of diamond price
price_dist <- ggplot(diamond_price, aes(price)) + 
  geom_histogram(binwidth = 500) + 
  xlab("Price ($)") + 
  ggtitle("Population Distribution") + 
  theme(text = element_text(size = 15))

#Get the mean, median and standard deviation of diamond price
price_parameters <- diamond_price |>
  summarize(pop_mean = mean(price),
            pop_med = median(price),
            pop_sd = sd(price))

#Get a single random sample of 50 random observations
sample_1 <- diamond_price |>
  rep_sample_n(50)

#Plotting the price distribution of the sample
price_sample1_dist <- ggplot(sample_1, aes(price)) + 
  geom_histogram(binwidth = 500) + 
  xlab("Price ($)") + 
  ggtitle("Sample 1 Distribution") + 
  theme(text = element_text(size = 15))

#Get 40 samples each holding 1500 random observations
price_samples <- rep_sample_n(diamond_price, size = 40, reps = 1500)

#Group by replicate number and calculate mean as point estimates
sample_estimates <- price_samples |>
  group_by(replicate) |>
  summarize(mean_price = mean(price)) 

#Visualize distribution of sample estimates
sampling_distribution <- ggplot(sample_estimates, aes(x = mean_price)) +
  geom_histogram(binwidth = 500) +
  xlab("Sample Mean Price ($)") + 
  ggtitle("Sampling Distribution (n = 40)")

#Try two more times
#Another sample distribution: 1500 observations each of size 20
sampling_distribution_20 <- rep_sample_n(diamond_price, size = 20, reps = 1500)
#Another sample distribution: 1500 observations each of size 100
sampling_distribution_100 <- rep_sample_n(diamond_price, size = 100, reps = 1500)

#Point estimates of sample distribution 20
sampling_distribution_20 <- sampling_distribution_20 |>
  group_by(replicate) |>
  summarize(mean_price = mean(price)) 
#Point estimates of sample distribution 100
sampling_distribution_100 <- sampling_distribution_100 |>
  group_by(replicate) |>
  summarize(mean_price = mean(price)) 

#Plot for sampling distribution 20
sampling_distribution_20_plot <- ggplot(sampling_distribution_20, aes(mean_price)) +
  geom_histogram(binwidth = 500) + 
  xlab("Sample Mean Price ($)") + 
  ggtitle("Sampling Distribution (n = 20)")
#plot for sampling distribution 100
sampling_distribution_100_plot <- ggplot(sampling_distribution_100, aes(mean_price)) + 
  geom_histogram(binwidth = 500) +
  xlab("Sample Mean Price ($)") + 
  ggtitle("Sampling Distribution (n = 100") 
#plot for sampling distribution 40
sampling_distribution_40_plot <- sampling_distribution

#Showing the sampling distribution plots altogether
sampling_distribution_plots <- plot_grid(
  sampling_distribution_20_plot,
  sampling_distribution_40_plot,
  sampling_distribution_100_plot,
  ncol = 1)

#bootstrapping
price_sample_1 <- diamond_price |>
  rep_sample_n(40) |>
  ungroup() |>
  select(price)

price_sample_1_mean <- price_sample_1 |>
  summarize(mean_price = mean(price))

#generating a bootstrap sample from the sample drawn from the population
boot1 <- price_sample_1 |>
  rep_sample_n(size = 40, replace = TRUE, reps = 1)

#generate 6 bootstrap samples
boot6 <- price_sample_1 |>
  rep_sample_n(size = 40, replace = TRUE, reps = 6)

#Plot all 6 bootstrap distributions
boot6_dist <- boot6 |>
  ggplot(aes(price)) +
  geom_histogram(binwidth = 500) + 
  facet_wrap(facets = vars(replicate)) + 
  xlab("Price ($") + 
  ggtitle("6 Bootstrap Samples") + 
  theme(text = element_text(size = 15))

#generate 1000 bootstrap samples
boot1000 <- price_sample_1 |>
  rep_sample_n(size = 40, replace = TRUE, reps = 1000)
#Calculate the means (point_estimate) of each bootstrap sample
boot1000_means <- boot1000 |>
  group_by(replicate) |>
  summarize(mean_price = mean(price))

#Visualizing distribution of point estimates
boot_1000_plot <- ggplot(boot1000_means, aes(mean_price)) + 
  geom_histogram(binwidth = 500) + 
  xlab("Price ($)") + 
  ggtitle("Point estimates (mean) of Bootstrap Samples (n = 1000") + 
  theme(text = element_text(size = 15))

#Getting the confidence level
boot1000_means |>
  select(mean_price) |>
  pull() |>
  quantile(c(0.025, 0.975))
#We are 95% confident the true population mean for diamond prices is between 3250 and 5495 
#The true population average price was 3933. 
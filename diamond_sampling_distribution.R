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


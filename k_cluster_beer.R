install.packages("tidyverse") 
install.packages("tidymodels") 
install.packages("tidyclust")
library(tidyverse)
library(tidymodels)
library(tidyclust)

#Read Craft Beers Data
beer_data <- read_csv("practice_data/beers.csv")

#Data Wrangling
#Filter for non NA rows for ibu, select ibu and abv
beer_data <- beer_data |>
  filter(!is.na(ibu)) |>
  select(ibu, abv)

#Preprocess data
kmeans_recipe <- recipe(~., data = beer_data) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())

#Build model specification for clustering
kmeans_model <- k_means(num_clusters = 2) |>
  set_engine("stats") 

#Build workflow, fit on data
kmeans_fit <- workflow() |>
  add_recipe(kmeans_recipe) |>
  add_model(kmeans_model) |>
  fit(beer_data)

#Add cluster assignment for each point to dataframe beer_data
labelled_beer <- augment(kmeans_fit, beer_data) 

#Visualizing the clusters
cluster_plot <- labelled_beer |>
  ggplot(aes(x = ibu, y = abv, color = .pred_cluster)) + 
  geom_point() +
  labs(x = "International Bittering Units (IBU)", y = "Alcoholic content by volume", color = "Cluster Label") + 
  theme(text = element_text(size = 20))

#Choosing best K for clustering
#Create range of K values
beer_ks <- tibble(num_clusters = seq(from = 1, to = 10, by = 1))

#Model specification without K 
kmeans_model_tune <- k_means(num_clusters = tune()) |>
  set_engine("stats", nstart = 25)

#Combine to new workflow
kmeans_tuning_stats <- workflow() |>
  add_recipe(kmeans_recipe) |>
  add_model(kmeans_model_tune) |>
  tune_cluster(resamples = apparent(beer_data), grid = beer_ks) |>
  collect_metrics(summarize = FALSE)

#Extract total WSSD for each K
tuning_stats <- kmeans_tuning_stats |>
  mutate(total_WSSD = .estimate) |>
  filter(.metric == "sse_within_total") |>
  select(num_clusters, total_WSSD) 

#Now plot K neighbors vs total WSSD
choosing_beer_k <- tuning_stats |>
  ggplot(aes(x = num_clusters, y = total_WSSD)) + 
  geom_point() + 
  geom_line() +
  labs(x = "Number of Clusters", y = "Total WSSD") + 
  theme(text = element_text(size = 15))

#Best K where WSSD starts to decrease less is K = 2
#Conclusion = there are ~2 types of beer in this dataset using the two variables ibu and abv


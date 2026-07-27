install.packages("tidymodels") 
install.packages("tidyverse") 
install.packages("janitor")
library(tidymodels)
library(tidyverse)
library(janitor)

#Read Data & Data Wrangling
#Convert column names from spanish to english
protein_data <- read_csv("practice_data/protein_dataset/proteinas.csv", col_names = c("protein_id", "sequence", "mw", "isoelectric_point",
                                                                                      "hydrophobicity", "total_charge",
                                                                                      "polar_proportion", "nonpolar_proportion",
                                                                                      "sequence_length", "class"), skip = 1)
#Change protein class names from spanish to english
protein_data <- protein_data |>
  mutate(class = recode(class, 
                        "Receptora" = "receptor", 
                        "Estrutural" = "structural", 
                        "Enzima" = "enzyme",
                        "Transporte" = "transport",
                        "Outras" = "other"))


#filter for 100 random datapoints for receptors
receptor_100 <- filter(protein_data, class == "receptor") |>
  slice_sample(n = 100)

receptor_data <- filter(protein_data, class == "receptor")
#checking relationship between sequence length and molecular weight visually
ggplot(receptor_100, aes(x = sequence_length, y = mw)) + 
  geom_point(alpha = 0.4) + 
  labs(x = "Sequence Length", y = "Molecular Weight", title = "Sequence Length vs MW")
#as predicted, a positive correlation

#I want to predict molecular weight given a particular value of sequence length. Use KNN Regression
#Predict molecular weight of receptor with sequence length 100 taking K = 4
predicted_mw <- receptor_data |>
  mutate(diff = abs(100 - sequence_length)) |>
  slice_min(diff, n = 4) |>
  summarize(predicted = mean(mw)) |>
  pull()

#split receptor data into training and testing groups
receptor_split <- initial_split(receptor_data, prop = 0.75, strata = mw)
receptor_train <- training(receptor_split)
receptor_test <- testing(receptor_split)

#Cross validation to choose K for a regression model
#Set model specification
receptor_model <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) |>
  set_engine("kknn") |>
  set_mode("regression") 

#Preprocess data
receptor_recipe <- recipe(mw~sequence_length, data = receptor_train) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors()) 

#Create split for 5-fold cross validation
receptor_vfold <- vfold_cv(receptor_train, v = 5, strata = mw)

#Create workflow
receptor_workflow <- workflow() |>
  add_recipe(receptor_recipe) |>
  add_model(receptor_model)

#Set candidate K values
k_vals = tibble(neighbors = seq(from = 1, to = 100, by = 10))

#Perform cross-validation and collect statistics
receptor_results <- receptor_workflow |>
  tune_grid(resamples = receptor_vfold, grid = k_vals) |>
  collect_metrics()

#Get K value with smallest RMSE
receptor_min <- receptor_results |>
  filter(.metric == "rmse") |>
  slice_min(mean, n=1)

#Make new model specification with proper K value
receptor_best_model <- nearest_neighbor(weight_func = "rectangular", neighbors = 81) |>
  set_engine("kknn") |>
  set_mode("regression") 

#Fit workflow on training data
receptor_best_fit <- workflow() |>
  add_recipe(receptor_recipe) |>
  add_model(receptor_best_model) |>
  fit(data = receptor_train)

#Evaluate regression model on testing data
receptor_summary <- receptor_best_fit |>
  predict(receptor_test) |>
  bind_cols(receptor_test) |>
  metrics(truth = mw, estimate = .pred)

#Same thing, this time using Linear Regression
receptor_split <- initial_split(receptor_data, prop = 0.75, strata = mw)
receptor_train <- training(receptor_split)
receptor_test <- testing(receptor_split)

#Model Specification
linear_model <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

#Preprocess Data
linear_recipe <- recipe(mw ~ sequence_length, data = receptor_train) 

#Fit data on workflow
linear_workflow <- workflow()|>
  add_recipe(linear_recipe) |>
  add_model(linear_model) |>
  fit(data = receptor_train)

#Visualizing Model Prediction on Training Data
receptor_preds <- linear_workflow |>
  predict(receptor_train) |>
  bind_cols(receptor_train)

receptor_predictions <- receptor_preds |>
  ggplot(aes(x = sequence_length, y = mw)) + 
  geom_point(alpha = 0.4) + 
  geom_line(aes(x = sequence_length, y = .pred), colour = "red") + 
  xlab("Sequence Length") + 
  ylab("Molecular Weight") + 
  theme(text = element_text(size = 15))
  
#Evaluate Regression Model using Test Data
receptor_rmse <- linear_workflow |>
  predict(receptor_test) |>
  bind_cols(receptor_test) |>
  metrics(truth = mw, estimate = .pred) |>
  filter(.metric == "rmse") |>
  select(.estimate) |>
  pull()
#RMSPE is 387, indicating that model's predictions on average are off by 387 units of MW

#Visualizing Model Prediction on Testing Data
receptor_test_predictions <- linear_workflow |>
  predict(receptor_test) |>
  bind_cols(receptor_test) 

ggplot(receptor_test_predictions, aes(x = sequence_length, y = mw)) +
  geom_point(alpha = 0.4) + 
  geom_line(aes(x = sequence_length, y = .pred), colour = "blue") + 
  labs(x = "Sequence Length", y = "Molecular Weight", title = "Sequence Length vs Molecular Weigth Prediction on Testing Data") + 
  theme(text = element_text(size = 15)) 

#The equation of line of best fit is y = 11.32 + 118.92x
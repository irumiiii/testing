install.packages("tidyverse") 
install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
install.packages("mlbench") 
library(mlbench) 
install.packages("janitor") 
library(janitor)

#load cancer data and clean the names. ALso change numerical values into integers from ordered factors for analysis
cancer_data <- as_tibble(BreastCancer)
cancer_data <- clean_names(cancer_data)
cancer_data <- cancer_data |>
  mutate(across(where(is.ordered), ~as.numeric(as.character(.))))

#Finding Relationship of mitoses and epith_c_size
ggplot(cancer_data, aes(x = mitoses, y = epith_c_size)) + 
  geom_point(aes(colour = class), alpha = 0.3) + 
  labs(x = "Mitoses", y = "Epithelial Cell Size", colour = "Class of Tumor")

#Finding Relationship of cl thickness and cell shape
ggplot(cancer_data, aes(x = cl_thickness, y = cell_shape)) + 
  geom_jitter(aes(colour = class), alpha = 0.5, size = 2, width = 0.2, height = 0.2) + 
  labs(x = "Cell Thickness", y = "Cell Shape", colour = "Class of Tumor",
       title = "Class of Tumor Classification using Cell Shape & Cell Thickness",
       subtitle = "Breast Cancer Dataset", 
       caption = "Source: mlbench package") + 
  scale_colour_brewer(palette = "Set1") + 
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), expand = c(0,0))


#Classifying tumor depending on cell shape and cell thickness
#Preprocessing data
cancer_recipe <- recipe(class ~ cl_thickness + cell_shape, data = cancer_data) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())

#Add Model Specification
cancer_model <- nearest_neighbor(weight_func = "rectangular", neighbors = 3) |>
  set_engine("kknn") |>
  set_mode("classification") 

#Build Workflow
cancer_workflow <- workflow() |>
  add_recipe(cancer_recipe) |>
  add_model(cancer_model) |>
  fit(data = cancer_data) 

#Several Predictions
observation1 <- tibble(cl_thickness = 2.5, cell_shape = 2.5) #likely benign
predict(cancer_workflow, observation1) #likely benign

observation2 <- tibble(cl_thickness = 7.5, cell_shape = 7.5)
predict(cancer_workflow, observation2) #likely malignant

observation3 <- tibble(cl_thickness = 10, cell_shape = 10) 
predict(cancer_workflow, observation3) #likely malignant

observation4 <- tibble(cl_thickness = 4, cell_shape = 3) 
predict(cancer_workflow, observation4) #likely benign






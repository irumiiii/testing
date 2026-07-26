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
receptor_data <- filter(protein_data, class == "receptor") |>
  slice_sample(n = 100)
#checking relationship between sequence length and molecular weight visually
ggplot(receptor_data, aes(x = sequence_length, y = mw)) + 
  geom_point(alpha = 0.4) + 
  labs(x = "Sequence Length", y = "Molecular Weight", title = "Sequence Length vs MW")
#as predicted, a positive correlation

#I want to predict molecular weight given a particular value of sequence length
#Predict molecular weight of receptor with sequence length 100 taking K = 4

predicted_mw <- receptor_data |>
  mutate(diff = abs(100 - sequence_length)) |>
  slice_min(diff, n = 4) |>
  summarize(predicted = mean(mw)) |>
  pull()
predicted_mw


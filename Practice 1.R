install.packages("tidyverse") 
library(tidyverse)

#loading dataset and changing column names
protein_data <- read_csv("practice_data/protein_dataset/proteinas.csv", col_names = c("protein_id", "sequence",
                                                                                      "molecular_weight", "isoelectric_point",
                                                                                      "hydrophobicity", "total_charge",
                                                                                      "polar_proportion", "nonpolar_proportion",
                                                                                      "sequence_length", "class"), skip = 1)
#Changing the spanish class names into english
protein_data <- protein_data |>
  mutate(class = recode(class,
                        "Receptora" = "Receptor",
                        "Estrutural" = "Structural", 
                        "Enzima" = "Enzyme", 
                        "Transporte" = "Transport",
                        "Outras" = "Other"))

#plotting total charge to nonpolar proportion depending on class of protein
plot_prac <- ggplot(protein_data, aes(x = total_charge, y = nonpolar_proportion)) + 
  geom_point(aes(colour = class, shape = class)) + 
  labs(x = "Total Charge", y = "Nonpolar Proportion", shape = "Class of Protein", colour = "Class of Protein") +
  facet_grid(rows = vars(class)) +
  theme(text = element_text(size = 8))
plot_prac


#creating low/medium/high category column for MW
protein_data <- protein_data |>
  mutate(mw_category = cut(molecular_weight, 
                           breaks = 3,
                           labels = c("Low", "Medium", "High")))

#selecting for MW, MW category, class
mw_category_class <- select(protein_data, molecular_weight, mw_category, class)

bar_chart <- mw_category_class |>
  group_by()
  ggplot(mw_category_class, aes(x = class, y = molecular_weight, fill = mw_category)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Protein Class", y = "Molecular Weight", fill = "MW Category")
bar_chart

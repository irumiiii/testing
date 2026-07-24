install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)

raw_ihc_data <- read_tsv("practice_data/normal_ihc_data.tsv")
raw_ihc_data <- clean_names(raw_ihc_data)

ggplot(data = toothgrowth_data, aes(x = vitamin_dose, y = tooth_length)) + 
  geom_point() + 
  xlab("Vitamin Dose") + 
  ylab("Tooth Length") + 
  theme(text = element_text(size = 10))

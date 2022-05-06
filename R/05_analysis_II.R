# Load libraries ----------------------------------------------------------
library(ggrepel)
library(tinyverse)
library(broom)
library(tidytext) 

# Define functions --------------------------------------------------------
source(file = "R/05_analysis.R")

# Load data ---------------------------------------------------------------
metadata_pca <- read_csv(file = "data/01_metadata.csv") %>% 
  select(subject_id, COVID) %>% 
  distinct() 


matrix_d0_pca <- read_csv(file = "data/02_matrix_d0.csv") %>% 
  select(subject_id, 2:7) %>% 
  distinct()


# Wrangle data ------------------------------------------------------------
# Join two dataset with "subject_id"
covid_pca <- merge(metadata_pca, matrix_d0_pca, by = "subject_id")

# Visualise data ----------------------------------------------------------

covid_pca1 <- covid_pca %>%
  as_tibble %>% 
  prcomp(center = TRUE, scale. = TRUE)

covid_pca1 %>% tidy("pcs")

covid_pca1 %>% tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

covid_pca1 %>% tidy("samples")

covid_pca1_aug <- covid_pca1 %>% augment(covid_pca)

covid_pca_result1 <- covid_pca1_aug %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = "COVID")) +
  geom_point()

########

# Save plot ---------------------------------------------------------------
ggsave(file = "results/04_covid_pca_result1.png") 
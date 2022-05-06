# Load libraries ----------------------------------------------------------
library(broom)

# Define functions --------------------------------------------------------
source(file = "R/05_analysis.R")


# Load data ---------------------------------------------------------------



# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...


# Model data
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------

library(broom)

matrix_d0_scale <- matrix_d0 %>%
  as_tibble %>% 
  select(P01579, P05231, P10145, O95786) %>% 
  prcomp(center = TRUE, scale. = TRUE) 

matrix_d0_scale %>% tidy("pcs")

matrix_d0_scale %>% tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

matrix_d0_scale %>% tidy("samples")


matrix_d0_scale_aug <- matrix_d0_scale %>% augment(metadata)

matrix_d0_scale_aug %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = "COVID")) +
  geom_point()

######### 
matrix_d0_scale<- matrix_d0/merge_df %>% 
  select(P01579, P05231, P10145, O95786) %>% 
  prcomp(center = TRUE, scale. = TRUE)

autoplot(matrix_d0_scale,
         data = metadata, colour = factor(metadata$covid),
         fill = factor(metadata$covid),
         frame = TRUE,
         frame.type = "norm"
) +
  labs(color = "COVID", fill = "COVID", title = "PCA - Protein Expression of Covid +/- patients at timepoint 0 ")
########

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
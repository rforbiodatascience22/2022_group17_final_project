#--------------------------------------------------------------------------------
# PCA COVID +/-
#--------------------------------------------------------------------------------

pca_data <- matrix_d0 %>%
  column_to_rownames("subject_id") %>%
  as_tibble() %>%
  prcomp(center = TRUE, scale. = TRUE)


subject_ids_timepoint0 <- matrix_d0 %>%
  dplyr::select(subject_id) %>%
  as.list() %>%
  unlist() %>%
  unname()

pca_metadata <- metadata %>%
  filter(subject_id %in% subject_ids_timepoint0) %>%
  mutate(COVID = case_when(
    COVID == 0 ~ "negative",
    COVID == 1 ~ "positive",
  )) %>%
  mutate("Age" = case_when(
    Age_cat == 1 ~ "20-34",
    Age_cat == 2 ~ "36-49",
    Age_cat == 3 ~ "50-64",
    Age_cat == 4 ~ "65-79",
    Age_cat == 5 ~ "80+"
  )) %>%
  # mutate to factor for color plotting
  mutate(COVID = factor(COVID), Age = factor(Age))

pca_covid <- autoplot(pca_data,
  data = pca_metadata,
  colour = "COVID"
) +
  scale_color_manual(values = c("#CF98C8", "#7FC9BA")) +
  scale_fill_manual(values = c("#CF98C8", "#7FC9BA")) +
  labs(title = "PCA - Protein Expression of COVID +/- Patients") +
  theme_minimal()
ggsave("results/pca_covid.png")

pca_age <- autoplot(pca_data,
  data = pca_metadata,
  colour = "Age"
) +
  scale_color_manual(values = c("#B659F7", "#CF98C8", "#7FC9BA", "#F3828C", "#6694B6")) +
  scale_fill_manual(values = c("B659F7", "#CF98C8", "#7FC9BA", "#F3828C", "#6694B6")) +
  labs(title = "PCA - Protein Expression of different Age groups") +
  theme_minimal()

ggsave("results/pca_age.png")

# model

#--------------------------------------------------------------------------------
# Boxplots of differential expressed viral response and interferon pathway proteins
#--------------------------------------------------------------------------------

uniprot_inflammatory_maker <- c(
  "P01579", "P05231", "P10145", "O95786", "O14625",
  "P80098", "O15467", "O00175", "Q8IU57"
)

boxplot_inflammatory_markers <- merge_df %>%
  filter(UniProt %in% uniprot_inflammatory_maker) %>%
  select(subject_id, NPX, UniProt, COVID) %>%
  mutate(COVID = case_when(
    COVID == 0 ~ "COVID -",
    COVID == 1 ~ "COVID +",
  )) %>%
  mutate(UniProt = case_when(
    UniProt == "P01579" ~ "Interferon y",
    UniProt == "P05231" ~ "IL-6",
    UniProt == "P10145" ~ "IL-8",
    UniProt == "O95786" ~ "DDX58",
    UniProt == "P80098" ~ "CCL7",
    UniProt == "O15467" ~ "CCL16",
    UniProt == "O00175" ~ "CCL24",
    UniProt == "Q8IU57" ~ "Infterferon lambda1",
    UniProt == "O14625" ~ "CXCL11"
  )) %>%
  ggplot(., aes(x = COVID, y = NPX, fill = COVID)) +
  geom_boxplot(aes(x = COVID, y = NPX, fill = COVID)) +
  facet_wrap(~UniProt, scale = "free_y") +
  scale_color_manual(values = c("#CF98C8", "#7FC9BA")) +
  scale_fill_manual(values = c("#CF98C8", "#7FC9BA")) +
  theme_light() +
  theme(axis.title.x = element_blank())

ggsave("results/boxplot_inflammatory_markers.png")

#--------------------------------------------------------------------------------
# Heatmap of Inflammation Panel on day 0
#--------------------------------------------------------------------------------

heatmap_data <- merge_df %>%
  filter(Timepoint == "D0", Panel == "Inflammation") %>%
  select(NPX, UniProt, subject_id) %>%
  pivot_wider(names_from = UniProt, values_from = NPX, values_fn = mean) %>%
  column_to_rownames("subject_id") %>%
  as.matrix() %>%
  t() %>%
  na.omit()

label_df <- metadata %>%
  filter(subject_id %in% colnames(heatmap_data)) %>%
  mutate(COVID = case_when(
    COVID == 0 ~ "COVID -",
    COVID == 1 ~ "COVID +",
  )) %>%
  mutate("Age" = case_when(
    Age_cat == 1 ~ "20-34",
    Age_cat == 2 ~ "36-49",
    Age_cat == 3 ~ "50-64",
    Age_cat == 4 ~ "65-79",
    Age_cat == 5 ~ "80+"
  )) %>%
  mutate("Acuity" = case_when(
    Acuity_max == 1 ~ "Death",
    Acuity_max == 2 ~ "Intubated",
    Acuity_max == 3 ~ "Hospitalized, supplementary O2",
    Acuity_max == 4 ~ "Hospitalized,  no supplementary O2",
    Acuity_max == 5 ~ "No Hospialziation"
  )) %>%
  # mutate to factor for color plotting
  mutate(COVID = factor(COVID), Acuity = factor(Acuity))


# remove base r
heatmap_labels <- ComplexHeatmap::HeatmapAnnotation(
  "Covid" = pull(label_df, COVID),
  "Age" = pull(lable_df, Age),
  "Severity" = pull(label_df, Acuity),
  col = list(
    "Covid" = c("COVID -" = "#CF98C8", "COVID +" = "#7FC9BA"),
    "Age" = c("20-34" = "white", "36-49" = "lightblue", "50-64" = "blue", "65-79" = "darkblue", "80+" = "black")
  )
)


heatmap_inflammation_d0 <- ComplexHeatmap::Heatmap(heatmap_data,
  name = "Inflammation Panel",
  top_annotation = heatmap_labels,
  show_column_names = FALSE,
  show_row_names = FALSE
)

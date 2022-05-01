# model

#--------------------------------------------------------------------------------
# Boxplots of differential expressed viral response and interferon pathway proteins
#--------------------------------------------------------------------------------

uniprot_inflammatory_maker = c("P01579", "P05231", "P10145", "O95786", "O14625",
                               "P80098", "O15467", "O00175", "Q8IU57")

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
  ggplot(.,aes(x = COVID, y = NPX, fill = COVID)) +
  geom_boxplot(aes(x = COVID, y = NPX, fill = COVID)) +
  facet_wrap(~UniProt, scale = "free_y") +
  theme_light() +
  theme(axis.title.x = element_blank())

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

label_df <- metadata  %>%
  filter(subject_id %in% colnames(heatmap_data))

# remove base r
heatmap_labels <-  ComplexHeatmap::HeatmapAnnotation(
  "Covid" = label_df$COVID,
  "Age" = label_df$Age_cat,
  "Severity" = label_df$Acuity_0
)

heatmap_inflammation_d0 <- ComplexHeatmap::Heatmap(heatmap_data, 
                                                   name = "Inflammation Panel",
                                                   top_annotation = heatmap_labels,
                                                   show_column_names = FALSE,
                                                  show_row_names = FALSE)
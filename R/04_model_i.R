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
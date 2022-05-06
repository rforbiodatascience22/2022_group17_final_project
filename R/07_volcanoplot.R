#-------------------------------------------------------------------------------
# Volcano Plot
#-------------------------------------------------------------------------------

# calculate log2 foldcahgen and pvalue

results_log2fc <- precalc_volcano(matrix_d0 = matrix_d0, metadata = metadata) %>%
  mutate(pvalue_log10 = -log10(value)) %>%
  mutate(Expression = case_when(
    pvalue_log10 > 5 & log2_fc > 0.09 ~ "higher",
    pvalue_log10 > 5 & log2_fc < -0.09 ~ "lower",
    TRUE ~ "no difference"
  )) %>%
  mutate(labeling = case_when(
    pvalue_log10 > 5 & log2_fc > 0.09 ~ Protein,
    pvalue_log10 > 5 & log2_fc < -0.09 ~ Protein,
    TRUE ~ ""
  ))

color_settings <- c("#B65EAF", "#489B80", "black")
names(color_settings) <- c("higher", "lower", "no difference")

results_log2fc %>%
  ggplot(aes(x = log2_fc, y = pvalue_log10, color = Expression, label = labeling)) +
  geom_point() +
  geom_label_repel(aes(label = labeling),
    box.padding   = 0.35,
    point.padding = 0.5,
    segment.color = "grey50"
  ) +
  scale_colour_manual(values = color_settings) +
  labs(
    title = "Volcano Plot \nDifferential Expression of Proteins in COVID infected individuals",
    x = "Log2 Fold Change \nProtein Expression",
    y = "-log10(p-value)"
  ) +
  theme_minimal()

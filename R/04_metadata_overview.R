
#-------------------------------------------------------------------------------
# Barplot COVID - / COVID +
#-------------------------------------------------------------------------------

covid_pos_neg_bar <- metadata %>%
  mutate(COVID = case_when(
    COVID == 0 ~ "COVID -",
    COVID == 1 ~ "COVID +",
  )) %>%
  ggplot(aes(factor(COVID), color = COVID, fill = COVID)) +
  geom_bar(stat = "count", position = "dodge", alpha = 0.9) +
  labs(title = "Study population") +
  scale_color_manual(values = c("#CF98C8", "#7FC9BA")) +
  scale_fill_manual(values = c("#CF98C8", "#7FC9BA")) +
  theme_light() +
  theme(axis.title.x = element_blank())

ggsave("results/metadata_overview_covid.png")

#-------------------------------------------------------------------------------
# Violin Plot
# Covid Acuity and Age
#-------------------------------------------------------------------------------

# 1 = Death
# 2 = Intubated
# 3 = Hospitalized, supplementary O2
# 4 = Hospitalized,  no supplementary O2
# 5 = Not Hospialziation

# the exact age for each individual is not published so we will cheat here a bit
# we only have the age categories

# AGE Categories
# 1 = 20-34
# 2 = 36-49
# 3 = 50-64
# 4 = 65-79
# 5 = 80 +

age_distribution_violin <- metadata %>%
  select(Age_cat, Acuity_max) %>%
  mutate("Age" = case_when(
    Age_cat == 1 ~ sample(20:34, 1),
    Age_cat == 2 ~ sample(36:49, 1),
    Age_cat == 3 ~ sample(50:64, 1),
    Age_cat == 4 ~ sample(65:79, 1),
    Age_cat == 5 ~ sample(80:87, 1)
  )) %>%
  mutate("Acuity" = case_when(
    Acuity_max == 1 ~ "Death",
    Acuity_max == 2 ~ "Intubated",
    Acuity_max == 3 ~ "Hospitalized, supplementary O2",
    Acuity_max == 4 ~ "Hospitalized,  no supplementary O2",
    Acuity_max == 5 ~ "No Hospialziation"
  )) %>%
  ggplot(aes(x = Acuity, y = Age, fill = Acuity, color = Acuity)) +
  geom_violin(trim = FALSE, alpha = 0.3) +
  stat_summary(
    fun.data = mean_sdl,
    geom = "pointrange"
  ) +
  labs(title = "Acuity Levels and Age distribution") +
  scale_color_manual(values = c("#8352A5", "#CF98C8", "#7FC9BA", "#F3828C", "#6694B6")) +
  scale_fill_manual(values = c("#8352A5", "#CF98C8", "#7FC9BA", "#F3828C", "#6694B6")) +
  theme_light() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

ggsave("results/metadata_overview_acuity_age.png")

#-------------------------------------------------------------------------------
# Boxplots neutralisation level in paients with and without covid
#-------------------------------------------------------------------------------

# neutralisation level vs covid status
neut_d0 <- total %>% filter(Day == 0)
neut_d3 <- total %>% filter(Day == 3)
neut_d7 <- total %>% filter(Day == 7)

scat0 <- ggplot(data = neut_d0, aes(x = COVID, y = Percent.Neutralization)) +
  geom_point(color = "darkblue") +
  geom_boxplot(color = "darkred") +
  coord_fixed() +
  labs(title = "Neutralisation level in patients", subtitle = "Day 0") +
  ylab("Neutralisation level [%]") +
  xlab("Covid Status") +
  theme_minimal()
scat0
ggsave("results/neutralisationlvl_covid0.png")

scat3 <- ggplot(data = neut_d3, aes(x = COVID, y = Percent.Neutralization)) +
  geom_point(color = "darkblue") +
  geom_boxplot(color = "darkred") +
  coord_fixed() +
  labs(title = "Neutralisation level in patients with Covid", subtitle = "Day 3") +
  ylab("Neutralisation level [%]") +
  xlab("Covid Status") +
  theme_minimal()
scat3
ggsave("results/neutralisationlvl_covid3.png")

scat7 <- ggplot(data = neut_d7, aes(x = COVID, y = Percent.Neutralization)) +
  geom_point(color = "darkblue") +
  geom_boxplot(color = "darkred") +
  coord_fixed() +
  labs(title = "Neutralisation level in patients with Covid", subtitle = "Day 7") +
  ylab("Neutralisation level [%]") +
  xlab("Covid Status") +
  theme_minimal()
scat7
ggsave("results/neutralisationlvl_covid7.png")

#-------------------------------------------------------------------------------
# Boxplots Acuity vs neutralisation
#-------------------------------------------------------------------------------
Acuity_p <- ggplot(data = Acuity_df, mapping = aes(x = COVID, y = Percent.Neutralization, fill = Acuity_max)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  labs(title = "Neutralisation level in patients", subtitle = "Comparison of neutralisation in case of different hospitalisation levels") +
  ylab("Neutralisation level [%]") +
  xlab("Covid Status") +
  theme_light()

Acuity_p
ggsave("results/acuity_vs_neutrlvl.png")

#-------------------------------------------------------------------------------
# Boxplots neutralisation level in patients with different severity
#-------------------------------------------------------------------------------
# Proportion of patients with neutralization levels over time and by severity level
Acuity_df <- total %>%
  mutate(Severity = case_when(
    Acuity_max == 1 ~ "Severe",
    Acuity_max == 2 ~ "Severe",
    Acuity_max == 3 ~ "Non - Severe",
    Acuity_max == 4 ~ "Non - Severe",
    Acuity_max == 5 ~ "Non - Severe"
  ))
severity_df <- filter(Acuity_df, Day %in% c("0", "3", "7"))

neutralisation_levels <- severity_df %>% mutate(Neutralisation = case_when(
  Percent.Neutralization < 0.25 ~ "0-25",
  Percent.Neutralization >= 0.25 & Percent.Neutralization < 0.50 ~ "25-50",
  Percent.Neutralization >= 0.50 & Percent.Neutralization < 0.75 ~ "50-75",
  Percent.Neutralization >= 0.75 & Percent.Neutralization <= 1 ~ "75-100"
))
neut_severity1 <- ggplot(data = severity_df, mapping = aes(x = Day, y = Percent.Neutralization, fill = Severity)) +
  geom_boxplot() +
  labs(title = "Neutralization levels in non-severe and severe patients over time") +
  ylab("Neutralisation level [%]") +
  theme_minimal()
neut_severity1
ggsave("results/neutralisationsevere_nonsevere.png")

severe_df <- filter(neutralisation_levels, Severity == "Severe") %>%
  group_by(subject_id, Day, Neutralisation)

non_severe_df <- filter(neutralisation_levels, Severity == "Non - Severe") %>%
  group_by(Neutralisation, Day)

neut_severe <- ggplot(data = severe_df, mapping = aes(x = Day, fill = Neutralisation)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  ylab("Proportion of patients") +
  labs(title = "Severe") +
  theme(
    axis.text.y = element_blank(), # remove y axis labels
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )
neut_non_severe <- ggplot(data = severe_df, mapping = aes(x = Day, fill = Neutralisation)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  ylab("") +
  labs(title = "Non-severe") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
  )
neut_severe + neut_non_severe
ggsave("results/neutralisationlvl_propoirtions.png")

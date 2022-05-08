grouped_by_patientid = merge_df %>% group_by(subject_id, Age_cat, HEART, LUNG, Resp_Symp) %>%
  summarise(Count=sum(subject_id))

pl1 = ggplot(data = grouped_by_patientid,
             aes(x = Age_cat,
                 fill = factor(HEART))) +
  geom_histogram(binwidth= 0.5, alpha = 0.5) +
  scale_fill_manual(labels = c("No Heart Disease", "Heart Disease"), values=c("#CC0066", "#FF9999")) +
  labs(title = "Heart disease occurences in patients", x = "Patient Age Group",
       y = "Number of patients",
       fill="Heart Disease") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

pl1

pl2 = ggplot(data = grouped_by_patientid,
             aes(x = Age_cat,
                 fill = factor(LUNG))) +
  geom_histogram(binwidth= 0.5, alpha = 0.5) +
  scale_fill_manual(labels = c("No Lung Disease", "Lung Disease"), values=c("#CC0066", "#FF9999")) +
  labs(title = "Lung disease occurences in patients",
       x = "Patient Age Group",
       y = "Count",
       fill = "Lung Disease") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")
pl2

pl3 = ggplot(data = merge_df,
             aes(x = Age_cat,
                 fill = factor(Resp_Symp))) +
  geom_histogram(binwidth= 0.5, alpha = 0.5) +
  scale_fill_manual(labels = c("no respiratory Symptoms", "respiratory symptoms"),values=c("#CC0066", "#FF9999")) +
  labs(title = "Respiratory symptoms occurences in patients",
       x = "Patient Age Group",
       y = "Count",
       fill = "Heart disease") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")
pl3


# Compute the frequency
counts0 <- descriptive_df %>%
  group_by(abs_neut_0_cat) %>%
  summarise(counts = n())

counts3 <- descriptive_df %>%
  group_by(abs_neut_3_cat) %>%
  summarise(counts = n())

counts7 <- descriptive_df %>%
  group_by(abs_neut_7_cat) %>%
  summarise(counts = n())

absolute_neut_0 <- ggplot(data=counts0, aes(x=abs_neut_0_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 0", y="counts") + scale_fill_brewer(palette = "Blues")
absolute_neut_3 <- ggplot(data=counts3, aes(x=abs_neut_3_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 3", y="counts") + scale_fill_distiller(palette = "Blues")
absolute_neut_7 <- ggplot(data=counts7, aes(x=abs_neut_7_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 7", y="counts") + scale_fill_distiller(palette = "Blues")

absolute_neut_0 + absolute_neut_3 + absolute_neut_7

# Compute the frequency

counts0 <- descriptive_df %>%
  group_by(abs_lymph_0_cat) %>%
  summarise(counts = n())

counts3 <- descriptive_df %>%
  group_by(abs_lymph_3_cat) %>%
  summarise(counts = n())

counts7 <- descriptive_df %>%
  group_by(abs_lymph_7_cat) %>%
  summarise(counts = n())

absolute_lymph_0 <- ggplot(data=counts0, aes(x=abs_lymph_0_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 0", y="counts") + scale_fill_brewer(palette = "Blues")
absolute_lymph_3 <- ggplot(data=counts3, aes(x=abs_lymph_3_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 3", y="counts") + scale_fill_distiller(palette = "Blues")
absolute_lymph_7 <- ggplot(data=counts7, aes(x=abs_lymph_7_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 7", y="counts") + scale_fill_distiller(palette = "Blues")

absolute_lymph_0 + absolute_lymph_3 + absolute_lymph_7

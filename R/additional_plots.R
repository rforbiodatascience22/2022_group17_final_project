
#age distribution
merge_df %>%
  ggplot(mapping = aes(x = Age_cat))+
  geom_histogram(binwidth = 0.5)+
  labs()

#NPX distribution
npx_distribution <- ggplot(data=merge_df, aes(x=NPX)) +
  geom_histogram(alpha=0.5) + 
  labs(title='NPX distribution') +
  facet_wrap(~BMI_cat)
npx_distribution

pl1 = ggplot(data = merge_df,
             aes(x = Age_cat,
                 color = factor(HEART))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_histogram(binwidth= 0.5, alpha = 0.5) +
  scale_fill_manual(labels = c("No Heart Disease", "Heart Disease")) +
  labs(x = "Patient Age Group",
       y = "Count",
       fill = "Heart disease") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

pl1

pl2 = ggplot(data = merge_df,
             aes(x = Age_cat,
                 color = factor(LUNG))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_histogram(binwidth= 0.5, alpha = 0.5) +
  scale_fill_manual(labels = c("No Lung Disease", "Lung Disease")) +
  labs(x = "Patient Age Group",
       y = "Count",
       fill = "Lung Disease") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")
pl2

pl3 = ggplot(data = merge_df,
             aes(x = Age_cat,
                 color = factor(Resp_Symp))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_histogram(binwidth= 0.5, alpha = 0.5) +
  scale_fill_manual(labels = c("no respiratory Symptoms", "respiratory symptoms")) +
  labs(x = "Patient Age Group",
       y = "Count",
       fill = "Heart disease") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")
pl3
library("patchwork")

# Compute the frequency
library(dplyr)
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

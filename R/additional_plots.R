#aqge distribution
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


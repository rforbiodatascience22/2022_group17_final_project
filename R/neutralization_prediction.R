#
total <- merge(merge_df, neut_assay_clean, by="subject_id")

total <- total %>%
  mutate(COVID = case_when(
    COVID == 0 ~ "Negative",
    COVID == 1 ~ "Positive",
  ))

#neutralisation level vs covid status
neut_d0 <- total %>% filter(Day==0)
neut_d3 <- total %>% filter(Day==3)
neut_d7 <- total %>% filter(Day==7)

scat0 <- ggplot(data=neut_d0, aes(x=COVID, y=Percent.Neutralization)) + geom_point(color='darkblue') + geom_boxplot(color='darkred') +
  coord_fixed()+
  labs(title = "Neutralisation level in patients with Covid", subtitle = "Day 0") +
  ylab('Neutralisation level [%]') + 
  xlab('Covid Status')
scat0

scat3 <- ggplot(data=neut_d3, aes(x=COVID, y=Percent.Neutralization)) + geom_point(color='darkblue') + geom_boxplot(color='darkred') +
  coord_fixed()+
  labs(title = "Neutralisation level in patients with Covid", subtitle = "Day 3") +
  ylab('Neutralisation level [%]') + 
  xlab('Covid Status')
scat3

scat7 <- ggplot(data=neut_d7, aes(x=COVID, y=Percent.Neutralization)) + geom_point(color='darkblue') + geom_boxplot(color='darkred') +
  coord_fixed()+
  labs(title = "Neutralisation level in patients with Covid", subtitle = "Day 7") +
  ylab('Neutralisation level [%]') + 
  xlab('Covid Status')
scat7

#Comparing severe and non severe patients and theirs neutralisation levelv

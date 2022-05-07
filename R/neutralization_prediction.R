descriptive_df <- merge_df %>%
  mutate(COVID = case_when(
    COVID == 0 ~ "COVID -",
    COVID == 1 ~ "COVID +",
  ))
descriptive_df <-  descriptive_df  %>%
  mutate("Age_cat" = case_when(
    Age_cat == 1 ~ "20-34",
    Age_cat == 2 ~ "36-49",
    Age_cat == 3 ~ "50-64",
    Age_cat == 4 ~ "65-79",
    Age_cat == 5 ~ "80+"
  ))

descriptive_df <-  descriptive_df  %>%
  mutate("BMI_cat" = case_when(
    BMI_cat == 0 ~ "18.5 underweight",
    BMI_cat == 1 ~ "8.5-24.9 normal",
    BMI_cat == 2 ~ "25.0-29.9 overweight",
    BMI_cat == 3 ~ "30.0-39.9 obese",
    BMI_cat == 4 ~ "40 severely obese",
    BMI_cat == 5 ~ "unknown"
  ))

descriptive_df <-  descriptive_df  %>%
  mutate("abs_neut_0_cat" = case_when(
    abs_neut_0_cat == 1 ~ '0-0.99',
    abs_neut_0_cat == 2 ~ '1.0-3.99',
    abs_neut_0_cat == 3 ~ '4.0-7.99',
    abs_neut_0_cat == 4 ~ '8.0-11.99',
    abs_neut_0_cat == 5 ~ '12+'
  ))


descriptive_df <-  descriptive_df  %>%
  mutate("abs_neut_3_cat" = case_when(
    abs_neut_3_cat  == 1 ~ '0-0.99',
    abs_neut_3_cat  == 2 ~ '1.0-3.99',
    abs_neut_3_cat  == 3 ~ '4.0-7.99',
    abs_neut_3_cat  == 4 ~ '8.0-11.99',
    abs_neut_3_cat  == 5 ~ '12+'
  ))

#descriptive_df %>% drop_na(abs_neut_7_cat)

descriptive_df <-  descriptive_df  %>%
  mutate("abs_neut_7_cat" = case_when(
    abs_neut_7_cat  == 1 ~ '0-0.99',
    abs_neut_7_cat  == 2 ~ '1.0-3.99',
    abs_neut_7_cat  == 3 ~ '4.0-7.99',
    abs_neut_7_cat  == 4 ~ '8.0-11.99',
    abs_neut_7_cat  == 5 ~ '12+',
  ))

library("patchwork")

# Compute the frequency
library(dplyr)
counts0 <- descriptive_df %>%
  group_by(abs_neut_0_cat, BMI_cat) %>%
  summarise(counts = n())

counts3 <- descriptive_df %>%
  group_by(abs_neut_3_cat, COVID) %>%
  summarise(counts = n())

counts7 <- descriptive_df %>%
  group_by(abs_neut_7_cat, COVID) %>%
  summarise(counts = n())

absolute_neut_0 <- ggplot(data=counts0, aes(x=abs_neut_0_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 0", y="counts")+ scale_fill_distiller(palette = "Blues")
absolute_neut_3 <- ggplot(data=counts3, aes(x=abs_neut_3_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 3", y="counts")+ scale_fill_distiller(palette = "Blues")
absolute_neut_7 <- ggplot(data=counts7, aes(x=abs_neut_7_cat, y=counts)) + geom_bar(position="stack", stat="identity")+
  labs(x="Absulute neutrophil count day 7", y="counts")+ scale_fill_distiller(palette = "Blues")

absolute_neut_0 + absolute_neut_3 + absolute_neut_7


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

descriptive_dfn0 <-  descriptive_df  %>%
  mutate("abs_neut_0_cat" = case_when(
    abs_neut_0_cat == 1 ~ '0-0.99',
    abs_neut_0_cat == 2 ~ '1.0-3.99',
    abs_neut_0_cat == 3 ~ '4.0-7.99',
    abs_neut_0_cat == 4 ~ '8.0-11.99',
    abs_neut_0_cat == 5 ~ '12+'
  ))

descriptive_dfn3 <-  descriptive_dfn0  %>%
  mutate("abs_neut_3_cat" = case_when(
    abs_neut_3_cat  == 1 ~ '0-0.99',
    abs_neut_3_cat  == 2 ~ '1.0-3.99',
    abs_neut_3_cat  == 3 ~ '4.0-7.99',
    abs_neut_3_cat  == 4 ~ '8.0-11.99',
    abs_neut_3_cat  == 5 ~ '12+'
  ))

descriptive_dfn7 <-  descriptive_dfn3  %>%
  mutate("abs_neut_7_cat" = case_when(
    abs_neut_7_cat  == 1 ~ '0-0.99',
    abs_neut_7_cat  == 2 ~ '1.0-3.99',
    abs_neut_7_cat  == 3 ~ '4.0-7.99',
    abs_neut_7_cat  == 4 ~ '8.0-11.99',
    abs_neut_7_cat  == 5 ~ '12+',
  ))

descriptive_df0 <-  descriptive_dfn7  %>%
  mutate("abs_lymph_0_cat" = case_when(
    abs_lymph_0_cat == 1 ~ '0-0.49',
    abs_lymph_0_cat == 2 ~ '0.5-0.99',
    abs_lymph_0_cat == 3 ~ '1.0-1.49',
    abs_lymph_0_cat == 4 ~ '1.5-1.99',
    abs_lymph_0_cat == 5 ~ '2+'
  ))

descriptive_df3 <-  descriptive_df0  %>%
  mutate("abs_lymph_3_cat" = case_when(
    abs_lymph_3_cat == 1 ~ '0-0.49',
    abs_lymph_3_cat == 2 ~ '0.5-0.99',
    abs_lymph_3_cat == 3 ~ '1.0-1.49',
    abs_lymph_3_cat == 4 ~ '1.5-1.99',
    abs_lymph_3_cat == 5 ~ '2+'
  ))

descriptive_df7 <-  descriptive_df3  %>%
  mutate("abs_lymph_0_cat" = case_when(
    abs_lymph_7_cat == 1 ~ '0-0.49',
    abs_lymph_7_cat == 2 ~ '0.5-0.99',
    abs_lymph_7_cat == 3 ~ '1.0-1.49',
    abs_lymph_7_cat == 4 ~ '1.5-1.99',
    abs_lymph_7_cat == 5 ~ '2+'
  ))

total <- merge(descriptive_df, neut_assay_clean, by="subject_id")

Acuity_df <- total %>% mutate(Acuity_max = case_when(
  Acuity_max == 1 ~ "Death",
  Acuity_max == 2 ~ "Intubated",
  Acuity_max == 3 ~ "Hospitalized,supp.O2",
  Acuity_max == 4 ~ "Hospitalized,no supp. O2",
  Acuity_max == 5 ~ "No Hospialziation"
))


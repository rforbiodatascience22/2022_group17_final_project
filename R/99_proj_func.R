# function ro rescale values
range01 <- function(x) {
  (
    scaled <- x - min(x)) / (max(x) - min(x))
  return(scaled)
}

#' Calculates the log2 foldchange and the p-value between COVID positive and
#' negative patients
#' @param matrix_d0 cleaned matrix with columns as rows and subject_ids as rows
#' @param metadata metadata dataframe that matches the matrix containing
#' information about covid
#' @return dataframe with log2_fold change and pvalue for each protein

precalc_volcano <- function(matrix_d0, metadata, log2_foldchange = TRUE) {
  # npx contains negative and positive values to calculate log2 fold change we
  # need positive values
  # get subject ids for grouping
  covid_0 <- metadata %>%
    filter(COVID == 0, subject_id %in% subject_ids_timepoint0) %>%
    pull(subject_id) %>%
    as.character()

  covid_1 <- metadata %>%
    filter(COVID == 1, subject_id %in% subject_ids_timepoint0) %>%
    pull(subject_id) %>%
    as.character()

  matrix_d0 %<>%
    mutate(subject_id = case_when(
      subject_id %in% covid_0 ~ "negative",
      subject_id %in% covid_1 ~ "positive",
    ))
  if (log2_foldchange) {
    scaled_df <- range01(matrix_d0[-1]) %>%
      add_column(group = pull(matrix_d0, subject_id))
    scaled_df %<>%
      pivot_longer(!group, names_to = "Protein", values_to = "NPX") %>%
      pivot_wider(names_from = group, values_from = NPX, values_fn = mean) %>%
      mutate("log2_fc" = log2(positive / negative))
  } else {
    # actually not a scaled df but nm
    scaled_df <- matrix_d0 %>%
      rename(group = subject_id) %>%
      pivot_longer(!group, names_to = "Protein", values_to = "NPX") %>%
      pivot_wider(names_from = group, values_from = NPX, values_fn = mean) %>%
      mutate("fc" = negative / positive)
  }

  # calculate p-values
  result <- lapply(matrix_d0[-1], function(x) t.test(x ~ matrix_d0$subject_id)$p.value)

  # merge everything in one df
  pvalue_df <- enframe(result) %>%
    unnest_longer(value) %>%
    mutate("Protein" = name)
  final_df <- merge(scaled_df, pvalue_df) %>%
    na.omit()
  return(final_df)
}

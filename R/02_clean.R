#-------------------------------------------------------------------------------
# Clean data
#-------------------------------------------------------------------------------

# save information in list for presentation and report
data_overview <- c()

data_overview["n_patients"] <- metadata %>%
  select(subject_id) %>%
  unique() %>%
  nrow()

# find columns with missing data
# not all subjects have information regarding different biomarkers ldh,...
# but we will keep metadata as this as the most important information (subject_id, COVID) is complete
data_overview["metadata_NA_columns"] <- colSums(is.na(metadata))

data_overview["n_proteins"] <- proteomics %>%
  select(UniProt) %>%
  unique() %>%
  nrow() 

# Quality Control Olink Proximity Extension Assay (PEA)
# not all samples passed the Quality control, those samples are removed
data_overview["QC"] <- proteomics %>% 
  select(QC_Warning) %>% 
  table() %>%
  as.data.frame()
proteomics %<>% 
  filter(QC_Warning == "PASS") %>%
  drop_na(subject_id)

#-------------------------------------------------------------------------------
# Join data
#-------------------------------------------------------------------------------

merge_df <- merge(metadata, proteomics, by = "subject_id")

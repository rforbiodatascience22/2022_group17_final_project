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
  table()

# the Olink method consists of various assays targeting different biomarkers
# Inflammation, Oncology, etc...
# since the dataset is a combination of several panels and one biomarker can be
# detected by several Assays, we decided to focus on the Inflammation Panel
proteomics %<>% 
  filter(QC_Warning == "PASS", Panel == "Inflammation") %>%
  drop_na(subject_id) %>%
  unite("sample_id", subject_id:Timepoint, remove = FALSE)

#-------------------------------------------------------------------------------
# Join data
#-------------------------------------------------------------------------------

# not the definition of clean data 
merge_df <- merge(metadata, proteomics, by = "subject_id")

# clean matrix
# rows are subjects 
# values is NPX
# columns unique proteins
matrix_d0 <- merge_df %>% 
  filter(Timepoint == "D0") %>%
  select(NPX, UniProt, subject_id) %>%
  pivot_wider(names_from = UniProt, values_from = NPX, values_fn = mean) %>%
  na.omit()

# NPX, Normalized Protein eXpression, is Olink’s arbitrary unit which is in Log2 
# scale. It is calculated from Ct values and data pre-processing (normalization) 
# is performed to minimize both intra- and inter-assay variation. NPX data allows 
# users to identify changes for individual protein levels across their sample set, 
# and then use this data to establish protein signatures.

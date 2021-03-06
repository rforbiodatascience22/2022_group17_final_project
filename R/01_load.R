# (data can not be directly pulled from the page unless you start using web scraping)
# unzip folder
# folder includes three files
unzip("_raw/MGH_Olink_COVID_Apr_27_2021.zip", exdir = "_raw/")

# load metadata
metadata <- read.csv("_raw/MGH_Olink_COVID_Apr_27_2021/MGH_COVID_Clinical_Info.txt", sep = ";")
# load proteomics
proteomics <- read.csv("_raw/MGH_Olink_COVID_Apr_27_2021/MGH_COVID_OLINK_NPX.txt", sep = ";")
# load neutralisation assay data
neut_data <- read_excel("_raw/neutr_assay_data.xlsx")

# Write data --------------------------------------------------------------
write.csv(
  x = metadata,
  file = "data/01_metadata.csv"
)
write_csv(
  x = proteomics,
  file = "data/01_proteomics.csv"
)
write_csv(
  x = neut_data,
  file = "data/01_neut_assay.csv"
)

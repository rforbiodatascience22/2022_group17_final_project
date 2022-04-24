# (data can not be directly pulled from the page unless you start using web scraping)
# unzip folder
# folder includes three files
unzip("_raw/MGH_Olink_COVID_Apr_27_2021.zip", exdir="_raw/")

# load metadata
metadata <- read.csv("_raw/MGH_Olink_COVID_Apr_27_2021/MGH_COVID_Clinical_Info.txt", sep = ";")
# load proteomics
proteomics <- read.csv("_raw/MGH_Olink_COVID_Apr_27_2021/MGH_COVID_OLINK_NPX.txt", sep = ";")
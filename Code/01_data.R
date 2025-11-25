library(tidyverse)
library(haven)
library(openxlsx)

### load data
folder <- "data/adam"
files <- list.files(folder, pattern = "\\.(sas7bdat|xlsx)$", full.names = TRUE)
for (f in files) {
  name <- tools::file_path_sans_ext(basename(f))%>%tolower()
  if(str_detect(f, 'xlsx')){
    df <- read.xlsx(f)%>%as_tibble()
  }else{
    df <- read_sas(f)%>%as_tibble()
  }
  df%>%
    mutate(AGE = as.numeric(AGE),
           TRT01P = factor(TRT01P, levels = c('试验组', '安慰剂组'), 
                           labels = c('试验组', '安慰剂组')),
           TRT01A = factor(TRT01A, levels = c('试验组', '安慰剂组'), 
                           labels = c('试验组', '安慰剂组')))
  assign(name, df)
}

crf_database_FolderModule <- read.xlsx("Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                          sheet = "FolderModule")%>%mutate(value = "X")%>%
  select(c("folderOID", "moduleOID", "folderModuleName", "value"))%>%as_tibble()

crf_database_folder <- read.xlsx("Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                                 sheet = "Folder")%>%mutate(value = "X")%>%
  select(c("folderOID", "folderName"))%>%as_tibble()
crf_field_full <- read.xlsx("../Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", sheet = "Field")%>%as_tibble()
crf_dictionary_full <- read.xlsx("../Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                                 sheet = "DataDictionaryEntry")%>%as_tibble()
crf_module_full <- read.xlsx("../Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                             sheet = "Module")%>%as_tibble()
crf_filed_sliced <- crf_field_full%>%
  select("formOID", "fieldOID", "fieldName", "controlType", "unit", "dataFormat", "dataDictionaryOID")

crf_dictionary_sliced <- crf_dictionary_full%>%select(1:4)

crf_database_FolderModule_merge <- crf_database_FolderModule%>%
  left_join(crf_database_folder)

mock_ready_crf <- crf_filed_sliced%>%
  left_join(crf_dictionary_sliced, by = join_by(dataDictionaryOID == dataDictionaryOID))


crf_all <- crf_database_FolderModule%>%select(1:3)%>%
  left_join(mock_ready_crf, join_by(moduleOID==formOID))%>%
  write.xlsx("Data/crf_all.xlsx")








crf_database_FolderModule%>%pivot_wider(names_from = folderOID, id_cols = c(moduleOID, folderModuleName), values_from = value)%>%
  view

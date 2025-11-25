library(tfrmt)
library(tidyverse)
library(openxlsx)
library(gt)

crf_field_full <- read.xlsx("../Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", sheet = "Field")%>%as_tibble()
crf_dictionary_full <- read.xlsx("../Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                                 sheet = "DataDictionaryEntry")%>%as_tibble()
crf_module_full <- read.xlsx("../Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                             sheet = "Module")%>%as_tibble()
crf_filed_sliced <- crf_field_full%>%
  select("formOID", "fieldOID", "fieldName", "controlType", "unit", "dataFormat", "dataDictionaryOID")
crf_dictionary_sliced <- crf_dictionary_full%>%select(1:4)
mock_ready_crf <- crf_filed_sliced%>%
  left_join(crf_dictionary_sliced, by = join_by(dataDictionaryOID == dataDictionaryOID))

crf_database_FolderModule <- read.xlsx("../Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                                       sheet = "FolderModule")%>%mutate(value = "X")%>%
  select(c("folderOID", "moduleOID", "folderModuleName", "value"))
crf_database_folder <- read.xlsx("../Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                                 sheet = "Folder")%>%mutate(value = "X")%>%
  select(c("folderOID", "folderName"))
crf_database_FolderModule_merge <- crf_database_FolderModule%>%left_join(crf_database_folder)
date_format = "YYYY-MM-DD"


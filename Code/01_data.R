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

crf_database <- read.xlsx("Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx", 
                          sheet = "FolderModule")%>%mutate(value = "X")
crf_database%>%pivot_wider(names_from = folderOID, id_cols = c(moduleOID, folderModuleName), values_from = value)%>%
  view





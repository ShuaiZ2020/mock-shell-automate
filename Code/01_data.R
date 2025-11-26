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
  left_join(mock_ready_crf, join_by(moduleOID==formOID))


  
testname_remake <- function(df_input, y){

  df_remove_age <- df_input%>%rows_delete(tibble(fieldName="隐藏年龄"), unmatched = "ignore")
  folderOID <- y$folderOID 
  moduleOID <-  y$moduleOID
  folderModuleName <- df_remove_age$folderModuleName%>%unique()
  if(str_detect(moduleOID, "LB")){
    df_keep <- df_remove_age%>%filter(str_detect(fieldOID,"LB"))
    test_name <- df_remove_age%>%filter(!str_detect(fieldOID,"LB"))%>%pull(fieldName)%>%unique()

    df_add <- expand_grid(fieldName = "实验室范围名称",
                          controlType  = "水平单选框", folderModuleName = folderModuleName, itemDataString = test_name)
    df_res <- bind_rows(df_keep, df_add)
  }else if(str_detect(moduleOID, "VS1")){
    df_keep <- df_remove_age%>%filter(str_detect(fieldOID,"VS"))
    test_name <- df_remove_age%>%filter(controlType=="文本框")%>%pull(fieldName)
    df_add <- expand_grid(fieldName = "项目",
                          controlType  = "水平单选框", 
                          folderModuleName = folderModuleName, 
                          itemDataString = test_name)%>%
      bind_rows(tibble(fieldName = "单位",
                       folderModuleName = folderModuleName, 
                       controlType  = "水平单选框"))%>%
      bind_rows(
        expand_grid(
          fieldName = "临床意义判定",
          controlType  = "水平单选框",
          folderModuleName = folderModuleName, 
          itemDataString = c("正常","异常无临床意义", "异常有临床意义","未查"
          )
        )
      )
    df_res <- bind_rows(df_keep, df_add)
    
    
  }
  else{
    df_res <- df_remove_age
  }
  df_res
}

crf_interm <- crf_all%>%group_by(folderOID, moduleOID)%>%group_modify(\(x,y) testname_remake(x,y))%>%
  ungroup()
crf_interm <- crf_interm%>%mutate(itemDataString = ifelse(str_detect(fieldName, "日期"), "YYYY-MM-DD", itemDataString))
crf_interm%>%write.xlsx("Data/crf_interm.xlsx")

crf_interm%>%distinct(moduleOID, folderModuleName)%>%
  write.xlsx("Data/moduleOID.xlsx")
df_input <- crf_all%>%filter(folderOID == 'V1', moduleOID=="SV")
df_moduleoid <- read.xlsx("Data/moduleOID.xlsx")%>%as_tibble()
crf_interm%>%left_join(df_moduleoid, join_by(moduleOID))%>%view

## event flow
crf_database_FolderModule%>%pivot_wider(names_from = folderOID, id_cols = c(moduleOID, folderModuleName), values_from = value)%>%
  view

library(tidyverse)
library(openxlsx)

crf_filed_sliced <- crf_field_full%>%
  select("formOID", "fieldOID", "fieldName", "controlType", "unit", "dataFormat", "dataDictionaryOID")
crf_dictionary_sliced <- crf_dictionary_full%>%select(1:4)

make_mock_shell <- function(formoid, row_num=3, date_format = "YYYY-MM-DD", 
                            col_combine = c(),
                            col_pre = c("受试者随机号", "随机组别"), replace_string = F, 
                            col_remove = c("单位", "备注"), filter_yes = "",
                            seed = "123"){
  set.seed(seed)
  mock_ready_crf <- crf_filed_sliced%>%filter(formOID==formoid)%>%
    left_join(crf_dictionary_sliced, by = join_by(dataDictionaryOID == dataDictionaryOID))
  fieldnames <- mock_ready_crf$fieldName%>%unique()%>%  setdiff(., col_remove)

  create_column<- function(fieldname, row_num, date_format){
    cont.type = mock_ready_crf%>%filter(fieldName==fieldname)%>%pull(controlType)%>%head(1)
    if(cont.type=="水平单选框"){
      res <- mock_ready_crf%>%filter(fieldName==fieldname)%>%pull(itemDataString)%>%sample(row_num, replace = T)
    }else if(cont.type=="垂直单选框"){
      res <- mock_ready_crf%>%filter(fieldName==fieldname)%>%pull(ordinal)%>%sample(row_num, replace = T)%>%
        as.character()
    }
    else if(cont.type == "日期框"){
      res <- sample(x = date_format, row_num, replace = T)
    }else{
      res <- sample(x = "xxx", row_num, replace = T)
    }
    if(replace_string){
      res <- case_when(str_detect(res, "其他，请")~"其他/xxxx",
                       .default = res)
      res <- case_when(str_detect(res, "否，请")~"否/xxxx",
                       .default = res)
    }
    return(c(res))
  }
  field_w_value <- lapply(fieldnames, \(x) create_column(fieldname = x, row_num=row_num, date_format = date_format))
  setNames(field_w_value, fieldnames)
  shell_body <- as.data.frame(field_w_value, col.names = fieldnames, check.names =F)
  if(length(col_pre)==0){
    res_tab <- shell_body
  }else{
    field_pre <- lapply(col_pre, \(x) c(sample("xxx", row_num, replace = T)))
    shell_pre <- as.data.frame(field_pre, col.names = col_pre, check.names =F)
    res_tab <- cbind(shell_pre, shell_body)
  }
  if(length(col_combine)!=0){
    new_colname <- paste0(col_combine,collapse = "\n/")
    res_tab <- res_tab%>%unite(!!new_colname,
                    col_combine, 
                    sep = "\n/",
                    remove = T)
  }
  if(filter_yes!=""){
    res_tab <- res_tab%>%filter(!!sym(filter_yes)=="是")%>%
      select(-!!sym(filter_yes))
  }
  res_tab[nrow(res_tab)+2,] <- "......"
  return(res_tab)
}

add_footnote <- function(footnote, ismedDrug = F, iswhodrug = F){
  if(ismedDrug){
    footnote <- c(footnote,
                  "SOC：系统器官分类，PT：首选术语",
                  "依据&MedDRA.进行编码")
  }
  
  if(iswhodrug){
    footnote <- c(footnote,
                  "ATC2：解剖学、治疗学及化学分类法2级，PN：首选药名",
                  "依据&DrugDIR.进行编码")
  }

  if(length(footnote)!=0){
    res <- paste0(c("::: {custom-style='Footnote'}",  
                    paste0(footnote, collapse =  "\n\n"), 
                    ":::",
                    "\n<br>\n"), 
                  collapse  = "\n")
    return(res)
  }else{
    return("<br>\n")
  }
}

add_saspath <- function(){
  paste0(c("::: {custom-style='路径'}", 
           
           "&lt;路径&gt;/< XXX.SAS>    draft YYYY-MM-DD HH:MM", 
           ":::",
           "<br>",
           "<br>"), 
         collapse  = "\n")
}

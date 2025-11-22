library(tidyverse)
library(openxlsx)

crf_filed_sliced <- crf_field_full%>%
  select("formOID", "fieldOID", "fieldName", "controlType", "unit", "dataFormat", "dataDictionaryOID")
crf_dictionary_sliced <- crf_dictionary_full%>%select(1:4)
mock_ready_crf <- crf_filed_sliced%>%
  left_join(crf_dictionary_sliced, by = join_by(dataDictionaryOID == dataDictionaryOID))



make_mock_shell <- function(formoid, row_num=3, 
                            col_combine = c(), col_pre = c("受试者随机号", "随机组别"), col_remove = c("单位", "备注"),
                            replace_string = F, filter_yes = "", seed = "123",
                            ismedDrug = F, iswhodrug = F,
                            fold_col = 
                            ){
  set.seed(seed)
  footnote_vec <- c()
  mock_ready_crf <- mock_ready_crf%>%filter(formOID==formoid)
  fieldnames <- mock_ready_crf$fieldName%>%unique()%>%setdiff(., col_remove)
  create_column<- function(fieldname, row_num, date_format, replace_string = F){
    mock_ready_crf_filename <- mock_ready_crf%>%filter(fieldName==fieldname)
    cont.type = mock_ready_crf_filename%>%pull(controlType)%>%head(1)
    if(cont.type=="水平单选框"){
      res <- mock_ready_crf_filename%>%pull(itemDataString)%>%sample(row_num, replace = T)
    }else if(cont.type=="垂直单选框"){
      res <- mock_ready_crf_filename%>%pull(ordinal)%>%sample(row_num, replace = T)%>%
        as.character()
      footnote <- paste0(fieldname,"：",
                         paste0(paste0(mock_ready_crf_filename$ordinal, "=", mock_ready_crf_filename$itemDataString), collapse = "，"))
      footnote_vec <<- c(footnote_vec, footnote)
    }
    else if(cont.type == "日期框"){
      res <- sample(x = date_format, row_num, replace = T)
    }else{
      res <- sample(x = "xxx", row_num, replace = T)
    }
    if(replace_string){
      res <- case_when(str_detect(res, "，请")~str_replace(res, "，请.*", "/xxx"),
                       .default = res)
    }
    return(res)
  }
  field_w_value <- lapply(fieldnames, \(x) create_column(fieldname = x, row_num=row_num, date_format = date_format, replace_string = replace_string))
  setNames(field_w_value, fieldnames)
  shell_body <- as.data.frame(field_w_value, col.names = fieldnames, check.names =F)
  ### add column are not in form
  if(length(col_pre)==0){
    res_tab <- shell_body
  }else{
    field_pre <- lapply(col_pre, \(x) c(sample("xxx", row_num, replace = T)))
    shell_pre <- as.data.frame(field_pre, col.names = col_pre, check.names =F)
    res_tab <- cbind(shell_pre, shell_body)
  }
  ### combine columns to one
  if(length(col_combine)!=0){
    new_colname <- paste0(col_combine,collapse = "\n/")
    res_tab <- res_tab%>%unite(!!new_colname,
                    col_combine, 
                    sep = "\n/",
                    remove = T)
  }
  ### if use who drug coding, add ATC/PN column and footnote
  if(iswhodrug){
    res_tab <- res_tab%>%add_column("ATC\n/PN"=sample("xxx\n/xxx", row_num, replace = T), .after = 3)
    footnote_vec <- c(footnote_vec, "ATC2：解剖学、治疗学及化学分类法2级，PN：首选药名","依据&DrugDIR.进行编码")
  }
  ### if use med drug coding, add SOC/PT column and footnote
  if(ismedDrug){
    res_tab <- res_tab%>%add_column("SOC\n/PT"=sample("xxx\n/xxx", row_num, replace = T), .after = 3)
    footnote_vec <- c(footnote_vec, "SOC：系统器官分类，PT：首选术语", "依据&MedDRA.进行编码")
  }
  ### filter columns with value of yes
  if(filter_yes!=""){
    res_tab <- res_tab%>%filter(!!sym(filter_yes)=="是")%>%
      select(-!!sym(filter_yes))
  }
  res_tab[nrow(res_tab)+2,] <- "......"
  res_list <- list(table = res_tab,
                   footnote = add_footnote_path(footnote_vec))
  return(res_list)
}


add_footnote_path <- function(footnote){
  
  ### footnote
  res_footnote <- ""
  if(length(footnote)!=0){
    res_footnote <- paste0(c("::: {custom-style='Footnote'}",  
                    paste0(footnote, collapse =  "\n\n"), 
                    ":::"
                    ), 
                  collapse  = "\n")

  }
  ### saspath
  res_saspath <-   paste0(c("\n\n<br>\n",
                            "::: {custom-style='路径'}", 
                            "&lt;路径&gt;/< XXX.SAS>    draft YYYY-MM-DD HH:MM", 
                            ":::",
                            "\n<br>",
                            "<br>\n"), 
                          collapse  = "\n")
  res <- paste0(res_footnote, res_saspath)
  return(res)
}


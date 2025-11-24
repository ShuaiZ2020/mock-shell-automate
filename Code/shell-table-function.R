library(tidyverse)
library(openxlsx)
library(officer)
library(tfrmt)
library(flextable)
make_mock_shell <- function(formoid, row_num=3, 
                            col_combine = c(), col_pre = c("受试者随机号", "随机组别"), col_remove = c("单位", "备注"),
                            replace_string = F, only_yes = F, seed = "123",
                            ismedDrug = F, iswhodrug = F, no_empty = T,
                            col_fold = c()
                            ){
  visit <- crf_database_FolderModule_merge%>%filter(moduleOID==formoid)%>%pull(folderName)
  set.seed(seed)
  footnote_vec <- c()
  mock_ready_crf <- mock_ready_crf%>%filter(formOID==formoid)
  fieldnames <- mock_ready_crf$fieldName%>%unique()%>%setdiff(., col_remove)
  ### filter columns with value of yes
  if(only_yes){
    whether_test <- fieldnames[1]
    fieldnames <- fieldnames[2:length(fieldnames)]
    footnote <- paste0("此列表仅收集",
                       whether_test%>%str_extract("(?<=是否).*(?=？)"),
                       "的受试者"
                       )
    footnote_vec <- c(footnote_vec, footnote)
  }
  create_column<- function(fieldname, row_num, date_format, replace_string = F){
    mock_ready_crf_filename <- mock_ready_crf%>%filter(fieldName==fieldname)
    cont.type = mock_ready_crf_filename%>%pull(controlType)%>%head(1)
    if(cont.type=="水平单选框"){

      res <- mock_ready_crf_filename%>%pull(itemDataString)%>%sample(row_num, replace = T)
    }else if(cont.type=="垂直单选框"|cont.type=="多选框"){
      if(mock_ready_crf_filename$itemDataString%>%str_detect("\\d[\u4E00-\u9FFF]：")%>%any()){
        grade <- mock_ready_crf_filename$itemDataString%>%str_extract("\\d[\u4E00-\u9FFF]")
        grade_desc <-  mock_ready_crf_filename$itemDataString%>%str_extract("(?<=\\d[\u4E00-\u9FFF]：).*")
        res <- sample(grade, row_num, replace = T)
        footnote <- paste0(fieldname,"：",
                           paste0(paste0(grade, "=", grade_desc), collapse = "，"))
        footnoot_vec <<- c(footnote_vec, footnote)
      }else{
        res <- mock_ready_crf_filename%>%pull(ordinal)%>%sample(row_num, replace = T)%>%
          as.character()
        footnote <- paste0(fieldname,"：",
                           paste0(paste0(mock_ready_crf_filename$ordinal, "=", mock_ready_crf_filename$itemDataString), collapse = "，"))
        footnote_vec <<- c(footnote_vec, footnote)
      }

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
  if(length(col_fold)!=0){
    col_fold_vec <- fieldnames[match(col_fold[1], fieldnames) : match(col_fold[2], fieldnames)]
    mock_ready_crf <- mock_ready_crf%>%
      mutate(itemDataString = if_else(fieldName%in%col_fold_vec, fieldName, itemDataString),
             controlType = if_else(fieldName%in%col_fold_vec, "水平单选框", controlType),
             fieldName = if_else(fieldName%in%col_fold_vec, "实验室范围名称", fieldName))%>%
      distinct(fieldName, itemDataString, .keep_all = T)
    fieldnames <- mock_ready_crf$fieldName%>%unique()%>%setdiff(., col_remove)
  }
  field_w_value <- lapply(fieldnames, \(x) create_column(fieldname = x, row_num=row_num, date_format = date_format, replace_string = replace_string))
  setNames(field_w_value, fieldnames)
  shell_body <- as.data.frame(field_w_value, col.names = fieldnames, check.names =F)
  ### add visit to table if multiple visit appear
  if(length(visit)>1){
    visit_col = subset_wrap(visit, 1:row_num)
    visit_df = as.data.frame(list(visit_col), col.names = c("访视"), check.names = F)
    res_tab <- cbind(visit_df, shell_body)
  }else{
    res_tab <- shell_body
  }
  ### add column are not in form
  if(length(col_pre)==0){
    res_tab <- res_tab
  }else{
    field_pre <- lapply(col_pre, \(x) c(sample("xxx", row_num, replace = T)))
    shell_pre <- as.data.frame(field_pre, col.names = col_pre, check.names =F)
    res_tab <- cbind(shell_pre, res_tab)
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
    res_tab <- res_tab%>%add_column("ATC2\n/PN"=sample("xxx\n/xxx", row_num, replace = T), .after = 3)
    footnote_vec <- c(footnote_vec, "ATC2：解剖学、治疗学及化学分类法2级，PN：首选药名","依据&DrugDIR.进行编码")
  }
  ### if use med drug coding, add SOC/PT column and footnote
  if(ismedDrug){
    res_tab <- res_tab%>%add_column("SOC\n/PT"=sample("xxx\n/xxx", row_num, replace = T), .after = 3)
    footnote_vec <- c(footnote_vec, "SOC：系统器官分类，PT：首选术语", "依据&MedDRA.进行编码")
  }
  if(no_empty){
    if(colnames(res_tab)%>%str_detect("是否")%>%any()){
      ques <- which(colnames(res_tab)%>%str_detect("是否"))[1]
      res_tab[res_tab[,ques]!="是", (ques+1):ncol(res_tab)] <- NA
    }

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
                  collapse  = "\n")%>%str_replace_all("\\~","\\\\~")

  }
  ### saspath
  res_saspath <-   paste0(c("\n\n<br>\n",
                            "::: {custom-style='路径'}", 
                            "&lt;路径&gt;/< XXX.SAS>   space_space_space draft YYYY-MM-DD HH:MM", 
                            ":::",
                            "\n<br>",
                            "<br>\n"), 
                          collapse  = "\n")
  res <- paste0(res_footnote, res_saspath)
  return(res)
}

add_title <- function(formoid, population, word_style){
  if(formoid%in%crf_module_full$moduleOID){
    modulename <- crf_module_full%>%filter(moduleOID==formoid)%>%pull(moduleName)

  }else{
    modulename <- formoid
  }
  res <- paste0(c(paste0("::: {custom-style='",word_style, "'}"),
                  paste0(modulename,"  "), 
                  population,
                  ":::\n"), 
                collapse  = "\n")
}
subset_wrap <- function(x, idx) {
  x[(idx - 1) %% length(x) + 1]
}

stack_lines <- function(text) {
  # Split by newline
  parts <- strsplit(text, "\n")[[1]]
  # Wrap each part in div
  html_parts <- paste0("<div>", parts, "</div>")
  # Join them back together
  html(paste(html_parts, collapse = ""))
}
set_tfrmt_label <- function(form_field, fieldname, column){
  fieldname <- fieldname%>%unlist()
  if(!is.na(as.numeric(form_field$dataFormat))%>%all()){

    df_filedname <- bind_rows(expand_grid(group = fieldname, column = column, label = c("例数 (%)"), param = c("n", "prop")),
                              expand_grid(group = fieldname, column = column, label = c("均值 (标准差)"), param = c("mean", "sd")),
                              expand_grid(group = fieldname, column = column, label = c("中位数"), param = "median"),
                              expand_grid(group = fieldname, column = column, label = c("最小值，最大值"), param = c("min", "max"))
    )
    
    res <- tfrmt(group = "group",
          label = "label",
          column = "column",
          param = "param",
          body_plan = body_plan(
            frmt_structure(group_val = ".default", label_val = "例数 (%)", 
                           frmt_combine("{n} ({prop})",
                                        n = frmt("xx"), prop = frmt("xx.x"))),
            frmt_structure(group_val = ".default", label_val = "最小值，最大值",
                           frmt_combine("{min}, {max}",
                                        min = frmt("xx.x"), max = frmt("xx.x"))),
            frmt_structure(group_val = ".default", label_val = "均值 (标准差)",
                           frmt_combine("{mean} ({sd})",
                                        mean = frmt("xx.xx"), sd = frmt("xx.xxx"))),
            frmt_structure(group_val = ".default", label_val = "中位数", median = frmt("xx.xx"))
          ),
          row_grp_plan = row_grp_plan(
            row_grp_structure(group_val = ".default",
                              element_block(post_space = "   ")
                              ) 
            )
          ) %>%
      print_mock_gt(df_filedname) 
    
      
  }else{
    df_filedname <- expand_grid(fieldname= fieldname, group = form_field$itemDataString, column = column, param = c("n", "prop"))
    res <- tfrmt(
      # Specify columns in the data
      group = "fieldname",
      label = "group",
      column = "column",
      param = "param",
      row_grp_plan = row_grp_plan(
        row_grp_structure(group_val = ".default",
                          element_block(post_space = "   "))),
      
      # Specify body plan
      body_plan = body_plan(
        frmt_structure(group_val = ".default", label_val = ".default",
                       frmt_combine("{n} ({prop})",
                                    n = frmt("xx.x"),
                                    prop = frmt("xx.x")))
      ))%>%print_mock_gt(df_filedname)
    
  }
  res
}

make_mock_shell_table <- function(formoid){
  mock_ready_crf_formoid <- mock_ready_crf%>%
    filter(formOID==formoid, !str_detect(fieldName, "日期|时间"))%>%
    mutate()
  tfrmt_table_list <- mock_ready_crf_formoid%>%
    group_by(fieldName)%>%
    group_map(\(x, y) {
      res <-   set_tfrmt_label(form_field = x, fieldname = y, column = c("试验组\nN=xxx\nn (%)", "对照组\nN=xxx\nn (%)", "合计\nN=xxx\nn (%)"))
      res <- res%>%pluck("_data")%>%select(-last_col())%>%slice(-nrow(.))%>%add_row()
      })
  tfrmt_table <- data.table::rbindlist(tfrmt_table_list, use.names = F)
  colnames(tfrmt_table)[1] <- NA
  tfrmt_table%>%format_table()
}




## *****************
## Author: Shuai Zhu
## Date: 9/18/2025
## Description: 1.检查大小写字体 2.检查最大值最小值间逗号
## Plan: only read first or last several line from rtf file. 对列名进行语义分析; 检查每组样本量是否正确
## ******************

library(striprtf)
library(tidyverse)
library(officer)
files_listing <- list.files("data/listing", pattern = "\\.rtf$", full.names = TRUE)
files_table <- list.files("data/table", pattern = "\\.rtf$", full.names = TRUE)

### 大于小于，最小值后面逗号，二者字体;总计|合计
find_comma <- function(text){
  if (any(str_detect(text,"最小值\\s*，\\s*最大值"))==T){
    print(paste(text[1], "最小值"))
  }
}
check_gt_lt <- function(text){
  lines_bool <- str_detect(text, "\uFF1C|\uFF1C")
  if (any(lines_bool)==T){
    print(paste(text[1], ">"))
    print(which(lines_bool==T))
  }
}
sample_size <- function(fl_dict){
  setnames <- names(fl_dict)
  size_set <- lapply(setnames, function(x) {
    adsl %>%
      filter(.data[[x]] == "Y") %>%
      select(all_of(x), TRT01P) %>%
      table()
  })
  names(size_set) <- setnames
  return(size_set)
}
check_set_sample_size <- function(text){
  set_name <- text[2]
  set_ind <- fl_dict==set_name
  if (any(set_ind)){
    size_set[[names(fl_dict)[set_ind]]]
  }

}

extract_true_title_footnote <- function(docx, toc_num_arg){
  start <- which(docx$style_name == 'SHELL表00-节标题2'&docx$text=='受试者分布')[1]
  docx_toc <- docx%>%filter(style_name=='toc 3')%>%
    mutate(ori_text = text,
           text = case_when(
          str_detect(text, "(?<!列)表\\s(?=\\w\\d+\\.\\d+(?:-\\d+\\.\\d+)?)")~str_replace(text, "(?<!列)表\\s(?=\\w\\d+\\.\\d+(?:-\\d+\\.\\d+)?)", "t"),
          str_detect(text, "图\\s(?=\\w\\d+\\.\\d+(?:-\\d+\\.\\d+)?)")~str_replace(text, "图\\s(?=\\w\\d+\\.\\d+(?:-\\d+\\.\\d+)?)", "f"),
          str_detect(text, "列表\\s(?=\\w\\d+\\.\\d+(?:-\\d+\\.\\d+)?)")~str_replace(text, "列表\\s(?=\\w\\d+\\.\\d+(?:-\\d+\\.\\d+)?)", "l"),
          .default = NA
      ),
      toc_num = str_extract(text, "\\w\\d+\\.\\d+(?:-\\d+\\.\\d+)?"),
      toc_num = str_replace_all(toc_num, "\\.|-", "_"),
      header_name = str_extract(ori_text, "(?<=\\d\\s).*(?=\\sPAGEREF\\s)"),
      header_name = str_replace(header_name, "\\s+(?=[\u4e00-\u9fff]*集)", ""),
      ori_text = str_extract(ori_text, ".*(?=\\sPAGEREF\\s)"),
      ori_text = str_replace(ori_text, "\\s+(?=[\u4e00-\u9fff]*集)", ""))%>%
    drop_na(header_name)%>%
    select(ori_text,toc_num, header_name)
  
  docx_body <- docx%>%slice(start:n())%>%
    filter(content_type == 'paragraph')%>%
    select(doc_index, text, style_name)
  
  docx_body_toc <- docx_body%>%
    left_join(docx_toc, by= c("text" = "header_name"))%>%
    mutate(style_name = case_when(
      str_detect(text, "数据来源")~"脚注",
      .default = style_name
    ))%>%
    fill(style_name, .direction  = "down")%>%
    filter(!(str_detect(style_name, "(?<!列)表")&str_detect(toc_num, "l")),
           !(str_detect(style_name, "列表")&str_detect(toc_num, "t")))%>%
    fill(toc_num, .direction = "down")%>%
    mutate(text = case_when(
      !is.na(ori_text)~ ori_text,
      .default = text
    ))%>%select(-ori_text)
  
  docx_body_toc%>%filter(toc_num==toc_num_arg)%>%
    as_tibble()
}
check_title_footnote_identical <- function(shell_note, rtf_text){
  shell_note_head_index <- which(str_detect(shell_note$text, "<路径>") )[1] - 1
  shellnote2 <- shell_note%>%
    slice_head(n = shell_note_head_index)%>%
    filter(text!="")%>%
    pull(text)%>%str_replace_all("\\sREF.*?MERGEFORMAT\\s", "")%>%
    str_replace_all("\\sREF.*?\\\\h\\s", "")%>%
    str_trim(side = 'right')

  footnote_index <- str_detect(rtf_text, "\\*\\| -")%>%which()
  title = paste0(rtf_text[rtf_text!=""][1], rtf_text[rtf_text!=""][2])
  if (footnote_index%>%length()==0){
    print("error")
  }
  rtf_footnote <- rtf_text[footnote_index]%>%
    str_replace_all("\\*\\| - ", "")%>%str_replace("\n \n路径.*?\\s\\|\\s","")%>%
    strsplit("\n- ")%>%unlist()
  rtf_footnote2 <- c(title, rtf_footnote)
  res <- which(shellnote2!= rtf_footnote2)
  if (length(res)!=0){
    cat("---shell----\n")
    cat(shellnote2[res]%>%paste0('\n'), sep = "")
    cat("\n---rtf----\n")
    cat(rtf_footnote2[res]%>%paste0('\n'), sep = "")
    cat("\n")
  }
}


docx <- officer::read_docx("SapShell/YDMD_2023045_布立西坦_TFL shell_V1.0.docx")%>%docx_summary()
files <- files_listing
for(i in 1:length(files)){
  true_footnote <- extract_true_title_footnote(docx, files[i]%>%basename()%>%tools::file_path_sans_ext())

  text <- read_rtf(files[i])
  print(paste(i, text[1]))
  check_title_footnote_identical(true_footnote, text)
  find_comma(text)
  check_gt_lt(text)
  # size_set <- sample_size(fl_dict)
}


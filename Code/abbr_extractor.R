
library(stringr)
library(openxlsx)
# read the qmd file
txt <- readLines("Report/report.qmd", encoding = "UTF-8")

abbr <- str_extract_all(txt[40:length(txt)], "[A-Za-z]+(?:-[A-Za-z]+)+|[A-Za-z]{2,}")%>%
  unlist()%>%unique()%>%sort()
data.frame("缩写"= abbr,
           "英文全称" = rep(NA, length(abbr)),
           "中文全称" = rep(NA, length(abbr)))%>%
  write.xlsx("Data/abbr.xlsx")

           
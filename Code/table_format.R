library(gtsummary)
library(flextable)
library(dplyr)
library(officer)
format_table <- function(ft){
  if(typeof(output_format)=="NULL"){
    output_format <- "docx"
  }
  if(output_format=="docx"){
    res <- ft%>%flextable()%>%
      align(align = "left", part = "all") %>%
      set_table_properties(layout = "autofit") %>%
      border_remove() %>%
      hline_top(part = "header", border = fp_border(width = 1/2)) %>%
      hline_bottom(part = "body", border = fp_border(width = 1/2)) %>%
      hline_bottom(part = "header", border = fp_border(width = 1/2)) %>%
      fontsize(size = 10.5, part = "all") %>%
      height_all(height = 0.22, part = "all") %>%    
      hrule(rule = "auto", part = "all") %>% 
      padding(padding.top = 0, padding.bottom = 0,
              padding.left = 1, padding.right = 1, part = "all") %>%
      font(fontname = "Times New Roman", part = "all")%>%
      set_table_properties(
        width = 1,           # width = 1 = 100% of page width
        layout = "autofit")%>%autofit()
  }else if(output_format == "typst"){
    res <- ft%>%gt()%>%opt_table_font(font = "SimSun")
  }else if(output_format == "html"){
    res <- ft%>%gt()%>%opt_table_font(font = "SimSun")
  }else{
    res <- ft%>%kbl()
  }
  return(res)
  
}

gtsummary2table <- function(ft, output_format = "docx"){
  if(typeof(output_format)=="NULL"){
    output_format <- "docx"
  }
  if(output_format=="docx"){
    res <- ft%>%as_flex_table()%>%
      font(fontname = "Times New Roman", part = "all")%>%
      set_table_properties(width = 1,layout = "autofit")%>%
      autofit()
  }else if(output_format == "typst"){
    res <- ft%>%as_gt()%>%
      gt::opt_table_font(
        font = list(
          "Times New Roman", "SimSun"
        )
      )
  }else if(output_format == "html"){
    res <- ft
  }
  return(res)
}
library(tidyverse)

library(tabulapdf)

protocol <- "./Background/12-iMSC预防aGvHD高危患者aGvHD的前瞻性临床研究 V2.0-Final-Clean.pdf"
text <- extract_text(protocol, pages = 1)
text%>%str_extract("(?<=方案编号：)\\s[a-zA-Z]+")

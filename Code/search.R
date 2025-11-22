library(readxl)

file <- "Background/ydmd_2025027_iMSC预防aGvHD的前瞻性临床研究_spec_1.0_20250427.xlsx"
sheets <- excel_sheets(file)

results <- list()

for (sh in sheets) {
  df <- read_excel(file, sheet = sh, col_types = "text")
  
  # check row contains "汉族"
  hit_rows <- apply(df, 1, function(x) any(grepl("结果", x %||% "", fixed = TRUE)))
  idx <- which(hit_rows)
  
  # 如果没有匹配，跳过
  if (length(idx) == 0) next
  
  # 保存结果
  results[[sh]] <- data.frame(
    sheet = sh,
    row = idx,
    text = apply(df[idx, ], 1, function(x) paste(x, collapse = " | ")),
    stringsAsFactors = FALSE
  )
}

# 合并所有 sheet 的结果
final_result <- do.call(rbind, results)
final_result

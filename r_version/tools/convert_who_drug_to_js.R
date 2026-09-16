library(here)
library(tidyverse)
library(jsonlite)

# WHO Drug/IDFの各バージョンを、Web版(web_tool)がfile://から直接<script src>で読み込めるJSデータファイルに
# 変換する。ダミーデータ生成ロジックが実際に使う列(drug_code/full_name_en/generic_name_en)だけに絞り、
# array-of-arrays形式(columns + rows)で出力する。
# 出力先: web_tool/data/who_drug/<version>.js
# 各ファイルは window.__whoDrugVersions["<version>"] = {columns:[...], rows:[[...], ...]} という形で、
# バージョン名(スペースを含みうる)をオブジェクトのキーとして持たせる(JS変数名にしない)

source(here("constant.R"))
source(here("read_who_drug_idf.R"))

output_dir <- here("..", "web_tool", "data", "who_drug")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

who_drug_versions <- list.dirs(who_drug_idf_parent_dir, full.names = FALSE, recursive = FALSE)
cat("変換対象バージョン:", paste(who_drug_versions, collapse = ", "), "\n")

who_drug_columns <- c("drug_code", "full_name_en", "generic_name_en")

for (version in who_drug_versions) {
  cat("変換中:", version, "...\n")
  idf <- build_who_drug_idf(who_drug_idf_parent_dir, version) %>%
    select(all_of(who_drug_columns)) %>%
    distinct() %>%
    mutate(across(everything(), as.character))

  json_text <- toJSON(list(columns = who_drug_columns, rows = unname(asplit(as.matrix(idf), 1))), auto_unbox = TRUE)

  safe_filename <- str_replace_all(version, "[^A-Za-z0-9._-]", "_")
  out_path <- file.path(output_dir, str_c(safe_filename, ".js"))
  js_content <- str_c(
    "window.__whoDrugVersions = window.__whoDrugVersions || {};\n",
    "window.__whoDrugVersions[", toJSON(version, auto_unbox = TRUE), "] = ", json_text, ";\n"
  )
  writeLines(js_content, out_path, useBytes = TRUE)
  cat("  ->", out_path, "(", nrow(idf), "行,", format(file.size(out_path) / 1e6, digits = 3), "MB)\n")
}

cat("完了。web_tool/data/versions.jsのwho_drugバージョン一覧も更新すること\n")

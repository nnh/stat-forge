library(here)
library(tidyverse)
library(jsonlite)

# MedDRAの各バージョンを、Web版(web_tool)がfile://から直接<script src>で読み込めるJSデータファイルに
# 変換する。ダミーデータ生成ロジックが実際に使う列(コード・英語名のみ。日本語名・カナ・currency等は
# 生成ロジックで未使用のため含めない)だけに絞り、行数分の繰り返しを避けるため
# array-of-arrays形式(columns + rows)で出力する。
# 出力先: web_tool/data/meddra/<version>.js
# 各ファイルは window.__meddraVersions["<version>"] = {columns:[...], rows:[[...], ...]} という形で、
# バージョン名(ドットや空白を含みうる)をオブジェクトのキーとして持たせる(JS変数名にしない)ことで、
# バージョン名の文字種に関わらず安全に扱えるようにする

source(here("constant.R"))
source(here("build_meddra_soc_pt_llt.R"))

output_dir <- here("..", "web_tool", "data", "meddra")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

meddra_versions <- list.dirs(meddra_dir, full.names = FALSE, recursive = FALSE)
cat("変換対象バージョン:", paste(meddra_versions, collapse = ", "), "\n")

meddra_columns <- c(
  "llt_code", "llt_name", "pt_code", "pt_name",
  "hlt_code", "hlt_name", "hlgt_code", "hlgt_name", "soc_code", "soc_name"
)

for (version in meddra_versions) {
  cat("変換中:", version, "...\n")
  hierarchy <- build_meddra_hierarchy(version) %>%
    select(all_of(meddra_columns)) %>%
    distinct() %>%
    mutate(across(everything(), as.character))

  json_text <- toJSON(list(columns = meddra_columns, rows = unname(asplit(as.matrix(hierarchy), 1))), auto_unbox = TRUE)

  safe_filename <- str_replace_all(version, "[^A-Za-z0-9._-]", "_")
  out_path <- file.path(output_dir, str_c(safe_filename, ".js"))
  js_content <- str_c(
    "window.__meddraVersions = window.__meddraVersions || {};\n",
    "window.__meddraVersions[", toJSON(version, auto_unbox = TRUE), "] = ", json_text, ";\n"
  )
  writeLines(js_content, out_path, useBytes = TRUE)
  cat("  ->", out_path, "(", nrow(hierarchy), "行,", format(file.size(out_path) / 1e6, digits = 3), "MB)\n")
}

cat("完了。web_tool/data/versions.jsのmeddraバージョン一覧も更新すること\n")

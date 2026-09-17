library(tidyverse)

# who_drug_idf_parent_dir(親フォルダ)と、その下にある「年 MarまたはSep 1」形式の
# who_drug_idf_version_folder(バージョンフォルダ名)を指定して、WhoDrug/IDFの各テーブルを読み込み、
# idf_combined(IDF側)にid_mapping(WHODD⇔IDFの対応)とwhodd_generic_names(WHODDの一般名)を
# 結合したテーブルを返す
build_who_drug_idf <- function(who_drug_idf_parent_dir, who_drug_idf_version_folder) {
  version_dir <- file.path(who_drug_idf_parent_dir, who_drug_idf_version_folder)
  whodd_dir <- file.path(version_dir, "WHODD")
  idf_dir <- file.path(version_dir, "IDF")

  if (!dir.exists(whodd_dir) || !dir.exists(idf_dir)) {
    return(build_who_drug_idf_from_js(who_drug_idf_version_folder))
  }

  # WHODD: DDDRCODE(WHO Drug Dictionary側の薬剤コード)とIDFCODE(IDF側のコード)の対応表。
  # タブ区切り・ヘッダー無し。5列目(note)は"WHO Global"などの注記で、無い行もある
  id_mapping <- read_tsv(
    file.path(whodd_dir, "IDMapping.csv"),
    col_names = c("ddd_label", "ddd_code", "idf_label", "idf_code", "note"),
    col_types = cols(.default = "c")
  )

  # WHODD: DDDRCODEごとの一般名(英語)。タブ区切り・ヘッダー無し
  whodd_generic_names <- read_tsv(
    file.path(whodd_dir, "WHODDsGenericNames.csv"),
    col_names = c("ddd_code", "generic_name_en"),
    col_types = cols(.default = "c")
  )

  # IDF: IDFコードごとの英語名。カンマ区切り(ダブルクォート囲み)・ヘッダー無し・ASCII
  idf_full_en <- read_csv(
    file.path(idf_dir, "full_en.txt"),
    col_names = c("SEQ", "full_name_en"),
    col_types = cols(.default = "c")
  )

  # IDF: IDFコードごとの日本語名。カンマ区切り(ダブルクォート囲み)・ヘッダー無し・CP932(Shift-JIS系)。
  # 4列目(一般名)以外の列の意味は未確認のため、仮の列名のまま
  idf_full_ja <- read_csv(
    file.path(idf_dir, "full_ja.txt"),
    locale = locale(encoding = "CP932"),
    col_names = c("SEQ", "col2", "col3", "generic_name", "col5"),
    col_types = cols(.default = "c")
  )

  # IDF: メインの薬剤情報テーブル(17列)。カンマ区切り(ダブルクォート囲み)・ヘッダー無し・CP932。
  # 使う列(1,4,14,15列目)以外の意味は未確認のため、仮の列名(X2,X3,...)のまま
  idf_data <- read_csv(
    file.path(idf_dir, "data.txt"),
    locale = locale(encoding = "CP932"),
    col_names = FALSE,
    col_types = cols(.default = "c")
  ) %>%
    rename(drug_code = X1, usage_category_kanji = X4, SEQ = X14, FLG = X15)

  # idf_data/idf_full_en/idf_full_jaを、上で名前を付けた列だけ残してSEQ(IDFコード)で結合する
  idf_combined <- idf_data %>%
    select(SEQ, drug_code, usage_category_kanji, FLG) %>%
    left_join(idf_full_en %>% select(SEQ, full_name_en), by = "SEQ") %>%
    left_join(idf_full_ja %>% select(SEQ, generic_name), by = "SEQ") %>%
    filter(FLG != "C")

  # idf_combinedとid_mappingを結合する。id_mapping$idf_codeはidf_combined$drug_codeと対応しており
  # (idf_combined$SEQとは桁数体系が異なり一致しない)、full_joinでどちらか一方にしか無いレコードも残す
  idf_id_mapping_combined <- idf_combined %>%
    full_join(id_mapping, by = c("drug_code" = "idf_code")) %>%
    left_join(whodd_generic_names, by = "ddd_code")

  idf_id_mapping_combined
}

# Web版が使うWHO Drug/IDFの.js(convert_who_drug_to_js.Rで生データから変換済み、
# drug_code/full_name_en/generic_name_enのみ)から読み込む。生データ(WHODD/IDFフォルダ)が
# 手元に無い環境向けのフォールバック
build_who_drug_idf_from_js <- function(version) {
  safe_filename <- str_replace_all(version, "[^A-Za-z0-9._-]", "_")
  js_path <- file.path(who_drug_js_dir, str_c(safe_filename, ".js"))
  if (!file.exists(js_path)) {
    stop("WHO Drug/IDFの生データフォルダも.jsファイルも見つかりません: ", js_path)
  }

  js_text <- read_file(js_path)
  json_str <- str_match(js_text, "(?s)window\\.__whoDrugVersions\\[[^\\]]*\\]\\s*=\\s*(\\{.*\\});")[, 2]
  parsed <- jsonlite::fromJSON(json_str)

  idf <- as_tibble(parsed$rows, .name_repair = "minimal")
  colnames(idf) <- parsed$columns
  idf
}

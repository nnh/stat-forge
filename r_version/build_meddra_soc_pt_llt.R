# MedDRAの.ascファイル(SOC/HLGT/HLT/PT/LLT、英語版・日本語版)を読み込み、
# SOC-HLGT-HLT-PT-LLTを結合した1つのdata frame(soc_pt_llt_hlgt_hlt)を作成する
library(tidyverse)
library(here)

source(here("constant.R"))

build_meddra_hierarchy <- function(target_subfolder = NULL) {
  # MedDRA直下のバージョン別サブフォルダ名の一覧
  meddra_subfolders <- list.dirs(meddra_dir, full.names = FALSE, recursive = FALSE)

  # 読み込み対象のサブフォルダ（バージョン）を指定。未指定なら先頭を使う
  if (is.null(target_subfolder)) {
    target_subfolder <- meddra_subfolders[1]
  }

  # 対象サブフォルダ内の.ascファイルパス一覧
  asc_files <- list.files(
    file.path(meddra_dir, target_subfolder),
    pattern = "\\.asc$",
    full.names = TRUE
  )

  # 全.ascファイルをファイル名をキーにしたリストとして読み込む（$区切り、ヘッダーなしのMedDRA標準形式）
  meddra_data <- asc_files %>%
    set_names(basename) %>%
    map(~ read_delim(.x, delim = "$", col_names = FALSE, show_col_types = FALSE))

  # SOC(器官別大分類)コード・名称(英語)にSOC名称(日本語)・並び順(seq)を結合
  soc_combined <- meddra_data[["soc.asc"]] %>%
    select(X1, X2) %>%
    left_join(
      meddra_data[["soc_j.asc"]] %>% select(X1, X2, X3),
      by = "X1", suffix = c("_en", "_ja")
    )
  colnames(soc_combined) <- c("soc_code", "soc_name", "soc_name_j", "seq")

  # SOCとHLGT(高位グループ用語)の対応表からSOCコード・HLGTコードを取り出し、soc_combinedに結合
  soc_hlgt_map <- meddra_data$soc_hlgt.asc %>% select("X1", "X2")
  colnames(soc_hlgt_map) <- c("soc_code", "hlgt_code")
  soc_hlgt_combined <- soc_combined %>% inner_join(soc_hlgt_map, by = "soc_code")

  # hlgt
  hlgt_en <- meddra_data$hlgt.asc %>% select("X1", "X2")
  colnames(hlgt_en) <- c("hlgt_code", "hlgt_name")
  hlgt_ja <- meddra_data$hlgt_j.asc %>% select("X1", "X2", "X3")
  colnames(hlgt_ja) <- c("hlgt_code", "hlgt_name_j", "hlgt_kana")
  hlgt <- hlgt_en %>% inner_join(hlgt_ja, by = "hlgt_code")

  soc_hlgt <- soc_hlgt_combined %>% inner_join(hlgt, by = "hlgt_code")

  # hlt
  hlgt_hlt_map <- meddra_data$hlgt_hlt.asc %>% select("X1", "X2")
  colnames(hlgt_hlt_map) <- c("hlgt_code", "hlt_code")

  hlt_en <- meddra_data$hlt.asc %>% select("X1", "X2")
  colnames(hlt_en) <- c("hlt_code", "hlt_name")
  hlt_ja <- meddra_data$hlt_j.asc %>% select("X1", "X2", "X3")
  colnames(hlt_ja) <- c("hlt_code", "hlt_name_j", "hlt_kana")
  hlt <- hlt_en %>%
    inner_join(hlt_ja, by = "hlt_code") %>%
    inner_join(hlgt_hlt_map, by = "hlt_code")

  soc_hlgt_hlt <- soc_hlgt %>% inner_join(hlt, by = "hlgt_code", relationship = "many-to-many")

  # pt
  hlt_pt_map <- meddra_data$hlt_pt.asc %>% select("X1", "X2")
  colnames(hlt_pt_map) <- c("hlt_code", "pt_code")
  pt_en <- meddra_data$pt.asc %>% select("X1", "X2", "X4")
  colnames(pt_en) <- c("pt_code", "pt_name", "pt_soc_code")
  pt_ja <- meddra_data$pt_j.asc %>% select("X1", "X2", "X3")
  colnames(pt_ja) <- c("pt_code", "pt_name_j", "pt_kana")
  pt <- pt_en %>%
    inner_join(pt_ja, by = "pt_code") %>%
    inner_join(hlt_pt_map, by = "pt_code")

  soc_pt_hlgt_hlt <- soc_hlgt_hlt %>% inner_join(pt, by = "hlt_code", relationship = "many-to-many")

  # llt
  llt_en <- meddra_data$llt.asc %>% select("X1", "X2", "X3", "X10")
  colnames(llt_en) <- c("llt_code", "llt_name", "pt_code", "llt_currency")
  llt_ja <- meddra_data$llt_j.asc %>% select("X1", "X2", "X3", "X4")
  colnames(llt_ja) <- c("llt_code", "llt_name_j", "llt_jcurr", "llt_kana")
  llt <- llt_en %>%
    inner_join(llt_ja, by = "llt_code") # %>%
  # filter(llt_currency == "Y" & llt_jcurr == "Y") %>%
  # select(-llt_currency, -llt_jcurr)

  soc_pt_llt_hlgt_hlt <- soc_pt_hlgt_hlt %>% inner_join(llt, by = "pt_code", relationship = "many-to-many")

  soc_pt_llt_hlgt_hlt
}

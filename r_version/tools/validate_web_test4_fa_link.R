library(here)

# test4(fortest4)のWebツール生成データ(dummy_data.zip展開後のCSV)に対して、
# FA(AE報告と同じフォーム上のリンクブロック。Web版のpopulateLinkedBlocks/splitLinkedDomainsで生成)が
# AEのAELLTCDに正しく連動しているかを確認する。
# AELLTCDがpresence_conditionsでFAOBJ/FAORRESがAELLTCDを参照している条件のexpected_value
# (必須LLTコード。deriveRequiredLltCodes/injectRequiredLltCodesで確実に注入される)に一致する
# 行だけFAOBJに値が入り、一致しない行は空欄のはず、という観点のチェックを、
# Webツールが実際に生成したCSVに対して行う。
#
# 事前準備: test_config.R の json_path を fortest4 に、other_domains_web_csv_dir を
# Webツールで「ZIPで一括ダウンロード」したdummy_data.zipの展開先に設定しておくこと
rm(list = ls())

source(here("test_config.R"))
library(jsonlite)
json <- jsonlite::read_json(json_path)
sheets <- json$sheets %>% keep( ~ .$alias_name == "sae_report") %>% .[[1]]
field_items <- sheets$field_items %>% keep( ~ .$name == "field20" | .$name == "field21")
validators <- field_items %>% map( ~ .$validators)
presences <- validators %>% map_vec( ~ .$presence$validate_presence_if) %>% unique()
presence_fields <- presences %>% str_split(fixed("||"))
codes <- presence_fields[[1]] %>% str_extract("\\d+$")
source(here("tools/validate_common.R"))

# gating_codes(presence_conditionsのexpected_value)・required_llt_codesはEDC仕様由来の情報のため、
# R側でload_edc_spec()を実行して取得する(被験者データ自体はこのあとWeb側のCSVに差し替える)
source(here("load_edc_spec.R"))
load_edc_spec(json_path)

ae_web <- read_csv(ae_web_csv_path, col_types = cols(.default = "c"), na = character(0))
fa_web <- read_csv(file.path(other_domains_web_csv_dir, "FA_dummy.csv"), col_types = cols(.default = "c"), na = character(0))
rm(list = setdiff(ls(), c("codes", "ae_web", "fa_web", "meddra")))
target_ae <- ae_web %>% filter(AELLTCD %in% codes)
target_fa <- fa_web %>% filter(FATESTCD == "QHCAUSAL")
ae_join <- target_ae %>% inner_join(target_fa, by=c("USUBJID", "AETERM"="FAOBJ"))
ae_anti_join <- target_ae %>% anti_join(target_fa, by=c("USUBJID", "AETERM"="FAOBJ"))

# target_ae(該当LLTコードのAE行)が1件残らずFAと対応していれば(=ae_joinと件数が一致し、
# ae_anti_joinが0件)、AE-FAリンクは正常。1件でも対応が取れていなければ異常としてstopで知らせる
if (nrow(target_ae) == nrow(ae_join) && nrow(ae_anti_join) == 0) {
  cat("AE-FAリンクチェック: 正常(該当AE ", nrow(target_ae), "件が全てFAと対応、不一致0件)\n", sep = "")
} else {
  stop(str_c(
    "AE-FAリンクチェック: 異常(該当AE=", nrow(target_ae), "件, FAと対応=", nrow(ae_join),
    "件, 対応無し=", nrow(ae_anti_join), "件)"
  ))
}

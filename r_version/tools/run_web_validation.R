library(here)

# Web版(JS)のDM/AE/DS/other_domainsドメイン生成をR版と一致しているか一括で検証する。
# validate_web_dm.R・validate_web_ae.R・validate_web_ds.R・validate_web_other_domains.Rを
# 毎回別々に手動でsourceする代わりに、このファイルを1回sourceすれば、load_edc_spec(json_path)の
# 実行から全てのバリデーションまで通しで実行できる。
#
# 前提: test_config.Rのjson_path・dm_web_csv_path・ae_web_csv_path・ds_web_csv_path・
# other_domains_web_csv_dirが、確認したいテストファイル1つ分の内容になっていること(Webツールで
# 同じjson_pathを読み込んで生成し、dummy_data.zipをダウンロード・展開したもの。
# other_domains_web_csv_dirはその展開先フォルダを指す)。
# 別のテストファイルを確認したい場合は、test_config.Rを書き換え、Web側も再生成・再ダウンロード
# してから、このファイルをもう一度sourceする(=4ファイル分確認したい場合は計4回実行する)
rm(list = ls())

source(here("test_config.R"))
source(here("load_edc_spec.R"))
load_edc_spec(json_path)

# R版とWeb版が同じJSON(同じ試験)から生成されたものかを、STUDYIDの一致で早期に確認する。
# json_pathの設定忘れ等でtest_config.Rの各パスが別の試験を指していると、以降の全チェックが
# 無意味なFAILの山になり原因が分かりにくいため、ここで最初にSTOPで気づけるようにする
dm_web_studyid_check <- read_csv(dm_web_csv_path, col_types = cols(.default = "c"), na = character(0))
r_studyid <- unique(dm[["STUDYID"]])
web_studyid <- unique(dm_web_studyid_check[["STUDYID"]])
if (!identical(r_studyid, web_studyid)) {
  stop(str_c(
    "R版とWeb版のSTUDYIDが一致しません(R版: ", paste(r_studyid, collapse = ", "),
    " / Web版: ", paste(web_studyid, collapse = ", "),
    ")。test_config.Rのjson_path・dm_web_csv_path等が同じ試験を指しているか確認してください。"
  ))
}
rm(dm_web_studyid_check, r_studyid, web_studyid)

cat("========== DMドメイン ==========\n")
source(here("tools/validate_web_dm.R"))

cat("\n========== AEドメイン ==========\n")
source(here("tools/validate_web_ae.R"))

cat("\n========== DSドメイン ==========\n")
source(here("tools/validate_web_ds.R"))

cat("\n========== other_domains ==========\n")
source(here("tools/validate_web_other_domains.R"))

# test4(fortest4)固有のチェック(AE報告のAELLTCDとFAリンクブロックの対応)は、test4のjsonの時だけ実行する。
# validate_web_test4_fa_link.R自体がtest_config.R・load_edc_spec.Rを読み直す自己完結型のため、
# ここではsourceするだけでよい
if (identical(basename(json_path), "fortest4_260826_1501.json")) {
  cat("\n========== test4: AE-FAリンクチェック ==========\n")
  source(here("tools/validate_web_test4_fa_link.R"))
}

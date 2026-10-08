library(here)
rm(list = ls())
source(here("resolve_os_path.R"))

# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、test_config.R(共通)ではなくここで指定する。リポジトリ外の任意の場所でよい
# TODO: test5のfixed_value_checks_test5.csvの実際のパスに置き換える
fixed_value_checks_csv_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/stat-forge-test/test5/fixed_value_checks_test5.csv",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\stat-forge-test\\test5\\fixed_value_checks_test5.csv"
)

source(here("test_config.R"))
# test_config.Rはjson_path(他テストとの切り替え用)も定義するが、このファイルは上で固定した
# json_pathを優先して使うため、test_config.R側の値で上書きしないよう再度設定し直す
# TODO: test5のJSONファイルの実際のパスに置き換える
json_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/stat-forge-test/test5/json/fortest5_260728_1634.json",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\stat-forge-test\\test5\\json\\fortest5_260728_1634.json"
)
source(here("tools/validate_common.R"))

# cdisc_variable_values・registration_n・who_drug_idfはEDC仕様(JSON)/辞書由来で被験者データには
# 依存しないため、R側でload_edc_spec()を実行して取得する(このとき同時に生成されるR版の
# ae/dm/ds/other_domains・discontinuation_dateは、このあと全てWeb版CSVの内容で上書きするため使わない)
source(here("load_edc_spec.R"))
load_edc_spec(json_path)

# R版(正しいjson_pathから生成)のSTUDYIDを、Web版CSVとの整合性チェックの期待値として控えておく。
# STUDYID文字列をここに直接書きたくないため、json_pathから実際に生成した値を使う
expected_studyid <- dm[["STUDYID"]][1]

rm(dm)
rm(ae)
rm(ds)
rm(other_domains)

# 被験者データ(ae/dm/ds/other_domains)をWebツールが生成したCSVで上書きする
dm <- read_csv(dm_web_csv_path, col_types = cols(.default = "c"), na = character(0))
ae <- read_csv(ae_web_csv_path, col_types = cols(.default = "c"), na = character(0))
ds <- read_csv(ds_web_csv_path, col_types = cols(.default = "c"), na = character(0))
other_domains <- load_csv_datasets(other_domains_web_csv_dir)
# load_csv_datasets()はファイル名から拡張子を除いた名前をそのままキーにするため、
# Webツールの出力ファイル名(例: CE_dummy.csv)の"_dummy"サフィックスを外してprefix名に揃える
names(other_domains) <- str_remove(names(other_domains), "_dummy$")
# facilities(施設一覧、code/ja/en列。SDTMドメインではないためother_domainsバリデーションの対象外にする)は
# DM.SITEIDとの整合性チェック用に別途取り出しておく
facilities <- other_domains[["facilities"]]
other_domains <- other_domains[setdiff(names(other_domains), c("DM", "AE", "DS", "facilities"))]

# json_pathの設定間違い(意図しない試験のJSONを指している)を、CSVの中身からも検知できるよう、
# 全ドメインのSTUDYIDがR版(正しいjson_path)のものと一致することを確認する
dm %>% check_studyid_matches(expected_studyid, domain_name = "DM")
ae %>% check_studyid_matches(expected_studyid, domain_name = "AE")
ds %>% check_studyid_matches(expected_studyid, domain_name = "DS")
for (prefix in names(other_domains)) {
  other_domains[[prefix]] %>% check_studyid_matches(expected_studyid, domain_name = prefix)
}

registration_n <- nrow(dm)
# discontinuation_dateは被験者ごとの中止日という「その乱数シードでの生成結果」に依存する値のため、
# Web版自身のdsから作り直す(R版のdiscontinuation_dateをそのまま使うと、対応するUSUBJIDの
# 中止日が互いに無関係な値になり誤検知するため)
discontinuation_date <- build_discontinuation_date_table(ds)

# 比較に不要な中間オブジェクトが環境に残らないよう、それら以外は削除する
# (source()より前に行うこと。後だと読み込んだ関数まで削除されてしまう)
rm(list = setdiff(ls(), c("ae", "dm", "ds", "other_domains", "facilities", "cdisc_variable_values", "registration_n", "who_drug_idf", "json_path", "discontinuation_date", "fixed_value_checks_csv_path")))

source(here("tools/validate_common.R"))

# DMのSITEIDが、facilities_dummy.csv(施設一覧)のcode列に含まれる値であることを確認する
dm %>% check_values_subset_of("SITEID", facilities[["code"]], domain_name = "DM/facilities")

names(other_domains) <- tolower(names(other_domains))

# グローバル環境に一括展開
list2env(other_domains, envir = .GlobalEnv)

# test5個別チェック
# ここにドメインごとのチェックを追加していく(参考: tools/validate_datasets_test1_web.R・
# test2_web.R・test3_web.R・test4_web.R)

# AE
c("AETERM", "AETOXGR", "AESTDTC", "AESER", "AEACN", "AEREL", "AEOUT", "AEENDTC") %>%
  check_required_vars(ae, ., domain_name = "AE")
c("AESER", "AETOXGR", "AEACN", "AEREL", "AEOUT") %>%
  walk(~ run_value_equals_checks_from_csv(ae, "AE", .x, fixed_value_checks_csv_path))

target_ae_cols <- c("AESDTH", "AESLIFE", "AESHOSP", "AESDISAB", "AESCONG", "AESMIE")
tmp_ae_y <- ae %>% filter(AESER == "Y")
target_ae_cols %>% walk(~ run_value_equals_checks_from_csv(tmp_ae_y, "AE", .x, fixed_value_checks_csv_path))
tmp_ae_y %>% check_required_vars(target_ae_cols, domain_name = "AE")
ae %>% filter(AESER != "Y") %>% check_blank_vars(target_ae_cols, domain_name = "AE")
"AESTDTC" %>% check_date_before_today(ae, ., domain_name = "AE")
"AEENDTC" %>% check_date_after_var_before_today(ae, ., "AESTDTC", domain_name = "AE")

# CE/PR(primary_graft_failure)
check_ce_engraftment <- function(ce, ceterm, suffix, fixed_value_checks_csv_path) {
  label <- str_c("CE(primary_graft_failure/", ceterm, ")")
  tmp_ce <- ce %>% filter(CESPID == "primary_graft_failure" & CETERM == ceterm)
  tmp_ce %>% check_required_vars("CEOCCUR", domain_name = label)
  target_ce_cols <- c("CETERM", "CEPRESP")
  tmp_ce_renamed <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
  str_c(target_ce_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))
  tmp_ce %>% filter(CEOCCUR == "Y") %>% check_required_vars("CESTDTC", domain_name = label)
  "CESTDTC" %>% check_date_before_today(tmp_ce %>% filter(CEOCCUR == "Y"), ., domain_name = label)
  tmp_ce %>% filter(CEOCCUR == "N") %>% check_blank_vars("CESTDTC", domain_name = label)
  cat(label, "チェック: OK(", nrow(tmp_ce), "件)\n", sep = "")
}
check_ce_engraftment(ce, "NEUTROPHIL ENGRAFTMENT", "_2", fixed_value_checks_csv_path)
check_ce_engraftment(ce, "PLATELET ENGRAFTMENT (>=20,000/uL)", "_3", fixed_value_checks_csv_path)
check_ce_engraftment(ce, "PLATELET ENGRAFTMENT (>=50,000/uL)", "_4", fixed_value_checks_csv_path)
check_ce_engraftment(ce, "RETICULOCYTE ENGRAFTMENT", "_5", fixed_value_checks_csv_path)

tmp_ce <- ce %>% filter(CESPID == "primary_graft_failure" & CETERM == "PRIMARY GRAFT FAILURE")
tmp_ce %>% check_required_vars("CEOCCUR", domain_name = "CE(primary_graft_failure)")
target_ce_cols <- c("CETERM", "CEPRESP", "CECAT")
suffix <- "_1"
tmp_ce_renamed <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
str_c(target_ce_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))
tmp_ce_y <- tmp_ce %>% filter(CEOCCUR == "Y")
target_ce_cols <- c("CEOUT")
tmp_ce_renamed <- tmp_ce_y %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
str_c(target_ce_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))
tmp_ce_y %>% check_required_vars(c("CESTDTC", "CEOUT"), domain_name = "CE(primary_graft_failure)")
"CESTDTC" %>% check_date_before_today(tmp_ce_y, ., domain_name = "CE(primary_graft_failure)")
tmp_ce %>% filter(CEOCCUR == "N") %>% check_blank_vars(c("CESTDTC", "CEOUT"), domain_name = "CE(primary_graft_failure)")

# PR(サルベージ移植)。PR行自体がCE(該当CETERM)のCEOCCUR=="Y"の被験者にのみ存在する
# (行の有無で条件分岐しており、値の空欄化ではない)ため、primary/secondary_graft_failureで
# 共通の関数にまとめる
check_pr_salvage_transplant <- function(ce, pr, alias_name, ceterm, suffix, fixed_value_checks_csv_path) {
  tmp_pr <- pr %>% filter(PRSPID == alias_name)
  tmp_pr %>% check_required_vars("PROCCUR", domain_name = "PR")
  target_pr_cols <- c("PRPRESP", "PRSTRTPT", "PRSTTPT")
  tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
  str_c(target_pr_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))

  tmp_ce <- ce %>% filter(CESPID == alias_name & CETERM == ceterm & CEOCCUR == "Y") %>% select(USUBJID)
  tmp_pr <- pr %>% filter(PRSPID == alias_name) %>% inner_join(tmp_ce, by = "USUBJID")
  target_pr_cols <- c("PROCCUR")
  tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
  str_c(target_pr_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))

  tmp_pr <- pr %>% filter(PRSPID == alias_name & PROCCUR == "Y")
  target_pr_cols <- c("PRTRT")
  tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
  str_c(target_pr_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))

  tmp_ce <- ce %>% filter(CESPID == alias_name & CETERM == ceterm & CEOCCUR == "Y") %>% select(USUBJID, CESTDTC)
  tmp_pr <- pr %>% filter(PRSPID == alias_name & PROCCUR == "Y") %>% inner_join(tmp_ce, by = "USUBJID")
  tmp_pr %>% check_date_after_var_before_today("PRSTDTC", "CESTDTC", domain_name = "PR")
}
check_pr_salvage_transplant(ce, pr, "primary_graft_failure", "PRIMARY GRAFT FAILURE", "_3", fixed_value_checks_csv_path)

# CE/PR(secondary_graft_failure)
tmp_ce <- ce %>% filter(CESPID == "secondary_graft_failure" & CETERM == "SECONDARY GRAFT FAILURE")
tmp_ce %>% check_required_vars("CEOCCUR", domain_name = "CE(secondary_graft_failure)")
target_ce_cols <- c("CETERM", "CEPRESP", "CECAT")
suffix <- "_6"
tmp_ce_renamed <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
str_c(target_ce_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))
tmp_ce_y <- tmp_ce %>% filter(CEOCCUR == "Y")
tmp_ce_y %>% check_required_vars(c("CESTDTC", "CEOUT"), domain_name = "CE(secondary_graft_failure)")
target_ce_cols <- c("CEOUT")
tmp_ce_renamed <- tmp_ce_y %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
str_c(target_ce_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))
"CESTDTC" %>% check_date_before_today(tmp_ce_y, ., domain_name = "CE(secondary_graft_failure)")
tmp_ce %>% filter(CEOCCUR == "N") %>% check_blank_vars(c("CESTDTC", "CEOUT"), domain_name = "CE(secondary_graft_failure)")

check_pr_salvage_transplant(ce, pr, "secondary_graft_failure", "SECONDARY GRAFT FAILURE", "_4", fixed_value_checks_csv_path)

# CE(agvhd)
check_ce_agvhd <- function(ce, ctcae_grade, suffix, fixed_value_checks_csv_path) {
  label <- str_c("CE(agvhd/Grade", ctcae_grade, ")")
  tmp_ce <- ce %>% filter(CESPID == "agvhd" & CETERM == "ACUTE GRAFT VERSUS HOST DISEASE" & CETOXGR == ctcae_grade)
  tmp_ce %>% check_required_vars("CEOCCUR", domain_name = label)
  target_ce_cols <- c("CETERM", "CECAT", "CEPRESP", "CETOXGR")
  tmp_ce_renamed <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
  str_c(target_ce_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))
  tmp_ce_y <- tmp_ce %>% filter(CEOCCUR == "Y")
  tmp_ce_y %>% check_required_vars(c("CESTDTC", "CEOUT"), domain_name = label)
  target_ce_cols <- c("CEOUT")
  tmp_ce_y_renamed <- tmp_ce_y %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
  str_c(target_ce_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ce_y_renamed, "CE", .x, fixed_value_checks_csv_path))
  "CESTDTC" %>% check_date_before_today(tmp_ce_y, ., domain_name = label)
  tmp_ce %>% filter(CEOCCUR == "N") %>% check_blank_vars(c("CESTDTC", "CEOUT"), domain_name = label)
  cat(label, "チェック: OK(", nrow(tmp_ce), "件)\n", sep = "")
}
check_ce_agvhd(ce, "2", "_7", fixed_value_checks_csv_path)
check_ce_agvhd(ce, "3", "_8", fixed_value_checks_csv_path)
check_ce_agvhd(ce, "4", "_9", fixed_value_checks_csv_path)

# CE(cgvhd)
check_ce_cgvhd <- function(ce, ctcae_grade, suffix, fixed_value_checks_csv_path) {
  label <- str_c("CE(cgvhd/", ctcae_grade, ")")
  tmp_ce <- ce %>% filter(CESPID == "cgvhd" & CETERM == "CHRONIC GRAFT VERSUS HOST DISEASE" & CETOXGR == ctcae_grade)
  tmp_ce %>% check_required_vars("CEOCCUR", domain_name = label)
  target_ce_cols <- c("CETERM", "CECAT", "CEPRESP", "CETOXGR")
  tmp_ce_renamed <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
  str_c(target_ce_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))
  tmp_ce_y <- tmp_ce %>% filter(CEOCCUR == "Y")
  tmp_ce_y %>% check_required_vars(c("CESTDTC", "CEOUT"), domain_name = label)
  target_ce_cols <- c("CEOUT")
  tmp_ce_y_renamed <- tmp_ce_y %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
  str_c(target_ce_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ce_y_renamed, "CE", .x, fixed_value_checks_csv_path))
  "CESTDTC" %>% check_date_before_today(tmp_ce_y, ., domain_name = label)
  tmp_ce %>% filter(CEOCCUR == "N") %>% check_blank_vars(c("CESTDTC", "CEOUT"), domain_name = label)
  cat(label, "チェック: OK(", nrow(tmp_ce), "件)\n", sep = "")
}
check_ce_cgvhd(ce, "Moderate", "_10", fixed_value_checks_csv_path)
check_ce_cgvhd(ce, "Severe", "_11", fixed_value_checks_csv_path)

# CE(a_cardiac_failure・cystitis_noninfective)
check_ce_simple <- function(ce, cespid, ceterm, suffix, fixed_value_checks_csv_path) {
  label <- str_c("CE(", cespid, "/", ceterm, ")")
  tmp_ce <- ce %>% filter(CESPID == cespid & CETERM == ceterm)
  tmp_ce %>% check_required_vars("CEOCCUR", domain_name = label)
  target_ce_cols <- c("CETERM", "CECAT", "CEPRESP")
  tmp_ce_renamed <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
  str_c(target_ce_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))
  tmp_ce_y <- tmp_ce %>% filter(CEOCCUR == "Y")
  tmp_ce_y %>% check_required_vars("CESTDTC", domain_name = label)
  "CESTDTC" %>% check_date_before_today(tmp_ce_y, ., domain_name = label)
  tmp_ce %>% filter(CEOCCUR == "N") %>% check_blank_vars("CESTDTC", domain_name = label)
  cat(label, "チェック: OK(", nrow(tmp_ce), "件)\n", sep = "")
}
check_ce_simple(ce, "a_cardiac_failure", "Cardiac failure", "_12", fixed_value_checks_csv_path)
check_ce_simple(ce, "a_cardiac_failure", "Left ventricular dysfunction", "_13", fixed_value_checks_csv_path)
check_ce_simple(ce, "cystitis_noninfective", "Cystitis noninfective", "_14", fixed_value_checks_csv_path)

# CE/MB(bacteraemia)
check_ce_simple(ce, "bacteraemia", "Bacteraemia", "_15", fixed_value_checks_csv_path)

tmp_mb <- mb %>% filter(MBSPID == "bacteraemia")
tmp_mb %>% check_required_vars("MBORRES", domain_name = "MB")
target_mb_cols <- c("MBTESTCD", "MBTEST", "MBSPEC", "MBRESCAT")
suffix <- "_1"
tmp_mb_renamed <- tmp_mb %>% rename_with(~ str_c(.x, suffix), all_of(target_mb_cols))
str_c(target_mb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_mb_renamed, "MB", .x, fixed_value_checks_csv_path))

# CE/FA(fungal_infections)
check_ce_simple(ce, "fungal_infections", "Fungal infection", "_16", fixed_value_checks_csv_path)

check_fa_organ <- function(fa, faloc, suffix, fixed_value_checks_csv_path) {
  label <- str_c("FA(fungal_infections/", faloc, ")")
  tmp_fa <- fa %>% filter(FASPID == "fungal_infections" & FALOC == faloc)
  tmp_fa %>% check_required_vars("FAORRES", domain_name = label)
  target_fa_cols <- c("FAOBJ", "FACAT", "FATEST", "FATESTCD", "FALOC")
  tmp_fa_renamed <- tmp_fa %>% rename_with(~ str_c(.x, suffix), all_of(target_fa_cols))
  str_c(target_fa_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_fa_renamed, "FA", .x, fixed_value_checks_csv_path))
  tmp_fa_orres_renamed <- tmp_fa %>% rename_with(~ str_c(.x, suffix), "FAORRES")
  run_value_equals_checks_from_csv(tmp_fa_orres_renamed, "FA", str_c("FAORRES", suffix), fixed_value_checks_csv_path)
  cat(label, "チェック: OK(", nrow(tmp_fa), "件)\n", sep = "")
}
fa_organ_faloc <- c("LUNG", "LIVER", "KIDNEY", "SPLEEN", "SKIN", "CENTRAL NERVOUS SYSTEM", "INTRAOCULAR", "BLOOD", "OTHER", "UNKNOWN")
for (i in seq_along(fa_organ_faloc)) {
  check_fa_organ(fa, fa_organ_faloc[i], str_c("_", 69 + i), fixed_value_checks_csv_path)
}

# CE(ebv_lpd)
check_ce_simple(ce, "ebv_lpd", "Post transplant Epstein-Barr virus associated lymphoproliferative disorder", "_17", fixed_value_checks_csv_path)

# CE/CM/PR(sos_report)
tmp_ce <- ce %>% filter(CESPID == "sos_report" & CESCAT == "")
tmp_ce %>% check_required_vars("CEOCCUR", domain_name = "CE(sos_report)")
target_ce_cols <- c("CETERM", "CECAT", "CEPRESP")
suffix <- "_18"
tmp_ce_renamed <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
str_c(target_ce_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))

tmp_ce_y <- tmp_ce %>% filter(CEOCCUR == "Y")
tmp_ce_y %>% check_required_vars(c("CESTDTC", "CESEV", "CEOUT"), domain_name = "CE(sos_report)")
target_ce_cols <- c("CESEV", "CEOUT")
tmp_ce_y_renamed <- tmp_ce_y %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
str_c(target_ce_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ce_y_renamed, "CE", .x, fixed_value_checks_csv_path))
"CESTDTC" %>% check_date_before_today(tmp_ce_y, ., domain_name = "CE(sos_report)")
tmp_ce %>% filter(CEOCCUR == "N") %>% check_blank_vars(c("CESTDTC", "CESEV", "CEOUT"), domain_name = "CE(sos_report)")

tmp_ce_recovered <- tmp_ce_y %>% filter(CEOUT == "RECOVERED/RESOLVED")
tmp_ce_recovered %>% check_required_vars("CEENDTC", domain_name = "CE(sos_report)")
"CEENDTC" %>% check_date_before_today(tmp_ce_recovered, ., domain_name = "CE(sos_report)")
tmp_ce_recovered %>% check_blank_vars("CEENRF", domain_name = "CE(sos_report)")

tmp_ce_ongoing <- tmp_ce_y %>% filter(CEOUT != "RECOVERED/RESOLVED")
tmp_ce_ongoing %>% check_required_vars("CEENRF", domain_name = "CE(sos_report)")
tmp_ce_ongoing %>% check_values_subset_of("CEENRF", "ONGOING", domain_name = "CE(sos_report)")
tmp_ce_ongoing %>% check_blank_vars("CEENDTC", domain_name = "CE(sos_report)")

tmp_ce_nadir <- ce %>% filter(CESPID == "sos_report" & CESCAT == "NADIR EVALUATION")
tmp_ce_nadir %>% check_required_vars("CESEV", domain_name = "CE(sos_report/nadir)")
target_ce_cols <- c("CETERM", "CECAT", "CESCAT", "CEPRESP", "CESEV")
suffix <- "_19"
tmp_ce_nadir_renamed <- tmp_ce_nadir %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
str_c(target_ce_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ce_nadir_renamed, "CE", .x, fixed_value_checks_csv_path))

tmp_cm <- cm %>% filter(CMSPID == "sos_report")
tmp_cm %>% check_required_vars("CMOCCUR", domain_name = "CM(sos_report)")
target_cm_cols <- c("CMTRT", "CMCAT", "CMPRESP", "CMOCCUR", "CMSTRTPT", "CMSTTPT")
suffix <- "_3"
tmp_cm_renamed <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm_renamed, "CM", .x, fixed_value_checks_csv_path))

tmp_pr <- pr %>% filter(PRSPID == "sos_report")
tmp_pr %>% check_required_vars("PROCCUR", domain_name = "PR(sos_report)")
target_pr_cols <- c("PRTRT", "PRPRESP", "PROCCUR", "PRSTRTPT", "PRSTTPT")
suffix <- "_5"
tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
str_c(target_pr_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))

# CE(relapse)
check_ce_simple(ce, "relapse", "HEMATOLOGIC RELAPSE", "_20", fixed_value_checks_csv_path)
check_ce_simple(ce, "second_cancer", "SECOND CANCER", "_21", fixed_value_checks_csv_path)

# CM
tmp_cm <- cm %>% filter(CMTRT == "CYCLOPHOSPHAMIDE HYDRATE")
tmp_cm %>% filter(CMOCCUR == "Y") %>% check_numeric_range("CMDOSE", min_value = 1, max_value = 15000, domain_name = "CM")
tmp_cm <- cm %>% filter(CMTRT == "Cytarabine")
tmp_cm %>% filter(CMOCCUR == "Y") %>% check_numeric_range("CMDOSE", min_value = 1, max_value = 18000, domain_name = "CM")
tmp_cm <- cm %>% filter(CMTRT == "ETOPOSIDE")
tmp_cm %>% filter(CMOCCUR == "Y") %>% check_numeric_range("CMDOSE", min_value = 1, max_value = 3600, domain_name = "CM")
tmp_cm <- cm %>% filter(CMTRT == "MELPHALAN")
tmp_cm %>% filter(CMOCCUR == "Y") %>% check_numeric_range("CMDOSE", min_value = 1, max_value = 480, domain_name = "CM")
tmp_cm <- cm %>% filter(CMTRT == "FLUDARABINE PHOSPHATE")
tmp_cm %>% filter(CMOCCUR == "Y") %>% check_numeric_range("CMDOSE", min_value = 1, max_value = 360, domain_name = "CM")
tmp_cm <- cm %>% filter(CMTRT == "CLOFARABINE")
tmp_cm %>% filter(CMOCCUR == "Y") %>% check_numeric_range("CMDOSE", min_value = 1, max_value = 600, domain_name = "CM")
tmp_cm <- cm %>% filter(CMTRT == "BUSULFAN" | CMTRT == "Busulfan")
tmp_cm %>% filter(CMOCCUR == "Y") %>% check_numeric_range("CMDOSE", min_value = 1, max_value = 30, domain_name = "CM")
tmp_cm <- cm %>% filter(
  CMTRT == "CYCLOPHOSPHAMIDE HYDRATE" |
    CMTRT == "Cytarabine" |
    CMTRT == "ETOPOSIDE" |
    CMTRT == "MELPHALAN" |
    CMTRT == "FLUDARABINE PHOSPHATE" |
    CMTRT == "CLOFARABINE" |
    CMTRT == "BUSULFAN" |
    CMTRT == "Busulfan") %>%
  filter(CMPRESP == "Y")
suffix <- "_1"
target_cm_cols <- c("CMCAT", "CMPRESP", "CMOCCUR")
tmp_cm_renamed <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm_renamed, "CM", .x, fixed_value_checks_csv_path))
tmp_cm <- tmp_cm %>% filter(CMOCCUR == "Y")
target_cm_cols <- c("CMDOSU")
tmp_cm_renamed <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm_renamed, "CM", .x, fixed_value_checks_csv_path))
tmp_cm <- cm %>% filter(
  CMTRT == "BLINATUMOMAB(GENETICAL RECOMBINATION)" |
    CMTRT == "INOTUZUMAB OZOGAMICIN(GENETICAL RECOMBINATION)" |
    CMTRT == "GEMTUZUMAB OZOGAMICIN(GENETICAL RECOMBINATION)" |
    CMTRT == "Chimeric antigen receptor-T cell therapy" |
    CMTRT == "THYMOGLOBULINE" |
    CMTRT == "ALEMTUZUMAB(GENETICAL RECOMBINATION)") %>%
  filter(CMPRESP == "Y")
target_cm_cols <- c("CMCAT", "CMPRESP", "CMOCCUR")
tmp_cm_renamed <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm_renamed, "CM", .x, fixed_value_checks_csv_path))

# CM(cmv-pt)
tmp_cm <- cm %>% filter(CMSPID == "cmv-pt")
tmp_cm %>% check_required_vars("CMOCCUR", domain_name = "CM(cmv-pt)")
target_cm_cols <- c("CMTRT", "CMCAT", "CMPRESP")
suffix <- "_2"
tmp_cm_renamed <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm_renamed, "CM", .x, fixed_value_checks_csv_path))
tmp_cm_y <- tmp_cm %>% filter(CMOCCUR == "Y")
tmp_cm_y %>% check_required_vars("CMSTDTC", domain_name = "CM(cmv-pt)")
"CMSTDTC" %>% check_date_before_today(tmp_cm_y, ., domain_name = "CM(cmv-pt)")
tmp_cm %>% filter(CMOCCUR == "N") %>% check_blank_vars("CMSTDTC", domain_name = "CM(cmv-pt)")

# EC(ec_a_400)
target_ec_cols <- c("ECTRT", "ECMOOD", "ECPRESP", "ECDOSU")
tmp_ec <- ec %>% filter(ECSPID == "ec_a_400")
tmp_ec %>% check_required_vars("ECOCCUR", domain_name = "EC")
suffix <- "_1"
tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))
tmp_ec <- tmp_ec %>% filter(ECOCCUR == "Y")
tmp_ec %>% check_required_vars(c("ECDOSE", "ECSTDTC", "ECENDTC"), domain_name = "EC")
tmp_ec %>% check_numeric_range("ECDOSE", min_value = 0, max_value = 70, domain_name = "EC")
"ECSTDTC" %>% check_date_before_today(tmp_ec, ., domain_name = "EC")
"ECENDTC" %>% check_date_before_today(tmp_ec, ., domain_name = "EC")

# EC(ec_b1_400)
target_ec_cols <- c("ECTRT", "ECMOOD", "ECPRESP", "ECDOSU")
tmp_ec <- ec %>% filter(ECSPID == "ec_b1_400")
tmp_ec %>% check_required_vars("ECOCCUR", domain_name = "EC")
suffix <- "_5"
tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))
tmp_ec <- tmp_ec %>% filter(ECOCCUR == "Y")
tmp_ec %>% check_required_vars(c("ECDOSE", "ECSTDTC", "ECENDTC"), domain_name = "EC")
tmp_ec %>% check_numeric_range("ECDOSE", min_value = 0, max_value = 70, domain_name = "EC")
"ECSTDTC" %>% check_date_before_today(tmp_ec, ., domain_name = "EC")
"ECENDTC" %>% check_date_before_today(tmp_ec, ., domain_name = "EC")

# EC(ec_b3_400)
target_ec_cols <- c("ECTRT", "ECMOOD", "ECPRESP")
tmp_ec <- ec %>% filter(ECSPID == "ec_b3_400")
tmp_ec %>% check_required_vars("ECOCCUR", domain_name = "EC")
suffix <- "_6"
tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))

tmp_ec_y <- tmp_ec %>% filter(ECOCCUR == "Y")
tmp_ec_y %>% check_required_vars(c("ECSTDTC", "ECENDTC"), domain_name = "EC")
"ECSTDTC" %>% check_date_before_today(tmp_ec_y, ., domain_name = "EC")
"ECENDTC" %>% check_date_after_var_before_today(tmp_ec_y, ., "ECSTDTC", domain_name = "EC")

tmp_ec %>% filter(ECOCCUR == "N") %>% check_blank_vars(c("ECSTDTC", "ECENDTC"), domain_name = "EC")

# EC(ec_b2_400)
target_ec_cols <- c("ECTRT", "ECMOOD", "ECPRESP")
tmp_ec <- ec %>% filter(ECSPID == "ec_b2_400")
tmp_ec %>% check_required_vars("ECOCCUR", domain_name = "EC")
suffix <- "_7"
tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))
tmp_ec_y <- tmp_ec %>% filter(ECOCCUR == "Y")
tmp_ec_y %>% check_required_vars("ECSTDTC", domain_name = "EC")
"ECSTDTC" %>% check_date_before_today(tmp_ec_y, ., domain_name = "EC")
"ECENDTC" %>% check_date_after_var_before_today(tmp_ec_y, ., "ECSTDTC", domain_name = "EC")
target_ec_cols <- "ECENRTPT"
tmp_ec <- ec %>% filter(ECSPID == "ec_b2_400" & ECENRTPT != "")
tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))

# EC(ec_standard_1_400)
target_ec_cols <- c("ECTRT", "ECMOOD", "ECPRESP", "ECDOSU")
tmp_ec <- ec %>% filter(ECSPID == "ec_standard_1_400")
tmp_ec %>% check_required_vars("ECOCCUR", domain_name = "EC")
suffix <- "_2"
tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))

tmp_ec_y <- tmp_ec %>% filter(ECOCCUR == "Y")
tmp_ec_y %>% check_required_vars(c("ECDOSTXT", "ECSTDTC"), domain_name = "EC")
tmp_ec_y_renamed <- tmp_ec_y %>% rename_with(~ str_c(.x, suffix), "ECDOSTXT")
run_value_equals_checks_from_csv(tmp_ec_y_renamed, "EC", str_c("ECDOSTXT", suffix), fixed_value_checks_csv_path)
"ECSTDTC" %>% check_date_before_today(tmp_ec_y, ., domain_name = "EC")

tmp_ec %>% filter(ECOCCUR == "N") %>% check_blank_vars(c("ECDOSTXT", "ECSTDTC"), domain_name = "EC")

# EC(ec_standard_2_400)
target_ec_cols <- c("ECTRT", "ECMOOD", "ECPRESP")

tmp_ec <- ec %>% filter(ECSPID == "ec_standard_2_400" & ECTRT == "Ciclosporin")
tmp_ec %>% check_required_vars("ECOCCUR", domain_name = "EC")
suffix <- "_3"
tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))
tmp_ec_y <- tmp_ec %>% filter(ECOCCUR == "Y")
tmp_ec_y %>% check_required_vars("ECSTDTC", domain_name = "EC")
"ECSTDTC" %>% check_date_before_today(tmp_ec_y, ., domain_name = "EC")
"ECENDTC" %>% check_date_after_var_before_today(tmp_ec_y, ., "ECSTDTC", domain_name = "EC")

tmp_ec <- ec %>% filter(ECSPID == "ec_standard_2_400" & ECTRT == "TACROLIMUS HYDRATE")
tmp_ec %>% check_required_vars("ECOCCUR", domain_name = "EC")
suffix <- "_4"
tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))
tmp_ec_y <- tmp_ec %>% filter(ECOCCUR == "Y")
tmp_ec_y %>% check_required_vars("ECSTDTC", domain_name = "EC")
"ECSTDTC" %>% check_date_before_today(tmp_ec_y, ., domain_name = "EC")
"ECENDTC" %>% check_date_after_var_before_today(tmp_ec_y, ., "ECSTDTC", domain_name = "EC")

# DM
c("RFICDTC", "BRTHDTC", "SEX", "RACE", "COUNTRY") %>% check_required_vars(dm, ., domain_name = "DM")
dm %>% check_date_before_today(c("BRTHDTC"), domain_name = "DM")
dm %>% check_date_after_var_before_today("RFICDTC", "BRTHDTC", domain_name = "DM")
c("SEX", "RACE", "COUNTRY") %>% walk(~ run_value_equals_checks_from_csv(dm, "DM", .x, fixed_value_checks_csv_path))
dm %>% check_blank_vars("ARM", domain_name = "DM")

# FA(ae_100)
# CTCAE有害事象のグレーディングパネル形式。GRADE(19項目、0-5の6択)/DIAGCERT(2項目、
# NONE/PROBABLE/PROVENの3択)/EBMTSVGR(1項目、DEATH/MILD/MODERATE/NONE/SEVERE/
# VERY SEVERE- MOD/MOFの6択)の3 FATESTCDがそれぞれ独立したFAOBJパネルを持つ。
# FATEST/FACATは固定値、FALOCは常に空欄
fa_grade_panel <- c(
  "Acute kidney injury", "Alanine aminotransferase increased",
  "Aspartate aminotransferase increased", "Blood bilirubin increased",
  "Cardiac failure", "Catheter related infection", "Cystitis noninfective",
  "Depressed level of consciousness", "Febrile neutropenia", "Haemorrhage intracranial",
  "Ileus", "Leukoencephalopathy", "Multi-organ failure", "Pancreatitis",
  "Posterior reversible encephalopathy syndrome", "Secondary haemophagocytic lymphohistiocytosis",
  "Seizure", "Sepsis", "Thrombotic microangiopathy"
)
fa_diagcert_panel <- c("Fungaemia", "Post transplant Epstein-Barr virus associated lymphoproliferative disorder")
fa_ebmtsvgr_panel <- "Sinusoidal obstruction syndrome"

# GRADEはFAOBJ(AE語)ごとにCTCAE Gradeで定義されている選択肢が異なる(例: Acute kidney injuryは
# 0/3/4/5のみでGrade1・2が定義されていない)ため、orres_per_faobj=TRUEにしてcheck_fa_panel内では
# FAORRESの値域チェックを行わず、呼び出し側でFAOBJごとに別途check_fa_grade_orres_by_faobj()を
# 呼び出しCSV(FAORRES_<FAOBJごとのsuffix>)で管理する
fa_grade_faobj_suffix <- c(
  "Acute kidney injury" = "_4",
  "Alanine aminotransferase increased" = "_5",
  "Aspartate aminotransferase increased" = "_6",
  "Blood bilirubin increased" = "_7",
  "Cardiac failure" = "_8",
  "Catheter related infection" = "_9",
  "Cystitis noninfective" = "_10",
  "Depressed level of consciousness" = "_11",
  "Febrile neutropenia" = "_12",
  "Haemorrhage intracranial" = "_13",
  "Ileus" = "_14",
  "Leukoencephalopathy" = "_15",
  "Multi-organ failure" = "_16",
  "Pancreatitis" = "_17",
  "Posterior reversible encephalopathy syndrome" = "_18",
  "Secondary haemophagocytic lymphohistiocytosis" = "_19",
  "Seizure" = "_20",
  "Sepsis" = "_21",
  "Thrombotic microangiopathy" = "_22"
)

# orres_valuesを指定した場合はコード内定数と照合し、orres_per_faobj=TRUEの場合はFAORRESの
# 値域チェックを呼び出し側に委ねる。いずれでもない(既定値)場合はRSTEST/RSCATと同様に
# FAORRESの許容値もCSV側(FAORRES_<suffix>)で管理する
check_fa_panel <- function(fa, faspid, fatestcd, expected_faobj, suffix, fixed_value_checks_csv_path, orres_values = NULL, orres_per_faobj = FALSE) {
  label <- str_c("FA(", faspid, "/", fatestcd, ")")
  tmp_fa <- fa %>% filter(FASPID == faspid & FATESTCD == fatestcd)

  actual_faobj <- tmp_fa %>% pull(FAOBJ) %>% unique() %>% sort()
  if (!setequal(actual_faobj, expected_faobj)) {
    stop(str_c(label, "パネル項目一致チェック: NG"))
  }
  usubjid_n <- n_distinct(tmp_fa[["USUBJID"]])
  missing_items <- tmp_fa %>% distinct(USUBJID, FAOBJ) %>% count(USUBJID) %>% filter(n < length(expected_faobj))
  if (nrow(missing_items) > 0) {
    stop(str_c(label, "被験者ごとの項目充足チェック: NG"))
  }
  cat(label, "パネル一致チェック: OK(", length(expected_faobj), "項目 x ", usubjid_n, "名)\n", sep = "")

  target_fa_cols <- c("FATEST", "FACAT")
  tmp_fa_renamed <- tmp_fa %>% rename_with(~ str_c(.x, suffix), all_of(target_fa_cols))
  str_c(target_fa_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_fa_renamed, "FA", .x, fixed_value_checks_csv_path))

  tmp_fa %>% check_required_vars("FAORRES", domain_name = label)
  if (orres_per_faobj) {
    # FAOBJ単位のチェックは呼び出し側(check_fa_grade_orres_by_faobj)で行う
  } else if (!is.null(orres_values)) {
    tmp_fa %>% check_values_subset_of("FAORRES", orres_values, domain_name = label)
  } else {
    tmp_fa_orres_renamed <- tmp_fa %>% rename_with(~ str_c(.x, suffix), "FAORRES")
    run_value_equals_checks_from_csv(tmp_fa_orres_renamed, "FA", str_c("FAORRES", suffix), fixed_value_checks_csv_path)
  }
  tmp_fa %>% check_blank_vars("FALOC", domain_name = label)
}

check_fa_grade_orres_by_faobj <- function(fa, faspid, faobj, suffix, fixed_value_checks_csv_path, fatestcd = "GRADE") {
  label <- str_c("FA(", faspid, "/", fatestcd, "/", faobj, ")")
  tmp_fa <- fa %>% filter(FASPID == faspid & FATESTCD == fatestcd & FAOBJ == faobj)
  tmp_fa_renamed <- tmp_fa %>% rename_with(~ str_c(.x, suffix), "FAORRES")
  run_value_equals_checks_from_csv(tmp_fa_renamed, "FA", str_c("FAORRES", suffix), fixed_value_checks_csv_path)
  cat(label, "値域チェック: OK(", nrow(tmp_fa), "件)\n", sep = "")
}

check_fa_panel(fa, "ae_100", "GRADE", fa_grade_panel, "_1", fixed_value_checks_csv_path, orres_per_faobj = TRUE)
for (faobj in names(fa_grade_faobj_suffix)) {
  check_fa_grade_orres_by_faobj(fa, "ae_100", faobj, fa_grade_faobj_suffix[[faobj]], fixed_value_checks_csv_path)
}
check_fa_panel(fa, "ae_100", "DIAGCERT", fa_diagcert_panel, "_2", fixed_value_checks_csv_path)
check_fa_panel(fa, "ae_100", "EBMTSVGR", fa_ebmtsvgr_panel, "_3", fixed_value_checks_csv_path)

fa_comptbl_faobj <- c("SEX", "Blood Type")
check_fa_panel(fa, "sct_300", "COMPTBL", fa_comptbl_faobj, "_23", fixed_value_checks_csv_path, orres_per_faobj = TRUE)
check_fa_grade_orres_by_faobj(fa, "sct_300", "SEX", "_24", fixed_value_checks_csv_path, fatestcd = "COMPTBL")
check_fa_grade_orres_by_faobj(fa, "sct_300", "Blood Type", "_25", fixed_value_checks_csv_path, fatestcd = "COMPTBL")

# ae_600はae_100と同じCTCAEグレーディングパネル(GRADE/DIAGCERT/EBMTSVGR、パネル項目・
# 許容値ともすべて同一)のため、同じfa_grade_panel/fa_diagcert_panel/fa_ebmtsvgr_panelを
# 別suffixで流用する
fa_grade_faobj_suffix_600 <- c(
  "Acute kidney injury" = "_27",
  "Alanine aminotransferase increased" = "_28",
  "Aspartate aminotransferase increased" = "_29",
  "Blood bilirubin increased" = "_30",
  "Cardiac failure" = "_31",
  "Catheter related infection" = "_32",
  "Cystitis noninfective" = "_33",
  "Depressed level of consciousness" = "_34",
  "Febrile neutropenia" = "_35",
  "Haemorrhage intracranial" = "_36",
  "Ileus" = "_37",
  "Leukoencephalopathy" = "_38",
  "Multi-organ failure" = "_39",
  "Pancreatitis" = "_40",
  "Posterior reversible encephalopathy syndrome" = "_41",
  "Secondary haemophagocytic lymphohistiocytosis" = "_42",
  "Seizure" = "_43",
  "Sepsis" = "_44",
  "Thrombotic microangiopathy" = "_45"
)
check_fa_panel(fa, "ae_600", "GRADE", fa_grade_panel, "_26", fixed_value_checks_csv_path, orres_per_faobj = TRUE)
for (faobj in names(fa_grade_faobj_suffix_600)) {
  check_fa_grade_orres_by_faobj(fa, "ae_600", faobj, fa_grade_faobj_suffix_600[[faobj]], fixed_value_checks_csv_path)
}
check_fa_panel(fa, "ae_600", "DIAGCERT", fa_diagcert_panel, "_46", fixed_value_checks_csv_path)
check_fa_panel(fa, "ae_600", "EBMTSVGR", fa_ebmtsvgr_panel, "_47", fixed_value_checks_csv_path)

# ae_700もae_100/ae_600と同じCTCAEグレーディングパネル(パネル項目・許容値ともすべて同一)
fa_grade_faobj_suffix_700 <- c(
  "Acute kidney injury" = "_49",
  "Alanine aminotransferase increased" = "_50",
  "Aspartate aminotransferase increased" = "_51",
  "Blood bilirubin increased" = "_52",
  "Cardiac failure" = "_53",
  "Catheter related infection" = "_54",
  "Cystitis noninfective" = "_55",
  "Depressed level of consciousness" = "_56",
  "Febrile neutropenia" = "_57",
  "Haemorrhage intracranial" = "_58",
  "Ileus" = "_59",
  "Leukoencephalopathy" = "_60",
  "Multi-organ failure" = "_61",
  "Pancreatitis" = "_62",
  "Posterior reversible encephalopathy syndrome" = "_63",
  "Secondary haemophagocytic lymphohistiocytosis" = "_64",
  "Seizure" = "_65",
  "Sepsis" = "_66",
  "Thrombotic microangiopathy" = "_67"
)
check_fa_panel(fa, "ae_700", "GRADE", fa_grade_panel, "_48", fixed_value_checks_csv_path, orres_per_faobj = TRUE)
for (faobj in names(fa_grade_faobj_suffix_700)) {
  check_fa_grade_orres_by_faobj(fa, "ae_700", faobj, fa_grade_faobj_suffix_700[[faobj]], fixed_value_checks_csv_path)
}
check_fa_panel(fa, "ae_700", "DIAGCERT", fa_diagcert_panel, "_68", fixed_value_checks_csv_path)
check_fa_panel(fa, "ae_700", "EBMTSVGR", fa_ebmtsvgr_panel, "_69", fixed_value_checks_csv_path)

# MH(registration)
target_mh_cols <- c("MHCAT", "MHPRESP", "MHENRTPT", "MHENTPT", "MHOCCUR", "MHTERM")
mh %>% check_required_vars(target_mh_cols, domain_name = "MH")
target_mh_cols %>%
  walk(~ run_value_equals_checks_from_csv(mh, "MH", .x, fixed_value_checks_csv_path))

# RS(registration)
check_rs_testcd <- function(rs, rstestcd, suffix, fixed_value_checks_csv_path, eval_fixed, rsdtc_required) {
  label <- str_c("RS(", rstestcd, ")")
  tmp_rs <- rs %>% filter(RSTESTCD == rstestcd)

  target_rs_cols <- c("RSTEST", "RSCAT", "RSORRES")
  tmp_rs %>% check_required_vars("RSORRES", domain_name = label)
  tmp_rs_renamed <- tmp_rs %>% rename_with(~ str_c(.x, suffix), all_of(target_rs_cols))
  str_c(target_rs_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_rs_renamed, "RS", .x, fixed_value_checks_csv_path))

  if (eval_fixed) {
    tmp_rs %>% check_required_vars("RSEVAL", domain_name = label)
    tmp_rs %>% check_values_subset_of("RSEVAL", "INVESTIGATOR", domain_name = label)
  } else {
    tmp_rs %>% check_blank_vars("RSEVAL", domain_name = label)
  }

  if (rsdtc_required) {
    tmp_rs %>% check_required_vars("RSDTC", domain_name = label)
  } else {
    tmp_rs %>% check_blank_vars("RSDTC", domain_name = label)
  }

  cat(label, "チェック: OK(", nrow(tmp_rs), "件)\n", sep = "")
}

check_rs_testcd(rs, "KPSS0101", "_1", fixed_value_checks_csv_path, eval_fixed = FALSE, rsdtc_required = TRUE)
check_rs_testcd(rs, "LPPSS101", "_2", fixed_value_checks_csv_path, eval_fixed = FALSE, rsdtc_required = TRUE)
check_rs_testcd(rs, "REMSTAT", "_3", fixed_value_checks_csv_path, eval_fixed = TRUE, rsdtc_required = FALSE)

# SUPPQUAL(registration)。QNAM/QLABEL/QVAL/QORIG等はCDISC SDTM標準上ドメイン名prefixを付けない
target_suppqual_cols <- c("RDOMAIN", "IDVAR", "IDVARVAL", "QNAM", "QLABEL", "QVAL", "QORIG")
suppqual %>% check_required_vars(target_suppqual_cols, domain_name = "SUPPQUAL")
target_suppqual_cols %>%
  walk(~ run_value_equals_checks_from_csv(suppqual, "SUPPQUAL", .x, fixed_value_checks_csv_path))

# LB(screening_100)
target_lb_cols <- c("LBTEST", "LBCAT", "LBSPEC", "LBMETHOD", "LBORRESU", "LBSCAT")

check_lb_testcd <- function(lb, lbtestcd, suffix, fixed_value_checks_csv_path) {
  label <- str_c("LB(", lbtestcd, ")")
  tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == lbtestcd)

  tmp_lb_renamed <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb_renamed, "LB", .x, fixed_value_checks_csv_path))

  tmp_lb_done <- tmp_lb %>% filter(LBSTAT != "NOT DONE")
  tmp_lb_notdone <- tmp_lb %>% filter(LBSTAT == "NOT DONE")
  tmp_lb_done %>% check_required_vars(c("LBORRES", "LBDTC"), domain_name = label)
  tmp_lb_notdone %>% check_blank_vars(c("LBORRES", "LBDTC"), domain_name = label)

  tmp_lb_done_renamed <- tmp_lb_done %>% rename_with(~ str_c(.x, suffix), "LBORRES")
  run_value_equals_checks_from_csv(tmp_lb_done_renamed, "LB", str_c("LBORRES", suffix), fixed_value_checks_csv_path)

  cat(label, "チェック: OK(", nrow(tmp_lb), "件)\n", sep = "")
}

check_lb_testcd(lb, "FUSGENID", "_1", fixed_value_checks_csv_path)
check_lb_testcd(lb, "GENMUTID", "_2", fixed_value_checks_csv_path)
check_lb_testcd(lb, "MRDQUAL", "_3", fixed_value_checks_csv_path)

# LB(lab_100)
check_lb_testcd_numeric <- function(lb, lbspid, lbtestcd, suffix, fixed_value_checks_csv_path, min_value = NA, max_value = NA) {
  label <- str_c("LB(", lbtestcd, ")")
  tmp_lb <- lb %>% filter(LBSPID == lbspid & LBTESTCD == lbtestcd)

  target_lb_cols <- c("LBTEST", "LBCAT", "LBSPEC", "LBMETHOD", "LBORRESU", "LBSCAT")
  tmp_lb_renamed <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb_renamed, "LB", .x, fixed_value_checks_csv_path))

  tmp_lb %>% filter(LBSTAT != "NOT DONE") %>% check_required_vars(c("LBORRES", "LBDTC"), domain_name = label)
  tmp_lb %>% filter(LBSTAT == "NOT DONE") %>% check_blank_vars(c("LBORRES", "LBDTC"), domain_name = label)

  if (!is.na(min_value) || !is.na(max_value)) {
    tmp_lb %>% check_numeric_range("LBORRES", min_value = min_value, max_value = max_value, domain_name = label)
  }

  cat(label, "チェック: OK(", nrow(tmp_lb), "件)\n", sep = "")
}

# field33(エストラジオール)は0-500000、field15(FSH)は0-300、field7(LH)は0-200、field24
# (テストステロン)は0-50がEDC仕様上の許容範囲(validate_numericality)
check_lb_testcd_numeric(lb, "lab_100", "ESTRDIOL", "_4", fixed_value_checks_csv_path, min_value = 0, max_value = 500000)
check_lb_testcd_numeric(lb, "lab_100", "FSH", "_5", fixed_value_checks_csv_path, min_value = 0, max_value = 300)
check_lb_testcd_numeric(lb, "lab_100", "LH", "_6", fixed_value_checks_csv_path, min_value = 0, max_value = 200)
check_lb_testcd_numeric(lb, "lab_100", "TESTOS", "_7", fixed_value_checks_csv_path, min_value = 0, max_value = 50)

# lab_700はlab_100と同じ4 LBTESTCD(LH/FSH/TESTOS/ESTRDIOL)・同じ数値範囲。TESTOS/ESTRDIOLは
# EDC仕様上field2(性別および登録時年齢)による性別条件(男性のみ/女性のみ)があるが、field2は
# どのドメイン変数にもマッピングされておらず(cdisc_sheet_configsに登場しない)、生成ロジック側にも
# 反映されないため、実データでは男女問わず両方の項目が生成される
check_lb_testcd_numeric(lb, "lab_700", "LH", "_16", fixed_value_checks_csv_path, min_value = 0, max_value = 200)
check_lb_testcd_numeric(lb, "lab_700", "FSH", "_17", fixed_value_checks_csv_path, min_value = 0, max_value = 300)
check_lb_testcd_numeric(lb, "lab_700", "TESTOS", "_18", fixed_value_checks_csv_path, min_value = 0, max_value = 50)
check_lb_testcd_numeric(lb, "lab_700", "ESTRDIOL", "_19", fixed_value_checks_csv_path, min_value = 0, max_value = 500000)

check_lb_scat_panel <- function(lb, lbtestcd, lbscat, suffix, fixed_value_checks_csv_path) {
  label <- str_c("LB(sct_300/", lbtestcd, "/", lbscat, ")")
  tmp_lb <- lb %>% filter(LBSPID == "sct_300" & LBTESTCD == lbtestcd & LBSCAT == lbscat)

  target_lb_cols <- c("LBTEST", "LBCAT", "LBSPEC")
  tmp_lb_renamed <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb_renamed, "LB", .x, fixed_value_checks_csv_path))

  tmp_lb_done <- tmp_lb %>% filter(LBSTAT != "NOT DONE")
  tmp_lb_notdone <- tmp_lb %>% filter(LBSTAT == "NOT DONE")
  tmp_lb_done %>% check_required_vars(c("LBORRES", "LBDTC"), domain_name = label)
  tmp_lb_notdone %>% check_blank_vars(c("LBORRES", "LBDTC"), domain_name = label)

  tmp_lb_done_renamed <- tmp_lb_done %>% rename_with(~ str_c(.x, suffix), "LBORRES")
  run_value_equals_checks_from_csv(tmp_lb_done_renamed, "LB", str_c("LBORRES", suffix), fixed_value_checks_csv_path)

  cat(label, "チェック: OK(", nrow(tmp_lb), "件)\n", sep = "")
}
check_lb_scat_panel(lb, "CMVIGGAB", "DONOR", "_8", fixed_value_checks_csv_path)
check_lb_scat_panel(lb, "CMVIGGAB", "RECIPIENT", "_9", fixed_value_checks_csv_path)

check_lb_testcd_numeric(lb, "sct_300", "NUCCE", "_10", fixed_value_checks_csv_path)
check_lb_testcd_numeric(lb, "sct_300", "CD34SM", "_11", fixed_value_checks_csv_path)

# LB(mrd_sct30_500)
check_lb_method_panel <- function(lb, lbmethod, suffix, fixed_value_checks_csv_path) {
  label <- str_c("LB(mrd_sct30_500/", lbmethod, ")")
  tmp_lb <- lb %>% filter(LBSPID == "mrd_sct30_500" & LBTESTCD == "MRDQUAL" & LBMETHOD == lbmethod)

  target_lb_cols <- c("LBTEST", "LBCAT", "LBSPEC", "LBMETHOD")
  tmp_lb_renamed <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb_renamed, "LB", .x, fixed_value_checks_csv_path))

  tmp_lb_done <- tmp_lb %>% filter(LBSTAT != "NOT DONE")
  tmp_lb_notdone <- tmp_lb %>% filter(LBSTAT == "NOT DONE")
  tmp_lb_done %>% check_required_vars(c("LBORRES", "LBDTC"), domain_name = label)
  tmp_lb_notdone %>% check_blank_vars(c("LBORRES", "LBDTC"), domain_name = label)

  tmp_lb_done_renamed <- tmp_lb_done %>% rename_with(~ str_c(.x, suffix), "LBORRES")
  run_value_equals_checks_from_csv(tmp_lb_done_renamed, "LB", str_c("LBORRES", suffix), fixed_value_checks_csv_path)

  cat(label, "チェック: OK(", nrow(tmp_lb), "件)\n", sep = "")
}
check_lb_method_panel(lb, "NEXT GENERATION SEQUENCING", "_12", fixed_value_checks_csv_path)
check_lb_method_panel(lb, "IG/TCR", "_13", fixed_value_checks_csv_path)
check_lb_method_panel(lb, "FLOW CYTOMETRY", "_14", fixed_value_checks_csv_path)
check_lb_method_panel(lb, "REAL-TIME POLYMERASE CHAIN REACTION ASSAY", "_15", fixed_value_checks_csv_path)

# PR
c("PROCCUR", "PRLOC") %>% check_required_vars(pr %>% filter(PRSPID == "conditioning_200"), ., domain_name = "PR")
suffix <- "_1"
target_pr_cols <- c("PRTRT", "PRPRESP", "PROCCUR", "PRDOSU", "PRLOC")
tmp_pr <- pr %>% filter(PRSPID == "conditioning_200")
tmp_pr %>% filter(PROCCUR == "Y") %>% check_numeric_range("PRDOSE", min_value = 1, max_value = 30, domain_name = "PR")
tmp_pr %>% filter(PROCCUR == "Y") %>% check_numeric_range("PRDOSFRQ", min_value = 0, domain_name = "PR")
tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
str_c(target_pr_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))

tmp_pr <- pr %>% filter(PRSPID == "sct_300")
target_pr_cols <- c("PRTRT", "PRCAT", "PRSCAT")
tmp_pr %>% check_required_vars(c(target_pr_cols, "PRSTDTC"), domain_name = "PR")
suffix <- "_2"
tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
str_c(target_pr_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))
"PRSTDTC" %>% check_date_before_today(tmp_pr, ., domain_name = "PR")
tmp_pr %>% check_blank_vars(
  c("PROCCUR", "PRPRESP", "PRDOSE", "PRDOSFRQ", "PRDOSU", "PRLOC", "PRSTRTPT", "PRSTTPT"),
  domain_name = "PR"
)

tmp_pr <- pr %>% filter(PRSPID == "primary_graft_failure")
tmp_pr %>% check_required_vars("PROCCUR", domain_name = "PR")
target_pr_cols <- c("PRPRESP", "PRSTRTPT", "PRSTTPT")
suffix <- "_3"
tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
str_c(target_pr_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))
tmp_ce <- ce %>% filter(CESPID == "primary_graft_failure" & CETERM == "PRIMARY GRAFT FAILURE" & CEOCCUR == "Y") %>% select(USUBJID)
tmp_pr <- pr %>% filter(PRSPID == "primary_graft_failure") %>% inner_join(tmp_ce, by="USUBJID")
target_pr_cols <- c("PROCCUR")
tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
str_c(target_pr_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))
tmp_pr <- pr %>% filter(PRSPID == "primary_graft_failure" & PROCCUR == "Y")
target_pr_cols <- c("PRTRT")
tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
str_c(target_pr_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))
tmp_ce <- ce %>% filter(CESPID == "primary_graft_failure" & CETERM == "PRIMARY GRAFT FAILURE" & CEOCCUR == "Y") %>% select(USUBJID, CESTDTC)
tmp_pr <- pr %>% filter(PRSPID == "primary_graft_failure" & PROCCUR == "Y") %>% inner_join(tmp_ce, by="USUBJID")
tmp_pr %>% check_date_after_var_before_today("PRSTDTC", "CESTDTC", domain_name = "PR")

tmp_pr <- pr %>% filter(PRSPID == "secondary_graft_failure")
tmp_pr %>% check_required_vars("PROCCUR", domain_name = "PR")
target_pr_cols <- c("PRPRESP", "PRSTRTPT", "PRSTTPT")
suffix <- "_4"
tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
str_c(target_pr_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))
tmp_pr_y <- tmp_pr %>% filter(PROCCUR == "Y")
tmp_pr_y %>% check_required_vars(c("PRTRT", "PRSTDTC"), domain_name = "PR")


# MI(screening_100)
target_mi_cols <- c("MITEST", "MICAT", "MISPEC", "MIMETHOD")

check_mi_micat <- function(mi, micat, suffix, fixed_value_checks_csv_path) {
  label <- str_c("MI(", micat, ")")
  tmp_mi <- mi %>% filter(MISPID == "screening_100" & MICAT == micat)

  tmp_mi_renamed <- tmp_mi %>% rename_with(~ str_c(.x, suffix), all_of(target_mi_cols))
  str_c(target_mi_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_mi_renamed, "MI", .x, fixed_value_checks_csv_path))

  tmp_mi_done <- tmp_mi %>% filter(MISTAT != "NOT DONE")
  tmp_mi_notdone <- tmp_mi %>% filter(MISTAT == "NOT DONE")
  tmp_mi_done %>% check_required_vars(c("MIORRES", "MIDTC"), domain_name = label)
  tmp_mi_notdone %>% check_blank_vars(c("MIORRES", "MIDTC"), domain_name = label)

  tmp_mi_done_renamed <- tmp_mi_done %>% rename_with(~ str_c(.x, suffix), "MIORRES")
  run_value_equals_checks_from_csv(tmp_mi_done_renamed, "MI", str_c("MIORRES", suffix), fixed_value_checks_csv_path)

  cat(label, "チェック: OK(", nrow(tmp_mi), "件)\n", sep = "")
}

check_mi_micat(mi, "CHROMOSOMAL STRUCTURE", "_1", fixed_value_checks_csv_path)
check_mi_micat(mi, "CHROMOSOME NUMBER", "_2", fixed_value_checks_csv_path)

# DS(discon・withdrawal)
target_ds_cols <- c("DSCAT", "EPOCH")

check_ds_disposition <- function(ds, dsspid, suffix, fixed_value_checks_csv_path) {
  label <- str_c("DS(", dsspid, ")")
  tmp_ds <- ds %>% filter(DSSPID == dsspid)

  target_cols <- c(target_ds_cols, "DSTERM")
  tmp_ds_renamed <- tmp_ds %>% rename_with(~ str_c(.x, suffix), all_of(target_cols))
  str_c(target_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ds_renamed, "DS", .x, fixed_value_checks_csv_path))

  tmp_ds %>% check_required_vars(c("DSSTDTC", "DSDTC"), domain_name = label)
  "DSDTC" %>% check_date_after_var_before_today(tmp_ds, ., "DSSTDTC", domain_name = label)

  cat(label, "チェック: OK(", nrow(tmp_ds), "件)\n", sep = "")
}
check_ds_disposition(ds, "discon", "_1", fixed_value_checks_csv_path)
check_ds_disposition(ds, "withdrawal", "_2", fixed_value_checks_csv_path)

# DD(死因)。EDC仕様上discon・withdrawalどちらのDSTERM=="DEATH"でも生成されうるため、
# DDSPIDの値は問わずDDドメイン全体を対象にする(DDTESTCD/DDTEST/DDORRESの選択肢はどちらの
# シート由来でも共通)
target_dd_cols <- c("DDTESTCD", "DDTEST")
suffix <- "_1"
dd_renamed <- dd %>% rename_with(~ str_c(.x, suffix), all_of(target_dd_cols))
str_c(target_dd_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(dd_renamed, "DD", .x, fixed_value_checks_csv_path))
dd %>% check_required_vars("DDORRES", domain_name = "DD")
dd_orres_renamed <- dd %>% rename_with(~ str_c(.x, suffix), "DDORRES")
run_value_equals_checks_from_csv(dd_orres_renamed, "DD", str_c("DDORRES", suffix), fixed_value_checks_csv_path)

# DD/DS USUBJID整合性チェック
# DDが存在する被験者と、DS(discon・withdrawal)でDSTERM=="DEATH"の被験者が完全に一致すること
# (どちらか一方にしか存在するUSUBJIDが無いこと)を確認する。EDC仕様上、DDはdiscon・withdrawal
# どちらのDSTERM=="DEATH"でも生成されうる(presence_conditionsに両シート分の定義がある)ため、
# withdrawal側だけでなくdiscon側も対象に含める
dd_usubjid <- dd %>% distinct(USUBJID) %>% pull(USUBJID)
ds_death_usubjid <- ds %>% filter(DSSPID %in% c("discon", "withdrawal") & DSTERM == "DEATH") %>% distinct(USUBJID) %>% pull(USUBJID)
only_in_dd <- setdiff(dd_usubjid, ds_death_usubjid)
only_in_ds_death <- setdiff(ds_death_usubjid, dd_usubjid)
if (length(only_in_dd) > 0 || length(only_in_ds_death) > 0) {
  stop(str_c(
    "DD/DS USUBJID整合性チェック: NG(DDのみに存在: ", paste(only_in_dd, collapse = ", "),
    " / DS(DEATH)のみに存在: ", paste(only_in_ds_death, collapse = ", "), ")"
  ))
}
cat("DD/DS USUBJID整合性チェック: OK(", length(dd_usubjid), "件)\n", sep = "")

# LB(fcmmrd_d60・fcmmrd_d180・fcmmrd_d365)
check_lb_fcmmrd <- function(lb, lbspid, lbtestcd, suffix, fixed_value_checks_csv_path) {
  label <- str_c("LB(", lbspid, "/", lbtestcd, ")")
  tmp_lb <- lb %>% filter(LBSPID == lbspid & LBTESTCD == lbtestcd)
  target_lb_cols <- c("LBTEST", "LBCAT", "LBSPEC", "LBMETHOD", "LBORRESU")
  tmp_lb_renamed <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb_renamed, "LB", .x, fixed_value_checks_csv_path))

  tmp_lb %>% filter(LBORRES == "" & LBSTAT == "NOT DONE") %>% check_required_vars("LBREASND", domain_name = label)
  tmp_lb_reasnd_renamed <- tmp_lb %>% filter(LBORRES == "" & LBSTAT == "NOT DONE") %>% rename_with(~ str_c(.x, suffix), "LBREASND")
  run_value_equals_checks_from_csv(tmp_lb_reasnd_renamed, "LB", str_c("LBREASND", suffix), fixed_value_checks_csv_path)

  tmp_lb %>% filter(LBSTAT != "NOT DONE") %>% check_required_vars(c("LBORRES", "LBDTC"), domain_name = label)
  tmp_lb %>% filter(LBSTAT == "NOT DONE") %>% check_blank_vars(c("LBORRES", "LBDTC"), domain_name = label)
  tmp_lb %>% check_numeric_range("LBORRES", min_value = 0, max_value = 100, domain_name = label)

  cat(label, "チェック: OK(", nrow(tmp_lb), "件)\n", sep = "")
}

fcmmrd_testcd_test <- c(
  CD3 = "CD3", CD4 = "CD4", CD8 = "CD8",
  RTET = "Recent Thymic Emigrant T", TREG = "Regulatory T", NAIVET = "Naive T",
  BCELL = "BCELL", NKCELL = "NKCELL", NKTCELL = "NKTCELL"
)
fcmmrd_suffix <- 20
for (fcmmrd_spid in c("fcmmrd_d60", "fcmmrd_d180", "fcmmrd_d365")) {
  for (fcmmrd_testcd in names(fcmmrd_testcd_test)) {
    check_lb_fcmmrd(lb, fcmmrd_spid, fcmmrd_testcd, str_c("_", fcmmrd_suffix), fixed_value_checks_csv_path)
    fcmmrd_suffix <- fcmmrd_suffix + 1
  }
}



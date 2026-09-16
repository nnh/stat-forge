library(here)
rm(list = ls())
# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、test_config.R(共通)ではなくここで指定する。リポジトリ外の任意の場所でよい
fixed_value_checks_csv_path <- "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test3/fixed_value_checks_test3.csv"

source(here("test_config.R"))
# test_config.Rはjson_path(他テストとの切り替え用)も定義するが、このファイルは上で固定した
# json_pathを優先して使うため、test_config.R側の値で上書きしないよう再度設定し直す
json_path <- "/Users/mariko/Downloads/test20260826/fortest3_260826_1452.json"
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

# test3個別チェック

# AE
# sae_report(重篤な有害事象報告)シート。category="ae_report"のためAEドメインに直接マッピングされ、
# AESPIDは"sae_report"+USUBJID内連番(sae_report1, sae_report2, ...)になる。全14項目とも
# presence_conditionsが無く無条件必須。AESTDTCは診断日(MH registrationのMHSTDTC)以降、
# AEENDTCは同じ行のAESTDTC以降であることが期待される(date_ref_bounds。以前は生成ロジック側が
# AE自身の日付項目でdate_ref_boundsを一切考慮しておらず、AESTDTCが診断日より前になり得るバグが
# あったが、MH(registration)の先行生成+date_ref_bounds反映により修正済み)
tmp_ae_sae <- ae %>% filter(str_detect(AESPID, "^sae_report"))
c(
  "AETERM", "AETOXGR", "AESTDTC", "AESDTH", "AESLIFE", "AESHOSP", "AESDISAB", "AESCONG",
  "AESMIE", "AESER", "AEACN", "AEREL", "AEOUT", "AEENDTC"
) %>% check_required_vars(tmp_ae_sae, ., domain_name = "AE")
c("AEREL", "AEOUT", "AEACN", "AETOXGR", "AESDTH", "AESLIFE", "AESHOSP", "AESDISAB", "AESCONG", "AESMIE", "AESER") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ae_sae, "AE", .x, fixed_value_checks_csv_path))
tmp_ae_diag <- mh %>% filter(MHCAT == "PRIMARY DIAGNOSIS") %>% select(USUBJID, diag_dtc = MHSTDTC)
tmp_ae_sae %>% inner_join(tmp_ae_diag, by = "USUBJID") %>%
  check_date_after_var_before_today("AESTDTC", "diag_dtc", domain_name = "AE")
tmp_ae_sae %>% check_date_after_var_before_today("AEENDTC", "AESTDTC", domain_name = "AE")

# ae(有害事象報告、非重篤)シート。sae_reportと同じcategory="ae_report"のためAEドメインに直接
# マッピングされる。sae_reportと異なりAESDTH/AESLIFE/AESHOSP/AESDISAB/AESCONG/AESMIE(重篤性基準)の
# 項目が無く、AEACNは選択肢ではなく"DRUG WITHDRAWN"固定(is_invisible)、AEOUTもFATALを含まない4択
# (sae_reportはFATAL込み5択)。presence_conditionsは無く全項目無条件必須。AEACN/AEOUTを含む全ての
# 値域はsae_reportの値域(CSVのAE,AEACN/AE,AEOUT等、suffix無し)の部分集合のため、新しいsuffixは
# 追加せず既存のCSV行をそのまま使う。AESTDTCに診断日等への明示的なref()参照は無く(sae_reportと
# 異なる)、AEENDTCが同じ行のAESTDTC以降であることのみdate_ref_boundsで規定されている
tmp_ae_plain <- ae %>% filter(str_detect(AESPID, "^ae[0-9]"))
c("AETERM", "AETOXGR", "AESTDTC", "AESER", "AEACN", "AEREL", "AEOUT", "AEENDTC") %>%
  check_required_vars(tmp_ae_plain, ., domain_name = "AE")
c("AETOXGR", "AESER", "AEREL") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ae_plain, "AE", .x, fixed_value_checks_csv_path))
tmp_ae_plain %>% check_date_before_today("AESTDTC", domain_name = "AE")
tmp_ae_plain %>% check_date_after_var_before_today("AEENDTC", "AESTDTC", domain_name = "AE")
suffix <- "_1"
target_ae_cols <- c("AEACN", "AEOUT")
tmp_ae_plain <- tmp_ae_plain %>% rename_with(~ str_c(.x, suffix), all_of(target_ae_cols))
str_c(target_ae_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ae_plain, "AE", .x, fixed_value_checks_csv_path))

# CE
# relapse(再発報告)シートのCE(再発の種類・再発日)。presenceのゲート元であるfield141(再発の有無、
# Y/N)はCDISC変数にマッピングされていないフィールドのため、presence_conditionsには反映されず
# (validate_formula_if/presence_ifの"f141==1"部分は解決不能で無視される)、実データ上CETERM/CEDTCは
# 常に必須になっている(EDC仕様上の意図とは異なる可能性があるが、現状の生成ロジックに合わせて検証する)
tmp_ce <- ce %>% filter(CELNKGRP == "RELAPSE")
c("CETERM", "CEDTC") %>% check_required_vars(tmp_ce, ., domain_name = "CE")
suffix <- "_1"
target_ce_cols <- c("CETERM")
tmp_ce <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
str_c(target_ce_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ce, "CE", .x, fixed_value_checks_csv_path))
tmp_ce_diag <- mh %>% filter(MHCAT == "PRIMARY DIAGNOSIS") %>% select(USUBJID, diag_dtc = MHSTDTC)
tmp_ce %>% inner_join(tmp_ce_diag, by = "USUBJID") %>%
  check_date_after_var_before_today("CEDTC", "diag_dtc", domain_name = "CE")

# smreport(二次がん報告)シートのCE(二次がんの有無・診断日)。CEOCCURは"Y"/"N"/"NA"の3択で、
# "Y"のときのみCEDTCが必須(presence_conditions: CEDTC<-CEOCCUR=="Y")。CEDTCの下限は
# registrationのRFSTDTC(症例登録日。diag_dtc(初発診断日)ではない点がrelapseと異なる)
tmp_ce_sm <- ce %>% filter(CESPID == "smreport")
c("CETERM", "CEPRESP", "CEOCCUR") %>% check_required_vars(tmp_ce_sm, ., domain_name = "CE")
suffix <- "_2"
target_ce_sm_cols <- c("CETERM", "CEPRESP", "CEOCCUR")
tmp_ce_sm <- tmp_ce_sm %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_sm_cols))
str_c(target_ce_sm_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ce_sm, "CE", .x, fixed_value_checks_csv_path))
tmp_ce_sm %>% filter(CEOCCUR_2 == "Y") %>% check_required_vars("CEDTC", domain_name = "CE")
tmp_ce_sm %>% filter(CEOCCUR_2 != "Y") %>% check_blank_vars("CEDTC", domain_name = "CE")
tmp_ce_sm %>% filter(CEOCCUR_2 == "Y") %>% inner_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  check_date_after_var_before_today("CEDTC", "RFSTDTC", domain_name = "CE")

# CM
cm %>% filter(CMSPID != "sct1") %>% check_required_vars("CMOCCUR", domain_name = "CM")
cm %>% filter(CMSPID == "sct1") %>% check_blank_vars("CMOCCUR", domain_name = "CM")

tmp_cm <- cm %>% filter(CMSPID == "sct1")
c("CMTRT", "CMSTDTC") %>% check_required_vars(tmp_cm, ., domain_name = "CM")
sct1_cm_target_cols <- c("CMTRT", "CMCAT", "VISITNUM")
suffix <- "_4"
tmp_cm_2 <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(sct1_cm_target_cols))
str_c(sct1_cm_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm_2, "CM", .x, fixed_value_checks_csv_path))
tmp_sv_2 <- sv %>% filter(SVSPID == "blin2") %>% select(USUBJID, blin2_svstdtc=SVSTDTC)
tmp_cm_3 <- tmp_cm %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_cm_3 %>% check_date_after_var_before_today("CMSTDTC", "blin2_svstdtc", domain_name = "CM")

# induction(VISITNUM=200)とearlyintensifi(VISITNUM=300)は、CMTRT/CMCATの組み合わせが完全に同一
# (ANTIFUNGAL DRUG、薬剤コード6343444=ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)、
# 薬剤コード6342406=FRESH-FROZEN HUMAN PLASMA、および8種のHEPARIN/抗凝固薬ブロック)のため、
# VISITNUMをパラメータにしたtribble+pwalkでまとめて検証する
cm_named_target_cols <- c("CMCAT", "CMOCCUR", "CMPRESP")
cm_named_checks <- tribble(
  ~visitnum, ~cmtrt, ~suffix,
  200, "ANTIFUNGAL DRUG", "_1",
  300, "ANTIFUNGAL DRUG", "_1",
  900, "ANTIFUNGAL DRUG", "_1",
  1000, "ANTIFUNGAL DRUG", "_1",
  1100, "ANTIFUNGAL DRUG", "_1",
  1200, "ANTIFUNGAL DRUG", "_1",
  1400, "ANTIFUNGAL DRUG", "_1",
  1500, "ANTIFUNGAL DRUG", "_1",
  1700, "ANTIFUNGAL DRUG", "_1",
  1600, "ANTIFUNGAL DRUG", "_1",
  1900, "ANTIFUNGAL DRUG", "_1",
  2000, "ANTIFUNGAL DRUG", "_1",
  2100, "ANTIFUNGAL DRUG", "_1",
  200, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  300, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  400, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  1100, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  1200, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  1400, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  1500, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  1600, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  1900, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  2000, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  2100, "ANTITHROMBIN GAMMA(GENETICAL RECOMBINATION)", "_2",
  200, "FRESH-FROZEN HUMAN PLASMA", "_3",
  300, "FRESH-FROZEN HUMAN PLASMA", "_3",
  400, "FRESH-FROZEN HUMAN PLASMA", "_3",
  1100, "FRESH-FROZEN HUMAN PLASMA", "_3",
  1200, "FRESH-FROZEN HUMAN PLASMA", "_3",
  1400, "FRESH-FROZEN HUMAN PLASMA", "_3",
  1500, "FRESH-FROZEN HUMAN PLASMA", "_3",
  1600, "FRESH-FROZEN HUMAN PLASMA", "_3",
  1900, "FRESH-FROZEN HUMAN PLASMA", "_3",
  2000, "FRESH-FROZEN HUMAN PLASMA", "_3",
  2100, "FRESH-FROZEN HUMAN PLASMA", "_3"
)
pwalk(cm_named_checks, function(visitnum, cmtrt, suffix) {
  tmp_cm <- cm %>% filter(CMTRT == cmtrt & VISITNUM == visitnum)
  tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(cm_named_target_cols))
  str_c(cm_named_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))
})

# hdmのANTIFUNGAL DRUG(VISITNUM=400)。CMCAT/CMOCCURの固定値はVISITNUM 200/300と同一のためsuffix "_1"を
# 再利用するが、この行にはCMPRESPフィールドが存在しないため、cm_named_checksには含めずCMCAT/CMOCCURのみ
# 個別にチェックする
tmp_cm <- cm %>% filter(CMTRT == "ANTIFUNGAL DRUG" & VISITNUM == 400)
suffix <- "_1"
cm_hdm_target_cols <- c("CMCAT", "CMOCCUR")
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(cm_hdm_target_cols))
str_c(cm_hdm_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))

# 同じ組み合わせ(ANTIFUNGAL DRUG, VISITNUM=400)にはhdm以外にhdm2/hdm5/hr3fisrtも該当し、
# こちらはCMPRESPフィールドを持つ(値は"Y")ため、hdmを除外した上でCMPRESP_1を別途チェックする
tmp_cm <- cm %>% filter(CMTRT == "ANTIFUNGAL DRUG" & VISITNUM == 400 & CMSPID != "hdm")
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), "CMPRESP")
str_c("CMPRESP", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))

cm_anticoag_target_cols <- c("CMOCCUR", "CMPRESP")
cm_anticoag_checks <- tribble(
  ~visitnum, ~cmtrt, ~cmcat,
  200, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  200, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  200, "HEPARINOID", "FIRST PREVENTION",
  200, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  200, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  200, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  200, "HEPARINOID", "SECOND PREVENTION",
  200, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  300, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  300, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  300, "HEPARINOID", "FIRST PREVENTION",
  300, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  300, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  300, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  300, "HEPARINOID", "SECOND PREVENTION",
  300, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  400, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  400, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  400, "HEPARINOID", "FIRST PREVENTION",
  400, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  400, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  400, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  400, "HEPARINOID", "SECOND PREVENTION",
  400, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  1100, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  1100, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  1100, "HEPARINOID", "FIRST PREVENTION",
  1100, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  1100, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  1100, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  1100, "HEPARINOID", "SECOND PREVENTION",
  1100, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  1200, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  1200, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  1200, "HEPARINOID", "FIRST PREVENTION",
  1200, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  1200, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  1200, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  1200, "HEPARINOID", "SECOND PREVENTION",
  1200, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  1400, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  1400, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  1400, "HEPARINOID", "FIRST PREVENTION",
  1400, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  1400, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  1400, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  1400, "HEPARINOID", "SECOND PREVENTION",
  1400, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  1500, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  1500, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  1500, "HEPARINOID", "FIRST PREVENTION",
  1500, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  1500, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  1500, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  1500, "HEPARINOID", "SECOND PREVENTION",
  1500, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  1600, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  1600, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  1600, "HEPARINOID", "FIRST PREVENTION",
  1600, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  1600, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  1600, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  1600, "HEPARINOID", "SECOND PREVENTION",
  1600, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  1900, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  1900, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  1900, "HEPARINOID", "FIRST PREVENTION",
  1900, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  1900, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  1900, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  1900, "HEPARINOID", "SECOND PREVENTION",
  1900, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  2000, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  2000, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  2000, "HEPARINOID", "FIRST PREVENTION",
  2000, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  2000, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  2000, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  2000, "HEPARINOID", "SECOND PREVENTION",
  2000, "OTHER ANTICOAGULANT", "SECOND PREVENTION",
  2100, "UNFRACTIONATED HEPARIN", "FIRST PREVENTION",
  2100, "LOW MOLECULAR HEPARIN", "FIRST PREVENTION",
  2100, "HEPARINOID", "FIRST PREVENTION",
  2100, "OTHER ANTICOAGULANT", "FIRST PREVENTION",
  2100, "UNFRACTIONATED HEPARIN", "SECOND PREVENTION",
  2100, "LOW MOLECULAR HEPARIN", "SECOND PREVENTION",
  2100, "HEPARINOID", "SECOND PREVENTION",
  2100, "OTHER ANTICOAGULANT", "SECOND PREVENTION"
)
pwalk(cm_anticoag_checks, function(visitnum, cmtrt, cmcat) {
  tmp_cm <- cm %>% filter(CMTRT == cmtrt & VISITNUM == visitnum & CMCAT == cmcat)
  suffix <- "_1"
  tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(cm_anticoag_target_cols))
  str_c(cm_anticoag_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))
})

# DM
c("RFICDTC", "BRTHDTC", "SEX", "RACE", "RFSTDTC") %>% check_required_vars(dm, ., domain_name = "DM")
dm %>% check_date_before_today(c("BRTHDTC"), domain_name = "DM")
dm %>% check_date_before_today(c("RFSTDTC"), domain_name = "DM")
dm %>% check_date_after_var_before_today("RFICDTC", "BRTHDTC", domain_name = "DM")
c("SEX", "RACE", "ETHNIC", "COUNTRY") %>% walk(~ run_value_equals_checks_from_csv(dm, "DM", .x, fixed_value_checks_csv_path))

# DD
c("DDTESTCD", "DDTEST", "DDORRES") %>% walk(~ run_value_equals_checks_from_csv(dd, "DD", .x, fixed_value_checks_csv_path))
# DDのUSUBJIDとDSの死亡(DSTERM=="Death")のUSUBJIDが一致することを確認する(どちらか一方にしか
# いない場合はNG)
dd_usubjid <- dd %>% pull(USUBJID) %>% unique()
ds_death_usubjid <- ds %>% filter(DSTERM == "DEATH") %>% pull(USUBJID) %>% unique()
if (!setequal(dd_usubjid, ds_death_usubjid)) {
  stop(str_c(
    "DD/DS(Death)対象USUBJID一致チェック: NG(DDのみ: ", paste(setdiff(dd_usubjid, ds_death_usubjid), collapse = ", "),
    " / DS(Death)のみ: ", paste(setdiff(ds_death_usubjid, dd_usubjid), collapse = ", "), ")"
  ))
}
cat("DD/DS(Death)対象USUBJID一致チェック: OK(", length(dd_usubjid), "件)\n", sep = "")

# DS
tmp_ds <- ds %>% filter(DSCAT == "DISPOSITION EVENT")
tmp_dm <- dm %>% select(USUBJID, RFSTDTC)
tmp_ds_2 <- tmp_ds %>% inner_join(tmp_dm, by="USUBJID")
tmp_ds_2 %>% check_date_after_var_before_today("DSSTDTC", "RFSTDTC", domain_name = "DS")
c("DSTERM", "DSSTDTC", "DSDTC") %>% check_required_vars(tmp_ds, ., domain_name = "DS")
suffix <- "_1"
ds_target_cols <- c("DSTERM", "EPOCH")
tmp_ds <- tmp_ds %>% rename_with(~ str_c(.x, suffix), all_of(ds_target_cols))
str_c(ds_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ds, "DS", .x, fixed_value_checks_csv_path))
tmp_ds %>% check_date_after_var_before_today("DSDTC", "DSSTDTC", domain_name = "DS")

# EC
tmp_ec <- ec %>% filter(ECTRT == "PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 150)
c("ECDOSE") %>% check_required_vars(tmp_ec, ., domain_name="EC")
suffix <- "_1"
ec_target_cols <- c("ECMOOD", "ECCAT", "ECDOSU")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE" & VISITNUM == 150)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "PEGASPARGASE" & VISITNUM == 200)
suffix <- "_1"
ec_target_cols <- c("ECMOOD")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
str_c(ec_target_cols, suffix) %>% check_required_vars(tmp_ec, ., domain_name = "EC")
"ECSTDTC" %>% check_date_before_today(tmp_ec, ., domain_name ="EC")

tmp_ec <- ec %>% filter(ECTRT == "PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 200)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_3"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "VINCRISTINE SULFATE" & VISITNUM == 200)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_3"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "DAUNORUBICIN HYDROCHLORIDE" & VISITNUM == 200)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_3"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "L-ASPARAGINASE" & VISITNUM == 200)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_4"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 200)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "CYCLOPHOSPHAMIDE HYDRATE" & VISITNUM == 300)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "CYTARABINE" & VISITNUM == 300)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "MERCAPTOPURINE HYDRATE" & VISITNUM == 300)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "L-ASPARAGINASE" & VISITNUM == 300)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_6"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 300)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hdmのMETHOTREXATE(VISITNUM=400)。VISITNUM=150のMETHOTREXATEと固定値は同一だが、ECROUTEフィールドが
# 無いため対象外とする(ECTRT/VISITNUMだけで絞り込んでいるため、hdm2/hdm5の同名ブロックもまとめて検証される)
tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE" & VISITNUM == 400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hdmのMETHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE(VISITNUM=400)。VISITNUM=200と
# 固定値(ECROUTE="INTRATHECAL"含む)が同一のためsuffix "_5"を再利用する
# (ECTRT/VISITNUMだけで絞り込んでいるため、hdm2/hdm5の同名ブロックもまとめて検証される)
tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hdm2のMERCAPTOPURINE HYDRATE(VISITNUM=400)。VISITNUM=300のMERCAPTOPURINE HYDRATEと固定値が
# 同一のためsuffix "_2"を再利用する
tmp_ec <- ec %>% filter(ECTRT == "MERCAPTOPURINE HYDRATE" & VISITNUM == 400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr3fisrtのCYTARABINE(VISITNUM=400)。VISITNUM=300のCYTARABINEと固定値が同一のためsuffix "_2"を再利用する
tmp_ec <- ec %>% filter(ECTRT == "CYTARABINE" & VISITNUM == 400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr3fisrtのL-ASPARAGINASE(VISITNUM=400)。VISITNUM=200のL-ASPARAGINASEと固定値(ECADJの11コード体系)が
# 同一のためsuffix "_4"を再利用する
tmp_ec <- ec %>% filter(ECTRT == "L-ASPARAGINASE" & VISITNUM == 400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_4"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr3fisrtのDEXAMETHASONE CIPECILATE(VISITNUM=400、新規薬剤)。固定値(ECADJの6コード体系)は
# suffix "_2"と同一のため再利用する
tmp_ec <- ec %>% filter(ECTRT == "DEXAMETHASONE CIPECILATE" & VISITNUM == 400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr3fisrtのETOPOSIDE(VISITNUM=400、新規薬剤)。固定値(ECADJの6コード体系)はsuffix "_2"と同一のため再利用する
tmp_ec <- ec %>% filter(ECTRT == "ETOPOSIDE" & VISITNUM == 400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# blin1のBLINATUMOMAB(VISITNUM=900、新規薬剤)。固定値(ECADJの6コード体系)はsuffix "_2"と同一のため再利用する
tmp_ec <- ec %>% filter(ECTRT == "BLINATUMOMAB" & VISITNUM == 900)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# blin1のMETHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE(VISITNUM=900)。VISITNUM=200/400と
# 固定値(ECROUTE="INTRATHECAL"含む)が同一のためsuffix "_5"を再利用する
tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 900)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# blin2のBLINATUMOMAB(VISITNUM=1000)。blin1と固定値が同一のためsuffix "_2"を再利用する
tmp_ec <- ec %>% filter(ECTRT == "BLINATUMOMAB" & VISITNUM == 1000)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# blin2のMETHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE(VISITNUM=1000)。blin1と固定値
# (ECROUTE="INTRATHECAL"含む)が同一のためsuffix "_5"を再利用する
tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 1000)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# blin3のBLINATUMOMAB(VISITNUM=1700)。blin1/blin2と固定値が同一のためsuffix "_2"を再利用する
tmp_ec <- ec %>% filter(ECTRT == "BLINATUMOMAB" & VISITNUM == 1700)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_2"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# blin3のMETHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE(VISITNUM=1700)。blin1/blin2と
# 固定値(ECROUTE="INTRATHECAL"含む)が同一のためsuffix "_5"を再利用する
tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 1700)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr2fisrt(VISITNUM=1100)の6薬剤。ECADJの6コード体系はsuffix "_2"と同一のため再利用する
# (VINDESINE SULFATE/IFOSFAMIDEは新規薬剤名だが、固定値の構成自体は既存と同じ)
hr2fisrt_ec_drugs <- c("VINDESINE SULFATE", "DEXAMETHASONE CIPECILATE", "DAUNORUBICIN HYDROCHLORIDE", "METHOTREXATE", "IFOSFAMIDE")
walk(hr2fisrt_ec_drugs, function(ectrt) {
  tmp_ec <- ec %>% filter(ECTRT == ectrt & VISITNUM == 1100)
  c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  suffix <- "_2"
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
})

# hr2fisrtのL-ASPARAGINASE(VISITNUM=1100)。ECADJの11コード体系はsuffix "_4"と同一のため再利用する
tmp_ec <- ec %>% filter(ECTRT == "L-ASPARAGINASE" & VISITNUM == 1100)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_4"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr2fisrtのMETHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE(VISITNUM=1100)。
# 固定値(ECROUTE="INTRATHECAL"含む)はsuffix "_5"と同一のため再利用する
tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 1100)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr1fisrt(VISITNUM=1200)の6薬剤。ECADJの6コード体系はsuffix "_2"と同一のため再利用する
hr1fisrt_ec_drugs <- c("VINCRISTINE SULFATE", "DEXAMETHASONE CIPECILATE", "CYTARABINE", "METHOTREXATE", "CYCLOPHOSPHAMIDE HYDRATE")
walk(hr1fisrt_ec_drugs, function(ectrt) {
  tmp_ec <- ec %>% filter(ECTRT == ectrt & VISITNUM == 1200)
  c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  suffix <- "_2"
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
})

# hr1fisrtのL-ASPARAGINASE(VISITNUM=1200)。ECADJの11コード体系はsuffix "_4"と同一のため再利用する
tmp_ec <- ec %>% filter(ECTRT == "L-ASPARAGINASE" & VISITNUM == 1200)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_4"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr1fisrtのMETHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE(VISITNUM=1200)。
# 固定値(ECROUTE="INTRATHECAL"含む)はsuffix "_5"と同一のため再利用する
tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 1200)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr3second(VISITNUM=1400)はhr3fisrt(400)と全く同じ5薬剤構成
hr3second_ec_drugs_2 <- c("CYTARABINE", "DEXAMETHASONE CIPECILATE", "ETOPOSIDE")
walk(hr3second_ec_drugs_2, function(ectrt) {
  tmp_ec <- ec %>% filter(ECTRT == ectrt & VISITNUM == 1400)
  c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  suffix <- "_2"
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
})

tmp_ec <- ec %>% filter(ECTRT == "L-ASPARAGINASE" & VISITNUM == 1400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_4"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 1400)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr2second(VISITNUM=1500)はhr2fisrt(1100)と全く同じ7薬剤構成
hr2second_ec_drugs <- c("VINDESINE SULFATE", "DEXAMETHASONE CIPECILATE", "DAUNORUBICIN HYDROCHLORIDE", "METHOTREXATE", "IFOSFAMIDE")
walk(hr2second_ec_drugs, function(ectrt) {
  tmp_ec <- ec %>% filter(ECTRT == ectrt & VISITNUM == 1500)
  c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  suffix <- "_2"
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
})

tmp_ec <- ec %>% filter(ECTRT == "L-ASPARAGINASE" & VISITNUM == 1500)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_4"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 1500)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# hr1second(VISITNUM=1600)はhr1fisrt(1200)と全く同じ7薬剤構成
hr1second_ec_drugs <- c("VINCRISTINE SULFATE", "DEXAMETHASONE CIPECILATE", "CYTARABINE", "METHOTREXATE", "CYCLOPHOSPHAMIDE HYDRATE")
walk(hr1second_ec_drugs, function(ectrt) {
  tmp_ec <- ec %>% filter(ECTRT == ectrt & VISITNUM == 1600)
  c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  suffix <- "_2"
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
})

tmp_ec <- ec %>% filter(ECTRT == "L-ASPARAGINASE" & VISITNUM == 1600)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_4"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 1600)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_5"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# erwaspシートはCRISANTASPASEの投与を2回分(寛解導入療法IA2,IA4=VISITNUM200、早期強化療法IB+L=VISITNUM300)
# 記録するが、選択肢構成・固定値は2回分で共通のため、参照日付(ref_data、2列目がref変数)だけを
# 呼び出し側で変えて共通化する
check_erwasp_crisantaspase <- function(visitnum, suffix, ref_data = NULL) {
  tmp_ec <- ec %>% filter(ECTRT == "CRISANTASPASE" & VISITNUM == visitnum)
  c("ECOCCUR") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  occur_col <- str_c("ECOCCUR", suffix)
  adj_col <- str_c("ECADJ", suffix)
  str_c(c("ECMOOD", "ECPRESP", "ECOCCUR"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
  tmp_ec_2 <- tmp_ec %>% filter(.data[[occur_col]] == "Y")
  str_c(c("ECADJ"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec_2, "EC", .x, fixed_value_checks_csv_path))
  c(adj_col, "ECSTDTC") %>% check_required_vars(tmp_ec_2, ., domain_name ="EC")

  # ref_dataが無い場合(そのVISITNUMに明示的なref()参照が無いチェーンの先頭)は日付チェックをスキップする
  if (!is.null(ref_data)) {
    ref_var <- names(ref_data)[2]
    tmp_ec_3 <- tmp_ec_2 %>% inner_join(ref_data, by="USUBJID")
    tmp_ec_3 %>% check_date_after_var_before_today("ECSTDTC", ref_var, domain_name = "EC")
  }
  tmp_ec_2
}

# VISITNUM==200(field8/ECSTDTC)はinductionのSVSTDTC以降であることが期待される(ref('induction', 820))
tmp_sv_2 <- sv %>% filter(SVSPID == "induction") %>% select(USUBJID, induction820=SVSTDTC)
tmp_ec_erwasp_200 <- check_erwasp_crisantaspase(200, "_7", tmp_sv_2)

# VISITNUM==300(field17/ECSTDTC)は同じerwaspシートのVISITNUM==200のECSTDTC以降であることが
# 期待される(ref('erwasp', ...)、同一alias内でlabelを跨いだ日付連鎖)
tmp_ec_erwasp_200_ref <- tmp_ec_erwasp_200 %>% select(USUBJID, erwasp000=ECSTDTC)
invisible(check_erwasp_crisantaspase(300, "_8", tmp_ec_erwasp_200_ref))

# erwasp_sct/erwasp_armblock/erwasp_hrは、VISITNUM=400→1100→1200と続く3段階の同一alias内チェーンを
# 共通して持つ(ECTRT/VISITNUMだけで絞り込んでいるため、3シート分がまとめて検証される)。固定値は
# suffix "_8"(VISITNUM=300)と同一のため再利用する。VISITNUM=400(先頭)には明示的なref()参照が
# 無いため、ref_dataを渡さず日付チェックを省略する
tmp_ec_erwasp_400 <- check_erwasp_crisantaspase(400, "_8")
ec %>% filter(VISITNUM == 400) %>% check_date_before_today("ECSTDTC", domain_name = "EC")
tmp_ec_erwasp_400_ref <- tmp_ec_erwasp_400 %>% select(USUBJID, erwasp_400=ECSTDTC)
tmp_ec_erwasp_1100 <- check_erwasp_crisantaspase(1100, "_8", tmp_ec_erwasp_400_ref)
tmp_ec_erwasp_1100_ref <- tmp_ec_erwasp_1100 %>% select(USUBJID, erwasp_1100=ECSTDTC)
tmp_ec_erwasp_1200 <- check_erwasp_crisantaspase(1200, "_8", tmp_ec_erwasp_1100_ref)
tmp_ec_erwasp_1200_ref <- tmp_ec_erwasp_1200 %>% select(USUBJID, erwasp_1200=ECSTDTC)

# erwasp_jacls02srのCRISANTASPASE(VISITNUM=1900)。ECADJの11コード体系・固定値はsuffix "_8"と
# 同一のため再利用する。他のerwasp系シートのようなチェーンの続きではなく単独のlabelのため、
# ref_dataを渡さず日付チェックを省略する
invisible(check_erwasp_crisantaspase(1900, "_8"))

# erwasp_armblockはVISITNUM=1200からさらに1400→1500→1600と続く(erwasp_sct/erwasp_hrにはこの
# 中間段階が無いため、CRISANTASPASE&VISITNUM=1400/1500/1600はerwasp_armblockの行のみが該当する)
tmp_ec_erwasp_1400 <- check_erwasp_crisantaspase(1400, "_8", tmp_ec_erwasp_1200_ref)
tmp_ec_erwasp_1400_ref <- tmp_ec_erwasp_1400 %>% select(USUBJID, erwasp_1400=ECSTDTC)
tmp_ec_erwasp_1500 <- check_erwasp_crisantaspase(1500, "_8", tmp_ec_erwasp_1400_ref)
tmp_ec_erwasp_1500_ref <- tmp_ec_erwasp_1500 %>% select(USUBJID, erwasp_1500=ECSTDTC)
tmp_ec_erwasp_1600 <- check_erwasp_crisantaspase(1600, "_8", tmp_ec_erwasp_1500_ref)
tmp_ec_erwasp_1600_ref <- tmp_ec_erwasp_1600 %>% select(USUBJID, erwasp_1600=ECSTDTC)

# erwasp_lrsrir・erwasp_hr・erwasp_armblockのCRISANTASPASE(VISITNUM=2000、ECADJの11コード体系・
# 固定値はsuffix "_8"と同一)。参照先は3シートで異なる: erwasp_armblockは自身のVISITNUM=1600
# (ref_labelチェーンの続き)、erwasp_hrは1400/1500/1600を経由せず自身のVISITNUM=1200を直接参照
# (date_ref_boundsで確認済み)、erwasp_lrsrirはチェーンの先頭で明示的なref()参照が無い単独のlabelの
# ため対象外。USUBJIDだけでref_dataを結合すると、同じ被験者がerwasp_sct(1200まで)とerwasp_lrsrir
# (2000)を両方持つ場合にerwasp_sctの1200をerwasp_lrsrirの参照として誤って突き合わせてしまう
# (実際に発生したバグ)ため、ECSPIDで各シートに絞り込んでから個別にinner_joinする
tmp_ec_erwasp_2000 <- check_erwasp_crisantaspase(2000, "_8")
ec %>% filter(ECSPID == "erwasp_lrsrir") %>% check_date_before_today("ECSTDTC", domain_name = "EC")

tmp_ec_erwasp_2000 %>%
  filter(ECSPID == "erwasp_armblock") %>%
  inner_join(tmp_ec_erwasp_1600_ref, by = "USUBJID") %>%
  check_date_after_var_before_today("ECSTDTC", "erwasp_1600", domain_name = "EC")
tmp_ec_erwasp_2000 %>%
  filter(ECSPID == "erwasp_hr") %>%
  inner_join(tmp_ec_erwasp_1200_ref, by = "USUBJID") %>%
  check_date_after_var_before_today("ECSTDTC", "erwasp_1200", domain_name = "EC")

# reinduction1(VISITNUM=1900)のEC。5薬剤ともECADJにpresence_conditionsが無く常に値を持つため、
# 従来通りECOCCURでの絞り込み無しでECADJの必須・固定値チェックを行う
check_reinduction1_ec_drug <- function(drug, suffix, has_route = FALSE) {
  tmp_ec <- ec %>% filter(ECTRT == drug & VISITNUM == 1900)
  c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  if (has_route) ec_target_cols <- c(ec_target_cols, "ECROUTE")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
}
check_reinduction1_ec_drug("VINCRISTINE SULFATE", "_2")
check_reinduction1_ec_drug("PREDNISOLONE SODIUM SUCCINATE", "_2")
check_reinduction1_ec_drug("PIRARUBICIN", "_2")
check_reinduction1_ec_drug("L-ASPARAGINASE", "_4")
check_reinduction1_ec_drug("METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE", "_5", has_route = TRUE)

# reinduction2(VISITNUM=2000)のEC。VINCRISTINE SULFATE/DEXAMETHASONE CIPECILATE/DOXORUBICIN
# HYDROCHLORIDE/L-ASPARAGINASEはreinduction1と同様、ECADJにpresence_conditionsが無く常に値を持つ
check_reinduction2_ec_drug <- function(drug, suffix) {
  tmp_ec <- ec %>% filter(ECTRT == drug & VISITNUM == 2000)
  c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
}
check_reinduction2_ec_drug("VINCRISTINE SULFATE", "_2")
check_reinduction2_ec_drug("DEXAMETHASONE CIPECILATE", "_2")
check_reinduction2_ec_drug("DOXORUBICIN HYDROCHLORIDE", "_2")
check_reinduction2_ec_drug("L-ASPARAGINASE", "_4")

# reinduction2のMETHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE(髄注、label=089)。ECADJは
# 他の薬剤と同様ゲーティング無しで常に値を持つが(validate_formula_ifはゲーティング条件として使わない
# よう修正済み。詳細はbuild_generation_constraints.Rの変更履歴を参照)、コード体系が他のECADJ_5と異なり
# 通常の6コードに加えて"Not Administration with CNS-1"を含む7コード体系のため、新しいsuffix "_9"を使う
tmp_ec <- ec %>% filter(ECTRT == "METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE" & VISITNUM == 2000)
c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
suffix <- "_9"
ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ", "ECROUTE")
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# reinduction3(VISITNUM=2100)のEC。4薬剤ともECADJにpresence_conditionsが無く常に値を持ち、
# いずれも6コード体系(CYTARABINE/CYCLOPHOSPHAMIDE HYDRATE/MERCAPTOPURINE HYDRATEはsuffix "_2"、
# METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE(髄注)はECROUTE="INTRATHECAL"付きの
# suffix "_5"と同一)のため、いずれも既存suffixを再利用する
check_reinduction3_ec_drug <- function(drug, suffix, has_route = FALSE) {
  tmp_ec <- ec %>% filter(ECTRT == drug & VISITNUM == 2100)
  c("ECOCCUR", "ECADJ") %>% check_required_vars(tmp_ec, ., domain_name ="EC")
  ec_target_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECADJ")
  if (has_route) ec_target_cols <- c(ec_target_cols, "ECROUTE")
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(ec_target_cols))
  str_c(ec_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
}
check_reinduction3_ec_drug("CYTARABINE", "_2")
check_reinduction3_ec_drug("CYCLOPHOSPHAMIDE HYDRATE", "_2")
check_reinduction3_ec_drug("MERCAPTOPURINE HYDRATE", "_2")
check_reinduction3_ec_drug("METHOTREXATE/CYTARABINE/PREDNISOLONE SODIUM SUCCINATE", "_5", has_route = TRUE)

# maintenance6mp(維持療法における6-MP経口投与)。category="multiple"のため被験者ごとに0件以上の
# レコードを持ちうる(ECSPIDは"maintenance6mp"+USUBJID内連番)。ECOCCUR(投与有無、Y/N)が"Y"の
# ときのみECDOSE(用量、固定選択肢ではなく汎用のダミー数値)が必須(presence_conditions:
# ECDOSE<-ECOCCUR=="Y")。ECSTDTCに明示的なref()参照は無いため、登録日(RFSTDTC)以降・今日以前で
# あることのみ確認する
tmp_ec_maint <- ec %>% filter(str_detect(ECSPID, "^maintenance6mp"))
c("ECOCCUR", "ECSTDTC") %>% check_required_vars(tmp_ec_maint, ., domain_name = "EC")
suffix <- "_10"
ec_maint_target_cols <- c("ECOCCUR", "ECDOSU", "ECMOOD", "ECPRESP", "ECTRT", "VISITNUM")
tmp_ec_maint <- tmp_ec_maint %>% rename_with(~ str_c(.x, suffix), all_of(ec_maint_target_cols))
str_c(ec_maint_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec_maint, "EC", .x, fixed_value_checks_csv_path))
tmp_ec_maint %>% filter(.data[[str_c("ECOCCUR", suffix)]] == "Y") %>% check_required_vars("ECDOSE", domain_name = "EC")
tmp_ec_maint %>% filter(.data[[str_c("ECOCCUR", suffix)]] != "Y") %>% check_blank_vars("ECDOSE", domain_name = "EC")
tmp_ec_maint %>% inner_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  check_date_before_today("ECSTDTC", domain_name = "EC")

# FA(Findings About)関連チェックはtools/validate_datasets_test3_fa.Rに切り出してある
source(here("tools/validate_datasets_test3_fa.R"))

# GRADE(重症度)評価パネル(79項目)は、prephase以降の治療フェーズ系18シートで共通して使われている
# (各シートのVISITNUMは互いに異なる)。シートごとにVISITNUMを指定し、check_fa_grade_panel()で
# 1シートずつ実行する(どのシートでワーニングが出ているか特定しやすいよう、pwalkでまとめず個別に呼ぶ)
check_fa_grade_panel("prephase", "150")
check_fa_grade_panel("induction", "200")
check_fa_grade_panel("earlyintensifi", "300")
check_fa_grade_panel("hdm", "400")
check_fa_grade_panel("hdm2", "400")
check_fa_grade_panel("hdm5", "400")
check_fa_grade_panel("hr1fisrt", "1200")
check_fa_grade_panel("hr2fisrt", "1100")
check_fa_grade_panel("hr3fisrt", "400")
check_fa_grade_panel("hr1second", "1600")
check_fa_grade_panel("hr2second", "1500")
check_fa_grade_panel("hr3second", "1400")
check_fa_grade_panel("blin1", "900")
check_fa_grade_panel("blin2", "1000")
check_fa_grade_panel("blin3", "1700")
check_fa_grade_panel("reinduction1", "1900")
check_fa_grade_panel("reinduction2", "2000")
check_fa_grade_panel("reinduction3", "2100")
# maitenanceのGRADEパネル(FATESTCD=="GRADE"の約79項目)は、EDC仕様上VISITNUMの既定値が"150"
# (SVのVISITNUM"2300"とは別。おそらくprephaseシートのGRADEパネルからのコピー元由来)。
# "2300"を指定すると常に0件ヒットし、シード・nを変えても解消しない(CSVのexpected_valueが
# 一度も出現しないという警告が多発する)ため、実際のVISITNUMである"150"を指定する
check_fa_grade_panel("maitenance", "150")

# maitenanceのFA(維持療法期間の入院回数)。FATESTCD="NUMEPISD"の2項目(感染症による入院/非感染症に
# よる入院)は、GRADEパネル(79項目)とは別にFAORRESが必須の数値(0以上、上限無し)であることを
# 確認する。FATEST/FATESTCD/FAORRESU/FAEPOCH/VISITNUMは2項目共通のためまとめてチェックし、
# FAOBJだけがlabel(099/100)ごとに異なるため個別にチェックする
tmp_fa <- fa %>% filter(FASPID == "maitenance" & FATESTCD == "NUMEPISD")
c("FAORRES") %>% check_required_vars(tmp_fa, ., domain_name = "FA")
tmp_fa %>% check_numeric_range("FAORRES", 0, domain_name = "FA")
fa_numepisd_target_cols <- c("FATEST", "FATESTCD", "FAORRESU", "FAEPOCH", "VISITNUM")
suffix <- "_17"
tmp_fa_2 <- tmp_fa %>% rename_with(~ str_c(.x, suffix), all_of(fa_numepisd_target_cols))
str_c(fa_numepisd_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_2, "FA", .x, fixed_value_checks_csv_path))
tmp_fa_infection <- tmp_fa %>% filter(FAOBJ == "Hospitalization for Infection") %>% rename_with(~ str_c(.x, suffix), "FAOBJ")
str_c("FAOBJ", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_infection, "FA", .x, fixed_value_checks_csv_path))
suffix <- "_18"
tmp_fa_noninfection <- tmp_fa %>% filter(FAOBJ == "Hospitalization for Non-Infection") %>% rename_with(~ str_c(.x, suffix), "FAOBJ")
str_c("FAOBJ", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_noninfection, "FA", .x, fixed_value_checks_csv_path))

# BLIN群イムノモニタリング各シート(immunomonitoring1/2/3)のASTCTGR(ASTCT Consensus Grading)。
# GRADEパネル(FATESTCD=="GRADE")とは別のテストコードで、FAOBJがCytokine release syndrome/
# Immune effector cell-associated neurotoxicity syndromeの2種類のみ、値域も0〜4(GRADEは0〜5)のため、
# check_fa_grade_panel()を再利用せずcheck_fa_testcd_no_loc()を個別に呼ぶ。固定値はシート間で共通のため
# 関数化してsuffixを使い回す
check_immuno_astctgr <- function(faspid) {
  tmp_fa <- fa %>% filter(FASPID == faspid & VISITNUM == "900")
  check_fa_testcd_no_loc(tmp_fa, "ASTCTGR", "Cytokine release syndrome", "_12", fixed_value_checks_csv_path, has_blfl = FALSE, has_orres_in_target = TRUE)
  check_fa_testcd_no_loc(tmp_fa, "ASTCTGR", "Immune effector cell-associated neurotoxicity syndrome", "_13", fixed_value_checks_csv_path, has_blfl = FALSE, has_orres_in_target = TRUE)
  # check_fa_testcd_no_loc()はFAOBJでの絞り込みを内部で行うだけでFAOBJ自体の値は確認しないため、
  # 2種類のFAOBJがそれぞれ期待通りの文字列であることを別途確認する
  tmp_fa_astctgr <- tmp_fa %>% filter(FATESTCD == "ASTCTGR")
  suffix <- "_14"
  tmp_fa_astctgr <- tmp_fa_astctgr %>% rename_with(~ str_c(.x, suffix), "FAOBJ")
  str_c("FAOBJ", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_astctgr, "FA", .x, fixed_value_checks_csv_path))
}
check_immuno_astctgr("immunomonitoring1")
check_immuno_astctgr("immunomonitoring2")
check_immuno_astctgr("immunomonitoring3")

# osteonecrosis1/2/3共通。OCCUR(骨壊死の有無、label000)とGRADE(重症度、label001。OCCUR=="Y"のときのみ)の
# 2段階構成で、GRADEの実施可否がFASTATではなくOCCUR自身のFAORRESに連動するため、
# check_fa_grade_panel()(FATESTCD=="GRADE"のみの79項目パネル用)にもcheck_fa_testcd_no_loc()の
# NOT DONE型(FASTATで実施可否が決まる前提)にも当てはまらず、個別に書く
check_osteonecrosis_fa <- function(spid, visitnum) {
  tmp_fa <- fa %>% filter(FASPID == spid & VISITNUM == visitnum)

  # FAOBJ=="Osteonecrosis"で絞り込む(latecomplicationのように他のFATESTCD=="OCCUR"項目が
  # 同じVISITNUMに混在する可能性があるため、念のためGRADE側と同様に明示的に絞り込む)
  tmp_fa_occur <- tmp_fa %>% filter(FATESTCD == "OCCUR" & FAOBJ == "Osteonecrosis")
  tmp_fa_occur %>% filter(FASTAT != "NOT DONE") %>% check_required_vars(c("FAORRES", "FADTC"), domain_name = "FA")
  tmp_fa_occur %>% filter(FASTAT == "NOT DONE") %>% check_blank_vars(c("FAORRES", "FADTC"), domain_name = "FA")

  # FASTAT(field10)自体の必須条件: validate_presence_ifがage(初発診断日(MH MHCAT=="PRIMARY DIAGNOSIS"の
  # MHSTDTC), 生年月日(DM BRTHDTC))>39。すなわち初発診断日時点の年齢が39歳を超える被験者だけFASTATに
  # 値が入りうる(コード定義は"NOT DONE"の1択のみなので、該当すれば必ず"NOT DONE"、非該当なら必ず空欄になる)。
  # 実データの年齢を計算し、この条件が正しく反映されているか確認する
  diagnosis_age <- mh %>%
    filter(MHCAT == "PRIMARY DIAGNOSIS") %>%
    select(USUBJID, diag_dtc = MHSTDTC) %>%
    inner_join(dm %>% select(USUBJID, BRTHDTC), by = "USUBJID") %>%
    mutate(diagnosis_age = as.numeric(as.Date(diag_dtc) - as.Date(BRTHDTC)) / 365.25)
  tmp_fa_occur_age <- tmp_fa_occur %>% inner_join(diagnosis_age, by = "USUBJID")
  mismatch_should_be_blank <- tmp_fa_occur_age %>% filter(diagnosis_age <= 39 & !(is.na(FASTAT) | FASTAT == ""))
  mismatch_should_be_not_done <- tmp_fa_occur_age %>% filter(diagnosis_age > 39 & (is.na(FASTAT) | FASTAT != "NOT DONE"))
  if (nrow(mismatch_should_be_blank) > 0 || nrow(mismatch_should_be_not_done) > 0) {
    stop(str_c(
      "FA(", spid, "): FASTAT(field10)の年齢条件(初発診断日時点の年齢>39でのみ提示)チェック: NG(",
      "39歳以下でFASTATが空欄でない: ", nrow(mismatch_should_be_blank), "件(",
      paste(mismatch_should_be_blank[["USUBJID"]], collapse = ", "), ")、",
      "39歳超でFASTATが\"NOT DONE\"でない: ", nrow(mismatch_should_be_not_done), "件(",
      paste(mismatch_should_be_not_done[["USUBJID"]], collapse = ", "), "))"
    ))
  }
  cat(
    "FA(", spid, "): FASTAT(field10)の年齢条件(初発診断日時点の年齢>39でのみ提示)チェック: OK(対象",
    nrow(tmp_fa_occur_age), "件中、39歳超: ", sum(tmp_fa_occur_age[["diagnosis_age"]] > 39), "件)\n",
    sep = ""
  )

  suffix <- "_15"
  tmp_fa_occur_2 <- tmp_fa_occur %>% rename_with(~ str_c(.x, suffix), c("FATEST", "FAOBJ"))
  str_c(c("FATEST", "FAOBJ"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_occur_2, "FA", .x, fixed_value_checks_csv_path))
  # FAORRESの値チェックはFASTAT!="NOT DONE"(実施済み)の行だけを対象にする
  tmp_fa_occur_done <- tmp_fa_occur_2 %>% filter(FASTAT != "NOT DONE") %>% rename_with(~ str_c(.x, suffix), "FAORRES")
  str_c("FAORRES", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_occur_done, "FA", .x, fixed_value_checks_csv_path))

  # FAOBJ=="Osteonecrosis"で絞り込む(latecomplicationのようにFATESTCD=="GRADE"を共有する
  # 他項目(高血糖・甲状腺機能異常等)が同じVISITNUMに混在するシートがあるため、
  # FATESTCDだけでは骨壊死のGRADEを一意に特定できない)
  tmp_fa_grade <- tmp_fa %>% filter(FATESTCD == "GRADE" & FAOBJ == "Osteonecrosis")
  suffix <- "_16"
  tmp_fa_grade_2 <- tmp_fa_grade %>% rename_with(~ str_c(.x, suffix), c("FATEST", "FACAT", "FAOBJ"))
  str_c(c("FATEST", "FACAT", "FAOBJ"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_grade_2, "FA", .x, fixed_value_checks_csv_path))
  tmp_fa_occur_ref <- tmp_fa_occur %>% select(USUBJID, occur_orres = FAORRES)
  tmp_fa_grade_3 <- tmp_fa_grade_2 %>% inner_join(tmp_fa_occur_ref, by="USUBJID")
  tmp_fa_grade_3 %>% filter(occur_orres == "Y") %>% check_required_vars("FAORRES", domain_name = "FA")
  tmp_fa_grade_3 %>% filter(occur_orres != "Y") %>% check_blank_vars("FAORRES", domain_name = "FA")
  tmp_fa_grade_y <- tmp_fa_grade_3 %>% filter(occur_orres == "Y") %>% rename(!!str_c("FAORRES", suffix) := FAORRES)
  str_c("FAORRES", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_grade_y, "FA", .x, fixed_value_checks_csv_path))
}
check_osteonecrosis_fa("osteonecrosis1", "1800")
check_osteonecrosis_fa("osteonecrosis2", "2500")
check_osteonecrosis_fa("osteonecrosis3", "3000")
# latecomplication(晩期合併症報告)シート内の骨壊死ブロックは、osteonecrosis1/2/3と
# presence_conditions/構造が完全に同一(FAORRES<-FASTAT blank、FASTAT<-age>39、
# GRADE<-OCCUR=="Y"、QS Immobility/Pain<-GRADE>=2 && QSSTAT blank)なので、同じ関数を再利用する
check_osteonecrosis_fa("latecomplication", "3100")

# latecomplicationの高血糖(Hyperglycemia)Grade。presence_conditions無し(常時必須)で、
# 値の許容セット(FATEST/FACAT/FAORRES 0-5)はcheck_fa_grade_panel()のgrade_no_loc_checksに
# 既にある"Hyperglycemia"(suffix "_5")とVISITNUM以外完全に同じため、そのsuffixを再利用する
check_fa_testcd_no_loc(
  fa %>% filter(FASPID == "latecomplication" & VISITNUM == "3100"),
  "GRADE", "Hyperglycemia", "_5", fixed_value_checks_csv_path,
  has_blfl = FALSE, has_orres_in_target = TRUE
)
# latecomplicationの甲状腺機能異常(Thyroid function abnormal)Grade。presence_conditions無し
# (常時必須)。FATEST/FACAT/FAORRESの値セットはHyperglycemiaと同じ形(0-5)だが、
# grade_no_loc_checksには登録が無い項目のため新しいsuffix "_19"を使う
check_fa_testcd_no_loc(
  fa %>% filter(FASPID == "latecomplication" & VISITNUM == "3100"),
  "GRADE", "Thyroid function abnormal", "_19", fixed_value_checks_csv_path,
  has_blfl = FALSE, has_orres_in_target = TRUE
)

# sct1のPRSTDTC(移植日)。osteonecrosis3のFADTC/QSDTCの下限参照(ref('sct1',16)+150.days)先
tmp_sct1_prstdtc_ref <- pr %>% filter(PRSPID == "sct1") %>% select(USUBJID, sct1_prstdtc = PRSTDTC)
# osteonecrosis3のFADTC(field9)は、EDC仕様上sct1のPRSTDTC+150日以降であることが期待される。
# osteonecrosis1/2のFADTCには同様の参照は無い
fa %>% filter(FASPID == "osteonecrosis3" & FATESTCD == "OCCUR") %>%
  inner_join(tmp_sct1_prstdtc_ref, by = "USUBJID") %>%
  check_date_after_var_before_today("FADTC", "sct1_prstdtc", domain_name = "FA", offset_days = 150)

suffix <- "_11"
target_fa_cols <- c("FATEST", "FAOBJ", "FACAT", "FAORRES", "VISITNUM")
tmp_fa <- fa %>% filter(FATESTCD == "EARLYRES")
tmp_fa <- tmp_fa %>% rename_with(~ str_c(.x, suffix), all_of(target_fa_cols))
str_c(target_fa_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))
tmp_sv <- sv %>% filter(SVSPID == "prephase") %>% select(USUBJID, prephase825=SVSTDTC)
tmp_fa_2 <- tmp_fa %>% inner_join(tmp_sv, by="USUBJID")
tmp_fa_2 %>% check_date_after_var_before_today("FADTC", "prephase825", domain_name = "FA")

# deepmycosisz(深在性真菌症、EORTC/MSG診断基準)シート。category="multiple"のため被験者ごとに0件
# 以上のレコードを持ちうる(FASPIDは"deepmycosisz"+USUBJID内連番)。VISITNUM/FACATは無く、
# presence_conditionsも無いため、FAOBJ/FAORRES/FATEST/FATESTCD/FADTCとも常に値を持つ。FADTCは
# prephaseのSVSTDTC以降であることが期待される(date_ref_bounds)
tmp_fa_deepmycosisz <- fa %>% filter(str_detect(FASPID, "^deepmycosisz"))
c("FADTC", "FAORRES") %>% check_required_vars(tmp_fa_deepmycosisz, ., domain_name = "FA")
suffix <- "_20"
fa_deepmycosisz_target_cols <- c("FAOBJ", "FAORRES", "FATEST", "FATESTCD")
tmp_fa_deepmycosisz <- tmp_fa_deepmycosisz %>% rename_with(~ str_c(.x, suffix), all_of(fa_deepmycosisz_target_cols))
str_c(fa_deepmycosisz_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_deepmycosisz, "FA", .x, fixed_value_checks_csv_path))
tmp_sv_prephase_dm <- sv %>% filter(SVSPID == "prephase") %>% select(USUBJID, prephase_svstdtc_dm = SVSTDTC)
tmp_fa_deepmycosisz %>% inner_join(tmp_sv_prephase_dm, by = "USUBJID") %>%
  check_date_after_var_before_today("FADTC", "prephase_svstdtc_dm", domain_name = "FA")

# LB: LBTESTCDごとの個別チェック(test3用)。指定visitnumのレコードに絞り込み、LBTEST/LBCAT/
# extra_cols(+has_blflならLBBLFL)+VISITNUMをsuffix付き列名にリネームしたうえで固定値と
# 一致することを確認する。has_not_done_split=TRUEの場合、LBSTAT=="NOT DONE"で分岐し、
# LBORRES/LBDTCの要否(NOT DONEなら空欄、それ以外なら必須)を確認する。has_orres_in_targetなら
# LBORRESもsuffix付き列名にリネームして固定値チェック対象に含める(NOT DONE行を含む全行が対象の
# ときのみ使える)。has_not_done_split=TRUEでLBORRESにも固定値があるとき(例: CHROMO)は、
# check_orres_value_when_done=TRUEを指定するとDONE行に絞ったうえでLBORRESの固定値チェックも行う
check_lb_testcd_at_visit <- function(lb, lbtestcd, visitnum, suffix, extra_cols, fixed_value_checks_csv_path,
                                      has_blfl = TRUE, has_not_done_split = FALSE,
                                      has_orres_in_target = FALSE, check_orres_value_when_done = FALSE) {
  target_lb_cols <- c("LBTEST", "LBCAT", extra_cols)
  if (has_blfl) target_lb_cols <- c(target_lb_cols, "LBBLFL")
  if (has_orres_in_target) target_lb_cols <- c(target_lb_cols, "LBORRES")

  tmp_lb <- lb %>% filter(LBTESTCD == lbtestcd & VISITNUM == visitnum)
  tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))

  orres_col <- if (has_orres_in_target) str_c("LBORRES", suffix) else "LBORRES"

  if (has_not_done_split) {
    tmp_lb_done <- tmp_lb %>% filter(LBSTAT != "NOT DONE")
    tmp_lb_not_done <- tmp_lb %>% filter(LBSTAT == "NOT DONE")
    c(orres_col, "LBDTC") %>% check_required_vars(tmp_lb_done, ., domain_name = "LB")
    c(orres_col, "LBDTC") %>% check_blank_vars(tmp_lb_not_done, ., domain_name = "LB")
    if (check_orres_value_when_done) {
      tmp_lb_done <- tmp_lb_done %>% rename(!!str_c("LBORRES", suffix) := LBORRES)
      str_c("LBORRES", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_lb_done, "LB", .x, fixed_value_checks_csv_path))
    }
  } else {
    c(orres_col, "LBDTC") %>% check_required_vars(tmp_lb, ., domain_name = "LB")
  }
}

lb <- lb %>% inner_join(dm %>% select(USUBJID, BRTHDTC), by="USUBJID")
lb %>% check_date_after_var_before_today("LBDTC", "BRTHDTC", domain_name = "LB")

check_lb_testcd_at_visit(lb, "WBC", 100, "_1", c("LBORRESU", "LBSPEC"), fixed_value_checks_csv_path)
check_lb_testcd_at_visit(lb, "CA", 100, "_2", c("LBORRESU", "LBSPEC"), fixed_value_checks_csv_path, has_not_done_split = TRUE)
check_lb_testcd_at_visit(lb, "CHROMO", 100, "_3", c("LBMETHOD"), fixed_value_checks_csv_path, has_not_done_split = TRUE, check_orres_value_when_done = TRUE)
check_lb_testcd_at_visit(lb, "NUMCROSM", 100, "_4", c("LBMETHOD"), fixed_value_checks_csv_path, has_orres_in_target = TRUE)
check_lb_testcd_at_visit(lb, "DNAINDEX", 100, "_5", c("LBMETHOD"), fixed_value_checks_csv_path, has_not_done_split = TRUE)
check_lb_testcd_at_visit(lb, "MOLRGN", 100, "_6", c("LBMETHOD"), fixed_value_checks_csv_path, has_orres_in_target = TRUE)
check_lb_testcd_at_visit(lb, "IKZF1ALT", 100, "_7", c("LBMETHOD"), fixed_value_checks_csv_path, has_not_done_split = TRUE, check_orres_value_when_done = TRUE)
check_lb_testcd_at_visit(lb, "TP53MUT", 100, "_8", c("LBMETHOD"), fixed_value_checks_csv_path, has_not_done_split = TRUE, check_orres_value_when_done = TRUE)
check_lb_testcd_at_visit(lb, "IAMP21", 100, "_9", c("LBMETHOD"), fixed_value_checks_csv_path, has_not_done_split = TRUE, check_orres_value_when_done = TRUE)
check_lb_testcd_at_visit(lb, "CD19", 100, "_10", c("LBMETHOD", "LBSPEC"), fixed_value_checks_csv_path, has_not_done_split = TRUE, check_orres_value_when_done = TRUE)

check_lb_testcd_at_visit(lb, "WBC", 200, "_1", c("LBORRESU", "LBSPEC"), fixed_value_checks_csv_path, has_blfl = FALSE, has_not_done_split = TRUE, check_orres_value_when_done = FALSE)
tmp_sv <- sv %>% filter(SVSPID == "prephase") %>% select(USUBJID, SVSTDTC)
tmp_lb <- lb %>% filter(LBTESTCD == "WBC" & VISITNUM == 200) %>% inner_join(tmp_sv, by="USUBJID")
tmp_lb %>% check_date_after_var_before_today("LBDTC", "SVSTDTC", domain_name = "LB")

check_lb_testcd_at_visit(lb, "BLASTLE", 200, "_11", c("LBORRESU", "LBSPEC"), fixed_value_checks_csv_path, has_blfl = FALSE, has_not_done_split = TRUE, check_orres_value_when_done = FALSE)
tmp_lb <- lb %>% filter(LBTESTCD == "WBC" & VISITNUM == 200) %>% select(USUBJID, tmp_dtc=LBDTC)
tmp_lb_2 <- lb %>% filter(LBTESTCD == "BLASTLE" & VISITNUM == 200)
tmp_lb_2 %>% check_numeric_range("LBORRES", 0, 100, domain_name = "LB")
tmp_lb_3 <- tmp_lb %>% inner_join(tmp_lb_2, by="USUBJID")
tmp_lb_3 %>% check_date_after_var_before_today("LBDTC", "tmp_dtc", domain_name = "LB")

check_lb_testcd_at_visit(lb, "MYBLALE", 200, "_12", c("LBORRESU", "LBSPEC"), fixed_value_checks_csv_path, has_blfl = FALSE, has_not_done_split = TRUE, check_orres_value_when_done = FALSE)
tmp_lb_2 <- lb %>% filter(LBTESTCD == "MYBLALE" & VISITNUM == 200)
tmp_lb_2 %>% check_numeric_range("LBORRES", 0, 100, domain_name = "LB")
# MYBLALEのLBDTCはFA(EARLYRES/Total Prednisolone Dose of 210 mg/m^2 or more)のFADTC(+1日、
# オフセット自体は他の日付チェックと同様に厳密には反映せず>=で確認)以降であることが期待される
tmp_fa <- fa %>% filter(FATESTCD == "EARLYRES" & FAOBJ == "Total Prednisolone Dose of 210 mg/m^2 or more") %>% select(USUBJID, tmp_dtc=FADTC)
tmp_lb_3 <- tmp_lb_2 %>% inner_join(tmp_fa, by="USUBJID")
tmp_lb_3 %>% check_date_after_var_before_today("LBDTC", "tmp_dtc", domain_name = "LB")

# evaluationtp1のMRDQV。LBORRESは固定値ではなく選択式(カテゴリ)のためcheck_lb_testcd_at_visit内の
# required_varsチェックのみで対応する。LBDTCはinductionlabのMYBLALE(VISITNUM=200)のLBDTC以降であることが
# 期待される(ref('inductionlab', 109))
check_lb_testcd_at_visit(lb, "MRDQV", 250, "_13", c("LBMETHOD", "LBSPEC"), fixed_value_checks_csv_path, has_blfl = FALSE, has_not_done_split = TRUE, check_orres_value_when_done = TRUE)
tmp_lb_2 <- lb %>% filter(LBTESTCD == "MRDQV" & VISITNUM == 250)
tmp_lb_2 %>% filter(LBSTAT == "NOT DONE") %>% select(LBREASND_13=LBREASND) %>% run_value_equals_checks_from_csv("LB", "LBREASND_13", fixed_value_checks_csv_path)
tmp_lb_2 %>% filter(LBSTAT == "NOT DONE") %>% check_required_vars("LBREASND", domain_name = "LB")
tmp_lb_2 %>% filter(LBSTAT != "NOT DONE") %>% check_blank_vars("LBREASND", domain_name = "LB")
tmp_lb <- lb %>% filter(LBTESTCD == "MYBLALE" & VISITNUM == 200) %>% select(USUBJID, tmp_dtc=LBDTC)
tmp_lb_3 <- tmp_lb_2 %>% inner_join(tmp_lb, by="USUBJID")
tmp_lb_3 %>% check_date_after_var_before_today("LBDTC", "tmp_dtc", domain_name = "LB")

# evaluationtp1のMYBLALE(VISITNUM=250)。LBDTCはMRDQVと同様、inductionlabのMYBLALE(VISITNUM=200)の
# LBDTC以降であることが期待される(ref('inductionlab', 109))
check_lb_testcd_at_visit(lb, "MYBLALE", 250, "_14", c("LBORRESU", "LBSPEC"), fixed_value_checks_csv_path, has_blfl = FALSE, has_not_done_split = TRUE, check_orres_value_when_done = FALSE)
tmp_lb_2 <- lb %>% filter(LBTESTCD == "MYBLALE" & VISITNUM == 250)
tmp_lb_2 %>% check_numeric_range("LBORRES", 0, 100, domain_name = "LB")
tmp_lb <- lb %>% filter(LBTESTCD == "MYBLALE" & VISITNUM == 200) %>% select(USUBJID, tmp_dtc=LBDTC)
tmp_lb_3 <- tmp_lb_2 %>% inner_join(tmp_lb, by="USUBJID")
tmp_lb_3 %>% check_date_after_var_before_today("LBDTC", "tmp_dtc", domain_name = "LB")

# nudtのNUDT15(VISITNUM=200)。LBDTCに明示的なref()参照は無く、上のBRTHDTC以降チェック(全LB共通)以外の
# 追加の日付チェックは不要
check_lb_testcd_at_visit(lb, "NUDT15", 200, "_15", c("LBMETHOD"), fixed_value_checks_csv_path, has_blfl = FALSE, has_not_done_split = TRUE, check_orres_value_when_done = TRUE)

# pcrmrdtp2のMRDQV(VISITNUM=350)。LBORRESのカテゴリはevaluationtp1のMRDQVと同一。LBDTCは
# evaluationtp1のOVRLRESP(VISITNUM=250)のRSDTC以降(ref('evaluationtp1', 119))、かつ
# evaluationtp2のOVRLRESP(VISITNUM=350)のRSDTC以前(ref('evaluationtp2', 21))であることが期待される
check_lb_testcd_at_visit(lb, "MRDQV", 350, "_16", c("LBMETHOD", "LBSPEC"), fixed_value_checks_csv_path, has_blfl = FALSE, has_not_done_split = TRUE, check_orres_value_when_done = TRUE)
tmp_lb_2 <- lb %>% filter(LBTESTCD == "MRDQV" & VISITNUM == 350)
tmp_lb_2 %>% filter(LBSTAT == "NOT DONE") %>% select(LBREASND_16=LBREASND) %>% run_value_equals_checks_from_csv("LB", "LBREASND_16", fixed_value_checks_csv_path)
tmp_lb_2 %>% filter(LBSTAT == "NOT DONE") %>% check_required_vars("LBREASND", domain_name = "LB")
tmp_lb_2 %>% filter(LBSTAT != "NOT DONE") %>% check_blank_vars("LBREASND", domain_name = "LB")
tmp_rs_min <- rs %>% filter(RSTESTCD == "OVRLRESP" & VISITNUM == 250) %>% select(USUBJID, tmp_dtc_min=RSDTC)
tmp_rs_max <- rs %>% filter(RSTESTCD == "OVRLRESP" & VISITNUM == 350) %>% select(USUBJID, tmp_dtc_max=RSDTC)
tmp_lb_3 <- tmp_lb_2 %>% inner_join(tmp_rs_min, by="USUBJID") %>% inner_join(tmp_rs_max, by="USUBJID")
tmp_lb_3 %>% check_date_after_var_before_today("LBDTC", "tmp_dtc_min", domain_name = "LB")
tmp_lb_4 <- tmp_lb_3 %>% filter(!is.na(LBDTC) & LBDTC > tmp_dtc_max)
if (nrow(tmp_lb_4) > 0) {
  stop(str_c("LB: LBDTC範囲チェック: ", nrow(tmp_lb_4), "件NG(evaluationtp2のRSDTC以前ではない。USUBJID: ", paste(tmp_lb_4[["USUBJID"]], collapse = ", "), ")"))
}
cat("LB: LBDTC範囲チェック: OK(LBDTCがevaluationtp2のRSDTC以前であることを確認、", nrow(tmp_lb_3), "件)\n", sep = "")

# relapse(再発報告)シートのCD19(LB)。VISITNUMを持たないためcheck_lb_testcd_at_visit()は使えず、
# LBSPID=="relapse"で直接絞り込む。LBORRESは"f141==1&&STAT.blank?"がpresence条件だが、f141は
# CDISC変数にマッピングされていないフィールドのためSTAT.blank?部分のみが実際に反映されている
# (CEのCETERM/CEDTCと同様、現状の生成ロジックに合わせて検証する)
tmp_lb_relapse <- lb %>% filter(LBSPID == "relapse")
lb_relapse_target_cols <- c("LBTEST", "LBCAT", "LBSPEC", "LBMETHOD", "LBORRES")
suffix <- "_23"
lb_relapse_target_cols <- c("LBTEST", "LBCAT", "LBSPEC", "LBMETHOD")
tmp_lb_relapse_2 <- tmp_lb_relapse %>% rename_with(~ str_c(.x, suffix), all_of(lb_relapse_target_cols))
str_c(lb_relapse_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_lb_relapse_2, "LB", .x, fixed_value_checks_csv_path))
lb_relapse_target_cols <- c("LBORRES")
tmp_lb_relapse_2 <- tmp_lb_relapse %>% rename_with(~ str_c(.x, suffix), all_of(lb_relapse_target_cols))
tmp_lb_relapse_done <- tmp_lb_relapse_2 %>% filter(LBSTAT != "NOT DONE")
tmp_lb_relapse_not_done <- tmp_lb_relapse_2 %>% filter(LBSTAT == "NOT DONE")
str_c(lb_relapse_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_lb_relapse_done, "LB", .x, fixed_value_checks_csv_path))
c("LBORRES_23", "LBDTC") %>% check_required_vars(tmp_lb_relapse_done, ., domain_name = "LB")
c("LBORRES_23", "LBDTC") %>% check_blank_vars(tmp_lb_relapse_not_done, ., domain_name = "LB")
tmp_lb_relapse_done %>% inner_join(tmp_ce_diag, by = "USUBJID") %>%
  check_date_after_var_before_today("LBDTC", "diag_dtc", domain_name = "LB")

# immunomonitoring1(BLIN群イムノモニタリング: BLIN 1サイクル目)のNEUTLE/EOSLE/BASOLE/MONOLE/LYMLE/WBCを
# VISITNUM 500/600/700/800/900の5時点で測定する。各testcdの固定値(LBTEST/LBCAT/LBORRESU/LBSPEC)は
# 時点によらず共通のため、testcdごとに1つのsuffixを5時点で使い回すtribble+pwalkでまとめて検証する
immuno1_lb_checks <- tribble(
  ~lbtestcd, ~visitnum, ~suffix,
  "NEUTLE", 500, "_17",
  "NEUTLE", 600, "_17",
  "NEUTLE", 700, "_17",
  "NEUTLE", 800, "_17",
  "NEUTLE", 900, "_17",
  "EOSLE", 500, "_18",
  "EOSLE", 600, "_18",
  "EOSLE", 700, "_18",
  "EOSLE", 800, "_18",
  "EOSLE", 900, "_18",
  "BASOLE", 500, "_19",
  "BASOLE", 600, "_19",
  "BASOLE", 700, "_19",
  "BASOLE", 800, "_19",
  "BASOLE", 900, "_19",
  "MONOLE", 500, "_20",
  "MONOLE", 600, "_20",
  "MONOLE", 700, "_20",
  "MONOLE", 800, "_20",
  "MONOLE", 900, "_20",
  "LYMLE", 500, "_21",
  "LYMLE", 600, "_21",
  "LYMLE", 700, "_21",
  "LYMLE", 800, "_21",
  "LYMLE", 900, "_21",
  "WBC", 500, "_22",
  "WBC", 600, "_22",
  "WBC", 700, "_22",
  "WBC", 800, "_22",
  "WBC", 900, "_22"
)
pwalk(immuno1_lb_checks, function(lbtestcd, visitnum, suffix) {
  check_lb_testcd_at_visit(lb, lbtestcd, visitnum, suffix, c("LBORRESU", "LBSPEC"), fixed_value_checks_csv_path, has_blfl = FALSE, has_not_done_split = TRUE, check_orres_value_when_done = FALSE)
})

# NEUTLE/EOSLE/BASOLE/MONOLE/LYMLEはLeukocytes中の割合(%)のため、0〜100の範囲であることを確認する
# (WBCは実数のカウント値のため対象外)
lb %>%
  filter(LBSPID == "immunomonitoring1" & LBTESTCD %in% c("NEUTLE", "EOSLE", "BASOLE", "MONOLE", "LYMLE") & LBSTAT != "NOT DONE") %>%
  check_numeric_range("LBORRES", 0, 100, domain_name = "LB")

# WBCはカウント値のため上限は設けず、0以上であることのみ確認する
lb %>%
  filter(LBSPID == "immunomonitoring1" & LBTESTCD == "WBC" & LBSTAT != "NOT DONE") %>%
  check_numeric_range("LBORRES", 0, domain_name = "LB")

# EDC仕様のref()を確認したところ、各時点(VISITNUM 600以降)の6項目(WBC+NEUTLE/EOSLE/BASOLE/MONOLE/LYMLE)は
# いずれも「同じ時点のWBC」ではなく「直前の時点のWBC」のLBDTCを共通の起点として参照している
# (例: VISITNUM=600の6項目は全てVISITNUM=500のWBCのLBDTCを参照。同一時点内の項目同士に依存関係は無い)。
# 先頭の時点(500)には参照が無いためチェック対象外
immuno1_visits <- c(500, 600, 700, 800, 900)
walk(2:length(immuno1_visits), function(i) {
  prev_v <- immuno1_visits[i - 1]
  cur_v <- immuno1_visits[i]
  tmp_ref <- lb %>% filter(LBSPID == "immunomonitoring1" & LBTESTCD == "WBC" & VISITNUM == prev_v) %>% select(USUBJID, tmp_dtc = LBDTC)
  tmp_lb <- lb %>% filter(LBSPID == "immunomonitoring1" & LBTESTCD %in% c("WBC", "NEUTLE", "EOSLE", "BASOLE", "MONOLE", "LYMLE") & VISITNUM == cur_v & LBSTAT != "NOT DONE")
  tmp_lb_2 <- tmp_lb %>% inner_join(tmp_ref, by = "USUBJID")
  tmp_lb_2 %>% check_date_after_var_before_today("LBDTC", "tmp_dtc", domain_name = "LB")
})

# MH
tmp_mh <- mh %>% filter(MHCAT == "PRIMARY DIAGNOSIS")
c("MHSTDTC") %>% check_required_vars(tmp_mh, ., domain_name = "MH")
tmp_mh %>% check_date_before_today(c("MHSTDTC"), domain_name = "MH")
mh_target_cols <- c("MHTERM", "MHPRESP", "MHOCCUR")
suffix <- "_1"
tmp_mh <- tmp_mh %>% rename_with(~ str_c(.x, suffix), all_of(mh_target_cols))
str_c(mh_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path))

mh_target_cols <- c("MHPRESP", "MHOCCUR", "MHENRTPT", "MHENTPT")
tmp_mh <- mh %>% filter(MHCAT == "GENERAL" & MHTERM == "Antithrombin III deficiency" & MHSTAT != "NOT DONE")
suffix <- "_2"
tmp_mh <- tmp_mh %>% rename_with(~ str_c(.x, suffix), all_of(mh_target_cols))
str_c(mh_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path))

tmp_mh <- mh %>% filter(MHCAT == "GENERAL" &
                        (MHTERM == "Protein C deficiency" | MHTERM == "Protein S deficiency" | MHTERM == "Plasminogen decreased" | MHTERM == "Hypofibrinogenaemia" | MHTERM == "Homocystinuria") &
                        MHSTAT != "NOT DONE")
suffix <- "_3"
tmp_mh <- tmp_mh %>% rename_with(~ str_c(.x, suffix), all_of(mh_target_cols))
str_c(mh_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path))

"MHTERM" %>% check_required_vars(filter(mh, MHOCCUR=="Y"), ., domain_name="MH")

# PC
# hdm5/hr2fisrtのCONC(薬物濃度測定、PCTPTNUM=24/42/48/66の4時点)。実施の有無(PCSTAT)はそのシート自身の
# METHOTREXATE投与有無(ECOCCUR)に連動し、投与していれば(ECOCCUR=="N")採血自体を行わない
# (PCSTAT="NOT DONE")。PCDTCは1時点目がそのシート自身のSVSTDTC以降、2時点目以降は直前の時点のPCDTC
# 以降であることが期待される(同一alias内でlabelを跨ぐ日付連鎖)。PCCATはEDC仕様上field_type="drug"の
# フィールドで、default_value("422240001")は薬剤コードとしてwho_drug_idfから薬剤名を引く仕様のため、
# 実際に格納される値は薬剤名"METHOTREXATE"になる(コード文字列そのものではない)。固定値・構造は
# シート間で共通のため関数化してsuffixを使い回す
pc_target_cols <- c("PCTEST", "PCCAT", "PCORRESU", "PCSPEC")
check_pc_conc_chain <- function(pcspid) {
  tmp_sv <- sv %>% filter(SVSPID == pcspid) %>% select(USUBJID, sv_dtc = SVSTDTC)
  pctptnums <- c(24, 42, 48, 66)
  ref_data <- tmp_sv
  ref_var <- "sv_dtc"
  for (tptnum in pctptnums) {
    tmp_pc <- pc %>% filter(PCSPID == pcspid & PCTPTNUM == tptnum)
    tmp_pc %>% filter(PCSTAT != "NOT DONE") %>% check_required_vars(c("PCORRES", "PCDTC"), domain_name = "PC")
    tmp_pc %>% filter(PCSTAT == "NOT DONE") %>% check_blank_vars(c("PCORRES", "PCDTC"), domain_name = "PC")
    suffix <- "_1"
    tmp_pc <- tmp_pc %>% rename_with(~ str_c(.x, suffix), all_of(pc_target_cols))
    str_c(pc_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_pc, "PC", .x, fixed_value_checks_csv_path))
    tmp_pc_2 <- tmp_pc %>% filter(PCSTAT != "NOT DONE") %>% inner_join(ref_data, by = "USUBJID")
    tmp_pc_2 %>% check_date_after_var_before_today("PCDTC", ref_var, domain_name = "PC")
    ref_data <- pc %>% filter(PCSPID == pcspid & PCTPTNUM == tptnum) %>% select(USUBJID, tmp_dtc = PCDTC)
    ref_var <- "tmp_dtc"
  }
}
check_pc_conc_chain("hdm5")
check_pc_conc_chain("hr2fisrt")

# PR
pr %>% filter(PRSPID != "sct1") %>% check_required_vars("PROCCUR", domain_name = "PR")
pr %>% filter(PRSPID == "sct1") %>% check_blank_vars("PROCCUR", domain_name = "PR")

tmp_pr <- pr %>% filter(PRSPID == "sct1")
c("PRCAT", "PRTRT", "PRSTDTC") %>% check_required_vars(tmp_pr, ., domain_name = "PR")
sct1_pr_target_cols <- c("VISITNUM", "PRTRT", "PRCAT")
suffix <- "_2"
tmp_pr_2 <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(sct1_pr_target_cols))
str_c(sct1_pr_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_pr_2, "PR", .x, fixed_value_checks_csv_path))
tmp_cm_sct1 <- cm %>% filter(CMSPID == "sct1") %>% select(USUBJID, sct1_cmstdtc=CMSTDTC)
tmp_pr_3 <- tmp_pr %>% inner_join(tmp_cm_sct1, by="USUBJID")
tmp_pr_3 %>% check_date_after_var_before_today("PRSTDTC", "sct1_cmstdtc", domain_name = "PR")

# sct2(2度目の造血幹細胞移植)。sct1と異なりPRCAT/VISITNUMは無く、PROCCUR/PRPRESP/PRSTRTPT/PRSTTPT/
# PRTRTは全てpresence_conditions無しの固定値(is_invisible)。PRSTDTCはdiscon(中止)のDSSTDTC以降
# であることが期待される(date_ref_bounds: ref('discon', ...))
tmp_pr_sct2 <- pr %>% filter(PRSPID == "sct2")
c("PROCCUR", "PRSTDTC", "PRPRESP", "PRSTRTPT", "PRSTTPT", "PRTRT") %>% check_required_vars(tmp_pr_sct2, ., domain_name = "PR")
suffix <- "_3"
sct2_pr_target_cols <- c("PROCCUR", "PRPRESP", "PRSTRTPT", "PRSTTPT", "PRTRT")
tmp_pr_sct2 <- tmp_pr_sct2 %>% rename_with(~ str_c(.x, suffix), all_of(sct2_pr_target_cols))
str_c(sct2_pr_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_pr_sct2, "PR", .x, fixed_value_checks_csv_path))
tmp_ds_discon <- ds %>% filter(DSSPID == "discon") %>% select(USUBJID, discon_dtc = DSSTDTC)
tmp_pr_sct2 %>% inner_join(tmp_ds_discon, by = "USUBJID") %>%
  check_date_after_var_before_today("PRSTDTC", "discon_dtc", domain_name = "PR")

# induction(VISITNUM=200)とearlyintensifi(VISITNUM=300)は、Central Venous Catheter Placementの
# 内容が完全に同一のため、VISITNUMをパラメータにしたtribble+pwalkでまとめて検証する
pr_target_cols <- c("PRPRESP", "PROCCUR")
pr_checks <- tribble(
  ~visitnum, ~prtrt, ~suffix,
  200, "Central Venous Catheter Placement", "_1",
  300, "Central Venous Catheter Placement", "_1",
  400, "Central Venous Catheter Placement", "_1",
  1100, "Central Venous Catheter Placement", "_1",
  1200, "Central Venous Catheter Placement", "_1",
  1400, "Central Venous Catheter Placement", "_1",
  1500, "Central Venous Catheter Placement", "_1",
  1600, "Central Venous Catheter Placement", "_1",
  1900, "Central Venous Catheter Placement", "_1",
  2000, "Central Venous Catheter Placement", "_1",
  2100, "Central Venous Catheter Placement", "_1"
)
pwalk(pr_checks, function(visitnum, prtrt, suffix) {
  tmp_pr <- pr %>% filter(PRTRT == prtrt & VISITNUM == visitnum)
  tmp_pr <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(pr_target_cols))
  str_c(pr_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_pr, "PR", .x, fixed_value_checks_csv_path))
})

# osteonecrosis1/2/3共通。QSTESTCD(IMOBIDX=Immobility Index/PNSEVIDX=Pain Severity Index)ごとに、
# QSORRES(1〜4の順序尺度)自体がQSSTAT(空欄=実施済み)かつGRADE(FA GRADE、骨壊死のGrade。label=001の
# FAORRES)が2以上のときだけ値を持つ(EDC仕様のvalidate_presence_if: "f16>=2&&STAT.blank?"、
# f16はGRADE)。QSDTCはQSSTATが空欄、かつQSORRESに値がある(=上記条件を満たす)ときだけ値を持つ
is_blank_val <- function(x) is.na(x) | x == ""
check_qs_osteo_testcd <- function(qs, fa, spid, qstestcd, suffix, fixed_value_checks_csv_path) {
  target_qs_cols <- c("QSTEST", "QSCAT", "VISITNUM")
  tmp_qs <- qs %>% filter(QSSPID == spid & QSTESTCD == qstestcd)
  tmp_qs_2 <- tmp_qs %>% rename_with(~ str_c(.x, suffix), all_of(target_qs_cols))
  str_c(target_qs_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_qs_2, "QS", .x, fixed_value_checks_csv_path))

  # FAOBJ=="Osteonecrosis"で絞り込む(latecomplicationのようにFATESTCD=="GRADE"を共有する
  # 他項目(高血糖・甲状腺機能異常等)が混在するシートでは、USUBJIDあたり複数のGRADE行が
  # 存在しうるため、FAOBJで絞らないとleft_join()で誤った行と結合されてしまう)
  grade <- fa %>% filter(FASPID == spid & FATESTCD == "GRADE" & FAOBJ == "Osteonecrosis") %>% select(USUBJID, grade_orres = FAORRES)
  tmp_qs_grade <- tmp_qs %>% left_join(grade, by = "USUBJID") %>%
    mutate(grade_num = suppressWarnings(as.numeric(grade_orres)))
  stat_done <- !(!is.na(tmp_qs_grade[["QSSTAT"]]) & tmp_qs_grade[["QSSTAT"]] == "NOT DONE")
  grade_ge2 <- !is.na(tmp_qs_grade[["grade_num"]]) & tmp_qs_grade[["grade_num"]] >= 2

  tmp_qs_grade %>% filter(stat_done & grade_ge2) %>% check_required_vars("QSORRES", domain_name = "QS")
  tmp_qs_grade %>% filter(!(stat_done & grade_ge2)) %>% check_blank_vars("QSORRES", domain_name = "QS")
  tmp_qs_grade %>% check_values_subset_of("QSORRES", c("1", "2", "3", "4"), domain_name = "QS")

  orres_present <- !is_blank_val(tmp_qs_grade[["QSORRES"]])
  tmp_qs_grade %>% filter(stat_done & orres_present) %>% check_required_vars("QSDTC", domain_name = "QS")
  tmp_qs_grade %>% filter(!(stat_done & orres_present)) %>% check_blank_vars("QSDTC", domain_name = "QS")
}
check_qs_osteo_testcd(qs, fa, "osteonecrosis1", "IMOBIDX", "_1", fixed_value_checks_csv_path)
check_qs_osteo_testcd(qs, fa, "osteonecrosis1", "PNSEVIDX", "_2", fixed_value_checks_csv_path)
# QSTEST/QSCATはosteonecrosis1と同一の固定値だが、VISITNUMが1800→2500と異なるため
# 同じsuffixは再利用できず、新しいsuffix "_3"/"_4"を使う
check_qs_osteo_testcd(qs, fa, "osteonecrosis2", "IMOBIDX", "_3", fixed_value_checks_csv_path)
check_qs_osteo_testcd(qs, fa, "osteonecrosis2", "PNSEVIDX", "_4", fixed_value_checks_csv_path)

# osteonecrosis2のQSDTC(field27/field36)は、EDC仕様上maitenanceシートのSVENDTC-28日以降であることが
# 期待される(validate_date_after_or_equal_to: "ref('maitenance',829)-28.days")。osteonecrosis1の
# QSDTCには同様の参照は無い
tmp_qs_maitenance_ref <- sv %>% filter(SVSPID == "maitenance") %>% select(USUBJID, maitenance_svendtc = SVENDTC)
qs %>% filter(QSSPID == "osteonecrosis2") %>%
  inner_join(tmp_qs_maitenance_ref, by = "USUBJID") %>%
  check_date_after_var_before_today("QSDTC", "maitenance_svendtc", domain_name = "QS", offset_days = -28)

check_qs_osteo_testcd(qs, fa, "osteonecrosis3", "IMOBIDX", "_5", fixed_value_checks_csv_path)
check_qs_osteo_testcd(qs, fa, "osteonecrosis3", "PNSEVIDX", "_6", fixed_value_checks_csv_path)

# osteonecrosis3のQSDTC(field27/field36)も、FADTCと同じくsct1のPRSTDTC+150日以降であることが期待される
# (ref('sct1',16)+150.days)
qs %>% filter(QSSPID == "osteonecrosis3") %>%
  inner_join(tmp_sct1_prstdtc_ref, by = "USUBJID") %>%
  check_date_after_var_before_today("QSDTC", "sct1_prstdtc", domain_name = "QS", offset_days = 150)

# latecomplicationシート内の骨壊死Immobility/Pain Indexも、osteonecrosis1/2/3と構造が同一
# (GRADE>=2 && QSSTAT blank)なので既存の関数を再利用する。QSTEST/QSCATは同一だがVISITNUMが
# 異なるため新しいsuffix "_7"/"_8"を使う
check_qs_osteo_testcd(qs, fa, "latecomplication", "IMOBIDX", "_7", fixed_value_checks_csv_path)
check_qs_osteo_testcd(qs, fa, "latecomplication", "PNSEVIDX", "_8", fixed_value_checks_csv_path)

# latecomplicationのNYHA心機能分類(QSTESTCD=="NYHACLS")。QSORRES/QSDTCともpresence_conditions無し
# (常時必須)
tmp_qs_nyha <- qs %>% filter(QSSPID == "latecomplication", QSTESTCD == "NYHACLS")
c("QSORRES", "QSDTC") %>% check_required_vars(tmp_qs_nyha, ., domain_name = "QS")
qs_nyha_target_cols <- c("QSTEST", "QSCAT", "VISITNUM", "QSORRES")
suffix <- "_9"
tmp_qs_nyha_2 <- tmp_qs_nyha %>% rename_with(~ str_c(.x, suffix), all_of(qs_nyha_target_cols))
str_c(qs_nyha_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_qs_nyha_2, "QS", .x, fixed_value_checks_csv_path))

# RS
c("RSORRES", "RSDTC") %>% check_required_vars(rs, ., domain_name="RS")
rs_target_cols <- c("RSTEST", "RSCAT" ,"RSORRES", "RSEVAL")

# evaluationtp1のOVRLRESP(VISITNUM=250)。RSDTCはinductionlabのMYBLALE(VISITNUM=200)以降であることが
# 期待される(ref('inductionlab', 109))
tmp_rs <- rs %>% filter(RSTESTCD == "OVRLRESP" & VISITNUM == 250)
if (nrow(tmp_rs) == 0) {
  stop("RS error visitnum==250")
}
suffix <- "_1"
tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, suffix), all_of(rs_target_cols))
str_c(rs_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))
tmp_lb <- lb %>% filter(LBTESTCD == "MYBLALE" & VISITNUM == 200) %>% select(USUBJID, tmp_dtc=LBDTC)
tmp_rs_2 <- tmp_rs %>% inner_join(tmp_lb, by="USUBJID")
tmp_rs_2 %>% check_date_after_var_before_today("RSDTC", "tmp_dtc", domain_name = "RS")

# evaluationtp2のOVRLRESP(VISITNUM=350)。RSDTCはevaluationtp1自身のOVRLRESP(VISITNUM=250)のRSDTC
# 以降であることが期待される(ref('evaluationtp1', 119)。inductionlab基準ではない)
tmp_rs <- rs %>% filter(RSTESTCD == "OVRLRESP" & VISITNUM == 350)
if (nrow(tmp_rs) == 0) {
  stop("RS error visitnum==350")
}
suffix <- "_2"
tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, suffix), all_of(rs_target_cols))
str_c(rs_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))
tmp_rs_evaluationtp1 <- rs %>% filter(RSTESTCD == "OVRLRESP" & VISITNUM == 250) %>% select(USUBJID, tmp_dtc=RSDTC)
tmp_rs_2 <- tmp_rs %>% inner_join(tmp_rs_evaluationtp1, by="USUBJID")
tmp_rs_2 %>% check_date_after_var_before_today("RSDTC", "tmp_dtc", domain_name = "RS")

# SC
c("SCTESTCD", "SCTEST") %>% walk(~ run_value_equals_checks_from_csv(sc, "SC", .x, fixed_value_checks_csv_path))
"SCORRES" %>% check_required_vars(sc, ., domain_name = "SC")

# SV
tmp_mh <- mh %>% filter(MHCAT == "PRIMARY DIAGNOSIS") %>% select(USUBJID, MHSTDTC)
sv <- sv %>% inner_join(tmp_mh, by="USUBJID")

tmp_sv <- sv %>% filter(SVSPID == "prephase")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "MHSTDTC", domain_name = "SV")
sv_target_cols <- c("VISITNUM")
suffix <- "_1"
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))

tmp_sv_2 <- sv %>% filter(SVSPID == "prephase") %>% select(USUBJID, prephase825=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "induction") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "prephase825", domain_name = "SV")

# earlyintensifiのSVSTDTCはinductionのSVSTDTC以降であることが期待される(ref('induction', 820))
tmp_sv_2 <- sv %>% filter(SVSPID == "induction") %>% select(USUBJID, induction820=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "earlyintensifi") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "induction820", domain_name = "SV")
suffix <- "_2"
tmp_sv <- sv %>% filter(SVSPID == "earlyintensifi")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))

# hdmのSVSTDTCはevaluationtp2のOVRLRESP(VISITNUM=350)のRSDTC以降であることが期待される
# (ref('evaluationtp2', ...))
tmp_rs_evaluationtp2 <- rs %>% filter(RSTESTCD == "OVRLRESP" & VISITNUM == 350) %>% select(USUBJID, evaluationtp2_rsdtc=RSDTC)
tmp_sv <- sv %>% filter(SVSPID == "hdm") %>% inner_join(tmp_rs_evaluationtp2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "evaluationtp2_rsdtc", domain_name = "SV")
suffix <- "_3"
tmp_sv <- sv %>% filter(SVSPID == "hdm")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# hdm2のSVSTDTCはhdmと同様、evaluationtp2のOVRLRESP(VISITNUM=350)のRSDTC以降であることが
# 期待される(ref('evaluationtp2', ...))
tmp_sv <- sv %>% filter(SVSPID == "hdm2") %>% inner_join(tmp_rs_evaluationtp2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "evaluationtp2_rsdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_3"
tmp_sv <- sv %>% filter(SVSPID == "hdm2")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# hdm5のSVSTDTCはhdm/hdm2と同様、evaluationtp2のOVRLRESP(VISITNUM=350)のRSDTC以降であることが
# 期待される(ref('evaluationtp2', ...))
tmp_sv <- sv %>% filter(SVSPID == "hdm5") %>% inner_join(tmp_rs_evaluationtp2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "evaluationtp2_rsdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_3"
tmp_sv <- sv %>% filter(SVSPID == "hdm5")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# hr3fisrtのSVSTDTCはhdm系列と同様、evaluationtp2のOVRLRESP(VISITNUM=350)のRSDTC以降であることが
# 期待される(ref('evaluationtp2', ...))
tmp_sv <- sv %>% filter(SVSPID == "hr3fisrt") %>% inner_join(tmp_rs_evaluationtp2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "evaluationtp2_rsdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_3"
tmp_sv <- sv %>% filter(SVSPID == "hr3fisrt")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# blin1のSVSTDTCはhdm系列と同様、evaluationtp2のOVRLRESP(VISITNUM=350)のRSDTC以降であることが
# 期待される(ref('evaluationtp2', ...))。VISITNUMはhdm系列/hr3fisrtの400とは異なり900のため、
# 新しいsuffix "_4"を使う
tmp_sv <- sv %>% filter(SVSPID == "blin1") %>% inner_join(tmp_rs_evaluationtp2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "evaluationtp2_rsdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_4"
tmp_sv <- sv %>% filter(SVSPID == "blin1")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# blin2のSVSTDTCはblin1自身のSVSTDTC以降であることが期待される(ref('blin1', ...)。
# evaluationtp2基準ではない)。VISITNUMは1000のため新しいsuffix "_5"を使う
tmp_sv_2 <- sv %>% filter(SVSPID == "blin1") %>% select(USUBJID, blin1_svstdtc=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "blin2") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "blin1_svstdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_5"
tmp_sv <- sv %>% filter(SVSPID == "blin2")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# hr2fisrtのSVSTDTCはhdm系列と同様、evaluationtp2のOVRLRESP(VISITNUM=350)のRSDTC以降であることが
# 期待される(ref('evaluationtp2', ...))。VISITNUMは1100のため新しいsuffix "_6"を使う
tmp_sv <- sv %>% filter(SVSPID == "hr2fisrt") %>% inner_join(tmp_rs_evaluationtp2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "evaluationtp2_rsdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_6"
tmp_sv <- sv %>% filter(SVSPID == "hr2fisrt")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# hr1fisrtのSVSTDTCはhr2fisrt自身のSVSTDTC以降であることが期待される(ref('hr2fisrt', ...)。
# evaluationtp2基準ではない)。VISITNUMは1200のため新しいsuffix "_7"を使う
tmp_sv_2 <- sv %>% filter(SVSPID == "hr2fisrt") %>% select(USUBJID, hr2fisrt_svstdtc=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "hr1fisrt") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "hr2fisrt_svstdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_7"
tmp_sv <- sv %>% filter(SVSPID == "hr1fisrt")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# hr3secondのSVSTDTCはhr1fisrt自身のSVSTDTC以降であることが期待される(ref('hr1fisrt', ...)。
# evaluationtp2基準ではない)。VISITNUMは1400のため新しいsuffix "_8"を使う
tmp_sv_2 <- sv %>% filter(SVSPID == "hr1fisrt") %>% select(USUBJID, hr1fisrt_svstdtc=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "hr3second") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "hr1fisrt_svstdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_8"
tmp_sv <- sv %>% filter(SVSPID == "hr3second")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# hr2secondのSVSTDTCはhr3second自身のSVSTDTC以降であることが期待される(ref('hr3second', ...)。
# evaluationtp2基準ではない)。VISITNUMは1500のため新しいsuffix "_9"を使う
tmp_sv_2 <- sv %>% filter(SVSPID == "hr3second") %>% select(USUBJID, hr3second_svstdtc=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "hr2second") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "hr3second_svstdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_9"
tmp_sv <- sv %>% filter(SVSPID == "hr2second")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# hr1secondのSVSTDTCはhr2second自身のSVSTDTC以降であることが期待される(ref('hr2second', ...)。
# evaluationtp2基準ではない)。VISITNUMは1600のため新しいsuffix "_10"を使う
tmp_sv_2 <- sv %>% filter(SVSPID == "hr2second") %>% select(USUBJID, hr2second_svstdtc=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "hr1second") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "hr2second_svstdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_10"
tmp_sv <- sv %>% filter(SVSPID == "hr1second")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# blin3のSVSTDTCはhr1fisrt自身のSVSTDTC以降であることが期待される(ref('hr1fisrt', ...)。
# evaluationtp2でもblin2でもない)。VISITNUMは1700のため新しいsuffix "_11"を使う
tmp_sv_2 <- sv %>% filter(SVSPID == "hr1fisrt") %>% select(USUBJID, hr1fisrt_svstdtc_2=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "blin3") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "hr1fisrt_svstdtc_2", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_11"
tmp_sv <- sv %>% filter(SVSPID == "blin3")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# reinduction1のSVSTDTCはhdm自身のSVSTDTC以降であることが期待される(ref('hdm', ...))。
# VISITNUMは1900のため新しいsuffix "_12"を使う
tmp_sv_2 <- sv %>% filter(SVSPID == "hdm") %>% select(USUBJID, hdm_svstdtc=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "reinduction1") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "hdm_svstdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_12"
tmp_sv <- sv %>% filter(SVSPID == "reinduction1")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# reinduction2のSVSTDTCはhdm系列と同様、evaluationtp2のOVRLRESP(VISITNUM=350)のRSDTC以降であることが
# 期待される(ref('evaluationtp2', ...))。VISITNUMは2000のため新しいsuffix "_13"を使う
tmp_sv <- sv %>% filter(SVSPID == "reinduction2") %>% inner_join(tmp_rs_evaluationtp2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "evaluationtp2_rsdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_13"
tmp_sv <- sv %>% filter(SVSPID == "reinduction2")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# reinduction3のSVSTDTCはreinduction2自身のSVSTDTC以降であることが期待される(ref('reinduction2', ...)。
# evaluationtp2基準ではない)。VISITNUMは2100のため新しいsuffix "_14"を使う
tmp_sv_2 <- sv %>% filter(SVSPID == "reinduction2") %>% select(USUBJID, reinduction2_svstdtc=SVSTDTC)
tmp_sv <- sv %>% filter(SVSPID == "reinduction3") %>% inner_join(tmp_sv_2, by="USUBJID")
tmp_sv %>% check_date_after_var_before_today("SVSTDTC", "reinduction2_svstdtc", domain_name = "SV")
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")
suffix <- "_14"
tmp_sv <- sv %>% filter(SVSPID == "reinduction3")
tmp_sv <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv, "SV", .x, fixed_value_checks_csv_path))
"SVSTDTC" %>% check_required_vars(tmp_sv, ., domain_name = "SV")

# maitenanceのSV(維持療法期間)。SVSTDTC自体には明示的な下限参照は無いが、SVENDTCはSVSTDTC以降で
# あることが期待される(ref('maitenance', ...)、同一label内の自己参照)。VISITNUMは2300のため
# 新しいsuffix "_15"を使う
tmp_sv <- sv %>% filter(SVSPID == "maitenance")
c("SVSTDTC", "SVENDTC") %>% check_required_vars(tmp_sv, ., domain_name = "SV")
tmp_sv %>% check_date_after_var_before_today("SVENDTC", "SVSTDTC", domain_name = "SV")
suffix <- "_15"
tmp_sv_x <- tmp_sv %>% rename_with(~ str_c(.x, suffix), all_of(sv_target_cols))
str_c(sv_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_sv_x, "SV", .x, fixed_value_checks_csv_path))

# 割り付け(allocation)確認
# allocation1(PCR-MRD(TP2)結果返却状況報告1)でY(TP2の結果返却あり)が選択された被験者は、
# sheet_groups(TP2-Returned)によりpcrmrdtp2シートが有効化されるため、全員が実際にpcrmrdtp2の
# LBレコードを持つはずである。allocation1の割り付けコード自体はCDISC変数にマッピングされていない
# (allocation1の実体フィールドはNote/Headingのみ)が、defaultグループに属する唯一の割り付けシートの
# ためDM.ARMにそのままコードが記録される(build_dm_domain.Rのbuild_subject_active_sheets参照)。
# 逆方向(Y以外の被験者にpcrmrdtp2レコードが無いこと)は確認しない。TP2結果が未返却(L/S/I/H)の
# 被験者も、allocation10(暫定リスク後のTP2結果再確認シート)で改めて"A"(結果あり)を選ぶと
# 同じくpcrmrdtp2が有効化されるため、Y以外でも正当にpcrmrdtp2を持ちうる
usubjid_arm_y <- dm %>% filter(ARM == "Y") %>% pull(USUBJID) %>% unique()
usubjid_pcrmrdtp2 <- lb %>% filter(str_detect(LBSPID, "^pcrmrdtp2")) %>% pull(USUBJID) %>% unique()
missing_pcrmrdtp2 <- setdiff(usubjid_arm_y, usubjid_pcrmrdtp2)
if (length(missing_pcrmrdtp2) > 0) {
  stop(str_c(
    "割り付け確認: allocation1=Y(", length(usubjid_arm_y), "名)のうちpcrmrdtp2レコードが無い被験者がいます: ",
    paste(missing_pcrmrdtp2, collapse = ", ")
  ))
}
cat("割り付け確認: allocation1=Y(", length(usubjid_arm_y), "名)は全員pcrmrdtp2レコードあり: OK\n", sep = "")

# allocation1でL(TP2結果返却なし、暫定リスクLR)が選択された被験者は、sheet_groups
# (TP2-NotReturned-LR)によりallocation4(低リスク自動割付)が有効化される。allocation4自体は
# CDISC変数を持たないが、その割り付け結果であるarm-JACLS-02SR/arm-B12SRのどちらかに必ず
# 割り付けられ(build_subject_active_sheetsはallocation4到達時に必ずcodeを1つsampleする)、
# それぞれの下流シート(erwasp_jacls02sr/erwasp_lrsrir、いずれもEC)が有効化されるため、
# 全員がそのどちらかのECレコードを持つはずである
usubjid_arm_l <- dm %>% filter(ARM == "L") %>% pull(USUBJID) %>% unique()
usubjid_lr_arm <- ec %>% filter(str_detect(ECSPID, "^erwasp_jacls02sr") | str_detect(ECSPID, "^erwasp_lrsrir")) %>%
  pull(USUBJID) %>% unique()
missing_lr_arm <- setdiff(usubjid_arm_l, usubjid_lr_arm)
if (length(missing_lr_arm) > 0) {
  stop(str_c(
    "割り付け確認: allocation1=L(", length(usubjid_arm_l), "名)のうちerwasp_jacls02sr/erwasp_lrsrirレコードが無い被験者がいます: ",
    paste(missing_lr_arm, collapse = ", ")
  ))
}
cat("割り付け確認: allocation1=L(", length(usubjid_arm_l), "名)は全員erwasp_jacls02sr/erwasp_lrsrirいずれかのレコードあり: OK\n", sep = "")

# allocation1でH(TP2結果返却なし、暫定リスクHR)が選択された被験者は、sheet_groups
# (TP2-NotReturned-HR)によりallocation3(高リスク対象アーム報告)が有効化される。allocation3の
# 3つの割り付け結果(R→allocation5→arm-Block/arm-BLIN、HS、CD=arm-Blockと同一シート)は
# いずれも下流にhr1fisrt(SV等)を含むため、hr1fisrtはallocation3のどの結果になっても
# 共通して現れる、経路によらない指標として使える
usubjid_arm_h <- dm %>% filter(ARM == "H") %>% pull(USUBJID) %>% unique()
usubjid_hr1fisrt <- sv %>% filter(str_detect(SVSPID, "^hr1fisrt")) %>% pull(USUBJID) %>% unique()
missing_hr1fisrt <- setdiff(usubjid_arm_h, usubjid_hr1fisrt)
if (length(missing_hr1fisrt) > 0) {
  stop(str_c(
    "割り付け確認: allocation1=H(", length(usubjid_arm_h), "名)のうちhr1fisrtレコードが無い被験者がいます: ",
    paste(missing_hr1fisrt, collapse = ", ")
  ))
}
cat("割り付け確認: allocation1=H(", length(usubjid_arm_h), "名)は全員hr1fisrtレコードあり: OK\n", sep = "")

# allocation1のDC(試験治療中止)は検証しない。discon(DS)シートはdefault(全員共通)グループに
# 属しており、ARMに関わらず全被験者がdiscon由来のDSレコード(DSTERM込み)を持つため、
# 「DCならdiscon(DS)レコードがある」というチェックはDCかどうかに関係なく常にPASSしてしまい、
# 意味を成さない(実際に検証した結果、DSTERM=="COMPLETED"(正常終了)の割合もARM=DCと他のARMで
# 有意差が無く、生成ロジック上ARMのコードとDS側の中止理由は無関係な別々の乱数で決まっている)。
# これはallocation2の暫定/確定リスク不一致と同じ根本原因(割り付けグループのif条件を評価しない)
# によるもので、修正しない方針のため、DCについても検証を行わない

# allocation1のS(暫定リスクSR)・I(暫定リスクIR)は、sheet_groups上はallocation10
# (TP2結果の再確認シート、CDISC変数マッピング無し)しか有効化しないため、確認可能な直接の
# 後続シートが無い(L/HのようにS/I自身から直接分岐するarm固有シートは存在しない)。allocation10で
# 後から"A"(結果返却あり)が選ばれれば他のARMと同じくpcrmrdtp2やその先の各リスク群シートに
# 進みうるが、この分岐はSDTM出力からは観測できないため、S/Iについては検証を行わない


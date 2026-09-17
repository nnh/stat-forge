library(here)

# check_cm_baseline1/check_tr_tu_dtc等の特別チェック・run_full_validationの呼び出しを含め、
# 比較元(生成データ)にWebツールが生成したCSV(dummy_data.zip展開後)を使う。
#
# 事前準備: dm_web_csv_path・ae_web_csv_path・ds_web_csv_path・other_domains_web_csv_dir
# (test_config.R)を、Webツールでfortest2用JSONを読み込んで生成し「ZIPで一括ダウンロード」した
# dummy_data.zipの展開先に設定しておくこと。json_pathは下記でfortest2用に固定しているため、
# test_config.R側の値(他テストと切り替えて使われる)を書き換える必要はない
rm(list = ls())

# このファイル固定のjson_path。test_config.R側のjson_pathは他テストとの切り替えで
# 意図せず別のJSONを指したままになりうる(実際に誤検知の原因になったため)、ここで固定する
json_path <- "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test2\\json\\fortest2_260826_1501.json"

# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、test_config.R(共通)ではなくここで指定する。リポジトリ外の任意の場所でよい
fixed_value_checks_csv_path <- "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test2\\fixed_value_checks_test2.csv"

source(here("test_config.R"))
# test_config.Rはjson_path(他テストとの切り替え用)も定義するが、このファイルは上で固定した
# json_pathを優先して使うため、test_config.R側の値で上書きしないよう再度設定し直す
json_path <- "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test2\\json\\fortest2_260826_1501.json"
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

# ここから下の共通処理(CM/TR特別チェック・run_full_validation呼び出し・ドメイン名一覧の確認)は
# validate_test2_shared.Rにまとめてある
source(here("tools/validate_test2_shared.R"))

# DMのSITEIDが、facilities_dummy.csv(施設一覧)のcode列に含まれる値であることを確認する
dm %>% check_values_subset_of("SITEID", facilities[["code"]], domain_name = "DM/facilities")

names(other_domains) <- tolower(names(other_domains))

# グローバル環境に一括展開
list2env(other_domains, envir = .GlobalEnv)

# test2個別チェック

# AE
target_ae_cols <- c("AELLT", "AELLTCD", "AEDECOD", "AEPTCD", "AEHLT", "AEHLTCD", "AEHLGT", "AEHLGTCD", "AEBODSYS", "AEBDSYCD", "AESOC", "AESOCCD")
ae %>% check_required_vars(target_ae_cols, domain_name = "AE")
target_ae_cols <- c("AETERM", "AESER", "AETOXGR", "AESTDTC", "AEACN", "AEREL", "AEOUT", "AEENDTC")
ae %>% check_required_vars(target_ae_cols, domain_name = "AE")
ae %>% check_date_before_today("AESTDTC", domain_name = "AE")
ae %>% check_date_after_var_before_today("AEENDTC", "AESTDTC", domain_name = "AE")
c("AESER", "AETOXGR", "AEACN", "AEREL", "AEOUT") %>% walk(~ run_value_equals_checks_from_csv(ae, "AE", .x, fixed_value_checks_csv_path))
target_ae_cols <- c("AESDTH", "AESLIFE", "AESHOSP", "AESDISAB", "AESCONG", "AESMIE")
tmp_ae <- ae %>% filter(AESER == "Y")
target_ae_cols %>% walk(~ run_value_equals_checks_from_csv(tmp_ae, "AE", .x, fixed_value_checks_csv_path))
tmp_ae %>% check_required_vars(target_ae_cols, domain_name = "AE")
tmp_ae <- ae %>% filter(AESER != "Y")
tmp_ae %>% check_blank_vars(target_ae_cols, domain_name = "AE")

# CE
target_ce_cols <- c("CETERM", "CEPRESP", "CEOCCUR")
ce %>% check_required_vars(target_ce_cols, domain_name = "CE")
target_ce_cols %>% walk(~ run_value_equals_checks_from_csv(ce, "CE", .x, fixed_value_checks_csv_path))
tmp_ce <- ce %>% filter(CEOCCUR == "Y")
tmp_ce %>% check_required_vars("CEDTC", domain_name = "CE")
tmp_ce %>% check_date_before_today("CEDTC", domain_name = "CE")
tmp_ce <- ce %>% filter(CEOCCUR != "Y")
tmp_ce %>% check_blank_vars("CEDTC", domain_name = "CE")

# CM
cm %>% check_date_before_today("CMSTDTC", domain_name = "CM")
cm %>% check_date_after_var_before_today("CMENDTC","CMSTDTC", domain_name = "CM")
tmp_cm <- cm %>% filter(CMENTPT == "BASELINE")
tmp_cm %>% check_required_vars(c("CMOCCUR"), domain_name = "CM")
target_cm_cols <- c("CMTRT", "CMCAT", "CMPRESP", "CMOCCUR", "CMENRTPT")
suffix <- "_1"
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))
tmp_cm %>% filter(CMOCCUR_1 == "Y") %>% check_required_vars("CMENDTC")
tmp_cm %>% filter(CMOCCUR_1 != "Y") %>% check_blank_vars("CMENDTC")

tmp_cm <- cm %>% filter(str_detect(CMSPID, "concomitant_drug_other"))
tmp_cm %>% check_required_vars(c("CMTRT", "CMSTDTC", "CMENDTC", "CMCAT"), domain_name = "CM")
suffix <- "_2"
target_cm_cols <- c("CMTRT", "CMCAT")
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))

tmp_cm <- cm %>% filter(!str_detect(CMSPID, "concomitant_drug_other")) %>% filter(CMCAT == "CONCOMITANT DRUG")
tmp_cm %>% check_required_vars(c("CMTRT", "CMSTDTC", "CMCAT"), domain_name = "CM")
tmp_cm %>% filter(CMENRF == "") %>% check_required_vars(c("CMENDTC"), domain_name = "CM")
tmp_cm %>% filter(CMENRF != "") %>% check_blank_vars(c("CMENDTC"), domain_name = "CM")
suffix <- "_3"
target_cm_cols <- c("CMCAT", "CMENRF")
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c("CMCAT", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))
str_c("CMENRF", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_cm %>% filter(CMENDTC == ""), "CM", .x, fixed_value_checks_csv_path))

# DM
dm %>% check_date_before_today("BRTHDTC", domain_name = "DM")
dm %>% check_date_after_var_before_today("RFICDTC", "BRTHDTC", domain_name = "DM")
dm %>% check_required_vars(c("RFICDTC", "BRTHDTC", "SEX", "RACE", "ETHNIC", "COUNTRY"), domain_name = "DM")
c("SEX", "RACE", "ETHNIC", "COUNTRY") %>% walk(~ run_value_equals_checks_from_csv(dm, "DM", .x, fixed_value_checks_csv_path))

# DS
ds %>% check_date_before_today("DSSTDTC", domain_name = "DS")
ds %>% check_date_after_var_before_today("DSDTC", "DSSTDTC", domain_name = "DS")
ds %>% check_required_vars(c("DSTERM"), domain_name = "DS")
target_ds_cols <- c("DSTERM", "DSCAT")
suffix <- "_0"
tmp_ds <- ds %>% filter(DSSPID == "allocation")
tmp_ds %>% check_blank_vars("EPOCH", domain_name = "DS")
tmp_ds <- tmp_ds %>% rename_with(~ str_c(.x, suffix), all_of(target_ds_cols))
str_c(target_ds_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ds, "DS", .x, fixed_value_checks_csv_path))
suffix <- "_1"
tmp_ds <- ds %>% filter(EPOCH == "TREATMENT" & DSSPID == "discon")
tmp_ds <- tmp_ds %>% rename_with(~ str_c(.x, suffix), all_of(target_ds_cols))
str_c(target_ds_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ds, "DS", .x, fixed_value_checks_csv_path))
tmp_ds %>% check_required_vars(c("DSDTC", "DSSTDTC"), domain_name = "DS")
suffix <- "_2"
tmp_ds <- ds %>% filter(EPOCH == "TREATMENT" & DSSPID == "discon_ind")
tmp_ds <- tmp_ds %>% rename_with(~ str_c(.x, suffix), all_of(target_ds_cols))
str_c(target_ds_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ds, "DS", .x, fixed_value_checks_csv_path))
tmp_ds %>% check_required_vars(c("DSDTC", "DSSTDTC"), domain_name = "DS")
suffix <- "_3"
tmp_ds <- ds %>% filter(EPOCH == "FOLLOW-UP" & DSSPID == "withdrawal")
tmp_ds <- tmp_ds %>% rename_with(~ str_c(.x, suffix), all_of(target_ds_cols))
str_c(target_ds_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ds, "DS", .x, fixed_value_checks_csv_path))
tmp_ds %>% check_required_vars(c("DSDTC", "DSSTDTC", "EPOCH"), domain_name = "DS")

# EC
ec %>% check_date_before_today("ECSTDTC", domain_name = "EC")
ec %>% check_date_after_var_before_today("ECENDTC", "ECSTDTC", domain_name = "EC")
ec %>% filter(ECOCCUR == "Y") %>% check_required_vars("ECSTDTC", domain_name = "EC")
ec %>% filter(ECOCCUR == "Y" & ECTRT == "5-FU" & ECROUTE == "INTRAVENOUS DRIP") %>% check_required_vars("ECENDTC", domain_name = "EC")
ec %>% check_required_vars(c("ECOCCUR"), domain_name = "EC")
c("ECOCCUR", "ECMOOD", "ECDOSFRM", "ECPRESP") %>% walk(~ run_value_equals_checks_from_csv(ec, "EC", .x, fixed_value_checks_csv_path))
run_ec_trt_checks_all_cycles(ec, fixed_value_checks_csv_path)

# EG
eg %>% check_date_before_today("EGDTC", domain_name = "EG")
eg_done <- eg %>% filter(EGSTAT != "NOT DONE")
eg_not_done <- eg %>% filter(EGSTAT == "NOT DONE")
c("EGTESTCD", "EGTEST", "EGORRES", "EGBLFL", "VISITNUM") %>% walk(~ run_value_equals_checks_from_csv(eg_done, "EG", .x, fixed_value_checks_csv_path))

# IE
ie %>% check_date_before_today("IEDTC", domain_name = "IE")
ie %>% run_ie_testcd_checks("IN01", "_1", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("IN02", "_2", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("IN03", "_3", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("IN04", "_4", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("IN05", "_5", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("IN06", "_6", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("IN07", "_7", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("IN08", "_8", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX01", "_9", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX02", "_10", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX03", "_11", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX04", "_12", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX05", "_13", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX5a", "_14", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX06", "_15", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX07", "_16", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX08", "_17", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX09", "_18", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX10", "_19", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX11", "_20", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX12", "_21", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX12a", "_22", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX13", "_23", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX14", "_24", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX14a", "_25", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX15", "_26", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX15a", "_27", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX16", "_28", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX17", "_29", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX18", "_30", fixed_value_checks_csv_path)

trlnkgrp_by_visitnum <- tibble::tribble(
  ~VISITNUM, ~TRLNKGRP,
  "100",     "A1",
  "600",     "A2",
  "1100",    "A3",
  "1600",    "A4",
  "2100",    "A5",
  "2600",    "A6",
  "3100",    "A7",
  "3600",    "A8",
  "4100",    "A9",
  "4600",    "A10",
  "5100",    "A11",
  "5600",    "A12",
  "6100",    "A13",
  "6600",    "A14",
  "7100",    "A15",
  "7600",    "A16",
  "8100",    "A17",
  "8600",    "A18"
)


# LB
tmp_lb <- lb %>% filter(LBTESTCD != "HCG")
tmp_lb %>% check_required_vars(c("LBTEST", "LBCAT", "LBDTC", "VISITNUM"), domain_name = "LB")
lb %>% check_date_before_today(c("LBDTC"), domain_name = "LB")
lb_done <- lb %>% filter(LBSTAT != "NOT DONE")
lb_not_done <- lb %>% filter(LBSTAT == "NOT DONE")
lb_done %>% check_required_vars(c("LBORRES", "LBSPEC"), domain_name="LB")
lb_not_done %>% check_blank_vars(c("LBORRES"), domain_name="LB")
# 腫瘍マーカー
target_lb_cols <- c("LBTEST", "LBCAT", "LBSPEC")
for (i in 1:nrow(trlnkgrp_by_visitnum)) {
  visitnum <- trlnkgrp_by_visitnum[i, "VISITNUM"] %>% as.numeric()
  has_blfl <- visitnum == 100
  check_lb_testcd(lb_done, "CEA", visitnum, "_1", fixed_value_checks_csv_path, has_blfl = has_blfl)
  check_lb_testcd(lb_done, "CA19_9AG", visitnum, "_2", fixed_value_checks_csv_path, has_blfl = has_blfl)
}

# 妊娠検査
suffix <- "_3"
tmp_lb <- lb_done %>% filter(LBTESTCD == "HCG")
check_sex <- tmp_lb %>% inner_join(dm, by="USUBJID") %>% select("SEX") %>% unique() %>% unlist()
if (check_sex != "F") {
  stop("性別エラー：HCG")
}
tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(c(target_lb_cols, "LBBLFL", "VISITNUM")))
str_c(c(target_lb_cols, "LBBLFL", "VISITNUM"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))
tmp_dm <- dm %>% filter(SEX == "F")
check_sex <- tmp_dm %>% left_join(filter(lb, LBTESTCD=="HCG"), by="USUBJID") %>% select(SEX, LBSTAT, LBDTC, LBORRES)
check_sex %>% filter(LBSTAT != "NOT DONE") %>% check_required_vars(c("LBDTC", "LBORRES"))
check_sex %>% filter(LBSTAT == "NOT DONE") %>% check_blank_vars(c("LBDTC", "LBORRES"))
target_visitnum <- 100
check_lb_testcd(lb_done, "RBC", target_visitnum, "_4", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "HGB", target_visitnum, "_5", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "HCT", target_visitnum, "_6", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "WBC", target_visitnum, "_7", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "NEUT", target_visitnum, "_8", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "PLAT", target_visitnum, "_9", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "ALB", target_visitnum, "_10", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "SODIUM", target_visitnum, "_11", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "K", target_visitnum, "_12", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "CL", target_visitnum, "_13", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "CA", target_visitnum, "_14", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "PHOS", target_visitnum, "_15", fixed_value_checks_csv_path)
check_lb_testcd(filter(lb_done, LBCAT=="CHEMISTRY"), "GLUC", target_visitnum, "_16", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "UREAN", target_visitnum, "_17", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "CYURIAC", target_visitnum, "_18", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "CREAT", target_visitnum, "_19", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "BILI", target_visitnum, "_20", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "AST", target_visitnum, "_21", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "ALT", target_visitnum, "_22", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "ALP", target_visitnum, "_23", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "LDH", target_visitnum, "_24", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "HBA1C", target_visitnum, "_25", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "CHOL", target_visitnum, "_26", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "TRIG", target_visitnum, "_27", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "INR", target_visitnum, "_28", fixed_value_checks_csv_path, has_unit = FALSE)
check_lb_testcd(lb_done, "APTT", target_visitnum, "_29", fixed_value_checks_csv_path)
check_lb_testcd(lb_done, "HBSAG", target_visitnum, "_30", fixed_value_checks_csv_path, has_unit = FALSE)
check_lb_testcd(lb_done, "HBSAB", target_visitnum, "_31", fixed_value_checks_csv_path, has_unit = FALSE)
check_lb_testcd(lb_done, "HBCAB", target_visitnum, "_32", fixed_value_checks_csv_path, has_unit = FALSE)
check_lb_testcd(lb_done, "HCAB", target_visitnum, "_33", fixed_value_checks_csv_path, has_unit = FALSE)
check_lb_testcd(filter(lb_done,LBCAT=="URINALYSIS"), "PROT", target_visitnum, "_34", fixed_value_checks_csv_path, has_unit = FALSE)
check_lb_testcd(filter(lb_done,LBCAT=="URINALYSIS"), "GLUC", target_visitnum, "_35", fixed_value_checks_csv_path, has_unit = FALSE)
check_lb_testcd(filter(lb_done,LBCAT=="URINALYSIS"), "OCCBLD", target_visitnum, "_36", fixed_value_checks_csv_path, has_unit = FALSE)
run_lb_testcd_checks_all_cycles(lb_done, fixed_value_checks_csv_path)

# MH
mh %>% check_required_vars(c("MHTERM"), domain_name = "MH")
tmp_mh <- mh %>% filter(MHCAT == "GENERAL" & MHENRTPT == "BEFORE")
tmp_mh <- tmp_mh %>% rename_with(~ str_c(.x, "_1"), c(MHENTPT))
c("MHENTPT_1") %>% walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path))
tmp_mh <- mh %>% filter(MHCAT == "GENERAL" & MHENRTPT == "ONGOING" & MHENTPT == 200)
tmp_mh %>% check_not_empty("MHCAT/MHENRTPT/MHENTPT該当行", domain_name = "MH")
tmp_mh <- mh %>% filter(MHCAT == "PRIMARY DIAGNOSIS")
tmp_mh <- tmp_mh %>% rename_with(~ str_c(.x, "_2"), c(MHTERM, MHPRESP, MHOCCUR, MHLOC, MHENRTPT, MHENTPT))
c("MHTERM_2", "MHPRESP_2", "MHOCCUR_2", "MHENRTPT_2", "MHENTPT_2") %>% walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path))
run_value_equals_checks_from_csv(filter(tmp_mh, MHLOC_2 != ""), "MH", "MHLOC_2", fixed_value_checks_csv_path)# MHLOCは必須ではない
tmp_mh <- mh %>% filter(MHCAT == "GENERAL" & MHENRTPT == "ONGOING" & MHENTPT == 100)
tmp_mh %>% check_required_vars(c("MHOCCUR"), domain_name = "MH")
tmp_mh <- tmp_mh %>% rename_with(~ str_c(.x, "_3"), c(MHTERM, MHPRESP, MHOCCUR))
c("MHTERM_3", "MHPRESP_3", "MHOCCUR_3") %>% walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path))

# MI
mi %>% check_required_vars(c("MIORRES", "MIDTC"), domain_name = "MI")
mi %>% check_date_before_today("MIDTC", domain_name = "MI")
c("MITESTCD", "MITEST", "MICAT", "MIORRES", "MIBLFL", "VISITNUM") %>% walk(~ run_value_equals_checks_from_csv(mi, "MI", .x, fixed_value_checks_csv_path))

# PR
target_pr_cols <- c("PRTRT", "PRPRESP", "PROCCUR")
pr %>% check_required_vars(target_pr_cols, domain_name = "PR")
target_pr_cols %>% walk(~ run_value_equals_checks_from_csv(pr, "PR", .x, fixed_value_checks_csv_path))
tmp_pr <- pr %>% filter(PROCCUR == "Y")
tmp_pr %>% check_required_vars("PRSTDTC", domain_name = "PR")
tmp_pr %>% check_date_before_today("PRSTDTC", domain_name = "PR")
tmp_pr <- pr %>% filter(PROCCUR != "Y")
tmp_pr %>% check_blank_vars("PRSTDTC", domain_name = "PR")

# QS
qs %>% check_required_vars(c("QSDTC"), domain_name = "QS")
qs %>% check_date_before_today("QSDTC", domain_name = "QS")
c("QSTESTCD", "QSTEST", "QSCAT", "QSORRES", "QSBLFL", "VISITNUM") %>% walk(~ run_value_equals_checks_from_csv(qs, "QS", .x, fixed_value_checks_csv_path))

# RS
rs %>% check_required_vars(c("RSORRES", "RSDTC"), domain_name = "RS")
rs %>% check_date_before_today("RSDTC", domain_name = "RS")
# RS: RSLNKGRPは腫瘍評価訪問(VISITNUM)ごとに固定の値を持つ(evaluation8=600→A2、
# evaluation16=1100→A3、...というように、JSON上でVISITNUMとRSLNKGRPが1対1に対応している)。
# その対応関係が生成データでも崩れていないかを確認する
rslnkgrp_by_visitnum <- tibble::tribble(
  ~VISITNUM, ~RSLNKGRP,
  "600",     "A2",
  "1100",    "A3",
  "1600",    "A4",
  "2100",    "A5",
  "2600",    "A6",
  "3100",    "A7",
  "3600",    "A8",
  "4100",    "A9",
  "4600",    "A10",
  "5100",    "A11",
  "5600",    "A12",
  "6100",    "A13",
  "6600",    "A14",
  "7100",    "A15",
  "7600",    "A16",
  "8100",    "A17",
  "8600",    "A18"
)

# rs側の実際の(VISITNUM, RSLNKGRP)の組み合わせを、上記の期待値と突き合わせる。
# 一致しない行があればstop()でエラーにする
rs_lnkgrp_check <- rs %>%
  distinct(VISITNUM, RSLNKGRP) %>%
  filter(VISITNUM %in% rslnkgrp_by_visitnum$VISITNUM) %>%
  left_join(rslnkgrp_by_visitnum, by = "VISITNUM", suffix = c("_actual", "_expected"))
rs_lnkgrp_mismatch <- rs_lnkgrp_check %>% filter(RSLNKGRP_actual != RSLNKGRP_expected)
if (nrow(rs_lnkgrp_mismatch) > 0) {
  stop(str_c(
    "RS: VISITNUM×RSLNKGRP対応チェック: ", nrow(rs_lnkgrp_mismatch), "件NG(",
    paste(str_c("VISITNUM=", rs_lnkgrp_mismatch$VISITNUM, "(実際:", rs_lnkgrp_mismatch$RSLNKGRP_actual, "/期待値:", rs_lnkgrp_mismatch$RSLNKGRP_expected, ")"), collapse = ", "),
    ")"
  ))
}
cat("RS: VISITNUM×RSLNKGRP対応チェック: OK(", nrow(rs_lnkgrp_check), "件)\n", sep = "")

target_rs_cols <-c("RSTEST", "RSCAT", "RSORRES", "RSEVAL")
tmp_rs <- rs %>% filter(RSTESTCD =="STAGE" & VISITNUM == 100)
suffix <- "_1"
tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, suffix), all_of(c(target_rs_cols, "RSBLFL")))
str_c(c(target_rs_cols, "RSBLFL"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))
for (i in 1:nrow(rslnkgrp_by_visitnum)) {
  visitnum <- rslnkgrp_by_visitnum[i, "VISITNUM"] %>% as.numeric()
  print(str_c("check RS, VISITNUM: ", visitnum))
  run_rs_testcd_checks(rs, visitnum, fixed_value_checks_csv_path)
}

# TR: TRLNKGRPも腫瘍評価訪問(VISITNUM)ごとに固定の値を持つ(RSと同じ対応関係に、baseline1の
# VISITNUM=100→A1が加わったもの)。その対応関係が生成データでも崩れていないかを確認する

# tr側の実際の(VISITNUM, TRLNKGRP)の組み合わせを、上記の期待値と突き合わせる。
# 一致しない行があればstop()でエラーにする
tr_lnkgrp_check <- tr %>%
  distinct(VISITNUM, TRLNKGRP) %>%
  filter(VISITNUM %in% trlnkgrp_by_visitnum$VISITNUM) %>%
  left_join(trlnkgrp_by_visitnum, by = "VISITNUM", suffix = c("_actual", "_expected"))
tr_lnkgrp_mismatch <- tr_lnkgrp_check %>% filter(TRLNKGRP_actual != TRLNKGRP_expected)
if (nrow(tr_lnkgrp_mismatch) > 0) {
  stop(str_c(
    "TR: VISITNUM×TRLNKGRP対応チェック: ", nrow(tr_lnkgrp_mismatch), "件NG(",
    paste(str_c("VISITNUM=", tr_lnkgrp_mismatch$VISITNUM, "(実際:", tr_lnkgrp_mismatch$TRLNKGRP_actual, "/期待値:", tr_lnkgrp_mismatch$TRLNKGRP_expected, ")"), collapse = ", "),
    ")"
  ))
}
cat("TR: VISITNUM×TRLNKGRP対応チェック: OK(", nrow(tr_lnkgrp_check), "件)\n", sep = "")

# TR: TRLNKID×TRLNKGRPごとの個別チェック。TRLNKGRP/TRORRESU/VISITNUMをsuffix付き列名にリネームして
# 必須・固定値チェックを行ったうえで、TRTESTCD=="LDIAM"(長径)/"SAXIS"(短径)それぞれのTRTESTを
# 別々のsuffix付き列名にリネームして固定値チェックする
check_tr_lnkid <- function(tr, suffix, ldiam_suffix, saxis_suffix, fixed_value_checks_csv_path) {
  visitnum <- tr$VISITNUM %>% unlist() %>% unique()
  trlnkid <- tr$TRLNKID %>% unlist() %>% unique()
  print(str_c("check TRTEST VISITNUM:", visitnum, ", TRLNKID:", trlnkid))
  trlnkgrp <- tr$TRLNKGRP %>% unlist() %>% unique()
  target_tr_cols <- c("TRORRESU")
  tmp_tr <- tr %>% filter(TRLNKID == trlnkid & TRLNKGRP == trlnkgrp)
  tmp_tr <- tmp_tr %>% rename_with(~ str_c(.x, suffix), all_of(target_tr_cols))
  tmp_tr %>% check_required_vars(str_c(target_tr_cols, suffix), domain_name = "TR")
  str_c(target_tr_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_tr, "TR", .x, fixed_value_checks_csv_path))

  tmp_tr_l <- tmp_tr %>% filter(TRTESTCD == "LDIAM") %>% rename(!!str_c("TRTEST", ldiam_suffix) := TRTEST)
  str_c("TRTEST", ldiam_suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_tr_l, "TR", .x, fixed_value_checks_csv_path))

  tmp_tr_s <- tmp_tr %>% filter(TRTESTCD == "SAXIS") %>% rename(!!str_c("TRTEST", saxis_suffix) := TRTEST)
  str_c("TRTEST", saxis_suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_tr_s, "TR", .x, fixed_value_checks_csv_path))
}
for (i in 1:nrow(trlnkgrp_by_visitnum)) {
  visitnum <- trlnkgrp_by_visitnum[i, "VISITNUM"] %>% as.numeric()
  for (trlnkid in c("T01", "T02", "T03", "T04", "T05")) {
    trlnkgrp <- trlnkgrp_by_visitnum %>% filter(VISITNUM == visitnum) %>% select(TRLNKGRP) %>% unlist()
      tmp_tr <- tr %>% filter(VISITNUM == visitnum & TRLNKID == trlnkid & TRLNKGRP == trlnkgrp)
      check_tr_lnkid(tmp_tr, "_1", "_2", "_3", fixed_value_checks_csv_path)
  }
}

# TU: TULNKIDごとの個別チェック。1番目の病変記録(is_first=TRUE)は位置/左右/測定方法/実測値/
# ベースラインフラグ/訪問番号が全て必須かつ固定値と一致することを確認する。2番目以降(is_first=FALSE)は
# 位置/左右/測定方法が空欄になりうるため値が入っている行のみ固定値チェックし、実測値/ベースライン
# フラグ/訪問番号は引き続き全行チェックする
check_tu_lnkid <- function(tu, tulnkid, suffix, fixed_value_checks_csv_path, is_first) {
  target_tu_cols <- c("TULOC", "TULAT", "TUMETHOD", "TUORRES", "TUBLFL", "VISITNUM")
  tmp_tu <- tu %>% filter(TULNKID == tulnkid)
  tmp_tu <- tmp_tu %>% rename_with(~ str_c(.x, suffix), all_of(target_tu_cols))

  if (is_first) {
    tmp_tu %>% check_required_vars(str_c(target_tu_cols, suffix), domain_name = "TU")
    str_c(target_tu_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_tu, "TU", .x, fixed_value_checks_csv_path))
  } else {
    c("TULOC", "TULAT", "TUMETHOD") %>% walk(~ {
      col <- str_c(.x, suffix)
      run_value_equals_checks_from_csv(filter(tmp_tu, .data[[col]] != ""), "TU", col, fixed_value_checks_csv_path)
    })
    str_c(c("TUORRES", "TUBLFL", "VISITNUM"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_tu, "TU", .x, fixed_value_checks_csv_path))
  }
}
check_tu_lnkid(tu, "T01", "_1", fixed_value_checks_csv_path, is_first = TRUE)
check_tu_lnkid(tu, "T02", "_2", fixed_value_checks_csv_path, is_first = FALSE)
check_tu_lnkid(tu, "T03", "_2", fixed_value_checks_csv_path, is_first = FALSE)
check_tu_lnkid(tu, "T04", "_2", fixed_value_checks_csv_path, is_first = FALSE)
check_tu_lnkid(tu, "T05", "_2", fixed_value_checks_csv_path, is_first = FALSE)
# TR/TU: 同一TULNKID(病変ID)について、TR短径(SAXIS)と長径(LDIAM)の記録が
# 同一USUBJID・TRMETHOD・TRDTCで対になっていること、およびTR(LDIAM)とTU(病変同定)が
# 同一USUBJID・METHOD・DTCで紐づいていることを確認する
# T01-T05を対象とする
check_tr_tu_link <- function(tu, tr, tulnkid, trlnkgrp = "A1") {
  tmp_tu <- tu %>% filter(TULNKID == tulnkid)
  tmp_tr_l <- tr %>% filter(TRLNKID == tulnkid & TRLNKGRP == trlnkgrp & TRTESTCD == "LDIAM")
  tmp_tr_s <- tr %>% filter(TRLNKID == tulnkid & TRLNKGRP == trlnkgrp & TRTESTCD == "SAXIS")

  tr_link_1 <- tmp_tr_l %>% anti_join(tmp_tr_s, by = c("USUBJID", "TRMETHOD", "TRDTC"))
  tr_link_2 <- tmp_tr_s %>% anti_join(tmp_tr_l, by = c("USUBJID", "TRMETHOD", "TRDTC"))
  if (nrow(tr_link_1) > 0 | nrow(tr_link_2) > 0) {
    stop(str_c("TR短径と長径リンクエラー：", tulnkid))
  }

  tu_tr_link <- tmp_tr_l %>% anti_join(tmp_tu, by = c("USUBJID", "TRMETHOD" = "TUMETHOD", "TRDTC" = "TUDTC"))
  if (nrow(tu_tr_link) > 0) {
    stop(str_c("TRTUリンクエラー：", tulnkid))
  }
  cat("TR/TUリンクチェック: 問題なし(", tulnkid, ")\n", sep = "")
}
for (i in 1:5) {
  lnkid <- str_c("T0", as.character(i))
  is_first <- i == 1
  tu_target <- ifelse(i == 1, "_1", "_2")
  check_tu_lnkid(tu, lnkid, tu_target, fixed_value_checks_csv_path, is_first = is_first)
  check_tr_tu_link(tu, tr, lnkid)
}

# TR,TU non-target
tmp_tr <- tr %>% filter(TRLNKID == "NT01")
suffix <- "_4"
target_tr_cols <- c("TRGRPID", "TRTESTCD", "TRTEST", "TRORRES")
tmp_tr <- tmp_tr %>% rename_with(~ str_c(.x, suffix), all_of(target_tr_cols))
str_c(target_tr_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_tr, "TR", .x, fixed_value_checks_csv_path))
tmp_tu <- tu %>% filter(TULNKID == "NT01")
target_tu_cols <- c("TUTESTCD", "TUTEST", "TUORRES", "TUBLFL", "VISITNUM")
tmp_tu <- tmp_tu %>% rename_with(~ str_c(.x, suffix), all_of(target_tu_cols))
str_c(target_tu_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_tu, "TU", .x, fixed_value_checks_csv_path))
tmp_tu %>% check_required_vars(c("TUMETHOD", "TUDTC"), domain_name = "TU")
tmp_tu %>% check_date_before_today("TUDTC", domain_name = "TU")
tmp_tr_tu <- tmp_tu %>% anti_join(tmp_tr, by=c("USUBJID", "TUMETHOD"="TRMETHOD", "TUDTC"="TRDTC"))
if (nrow(tmp_tr_tu) > 0) {
  stop("TR, TU non-target error")
}

# TR
# 効果判定NEWは単独
target_tr_cols <- c("TRLNKID", "TRTESTCD", "TRTEST", "TRORRES", "TRMETHOD")
suffix <- "_6"
for (i in 2:nrow(trlnkgrp_by_visitnum)) {
  visitnum <- trlnkgrp_by_visitnum[i, "VISITNUM"] %>% as.numeric()
  print(visitnum)
  tmp_tr <- tr %>% filter(TRGRPID == "NEW" & TRSTAT != "NOT DONE" & VISITNUM == visitnum)
  tmp_tr %>% check_required_vars(c("TRORRES", "TRMETHOD"), domain_name = "TU")
  tmp_tr <- tmp_tr %>% rename_with(~ str_c(.x, suffix), all_of(target_tr_cols))
  str_c(target_tr_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_tr, "TR", .x, fixed_value_checks_csv_path))
}

# VS
vs %>% check_date_before_today(c("VSDTC"), domain_name = "VS")
vs_done <- vs %>% filter(VSSTAT != "NOT DONE")
vs_not_done <- vs %>% filter(VSSTAT == "NOT DONE")
vs_done %>% check_required_vars("VSORRES", domain_name = "VS")
vs_not_done %>% check_blank_vars("VSORRES", domain_name = "VS")
target_vs_cols <- c("VSTEST", "VSORRESU")
suffix <- "_1"
tmp_vs <- vs_done %>% filter(VSTESTCD=="HEIGHT")
tmp_vs <- tmp_vs %>% rename_with(~ str_c(.x, suffix), all_of(c(target_vs_cols, "VSBLFL", "VISITNUM")))
str_c(c(target_vs_cols, "VSBLFL", "VISITNUM"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_vs, "VS", .x, fixed_value_checks_csv_path))
target_visitnum <- 100
suffix <- "_2"
tmp_vs <- vs_done %>% filter(VSTESTCD=="WEIGHT")
tmp_vs <- tmp_vs %>% rename_with(~ str_c(.x, suffix), all_of(c(target_vs_cols, "VSBLFL")))
str_c(c(target_vs_cols, "VSBLFL"), suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_vs, "VS", .x, fixed_value_checks_csv_path, visit = target_visitnum))
target_visitnum <- 200
check_vs_testcd(vs_done, "TEMP", target_visitnum, "_3", fixed_value_checks_csv_path, vstptnum = 10)
check_vs_testcd(vs_done, "PULSE", target_visitnum, "_4", fixed_value_checks_csv_path, vstptnum = 10)
check_vs_testcd(vs_done, "SYSBP", target_visitnum, "_5", fixed_value_checks_csv_path, vstptnum = 10)
check_vs_testcd(vs_done, "DIABP", target_visitnum, "_6", fixed_value_checks_csv_path, vstptnum = 10)
check_vs_testcd(vs_done, "TEMP", target_visitnum, "_3", fixed_value_checks_csv_path, vstptnum = 20, has_blfl = FALSE)
check_vs_testcd(vs_done, "PULSE", target_visitnum, "_4", fixed_value_checks_csv_path, vstptnum = 20, has_blfl = FALSE)
check_vs_testcd(vs_done, "SYSBP", target_visitnum, "_5", fixed_value_checks_csv_path, vstptnum = 20, has_blfl = FALSE)
check_vs_testcd(vs_done, "DIABP", target_visitnum, "_6", fixed_value_checks_csv_path, vstptnum = 20, has_blfl = FALSE)
run_vs_testcd_checks_all_cycles(vs_done, fixed_value_checks_csv_path)


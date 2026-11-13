library(here)
rm(list = ls())
source(here("resolve_os_path.R"))

# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、ここで指定する。リポジトリ外の任意の場所でよい
fixed_value_checks_csv_path <- file.path(box_root, "Datacenter/Users/ohtsuka/2026/stat-forge-test/test6/fixed_value_checks_test6.csv")

# Webツールの「ZIPで一括ダウンロード」(dummy_data.zip)を、ダウンロードフォルダに展開したフォルダ
other_domains_web_csv_dir <- fs::path_home("Downloads", "dummy_data")
dm_web_csv_path <- file.path(other_domains_web_csv_dir, "DM_dummy.csv")
ae_web_csv_path <- file.path(other_domains_web_csv_dir, "AE_dummy.csv")
ds_web_csv_path <- file.path(other_domains_web_csv_dir, "DS_dummy.csv")

# このファイル固定のjson_path
json_path <- file.path(box_root, "Datacenter/Users/ohtsuka/2026/stat-forge-test/test6/json/fortest6_261028_1626.json")
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

# test6個別チェック
# ここにドメインごとのチェックを追加していく(参考: tools/validate_datasets_test1_web.R〜test5_web.R)
# data(domainドメイン)のtarget列(suffix付き)が、fixed_value_checks_csv_pathの期待値と一致することを確認する
run_target_value_checks <- function(data, domain) {
  str_c(target, suffix) %>% walk(~ run_value_equals_checks_from_csv(data, domain, .x, fixed_value_checks_csv_path))
}

# AE(ae1〜ae3)。重篤性・評価項目はsuffix無しでCSV登録している。重篤性サブ基準(AESDTH/AESLIFE/
# AESHOSP/AESDISAB/AESCONG/AESMIE)はAESER=="Y"の時のみ必須(それ以外は空欄)
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

# CM(screening_100のPRIOR TREATMENT・BEFORE・START OF TREATMENT)
tmp_cm_prior <- cm %>%
  filter(CMSPID == "screening_100" & CMCAT == "PRIOR TREATMENT" & CMENRTPT == "BEFORE" & CMENTPT == "START OF TREATMENT")
# 各被験者に0〜5件であること(DMの全被験者を対象にし、該当レコードが無い被験者は0件として数える)
tmp_cm_prior_count <- dm %>%
  select(USUBJID) %>%
  left_join(tmp_cm_prior %>% count(USUBJID, name = "record_count"), by = "USUBJID") %>%
  mutate(record_count = coalesce(record_count, 0L))
tmp_cm_prior_count %>% check_numeric_range("record_count", min_value = 0, max_value = 5, domain_name = "CM(PRIOR TREATMENT)")
# CMTRTが入っている行はCMENDTCが必須、CMTRTが空の行はCMENDTCも空欄
tmp_cm_prior %>% filter(CMTRT != "") %>% check_required_vars("CMENDTC", domain_name = "CM(PRIOR TREATMENT)")
tmp_cm_prior %>% filter(CMTRT == "") %>% check_blank_vars("CMENDTC", domain_name = "CM(PRIOR TREATMENT)")

# CM(pretrtシート(CMSPIDがpretrt+連番)、CMCATがPRIOR TREATMENT)
suffix <- "_1"
target <- c("CMENRTPT", "CMENTPT")
tmp_cm_1 <- cm %>% filter(str_detect(CMSPID, "^pretrt") & CMCAT == "PRIOR TREATMENT")
label <- "CM(pretrt/PRIOR TREATMENT)"
tmp_cm_1 %>% check_not_empty("レコード有無チェック", domain_name = label)
c("CMTRT", "CMENDTC") %>% check_required_vars(tmp_cm_1, ., domain_name = label)
tmp_cm_1 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("CM")

# CM(cmシート(CMSPIDがcm+連番)、CMCATがCONCOMITANT DRUG)
suffix <- "_2"
tmp_cm_2 <- cm %>% filter(str_detect(CMSPID, "^cm[0-9]") & CMCAT == "CONCOMITANT DRUG")
label <- "CM(cm/CONCOMITANT DRUG)"
tmp_cm_2 %>% check_not_empty("レコード有無チェック", domain_name = label)
c("CMTRT", "CMSTDTC") %>% check_required_vars(tmp_cm_2, ., domain_name = label)
tmp_cm_2 %>% check_date_before_today("CMSTDTC", domain_name = label)
# CMENRFが空の行はCMENDTCが必須、CMENRFが入っている行はCMENRFが選択肢のいずれかでCMENDTCが空欄
tmp_cm_2 %>%
  filter(CMENRF == "") %>%
  check_required_vars("CMENDTC", domain_name = label)
target <- "CMENRF"
tmp_cm_2 %>%
  filter(CMENRF != "") %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("CM")
tmp_cm_2 %>%
  filter(CMENRF != "") %>%
  check_blank_vars("CMENDTC", domain_name = str_c(label, "/CMENRFあり"))
tmp_cm_2 %>% check_date_after_var_before_today("CMENDTC", "CMSTDTC", domain_name = label)

# CM(posttrtシート(CMSPIDがposttrt+連番)、CMCATがAFTER TREATMENT)
# CMENDTCは必須ではなく、CMENRFとの関係はformula側の条件のみ(ジェネレータは使わない)のためチェックしない
suffix <- "_3"
tmp_cm_3 <- cm %>% filter(str_detect(CMSPID, "^posttrt") & CMCAT == "AFTER TREATMENT")
label <- "CM(posttrt/AFTER TREATMENT)"
tmp_cm_3 %>% check_not_empty("レコード有無チェック", domain_name = label)
c("CMTRT", "CMSTDTC") %>% check_required_vars(tmp_cm_3, ., domain_name = label)
tmp_cm_3 %>% check_date_before_today("CMSTDTC", domain_name = label)
target <- "CMENRF"
tmp_cm_3 %>%
  filter(CMENRF != "") %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("CM")
tmp_cm_3 %>% check_date_after_var_before_today("CMENDTC", "CMSTDTC", domain_name = label)

# DM
c("RFICDTC", "BRTHDTC", "SEX", "COUNTRY") %>% check_required_vars(dm, ., domain_name = "DM")
dm %>% check_date_before_today("BRTHDTC", domain_name = "DM")
dm %>% check_date_after_var_before_today("RFICDTC", "BRTHDTC", domain_name = "DM")
c("SEX", "RACE", "COUNTRY") %>% walk(~ run_value_equals_checks_from_csv(dm, "DM", .x, fixed_value_checks_csv_path))

# 同意取得日(RFICDTC)時点の年齢(満年齢)を、以降のチェック用にdmのage_at_consent列として持たせる
age_years <- function(birth_date, ref_date) {
  birth_date <- as.Date(birth_date)
  ref_date <- as.Date(ref_date)
  years <- as.integer(format(ref_date, "%Y")) - as.integer(format(birth_date, "%Y"))
  had_birthday <- format(ref_date, "%m-%d") >= format(birth_date, "%m-%d")
  years - as.integer(!had_birthday)
}
dm <- dm %>% mutate(age_at_consent = age_years(BRTHDTC, RFICDTC))

# DS(DSSPIDがdiscon、DSCATがDISPOSITION EVENT、EPOCHがTREATMENT)
suffix <- "_1"
target <- "DSTERM"
tmp_ds_1 <- ds %>% filter(DSSPID == "discon" & DSCAT == "DISPOSITION EVENT" & EPOCH == "TREATMENT")
label <- "DS(discon/DISPOSITION EVENT/TREATMENT)"
tmp_ds_1 %>% check_not_empty("レコード有無チェック", domain_name = label)
c("DSTERM", "DSSTDTC") %>% check_required_vars(tmp_ds_1, ., domain_name = label)
tmp_ds_1 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("DS")
tmp_ds_1 %>% check_date_before_today("DSSTDTC", domain_name = label)

# DS(DSSPIDがwithdrawal)
suffix <- "_2"
target <- c("DSTERM", "DSCAT", "EPOCH")
tmp_ds_2 <- ds %>% filter(DSSPID == "withdrawal")
label <- "DS(withdrawal)"
tmp_ds_2 %>% check_not_empty("レコード有無チェック", domain_name = label)
c("DSTERM", "EPOCH", "DSSTDTC") %>% check_required_vars(tmp_ds_2, ., domain_name = label)
tmp_ds_2 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("DS")
tmp_ds_2 %>% check_date_before_today("DSSTDTC", domain_name = label)

# DS(DSSPIDがdisconpem)
suffix <- "_3"
target <- c("DSTERM", "DSCAT", "DSSCAT", "EPOCH")
tmp_ds_3 <- ds %>% filter(DSSPID == "disconpem")
label <- "DS(disconpem)"
tmp_ds_3 %>% check_not_empty("レコード有無チェック", domain_name = label)
c("DSTERM", "DSSTDTC") %>% check_required_vars(tmp_ds_3, ., domain_name = label)
tmp_ds_3 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("DS")
tmp_ds_3 %>% check_date_before_today("DSSTDTC", domain_name = label)


# EC(ECSPIDがec_200、ECTRTがIVONESCIMAB)
suffix <- "_1"
target <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECDOSU", "ECDOSFRM", "ECROUTE")
tmp_ec_1 <- ec %>% filter(ECSPID == "ec_200" & ECTRT == "IVONESCIMAB")
tmp_ec_1 %>% check_not_empty("レコード有無チェック", domain_name = "EC(IVONESCIMAB)")
tmp_ec_1 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("EC")
# ECADJは空欄を許容し、値が入っている行のみCSVの選択肢でチェックする
target <- "ECADJ"
tmp_ec_1 %>%
  filter(ECADJ != "") %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("EC")
c("ECDOSE", "ECSTDTC", "ECENDTC") %>% check_required_vars(tmp_ec_1, ., domain_name = "EC(IVONESCIMAB)")
tmp_ec_1 %>% check_date_before_today("ECSTDTC", domain_name = "EC(IVONESCIMAB)")
tmp_ec_1 %>% check_date_before_today("ECENDTC", domain_name = "EC(IVONESCIMAB)")

# EC(ECSPIDがec_200、ECTRTがPEMETREXED SODIUM HYDRATE)
suffix <- "_2"
target <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECDOSU", "ECDOSFRM", "ECROUTE")
tmp_ec_2 <- ec %>% filter(ECSPID == "ec_200" & ECTRT == "PEMETREXED SODIUM HYDRATE")
tmp_ec_2 %>% check_not_empty("レコード有無チェック", domain_name = "EC(PEMETREXED)")
tmp_ec_2 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("EC")
# ECADJは空欄を許容し、値が入っている行のみCSVの選択肢でチェックする
target <- "ECADJ"
tmp_ec_2 %>%
  filter(ECADJ != "") %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("EC")
tmp_ec_2 %>% check_required_vars("ECDOSE", domain_name = "EC(PEMETREXED)")
tmp_ec_2 %>% check_date_before_today("ECSTDTC", domain_name = "EC(PEMETREXED)")
tmp_ec_2 %>% check_date_before_today("ECENDTC", domain_name = "EC(PEMETREXED)")

# EG(EGSPIDがlab_100)。suffixはEGTESTCDごとにCSVの番号(_1〜_3)に対応させる
eg_suffixes <- c(INTP = "_1", QTCFSB = "_2", QTCUNS = "_3")
for (egtestcd in names(eg_suffixes)) {
  suffix <- eg_suffixes[[egtestcd]]
  tmp_eg_1 <- eg %>% filter(EGSPID == "lab_100" & EGTESTCD == egtestcd)
  label <- str_c("EG(", egtestcd, ")")
  tmp_eg_1 %>% check_not_empty("レコード有無チェック", domain_name = label)
  # INTPはEGTESTとEGORRES(NOT DONE以外の行)、QTCFSB/QTCUNSはEGTESTとEGORRESUの固定値チェック
  target <- if (egtestcd == "INTP") "EGTEST" else c("EGTEST", "EGORRESU")
  tmp_eg_1 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("EG")
  if (egtestcd == "INTP") {
    target <- "EGORRES"
    tmp_eg_1 %>%
      filter(EGSTAT != "NOT DONE") %>%
      rename_with(~ str_c(.x, suffix), all_of(target)) %>%
      run_target_value_checks("EG")
    tmp_eg_1 %>% check_blank_vars("EGORRESU", domain_name = label)
  } else {
    # QTCFSB/QTCUNSはEGORRESが入っている行が全て数値であること
    egorres <- tmp_eg_1 %>% filter(EGORRES != "") %>% pull(EGORRES)
    non_numeric <- egorres[is.na(suppressWarnings(as.numeric(egorres)))]
    if (length(non_numeric) > 0) {
      stop(str_c(label, ": EGORRES数値チェック: ", length(non_numeric), "件NG(数値以外の値: ", quote_join(unique(non_numeric)), ")"))
    }
    cat(label, ": EGORRES数値チェック: OK(EGORRESが全", length(egorres), "行で数値)\n", sep = "")
  }
  # EGSTATがNOT DONEの行はEGORRESが空欄、それ以外の行はEGORRESが必須
  tmp_eg_1 %>%
    filter(EGSTAT == "NOT DONE") %>%
    check_blank_vars("EGORRES", domain_name = str_c("EG(", egtestcd, "/NOT DONE)"))
  tmp_eg_1 %>%
    filter(EGSTAT != "NOT DONE") %>%
    check_required_vars("EGORRES", domain_name = label)
  tmp_eg_1 %>% check_required_vars("EGDTC", domain_name = label)
  tmp_eg_1 %>% check_date_before_today("EGDTC", domain_name = label)
}

# IE
suffix <- "_1"
target <- c("IETEST", "IECAT", "IEORRES")
tmp_ie_in01 <- ie %>%
  filter(IETESTCD == "IN01") %>%
  rename_with(~ str_c(.x, suffix), all_of(target))
tmp_ie_in01 %>% run_target_value_checks("IE")
# IEORRESが入っている行はIEDTCが今日以前の日付で必須、IEORRESが空の行はIEDTCも空欄
tmp_ie_reported <- ie %>% filter(IEORRES != "")
tmp_ie_reported %>% check_required_vars("IEDTC", domain_name = "IE")
tmp_ie_reported %>% check_date_before_today("IEDTC", domain_name = "IE")
ie %>% filter(IEORRES == "") %>% check_blank_vars("IEDTC", domain_name = "IE")

suffix <- "_2"
tmp_ie_in02 <- ie %>%
  filter(IETESTCD == "IN02") %>%
  rename_with(~ str_c(.x, suffix), all_of(target))
tmp_ie_in02 %>% run_target_value_checks("IE")
age <- dm %>% select(USUBJID, age_at_consent)
tmp_ie_in02_age <- tmp_ie_in02 %>% inner_join(age, by="USUBJID")
# IN02がある被験者は同意取得時の年齢が75歳以上、IN02が無い被験者は75歳未満であること
tmp_ie_in02_age %>% check_numeric_range("age_at_consent", min_value = 75, domain_name = "IE(IN02あり)")
age %>%
  filter(!(USUBJID %in% tmp_ie_in02_age[["USUBJID"]])) %>%
  check_numeric_range("age_at_consent", max_value = 74, domain_name = "DM(IN02なし)")

# IN03〜IN11はIN01と同じ固定値チェック(IETEST/IECAT/IEORRES、suffixは_3〜_11)
for (i in 3:11) {
  suffix <- str_c("_", i)
  ie %>%
    filter(IETESTCD == sprintf("IN%02d", i)) %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("IE")
}

# EX01〜EX25もIN01と同じ固定値チェック(suffixはIN11の続きで_12〜_36)
for (i in 1:25) {
  suffix <- str_c("_", 11 + i)
  ie %>%
    filter(IETESTCD == sprintf("EX%02d", i)) %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("IE")
}

# LB(LBSPIDがlab_100)。suffixはLBTESTCDごとにCSVの番号に対応させる
lb_suffixes <- c(WBC = "_1", NEUT = "_2", RBC = "_3", HGB = "_4", PLAT = "_5", ALB = "_6", BILI = "_7",
                 AST = "_8", ALT = "_9", LDH = "_10", ALP = "_11", GGT = "_12", CK = "_13", AMYLASE = "_14",
                 UREAN = "_15", CREAT = "_16", SODIUM = "_17", K = "_18", CA = "_19", GLUC = "_20", CRP = "_21",
                 INR = "_22", TSH = "_23", T4FR = "_24",
                 PROT = "_25", PROTEXR = "_26")
# 単位(LBORRESU)が無い検査。LBORRESUは固定値チェックの対象から外し、空欄であることをチェックする
lb_no_unit <- c("INR", "PROT")
# LBORRESが選択式の検査。数値チェックの代わりにNOT DONE以外の行のLBORRESをCSVの選択肢でチェックする
lb_categorical <- c("PROT")
target <- c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC")
for (lbtestcd in names(lb_suffixes)) {
  suffix <- lb_suffixes[[lbtestcd]]
  tmp_lb_1 <- lb %>% filter(LBSPID == "lab_100" & LBTESTCD == lbtestcd)
  label <- str_c("LB(", lbtestcd, ")")
  tmp_lb_1 %>% check_not_empty("レコード有無チェック", domain_name = label)
  lb_target <- if (lbtestcd %in% lb_no_unit) setdiff(target, "LBORRESU") else target
  str_c(lb_target, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb_1 %>% rename_with(~ str_c(.x, suffix), all_of(lb_target)),
                                            "LB", .x, fixed_value_checks_csv_path))
  if (lbtestcd %in% lb_no_unit) {
    tmp_lb_1 %>% check_blank_vars("LBORRESU", domain_name = label)
  }
  # LBSTATがNOT DONEの行はLBORRESが空欄、それ以外の行はLBORRESが必須
  tmp_lb_1 %>%
    filter(LBSTAT == "NOT DONE") %>%
    check_blank_vars("LBORRES", domain_name = str_c("LB(", lbtestcd, "/NOT DONE)"))
  tmp_lb_1 %>%
    filter(LBSTAT != "NOT DONE") %>%
    check_required_vars("LBORRES", domain_name = label)
  if (lbtestcd %in% lb_categorical) {
    tmp_lb_1 %>%
      filter(LBSTAT != "NOT DONE") %>%
      rename(!!str_c("LBORRES", suffix) := LBORRES) %>%
      run_value_equals_checks_from_csv("LB", str_c("LBORRES", suffix), fixed_value_checks_csv_path)
  } else {
    # LBORRESが入っている行は全て数値であること
    lborres <- tmp_lb_1 %>% filter(LBORRES != "") %>% pull(LBORRES)
    non_numeric <- lborres[is.na(suppressWarnings(as.numeric(lborres)))]
    if (length(non_numeric) > 0) {
      stop(str_c(label, ": LBORRES数値チェック: ", length(non_numeric), "件NG(数値以外の値: ", quote_join(unique(non_numeric)), ")"))
    }
    cat(label, ": LBORRES数値チェック: OK(LBORRESが全", length(lborres), "行で数値)\n", sep = "")
  }
  tmp_lb_1 %>% check_required_vars("LBDTC", domain_name = label)
  tmp_lb_1 %>% check_date_before_today("LBDTC", domain_name = label)
}


# MH
suffix <- "_1"
target <- c("MHPRESP", "MHOCCUR")
tmp_mh_1 <- mh %>% filter(MHCAT == "PRIMARY DIAGNOSIS" & MHTERM == "Non-squamous non-small cell lung cancer")
tmp_mh_1 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("MH")
tmp_mh_1 %>% check_required_vars("MHSTDTC", domain_name = "MH(PRIMARY DIAGNOSIS)")
tmp_mh_1 %>% check_date_before_today("MHSTDTC", domain_name = "MH(PRIMARY DIAGNOSIS)")

# MH(screening_100のGENERAL・BEFORE・START OF TREATMENT)のレコードが、各被験者に0〜5件であること
# (DMの全被験者を対象にし、該当レコードが無い被験者は0件として数える)
tmp_mh_general_before_count <- dm %>%
  select(USUBJID) %>%
  left_join(
    mh %>%
      filter(MHSPID == "screening_100" & MHCAT == "GENERAL" & MHENRTPT == "BEFORE" & MHENTPT == "START OF TREATMENT") %>%
      count(USUBJID, name = "record_count"),
    by = "USUBJID"
  ) %>%
  mutate(record_count = coalesce(record_count, 0L))
tmp_mh_general_before_count %>% check_numeric_range("record_count", min_value = 0, max_value = 5, domain_name = "MH(GENERAL/BEFORE)")

# MH(MHLLTCDが以下のコード、MHENTPTがSTART OF TREATMENT)
# 10020772: Hypertension, 10058108: Dyslipidaemia, 10012601: Diabetes mellitus,
# 10010952: COPD, 10007649: Cardiovascular disorder
suffix <- "_2"
target <- c("MHCAT", "MHPRESP", "MHOCCUR", "MHENRTPT")
mh_2_llt_codes <- c("10020772", "10058108", "10012601", "10010952", "10007649")
for (llt_code in mh_2_llt_codes) {
  tmp_mh_2 <- mh %>% filter(MHLLTCD == llt_code & MHENTPT == "START OF TREATMENT")
  label <- str_c("MH(MHLLTCD=", llt_code, ")")
  tmp_mh_2 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_mh_2 %>% check_blank_vars("MHSTDTC", domain_name = label)
  tmp_mh_2 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("MH")
}
# 上記以外のコードの病名も、同じ条件(screening_100のGENERAL・ONGOING・START OF TREATMENT)で
# レコードが発生していること
mh %>%
  filter(
    MHSPID == "screening_100" & MHCAT == "GENERAL" & MHENRTPT == "ONGOING" & MHENTPT == "START OF TREATMENT" &
      !(MHLLTCD %in% mh_2_llt_codes)
  ) %>%
  check_not_empty("レコード有無チェック", domain_name = "MH(上記以外のコード)")

# MH(MHTERMがMETASTASIS、MHLOCがBRAIN・LIVER・SITE OTHER THAN BRAIN OR LIVER)
suffix <- "_3"
target <- c("MHCAT", "MHPRESP", "MHOCCUR", "MHENRTPT", "MHENTPT")
for (mhloc in c("BRAIN", "LIVER", "SITE OTHER THAN BRAIN OR LIVER")) {
  tmp_mh_3 <- mh %>% filter(MHTERM == "METASTASIS" & MHLOC == mhloc)
  label <- str_c("MH(METASTASIS/", mhloc, ")")
  tmp_mh_3 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_mh_3 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("MH")
  c("MHOCCUR", "MHSTDTC") %>% check_required_vars(tmp_mh_3, ., domain_name = label)
}


# MH(medhistory・complicationシート(MHSPIDがシート名+連番))。suffixはシートごとにCSVの番号に対応させる
mh_sheet_suffixes <- c(medhistory = "_4", complication = "_5")
target <- c("MHCAT", "MHENRTPT", "MHENTPT")
for (mh_sheet in names(mh_sheet_suffixes)) {
  suffix <- mh_sheet_suffixes[[mh_sheet]]
  tmp_mh_4 <- mh %>% filter(str_detect(MHSPID, str_c("^", mh_sheet)))
  label <- str_c("MH(", mh_sheet, ")")
  tmp_mh_4 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_mh_4 %>% check_required_vars("MHTERM", domain_name = label)
  tmp_mh_4 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("MH")
}

# MI(MITESTCDがHISTTYPC、MIMETHODがBIOPSY)
suffix <- "_1"
target <- c("MITEST", "MISPEC", "MIENRTPT", "MIENTPT")
tmp_mi_1 <- mi %>% filter(MITESTCD == "HISTTYPC" & MIMETHOD == "BIOPSY")
tmp_mi_1 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("MI")
# MISPECは条件によらず必須
tmp_mi_1 %>% check_required_vars("MISPEC", domain_name = "MI(HISTTYPC)")
# MISTATがNOT DONEの行はMIORRESが空欄
tmp_mi_1 %>%
  filter(MISTAT == "NOT DONE") %>%
  check_blank_vars("MIORRES", domain_name = "MI(HISTTYPC)")
# MISTATがNOT DONE以外の行はMIORRESが選択肢のいずれか
target <- "MIORRES"
tmp_mi_1 %>%
  filter(MISTAT != "NOT DONE") %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("MI")
# MI(MITESTCDがHISTTYPC、MIMETHODがSURGICAL RESECTION)
suffix <- "_2"
target <- c("MITEST", "MISPEC", "MIENRTPT", "MIENTPT")
tmp_mi_2 <- mi %>% filter(MITESTCD == "HISTTYPC" & MIMETHOD == "SURGICAL RESECTION")
tmp_mi_2 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("MI")
# MISPECは条件によらず必須
tmp_mi_2 %>% check_required_vars("MISPEC", domain_name = "MI(HISTTYPC/SURGICAL RESECTION)")
# MISTATがNOT DONEの行はMIORRESが空欄
tmp_mi_2 %>%
  filter(MISTAT == "NOT DONE") %>%
  check_blank_vars("MIORRES", domain_name = "MI(HISTTYPC/SURGICAL RESECTION)")
# MISTATがNOT DONE以外の行はMIORRESが選択肢のいずれか
target <- "MIORRES"
tmp_mi_2 %>%
  filter(MISTAT != "NOT DONE") %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("MI")

# MI(MITESTCDがPDL1)
suffix <- "_3"
target <- c("MITEST", "MISPEC", "MIMETHOD", "MIORRESU")
tmp_mi_3 <- mi %>% filter(MITESTCD == "PDL1")
tmp_mi_3 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("MI")
# MIORRESU、MITEST、MISPEC、MIMETHOD、MIDTCは条件によらず必須。MIDTCは今日以前
c("MIORRESU", "MITEST", "MISPEC", "MIMETHOD", "MIDTC") %>% check_required_vars(tmp_mi_3, ., domain_name = "MI(PDL1)")
tmp_mi_3 %>% check_date_before_today("MIDTC", domain_name = "MI(PDL1)")
# MISTATがNOT DONEの行はMIORRESが空欄
tmp_mi_3 %>%
  filter(MISTAT == "NOT DONE") %>%
  check_blank_vars("MIORRES", domain_name = "MI(PDL1)")
# MISTATがNOT DONE以外の行はMIORRESが選択肢のいずれか
target <- "MIORRES"
tmp_mi_3 %>%
  filter(MISTAT != "NOT DONE") %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("MI")

# PR(PRTRTがRADIOTHERAPY、PRCATがPRIOR THERAPY)
suffix <- "_1"
target <- c("PRPRESP", "PROCCUR", "PRDOSU", "PRENRTPT")
tmp_pr_1 <- pr %>% filter(PRTRT == "RADIOTHERAPY" & PRCAT == "PRIOR THERAPY")
tmp_pr_1 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("PR")
# PROCCURがYの行はPRENDTCとPRDOSEが必須
tmp_pr_1 %>%
  filter(PROCCUR == "Y") %>%
  check_required_vars(c("PRENDTC", "PRDOSE"), domain_name = "PR(RADIOTHERAPY)")
# PROCCURがNの行はPRENDTCとPRDOSEが空欄
tmp_pr_1 %>%
  filter(PROCCUR == "N") %>%
  check_blank_vars(c("PRENDTC", "PRDOSE"), domain_name = "PR(RADIOTHERAPY)")

# PR(PRTRTがLUNG SURGERY、PRCATがPRIOR THERAPY)
suffix <- "_2"
target <- c("PRPRESP", "PROCCUR", "PRENRTPT")
tmp_pr_2 <- pr %>% filter(PRTRT == "LUNG SURGERY" & PRCAT == "PRIOR THERAPY")
tmp_pr_2 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("PR")
# PROCCURがYの行はPRSTDTCが必須
tmp_pr_2 %>%
  filter(PROCCUR == "Y") %>%
  check_required_vars("PRSTDTC", domain_name = "PR(LUNG SURGERY)")
# PROCCURがNの行はPRSTDTCが空欄
tmp_pr_2 %>%
  filter(PROCCUR == "N") %>%
  check_blank_vars("PRSTDTC", domain_name = "PR(LUNG SURGERY)")

# PR(prシート(PRSPIDがpr+連番)のCOMBINATION THERAPYはsuffix=_3、postprシート(PRSPIDがpostpr+連番)の
# AFTER THERAPYは_4)。両シートとも項目構成・入力チェックが同じため同じチェックを行う
pr_sheets <- tibble(
  sheet = c("pr", "postpr"),
  prcat = c("COMBINATION THERAPY", "AFTER THERAPY"),
  suffix = c("_3", "_4")
)
for (i in seq_len(nrow(pr_sheets))) {
  suffix <- pr_sheets$suffix[i]
  target <- c("PRTRT", "PRPRESP", "PROCCUR")
  tmp_pr_3 <- pr %>% filter(str_detect(PRSPID, str_c("^", pr_sheets$sheet[i], "[0-9]")) & PRCAT == pr_sheets$prcat[i])
  label <- str_c("PR(", pr_sheets$sheet[i], "/", pr_sheets$prcat[i], ")")
  tmp_pr_3 %>% check_not_empty("レコード有無チェック", domain_name = label)
  c("PRTRT", "PRSTDTC") %>% check_required_vars(tmp_pr_3, ., domain_name = label)
  tmp_pr_3 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("PR")
  tmp_pr_3 %>% check_date_before_today("PRSTDTC", domain_name = label)
  # PRENRFが空の行はPRENDTCが必須、PRENRFが入っている行はPRENRFが選択肢のいずれかでPRENDTCが空欄
  tmp_pr_3 %>%
    filter(PRENRF == "") %>%
    check_required_vars("PRENDTC", domain_name = label)
  target <- "PRENRF"
  tmp_pr_3 %>%
    filter(PRENRF != "") %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("PR")
  tmp_pr_3 %>%
    filter(PRENRF != "") %>%
    check_blank_vars("PRENDTC", domain_name = str_c(label, "/PRENRFあり"))
  tmp_pr_3 %>% check_date_after_var_before_today("PRENDTC", "PRSTDTC", domain_name = label)
}

# RS(RSTESTCDがSTAGE)
suffix <- "_1"
target <- c("RSTEST", "RSCAT", "RSEVAL", "RSORRES")
tmp_rs_1 <- rs %>% filter(RSTESTCD == "STAGE")
tmp_rs_1 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("RS")
c("RSORRES", "RSDTC", "RSTEST", "RSCAT", "RSEVAL") %>% check_required_vars(tmp_rs_1, ., domain_name = "RS(STAGE)")
tmp_rs_1 %>% check_date_before_today("RSDTC", domain_name = "RS(STAGE)")
# RS(RSTESTCDがECOG101)
suffix <- "_2"
target <- c("RSTEST", "RSCAT")
tmp_rs_2 <- rs %>% filter(RSTESTCD == "ECOG101")
tmp_rs_2 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("RS")
# RSSTATがNOT DONEの行はRSORRESが空欄、それ以外の行はRSORRESが選択肢のいずれか
tmp_rs_2 %>%
  filter(RSSTAT == "NOT DONE") %>%
  check_blank_vars("RSORRES", domain_name = "RS(ECOG101/NOT DONE)")
target <- "RSORRES"
tmp_rs_2 %>%
  filter(RSSTAT != "NOT DONE") %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("RS")
tmp_rs_2 %>% check_blank_vars("RSEVAL", domain_name = "RS(ECOG101)")
tmp_rs_2 %>% check_required_vars("RSDTC", domain_name = "RS(ECOG101)")
tmp_rs_2 %>% check_date_before_today("RSDTC", domain_name = "RS(ECOG101)")

# RS(evaluation_400(Week6)とevaluationpd(PDによる治験治療中止時)は同じ定義のため同じチェックを行う)
for (rsspid in c("evaluation_400", "evaluationpd")) {
  # RS(RSTESTCDがTRGRESP)
  suffix <- "_3"
  target <- c("RSSCAT", "RSTEST", "RSCAT", "RSEVAL")
  tmp_rs_3 <- rs %>% filter(RSSPID == rsspid & RSTESTCD == "TRGRESP")
  label <- str_c("RS(", rsspid, "/TRGRESP)")
  tmp_rs_3 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_rs_3 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("RS")
  # RSSTATがNOT DONEの行はRSORRES・RSDTCが空欄、それ以外の行はこれらが必須でRSORRESは選択肢のいずれか
  tmp_rs_3 %>%
    filter(RSSTAT == "NOT DONE") %>%
    check_blank_vars(c("RSORRES", "RSDTC"), domain_name = str_c(label, "/NOT DONE"))
  tmp_rs_3 %>%
    filter(RSSTAT != "NOT DONE") %>%
    check_required_vars(c("RSORRES", "RSDTC"), domain_name = label)
  target <- "RSORRES"
  tmp_rs_3 %>%
    filter(RSSTAT != "NOT DONE") %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("RS")
  tmp_rs_3 %>% check_date_before_today("RSDTC", domain_name = label)

  # RS(RSTESTCDがOVRLRESP、RSCATがRECIST 1.1)
  suffix <- "_4"
  target <- c("RSTEST", "RSEVAL")
  tmp_rs_4 <- rs %>% filter(RSSPID == rsspid & RSTESTCD == "OVRLRESP" & RSCAT == "RECIST 1.1")
  label <- str_c("RS(", rsspid, "/OVRLRESP/RECIST 1.1)")
  tmp_rs_4 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_rs_4 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("RS")
  # RSSTATがNOT DONEの行はRSORRESが空欄、それ以外の行はRSORRESが必須で選択肢のいずれか
  tmp_rs_4 %>%
    filter(RSSTAT == "NOT DONE") %>%
    check_blank_vars("RSORRES", domain_name = str_c(label, "/NOT DONE"))
  tmp_rs_4 %>%
    filter(RSSTAT != "NOT DONE") %>%
    check_required_vars("RSORRES", domain_name = label)
  target <- "RSORRES"
  tmp_rs_4 %>%
    filter(RSSTAT != "NOT DONE") %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("RS")
  # RSORRESが入っている行はRSDTCが必須
  tmp_rs_4 %>%
    filter(RSORRES != "") %>%
    check_required_vars("RSDTC", domain_name = label)
  tmp_rs_4 %>% check_date_before_today("RSDTC", domain_name = label)

  # RS(RSTESTCDがOVRLRESP、RSCATがiRECIST)
  suffix <- "_5"
  target <- c("RSTEST", "RSEVAL")
  tmp_rs_5 <- rs %>% filter(RSSPID == rsspid & RSTESTCD == "OVRLRESP" & RSCAT == "iRECIST")
  label <- str_c("RS(", rsspid, "/OVRLRESP/iRECIST)")
  tmp_rs_5 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_rs_5 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("RS")
  # RSORRESは必須ではない。RSORRESが入っている行はRSORRESが選択肢のいずれかで、RSDTCが必須かつ今日以前
  target <- "RSORRES"
  tmp_rs_5 %>%
    filter(RSORRES != "") %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("RS")
  tmp_rs_5 %>%
    filter(RSORRES != "") %>%
    check_required_vars("RSDTC", domain_name = label)
  tmp_rs_5 %>%
    filter(RSORRES != "") %>%
    check_date_before_today("RSDTC", domain_name = label)
}


# SU(SUCATがTOBACCOはsuffix=_1、ALCOHOLは_2)
target <- c("SUTRT", "SUDOSTXT")
su_suffix_by_cat <- c(TOBACCO = "_1", ALCOHOL = "_2")
for (sucat in names(su_suffix_by_cat)) {
  suffix <- su_suffix_by_cat[[sucat]]
  su %>%
    filter(SUCAT == sucat) %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("SU")
}
su %>% check_required_vars("SUDTC", domain_name = "SU")
su %>% check_date_before_today("SUDTC", domain_name = "SU")







# TU(TUSPIDがscreening_100、TULNKIDがT01〜T05)
suffix <- "_1"
for (tulnkid in sprintf("T%02d", 1:5)) {
  tmp_tu_1 <- tu %>% filter(TUSPID == "screening_100" & TULNKID == tulnkid)
  label <- str_c("TU(", tulnkid, ")")
  tmp_tu_1 %>% check_not_empty("レコード有無チェック", domain_name = label)
  target <- c("TUTESTCD", "TUTEST", "TUORRES", "TULOC", "TULAT", "TUMETHOD", "TUEVAL")
  tmp_tu_1 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("TU")
  # TULOCがLUNGの行はTUCATが選択肢のいずれか、LUNG以外の行はTUCATが空欄
  target <- "TUCAT"
  tmp_tu_1 %>%
    filter(TULOC == "LUNG") %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("TU")
  tmp_tu_1 %>%
    filter(TULOC != "LUNG") %>%
    check_blank_vars("TUCAT", domain_name = str_c("TU(", tulnkid, "/LUNG以外)"))
  # TULOCは必須。TULOCが入っている行はTULAT・TUMETHOD・TUDTCが必須、TULOCが空の行はこれらも空欄
  tmp_tu_1 %>% check_required_vars("TULOC", domain_name = label)
  tmp_tu_1 %>%
    filter(TULOC != "") %>%
    check_required_vars(c("TULAT", "TUMETHOD", "TUDTC"), domain_name = label)
  tmp_tu_1 %>%
    filter(TULOC == "") %>%
    check_blank_vars(c("TULAT", "TUMETHOD", "TUDTC"), domain_name = label)
}
# TR(TRSPIDがscreening_100、TRLNKIDがT01〜T05)
suffix <- "_1"
target <- c("TRGRPID", "TRTESTCD", "TRTEST", "TRORRESU", "TREVAL")
for (trlnkid in sprintf("T%02d", 1:5)) {
  tmp_tr_1 <- tr %>% filter(TRSPID == "screening_100" & TRLNKID == trlnkid)
  label <- str_c("TR(", trlnkid, ")")
  tmp_tr_1 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_tr_1 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("TR")
  c("TRORRES", "TRMETHOD", "TRDTC") %>% check_required_vars(tmp_tr_1, ., domain_name = label)
  tmp_tr_1 %>% check_date_before_today("TRDTC", domain_name = label)
  # TUとリンクしていること(同一USUBJIDでTRMETHOD=TUMETHOD、TRDTC=TUDTC)。TU・TRのどちらか片方にしかないレコードも対象。
  # EDC仕様の不備(標的病変2〜5の検査方法・検査日の参照先が標的病変1になっている、長径に提示条件が無い)により
  # T02〜T05は不一致になるため、エラーではなく一文のwarningにする
  tmp_tu_1 <- tu %>% filter(TUSPID == "screening_100" & TULNKID == trlnkid)
  tr_only <- tmp_tr_1 %>% anti_join(tmp_tu_1, by = c("USUBJID", "TRMETHOD" = "TUMETHOD", "TRDTC" = "TUDTC"))
  tu_only <- tmp_tu_1 %>% anti_join(tmp_tr_1, by = c("USUBJID", "TUMETHOD" = "TRMETHOD", "TUDTC" = "TRDTC"))
  if (nrow(tr_only) > 0 | nrow(tu_only) > 0) {
    warning(str_c(label, ": TU/TRリンクチェック: 確認不要"), call. = FALSE)
  } else {
    cat(label, ": TU/TRリンクチェック: OK(TRMETHOD・TRDTCがTUのTUMETHOD・TUDTCと一致、", nrow(tmp_tr_1), "件)\n", sep = "")
  }
}
# TR(TRSPIDがscreening_100、TRLNKIDがNT01)
suffix <- "_2"
target <- c("TRGRPID", "TRTESTCD", "TRTEST", "TRORRES", "TREVAL", "TRLNKGRP", "TRMETHOD")
tmp_tr_2 <- tr %>% filter(TRSPID == "screening_100" & TRLNKID == "NT01")
tmp_tr_2 %>% check_not_empty("レコード有無チェック", domain_name = "TR(NT01)")
tmp_tr_2 %>%
  rename_with(~ str_c(.x, suffix), all_of(target)) %>%
  run_target_value_checks("TR")
c("TRORRES", "TRMETHOD", "TRDTC") %>% check_required_vars(tmp_tr_2, ., domain_name = "TR(NT01)")
tmp_tr_2 %>% check_date_before_today("TRDTC", domain_name = "TR(NT01)")
# VS(VSSPIDがlab_100)。suffixはVSTESTCDごとにCSVの番号(_1〜_8)に対応させる
vs_suffixes <- c(HEIGHT = "_1", WEIGHT = "_2", SYSBP = "_3", DIABP = "_4", PULSE = "_5", RESP = "_6", TEMP = "_7", SPO2 = "_8")
target <- c("VSTEST", "VSORRESU")
for (vstestcd in names(vs_suffixes)) {
  suffix <- vs_suffixes[[vstestcd]]
  tmp_vs_1 <- vs %>% filter(VSSPID == "lab_100" & VSTESTCD == vstestcd)
  label <- str_c("VS(", vstestcd, ")")
  tmp_vs_1 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_vs_1 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("VS")
  # VSSTATがNOT DONEの行はVSORRESが空欄、それ以外の行はVSORRESが必須
  tmp_vs_1 %>%
    filter(VSSTAT == "NOT DONE") %>%
    check_blank_vars("VSORRES", domain_name = str_c("VS(", vstestcd, "/NOT DONE)"))
  tmp_vs_1 %>%
    filter(VSSTAT != "NOT DONE") %>%
    check_required_vars("VSORRES", domain_name = label)
  tmp_vs_1 %>% check_required_vars("VSDTC", domain_name = label)
  tmp_vs_1 %>% check_date_before_today("VSDTC", domain_name = label)
}


# TR(evaluation_400(Week6)とevaluationpd(PDによる治験治療中止時)は同じ定義のため同じチェックを行う)
for (trspid in c("evaluation_400", "evaluationpd")) {
  # TR(TRLNKIDがT01〜T05)
  for (trlnkid in sprintf("T%02d", 1:5)) {
    # TUMSTATE
    suffix <- "_3"
    tmp_tr_3 <- tr %>% filter(TRSPID == trspid & TRLNKID == trlnkid & TRTESTCD == "TUMSTATE")
    label <- str_c("TR(", trspid, "/", trlnkid, "/TUMSTATE)")
    tmp_tr_3 %>% check_not_empty("レコード有無チェック", domain_name = label)
    c("TRTEST", "TREVAL") %>% check_required_vars(tmp_tr_3, ., domain_name = label)
    target <- c("TRTESTCD", "TRTEST", "TREVAL", "TRGRPID")
    tmp_tr_3 %>%
      rename_with(~ str_c(.x, suffix), all_of(target)) %>%
      run_target_value_checks("TR")
    # TRSTATがNOT DONEの行はTRORRES・TRMETHOD・TRDTCが空欄、それ以外の行はこれらが必須でTRORRES・TRMETHODは選択肢のいずれか
    tmp_tr_3 %>%
      filter(TRSTAT == "NOT DONE") %>%
      check_blank_vars(c("TRORRES", "TRMETHOD", "TRDTC"), domain_name = str_c(label, "/NOT DONE"))
    tmp_tr_3 %>%
      filter(TRSTAT != "NOT DONE") %>%
      check_required_vars(c("TRORRES", "TRMETHOD", "TRDTC"), domain_name = label)
    target <- c("TRORRES", "TRMETHOD")
    tmp_tr_3 %>%
      filter(TRSTAT != "NOT DONE") %>%
      rename_with(~ str_c(.x, suffix), all_of(target)) %>%
      run_target_value_checks("TR")
    tmp_tr_3 %>% check_date_before_today("TRDTC", domain_name = label)

    # DIAMETER
    suffix <- "_4"
    target <- c("TRGRPID", "TRTEST", "TRORRESU", "TREVAL")
    tmp_tr_4 <- tr %>% filter(TRSPID == trspid & TRLNKID == trlnkid & TRTESTCD == "DIAMETER")
    label <- str_c("TR(", trspid, "/", trlnkid, "/DIAMETER)")
    tmp_tr_4 %>% check_not_empty("レコード有無チェック", domain_name = label)
    tmp_tr_4 %>%
      rename_with(~ str_c(.x, suffix), all_of(target)) %>%
      run_target_value_checks("TR")
    # TRORRESが入っている行は全て数値であること
    trorres <- tmp_tr_4 %>% filter(TRORRES != "") %>% pull(TRORRES)
    non_numeric <- trorres[is.na(suppressWarnings(as.numeric(trorres)))]
    if (length(non_numeric) > 0) {
      stop(str_c(label, ": TRORRES数値チェック: ", length(non_numeric), "件NG(数値以外の値: ", quote_join(unique(non_numeric)), ")"))
    }
    cat(label, ": TRORRES数値チェック: OK(TRORRESが全", length(trorres), "行で数値)\n", sep = "")
  }

  # TR(TRTESTCDがSUMDIAM)
  suffix <- "_5"
  target <- c("TRGRPID", "TRTEST", "TRORRESU", "TREVAL")
  tmp_tr_5 <- tr %>% filter(TRSPID == trspid & TRTESTCD == "SUMDIAM")
  label <- str_c("TR(", trspid, "/SUMDIAM)")
  tmp_tr_5 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_tr_5 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("TR")
  tmp_tr_5 %>% check_blank_vars("TRLNKID", domain_name = label)
  # TRSTATがNOT DONEの行はTRORRES・TRDTCが空欄、それ以外の行はこれらが必須
  tmp_tr_5 %>%
    filter(TRSTAT == "NOT DONE") %>%
    check_blank_vars(c("TRORRES", "TRDTC"), domain_name = str_c(label, "/NOT DONE"))
  tmp_tr_5 %>%
    filter(TRSTAT != "NOT DONE") %>%
    check_required_vars(c("TRORRES", "TRDTC"), domain_name = label)
  # TRORRESが入っている行は全て数値であること
  trorres <- tmp_tr_5 %>% filter(TRORRES != "") %>% pull(TRORRES)
  non_numeric <- trorres[is.na(suppressWarnings(as.numeric(trorres)))]
  if (length(non_numeric) > 0) {
    stop(str_c(label, ": TRORRES数値チェック: ", length(non_numeric), "件NG(数値以外の値: ", quote_join(unique(non_numeric)), ")"))
  }
  cat(label, ": TRORRES数値チェック: OK(TRORRESが全", length(trorres), "行で数値)\n", sep = "")
  tmp_tr_5 %>% check_date_before_today("TRDTC", domain_name = label)


  # TR(TRLNKIDがNT01)
  suffix <- "_6"
  target <- c("TRGRPID", "TRTESTCD", "TRTEST", "TREVAL")
  tmp_tr_6 <- tr %>% filter(TRSPID == trspid & TRLNKID == "NT01")
  label <- str_c("TR(", trspid, "/NT01)")
  tmp_tr_6 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_tr_6 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("TR")
  # TRSTATがNOT DONEの行はTRORRES・TRMETHOD・TRDTCが空欄、それ以外の行はこれらが必須でTRORRES・TRMETHODは選択肢のいずれか
  tmp_tr_6 %>%
    filter(TRSTAT == "NOT DONE") %>%
    check_blank_vars(c("TRORRES", "TRMETHOD", "TRDTC"), domain_name = str_c(label, "/NOT DONE"))
  tmp_tr_6 %>%
    filter(TRSTAT != "NOT DONE") %>%
    check_required_vars(c("TRORRES", "TRMETHOD", "TRDTC"), domain_name = label)
  target <- c("TRORRES", "TRMETHOD")
  tmp_tr_6 %>%
    filter(TRSTAT != "NOT DONE") %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("TR")
  tmp_tr_6 %>% check_date_before_today("TRDTC", domain_name = label)

  # TR(TRLNKIDがNEW01)
  suffix <- "_7"
  target <- c("TRGRPID", "TRTESTCD", "TRTEST", "TREVAL")
  tmp_tr_7 <- tr %>% filter(TRSPID == trspid & TRLNKID == "NEW01")
  label <- str_c("TR(", trspid, "/NEW01)")
  tmp_tr_7 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_tr_7 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("TR")
  # TRSTATがNOT DONEの行はTRORRES・TRMETHOD・TRDTCが空欄、それ以外の行はこれらが必須でTRORRES・TRMETHODは選択肢のいずれか
  tmp_tr_7 %>%
    filter(TRSTAT == "NOT DONE") %>%
    check_blank_vars(c("TRORRES", "TRMETHOD", "TRDTC"), domain_name = str_c(label, "/NOT DONE"))
  tmp_tr_7 %>%
    filter(TRSTAT != "NOT DONE") %>%
    check_required_vars(c("TRORRES", "TRMETHOD", "TRDTC"), domain_name = label)
  target <- c("TRORRES", "TRMETHOD")
  tmp_tr_7 %>%
    filter(TRSTAT != "NOT DONE") %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("TR")
  tmp_tr_7 %>% check_date_before_today("TRDTC", domain_name = label)
}




# SUPPQUAL。suffixはQNAMごとにCSVの番号に対応させる。QNAM/QLABEL/QVAL/QORIG等はCDISC SDTM標準上ドメイン名prefixを付けない
suppqual_suffixes <- c(AEACNPEM = "_1", AERELPEM = "_2")
for (qnam in names(suppqual_suffixes)) {
  suffix <- suppqual_suffixes[[qnam]]
  target <- c("RDOMAIN", "IDVAR", "IDVARVAL", "QLABEL", "QORIG")
  tmp_suppqual_1 <- suppqual %>% filter(QNAM == qnam)
  label <- str_c("SUPPQUAL(", qnam, ")")
  tmp_suppqual_1 %>% check_not_empty("レコード有無チェック", domain_name = label)
  tmp_suppqual_1 %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("SUPPQUAL")
  # QVALは必須ではない。QVALが入っている行のみCSVの選択肢でチェックする
  target <- "QVAL"
  tmp_suppqual_1 %>%
    filter(QVAL != "") %>%
    rename_with(~ str_c(.x, suffix), all_of(target)) %>%
    run_target_value_checks("SUPPQUAL")
}

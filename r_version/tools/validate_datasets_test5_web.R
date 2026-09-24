library(here)
rm(list = ls())
source(here("resolve_os_path.R"))

# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、test_config.R(共通)ではなくここで指定する。リポジトリ外の任意の場所でよい
# TODO: test5のfixed_value_checks_test5.csvの実際のパスに置き換える
fixed_value_checks_csv_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test5/fixed_value_checks_test5.csv",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test5\\fixed_value_checks_test5.csv"
)

source(here("test_config.R"))
# test_config.Rはjson_path(他テストとの切り替え用)も定義するが、このファイルは上で固定した
# json_pathを優先して使うため、test_config.R側の値で上書きしないよう再度設定し直す
# TODO: test5のJSONファイルの実際のパスに置き換える
json_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test5/json/fortest5_260728_1634.json",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test5\\json\\fortest5_260728_1634.json"
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

# DM
c("RFICDTC", "BRTHDTC", "SEX", "RACE", "COUNTRY") %>% check_required_vars(dm, ., domain_name = "DM")
dm %>% check_date_before_today(c("BRTHDTC"), domain_name = "DM")
dm %>% check_date_after_var_before_today("RFICDTC", "BRTHDTC", domain_name = "DM")
c("SEX", "RACE", "COUNTRY") %>% walk(~ run_value_equals_checks_from_csv(dm, "DM", .x, fixed_value_checks_csv_path))
dm %>% check_blank_vars("ARM", domain_name = "DM")

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

# SUPPQUAL(registration)
# RS.RSSPID=="sct_300"に対する追加情報(Planned Transplant Donor Relationship)。
# RDOMAIN/IDVAR/IDVARVAL/QNAM/QLABEL/QORIGは固定値、QVALはUNRELATED/RELATEDの2択
target_suppqual_cols <- c("SUPPQUALRDOMAIN", "SUPPQUALIDVAR", "SUPPQUALIDVARVAL", "SUPPQUALQNAM", "SUPPQUALQLABEL", "SUPPQUALQVAL", "SUPPQUALQORIG")
suppqual %>% check_required_vars(target_suppqual_cols, domain_name = "SUPPQUAL")
target_suppqual_cols %>%
  walk(~ run_value_equals_checks_from_csv(suppqual, "SUPPQUAL", .x, fixed_value_checks_csv_path))

# LB(screening_100)
# FUSGENID(融合遺伝子)/GENMUTID(遺伝子変異)/MRDQUAL(MRD定性値、4項目分析法)の3 LBTESTCD。
# LBTEST/LBCAT/LBSPEC/LBMETHOD/LBORRESU/LBSCATは固定値(FUSGENID/GENMUTIDはLBSPEC/LBMETHOD等が
# 常に空欄、MRDQUALのみLBSPEC="BONE MARROW"・LBMETHODが4択)。LBORRESの許容値も各LBTESTCDごとに
# CSV側で管理する。LBSTATが空欄(実施)の行はLBORRES/LBDTCが必須、LBSTAT=="NOT DONE"の行は
# LBORRES/LBDTCが空欄
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

check_fa_grade_orres_by_faobj <- function(fa, faspid, faobj, suffix, fixed_value_checks_csv_path) {
  label <- str_c("FA(", faspid, "/GRADE/", faobj, ")")
  tmp_fa <- fa %>% filter(FASPID == faspid & FATESTCD == "GRADE" & FAOBJ == faobj)
  tmp_fa_renamed <- tmp_fa %>% rename_with(~ str_c(.x, suffix), "FAORRES")
  run_value_equals_checks_from_csv(tmp_fa_renamed, "FA", str_c("FAORRES", suffix), fixed_value_checks_csv_path)
  cat(label, "Grade値域チェック: OK(", nrow(tmp_fa), "件)\n", sep = "")
}

check_fa_panel(fa, "ae_100", "GRADE", fa_grade_panel, "_1", fixed_value_checks_csv_path, orres_per_faobj = TRUE)
for (faobj in names(fa_grade_faobj_suffix)) {
  check_fa_grade_orres_by_faobj(fa, "ae_100", faobj, fa_grade_faobj_suffix[[faobj]], fixed_value_checks_csv_path)
}
check_fa_panel(fa, "ae_100", "DIAGCERT", fa_diagcert_panel, "_2", fixed_value_checks_csv_path)
check_fa_panel(fa, "ae_100", "EBMTSVGR", fa_ebmtsvgr_panel, "_3", fixed_value_checks_csv_path)

# PR(conditioning_200)
"PROCCUR" %>% check_required_vars(pr %>% filter(PRSPID == "conditioning_200"), ., domain_name = "PR")
suffix <- "_1"
target_pr_cols <- c("PRTRT", "PRPRESP", "PROCCUR", "PRDOSU")
tmp_pr <- pr %>% filter(PRSPID == "conditioning_200" & PRLOC == "BODY")
tmp_pr %>% filter(PROCCUR == "Y") %>% check_numeric_range("PRDOSE", min_value = 1, max_value = 30, domain_name = "PR")
tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_cols))
str_c(target_pr_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))
tmp_lb %>% check_numeric_range("LBORRES", min_value = min_value, max_value = max_value, domain_name = label)


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


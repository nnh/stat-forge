library(here)

# validate_datasets_test1.Rと同じ処理(check_cm_cmtrt/check_tr_tu_dtc等の特別チェック・
# run_full_validationの呼び出し方まで含め)を行うが、比較元(生成データ)をR版のその場生成ではなく、
# Webツールが生成したCSV(dummy_data.zip展開後)に差し替えたもの。
#
# 事前準備: dm_web_csv_path・ae_web_csv_path・ds_web_csv_path・other_domains_web_csv_dir
# (test_config.R)を、Webツールでfortest1用JSONを読み込んで生成し「ZIPで一括ダウンロード」した
# dummy_data.zipの展開先に設定しておくこと。json_pathは下記でfortest1用に固定しているため、
# test_config.R側の値(他テストと切り替えて使われる)を書き換える必要はない
rm(list = ls())

# このファイル固定のjson_path。test_config.R側のjson_pathは他テストとの切り替えで
# 意図せず別のJSONを指したままになりうる(実際に誤検知の原因になったため)、ここで固定する
json_path <- "/Users/mariko/Downloads/test20260826/fortest1_260826_1112.json"

# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、test_config.R(共通)ではなくここで指定する。リポジトリ外の任意の場所でよい
fixed_value_checks_csv_path <- "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test1/fixed_value_checks_test1.csv"

source(here("test_config.R"))
# test_config.Rはjson_path(他テストとの切り替え用)も定義するが、このファイルは上で固定した
# json_pathを優先して使うため、test_config.R側の値で上書きしないよう再度設定し直す
json_path <- "/Users/mariko/Downloads/test20260826/fortest1_260826_1112.json"
source(here("tools/validate_common.R"))

# cdisc_variable_values・registration_nはEDC仕様(JSON)由来で被験者データには依存しないため、
# R側でload_edc_spec()を実行して取得する(このとき同時に生成されるR版のae/dm/ds/other_domains・
# discontinuation_dateは、このあと全てWeb版CSVの内容で上書きするため使わない)
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
rm(list = setdiff(ls(), c("ae", "dm", "ds", "other_domains", "facilities", "cdisc_variable_values", "registration_n", "json_path", "discontinuation_date", "fixed_value_checks_csv_path")))

source(here("tools/validate_common.R"))

# ここから下はvalidate_datasets_test1.Rと共通の処理(CM/TR特別チェック・run_full_validation
# 呼び出し・ドメイン名一覧の確認)。validate_test1_shared.Rにまとめてある
source(here("tools/validate_test1_shared.R"))
rm(generated_datasets)
# 値必須・空欄・日付チェックで繰り返し参照するドメインを短い変数名に控えておく(タイプ量を減らすため)。
# cmはgrDevicesパッケージの関数名と同じだが、ここで代入することでローカル変数が優先される(shadow)
# だけなので問題ない
# リストの名前を小文字に変換
names(other_domains) <- tolower(names(other_domains))

# グローバル環境に一括展開
list2env(other_domains, envir = .GlobalEnv)

# test1個別チェック

# 日付整合性
ae %>% check_date_before_today("AESTDTC", domain_name = "AE")
ae %>% check_date_after_var_before_today("AEENDTC", "AESTDTC", domain_name = "AE")
ce %>% check_date_before_today("CEDTC", domain_name = "CE")
cm %>% check_date_before_today("CMSTDTC", domain_name = "CM")
dm %>% check_date_before_today("BRTHDTC", domain_name = "DM")
dm %>% check_date_before_today("RFSTDTC", domain_name = "DM")
dm %>% check_date_after_var_before_today("RFICDTC", "BRTHDTC", domain_name = "DM")
ds %>% check_date_before_today("DSSTDTC", domain_name = "DS")
ds %>% check_date_after_var_before_today("DSDTC", "DSSTDTC", domain_name = "DS")
ec %>% check_date_before_today("ECSTDTC", domain_name = "CM")
lb %>% check_date_before_today("LBDTC", domain_name = "LB")
mh %>% check_date_before_today("MHDTC", domain_name = "MH")
pe %>% check_date_before_today("PEDTC", domain_name = "PE")
pr %>% check_date_before_today("PRSTDTC", domain_name = "PR")
qs %>% check_date_before_today("QSDTC", domain_name = "QS")
rs %>% check_date_before_today("RSDTC", domain_name = "RS")
sc %>% check_date_before_today("SCDTC", domain_name = "SC")
tr %>% check_date_before_today("TRDTC", domain_name = "TR")
# CMSTDTC <= RSDTC <= 今日であることを確認、範囲外の場合STOPエラーとする
tmp_cm <- cm %>% filter(SPDEVID == 1) %>% select(USUBJID, CMSTDTC)
tmp_rs <- rs %>% filter(SPDEVID == 1) %>% select(USUBJID, RSDTC)
tmp_cm %>% inner_join(tmp_rs, by = "USUBJID") %>% check_date_after_var_before_today("RSDTC", "CMSTDTC", domain_name = "CM/RS")
tmp_cm_2 <- cm %>% filter(SPDEVID == 2) %>% select(USUBJID, CMSTDTC_2=CMSTDTC)
tmp_cm %>% inner_join(tmp_cm_2, by = "USUBJID") %>% check_date_after_var_before_today("CMSTDTC_2", "CMSTDTC", domain_name = "CM")
tmp_rs_2 <- rs %>% filter(SPDEVID == 2) %>% select(USUBJID, RSDTC)
tmp_cm_2 %>% inner_join(tmp_rs_2, by = "USUBJID") %>% check_date_after_var_before_today("RSDTC", "CMSTDTC_2", domain_name = "CM/RS")
tmp_cm_3 <- cm %>% filter(SPDEVID == 3) %>% select(USUBJID, CMSTDTC_3=CMSTDTC)
tmp_cm_3 %>% inner_join(tmp_cm_2, by = "USUBJID") %>% check_date_after_var_before_today("CMSTDTC_3", "CMSTDTC_2", domain_name = "CM")
tmp_rs_3 <- rs %>% filter(SPDEVID == 3) %>% select(USUBJID, RSDTC)
tmp_cm_3 %>% inner_join(tmp_rs_3, by = "USUBJID") %>% check_date_after_var_before_today("RSDTC", "CMSTDTC_3", domain_name = "CM/RS")
tmp_cm_4 <- cm %>% filter(SPDEVID == 4) %>% select(USUBJID, CMSTDTC_4=CMSTDTC)
tmp_cm_4 %>% inner_join(tmp_cm_3, by = "USUBJID") %>% check_date_after_var_before_today("CMSTDTC_4", "CMSTDTC_3", domain_name = "CM")
tmp_rs_4 <- rs %>% filter(SPDEVID == 4) %>% select(USUBJID, RSDTC)
tmp_cm_4 %>% inner_join(tmp_rs_4, by = "USUBJID") %>% check_date_after_var_before_today("RSDTC", "CMSTDTC_4", domain_name = "CM/RS")
tmp_cm_5 <- cm %>% filter(SPDEVID == 5) %>% select(USUBJID, CMSTDTC_5=CMSTDTC)
tmp_cm_5 %>% inner_join(tmp_cm_4, by = "USUBJID") %>% check_date_after_var_before_today("CMSTDTC_5", "CMSTDTC_4", domain_name = "CM")
tmp_rs_5 <- rs %>% filter(SPDEVID == 5) %>% select(USUBJID, RSDTC)
tmp_cm_5 %>% inner_join(tmp_rs_5, by = "USUBJID") %>% check_date_after_var_before_today("RSDTC", "CMSTDTC_5", domain_name = "CM/RS")
# DMのSITEIDが、facilities_dummy.csv(施設一覧)のcode列に含まれる値であることを確認する
dm %>% check_values_subset_of("SITEID", facilities[["code"]], domain_name = "DM/facilities")
# 数値上限下限
fa %>% filter(FAOBJ == "Tumor Involvement") %>% check_numeric_range("FAORRES", min_value = 0, max_value = 99, domain_name = "FA")
lb %>% filter(LBTESTCD == "PBTCCE") %>% check_numeric_range("LBORRES", min_value = 0, max_value = 100, domain_name = "LB")
lb %>% filter(LBTESTCD == "HGB") %>% check_numeric_range("LBORRES", max_value = 30, domain_name = "LB")
lb %>% filter(LBTESTCD == "HCT") %>% check_numeric_range("LBORRES", min_value = 0, max_value = 99, domain_name = "LB")
lb %>% filter(LBTESTCD == "PLAT") %>% check_numeric_range("LBORRES", max_value = 999, domain_name = "LB")
lb %>% filter(LBTESTCD == "PLOT") %>% check_numeric_range("LBORRES", min_value = 0, max_value = 99, domain_name = "LB")
lb %>% filter(LBTESTCD == "ALB") %>% check_numeric_range("LBORRES", max_value = 99, domain_name = "LB")
lb %>% filter(LBTESTCD == "BILI") %>% check_numeric_range("LBORRES", max_value = 99, domain_name = "LB")
lb %>% filter(LBTESTCD == "AST") %>% check_numeric_range("LBORRES", max_value = 9999, domain_name = "LB")
lb %>% filter(LBTESTCD == "ALT") %>% check_numeric_range("LBORRES", max_value = 9999, domain_name = "LB")
lb %>% filter(LBTESTCD == "LDH") %>% check_numeric_range("LBORRES", max_value = 9999, domain_name = "LB")
lb %>% filter(LBTESTCD == "ALP") %>% check_numeric_range("LBORRES", max_value = 9999, domain_name = "LB")
lb %>% filter(LBTESTCD == "GGT") %>% check_numeric_range("LBORRES", max_value = 9999, domain_name = "LB")
lb %>% filter(LBTESTCD == "UREAN") %>% check_numeric_range("LBORRES", max_value = 99, domain_name = "LB")
lb %>% filter(LBTESTCD == "CREAT") %>% check_numeric_range("LBORRES", max_value = 99, domain_name = "LB")
lb %>% filter(LBTESTCD == "SODIUM") %>% check_numeric_range("LBORRES", max_value = 999, domain_name = "LB")
lb %>% filter(LBTESTCD == "K") %>% check_numeric_range("LBORRES", max_value = 10, domain_name = "LB")
lb %>% filter(LBTESTCD == "CL") %>% check_numeric_range("LBORRES", max_value = 999, domain_name = "LB")
lb %>% filter(LBTESTCD == "CA") %>% check_numeric_range("LBORRES", max_value = 999, domain_name = "LB")
lb %>% filter(LBTESTCD == "PHOS") %>% check_numeric_range("LBORRES", max_value = 999, domain_name = "LB")
lb %>% filter(LBTESTCD == "CRP") %>% check_numeric_range("LBORRES", max_value = 99, domain_name = "LB")
lb %>% filter(LBTESTCD == "IL2SR") %>% check_numeric_range("LBORRES", max_value = 99999, domain_name = "LB")
lb %>% filter(LBTESTCD == "FIBRINO") %>% check_numeric_range("LBORRES", max_value = 9999, domain_name = "LB")


# FA(FABLFL=="Y"、baseline評価)のFAOBJ==faobjについて、FASTATに応じたFAORRESの必須/空欄を確認する。
# FASTAT==""(実施済み)ならFAORRESは必須、FASTAT=="NOT DONE"(未実施)ならFAORRESは空欄のはず
check_fa_baseline_orres <- function(fa, faobj) {
  label <- str_c("FA(", faobj, ")")
  fa %>% filter(FABLFL == "Y", FAOBJ == faobj, FASTAT == "") %>% check_required_vars("FAORRES", domain_name = label)
  fa %>% filter(FABLFL == "Y", FAOBJ == faobj, FASTAT == "NOT DONE") %>% check_blank_vars("FAORRES", domain_name = label)
}

# LB(LBTESTCD==lbtestcd)について、LBSTATに応じたLBORRESの必須/空欄を確認する。
# LBSTAT==""(実施済み)ならLBORRESは必須、LBSTAT=="NOT DONE"(未実施)ならLBORRESは空欄のはず
check_lb_status_orres <- function(lb, lbtestcd) {
  label <- str_c("LB(", lbtestcd, ")")
  lb %>% filter(LBTESTCD == lbtestcd, LBSTAT == "") %>% check_required_vars("LBORRES", domain_name = label)
  lb %>% filter(LBTESTCD == lbtestcd, LBSTAT == "NOT DONE") %>% check_blank_vars("LBORRES", domain_name = label)
}

# LB(LBTESTCD==lbtestcd)について、LBORRESが必須であることと、target_lb(列名ベクトル)にsuffixを
# 付けた列(例: LBTEST_1)がfixed_value_checks_csv_pathの固定値と一致することを確認する。
# 同じLBTESTCDを複数回チェックする際に列名が重複しないよう、suffixで区別できるようにする想定。
# 列名変更後のtmp_lb(LBTESTCD==lbtestcdに絞り込んだデータ)を返す
check_lb_testcd_fixed_values <- function(lb, lbtestcd, target_lb, suffix, csv_path) {
  tmp_lb <- lb %>% filter(LBTESTCD == lbtestcd)
  lb_cols <- target_lb %>% str_c(suffix)
  tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb))
  lb_cols %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, csv_path))
  tmp_lb
}

# CM/RS/MH(SPDEVID==spdevid)について、prior_line_therapy(SC/PLOTNUM)のSCORRESがspdevid以上
# (thresholds[[spdevid]]に該当するUSUBJID。check_cm_cmtrtのthresholdsと同じ考え方)なら
# CMTRT/CMSTDTC・RSORRES/RSDTCが必須、それ以外は空欄のはずであることを確認する。
# MHOCCURは、それに加えてそのSPDEVIDのRS応答がCR/PRの被験者だけ必須(それ以外は空欄のはず)
check_prior_line_therapy_gating <- function(spdevid, cm, rs, mh, prior_line_therapy_by_scorres) {
  thresholds <- list(
    "2" = c("2", "3", "4", "5<="),
    "3" = c("3", "4", "5<="),
    "4" = c("4", "5<="),
    "5" = c("5<=")
  )
  target_usubjid <- bind_rows(prior_line_therapy_by_scorres[thresholds[[as.character(spdevid)]]])
  target_cm <- cm %>% filter(SPDEVID == spdevid) %>% inner_join(target_usubjid, by = "USUBJID")
  target_rs <- rs %>% filter(SPDEVID == spdevid) %>% inner_join(target_usubjid, by = "USUBJID")

  if (!setequal(target_cm[["USUBJID"]], target_rs[["USUBJID"]])) {
    stop(str_c(
      "CM/RS対象USUBJID一致チェック: NG(CMのみ: ", paste(setdiff(target_cm[["USUBJID"]], target_rs[["USUBJID"]]), collapse = ", "),
      " / RSのみ: ", paste(setdiff(target_rs[["USUBJID"]], target_cm[["USUBJID"]]), collapse = ", "), ")"
    ))
  }
  cat("CM/RS対象USUBJID一致チェック: OK\n")

  target_cm %>% check_required_vars(c("CMTRT", "CMSTDTC"), domain_name = "CM")
  target_rs %>% check_required_vars(c("RSORRES", "RSDTC"), domain_name = "RS")
  cm %>% filter(SPDEVID == spdevid) %>% anti_join(target_usubjid, by = "USUBJID") %>%
    check_blank_vars(c("CMTRT", "CMSTDTC"), domain_name = "CM")
  rs %>% filter(SPDEVID == spdevid) %>% anti_join(target_usubjid, by = "USUBJID") %>%
    check_blank_vars(c("RSORRES", "RSDTC"), domain_name = "RS")

  RESPONDER_RSORRES <- c("CR", "PR")
  rs_cr_pr_usubjid <- rs %>% filter(SPDEVID == spdevid, RSORRES %in% RESPONDER_RSORRES) %>% select(USUBJID)
  target_usubjid_mh <- target_usubjid %>% inner_join(rs_cr_pr_usubjid, by = "USUBJID")
  target_mh <- mh %>% filter(SPDEVID == spdevid) %>% inner_join(target_usubjid_mh, by = "USUBJID")
  target_mh %>% check_required_vars("MHOCCUR", domain_name = "MH")
  mh %>% filter(SPDEVID == spdevid) %>% anti_join(target_usubjid_mh, by = "USUBJID") %>%
    check_blank_vars("MHOCCUR", domain_name = "MH")
  test_bestresp <- target_rs %>% inner_join(target_mh, by="USUBJID") %>% select(RSORRES) %>% unlist() %>% unique()
  if (!setequal(test_bestresp, RESPONDER_RSORRES)) {
    warning(str_c(
      "MH該当被験者のRS最良奏効一致チェック: NG(期待値: ", paste(RESPONDER_RSORRES, collapse = ", "),
      " / 実際の値: ", paste(test_bestresp, collapse = ", "), ")"
    ), call. = FALSE, immediate. = TRUE)
  } else {
    cat("MH該当被験者のRS最良奏効一致チェック: OK(", paste(test_bestresp, collapse = ", "), ")\n", sep = "")
  }
}

# 値必須チェック
# ae
ae %>% check_required_vars(c("AETERM", "AETOXGR", "AESTDTC", "AESER", "AEACN", "AEREL", "AEOUT", "AEENDTC"), domain_name = "AE")
c("AETOXGR") %>% walk(~ run_value_equals_checks_from_csv(ae, "AE", .x, fixed_value_checks_csv_path))
target_ae_cols <- c("AESDTH", "AESLIFE", "AESHOSP", "AESDISAB","AESCONG", "AESMIE")
tmp_ae <- ae %>% filter(AESER == "Y")
tmp_ae %>% check_required_vars(target_ae_cols, domain_name = "AE")
target_ae_cols %>% walk(~ run_value_equals_checks_from_csv(tmp_ae, "AE", .x, fixed_value_checks_csv_path))
tmp_ae <- ae %>% filter(AESER == "N")
tmp_ae %>% check_blank_vars(target_ae_cols, domain_name = "AE")
# ce
c("CEPRESP", "CEOCCUR", "CECAT") %>% walk(~ run_value_equals_checks_from_csv(ce, "CE", .x, fixed_value_checks_csv_path))
ce %>% filter(CEOCCUR == "Y") %>% check_required_vars("CETERM", domain_name = "CE")
ce %>% filter(CEOCCUR == "N") %>% check_blank_vars("CETERM", domain_name = "CE")

# cm
cm %>% filter(SPDEVID == 1) %>% check_required_vars(c("CMTRT", "CMSTDTC"), domain_name = "CM")
prior_line_therapy <- sc %>% filter(SCTESTCD == "PLOTNUM")
prior_line_therapy_by_scorres <- prior_line_therapy$SCORRES %>%
  unique() %>%
  set_names() %>%
  map(~ prior_line_therapy %>% filter(SCORRES == .x) %>% select(USUBJID))

check_prior_line_therapy_gating(2, cm, rs, mh, prior_line_therapy_by_scorres)
check_prior_line_therapy_gating(3, cm, rs, mh, prior_line_therapy_by_scorres)
check_prior_line_therapy_gating(4, cm, rs, mh, prior_line_therapy_by_scorres)
check_prior_line_therapy_gating(5, cm, rs, mh, prior_line_therapy_by_scorres)
c("CMCAT", "CMPRESP", "CMOCCUR", "CMENRTPT", "CMENTPT") %>%
  walk(~ run_value_equals_checks_from_csv(cm, "CM", .x, fixed_value_checks_csv_path))
tmp_cm <- cm %>% filter(SPDEVID == 1)
c("CMTRT") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))
tmp_cm <- cm %>% filter(SPDEVID != 1 & SPDEVID != 5)
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, "_1"), c(CMTRT))
c("CMTRT_1") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))
# 治療歴５だけは複数治療名が入る
tmp_cm <- cm %>%
  filter(SPDEVID == 5) %>%
  rename(CMTRT_1 = CMTRT)
tmp_cm %>%
  separate_rows(CMTRT_1, sep = ",\\s*") %>%
  run_value_equals_checks_from_csv("CM", "CMTRT_1", fixed_value_checks_csv_path)
dm %>% check_required_vars(c("RFICDTC", "BRTHDTC", "SEX", "RACE", "RFSTDTC"), domain_name = "DM")
c("SEX", "RACE") %>% walk(~ run_value_equals_checks_from_csv(dm, "DM", .x, fixed_value_checks_csv_path))
# DS
c("DSCAT") %>%
  walk(~ run_value_equals_checks_from_csv(ds, "DS", .x, fixed_value_checks_csv_path))
tmp_ds <- ds %>% filter(EPOCH == "FOLLOW-UP") %>% rename(DSTERM_1=DSTERM)
c("DSTERM_1") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ds, "DS", .x, fixed_value_checks_csv_path))
tmp_ds <- ds %>% filter(EPOCH == "TREATMENT") %>% rename(DSTERM_2=DSTERM)
c("DSTERM_2") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ds, "DS", .x, fixed_value_checks_csv_path))
# FollowUpよりTreatmentの方が先、DEATHの整合性を確認
treatment_death <- ds %>% filter(EPOCH == "TREATMENT" & DSTERM == "DEATH") %>% select(USUBJID, DSSTDTC)
followup_death <- ds %>% filter(EPOCH == "FOLLOW-UP" & DSTERM == "DEATH") %>% select(USUBJID, DSSTDTC, DSDTC)
# TreatmentでDEATHになっている被験者は、FollowUpでもDEATHになっているはず
# (TreatmentのDEATHがFollowUpに引き継がれていない場合はNG)
missing_followup_from_treatment <- treatment_death %>% anti_join(followup_death, by = "USUBJID")
if (nrow(missing_followup_from_treatment) > 0) {
  stop(str_c(
    "Treatment死亡→FollowUp死亡一致チェック: NG(FollowUpに死亡記録が無いUSUBJID: ",
    paste(unique(missing_followup_from_treatment[["USUBJID"]]), collapse = ", "), ")"
  ))
}
cat("Treatment死亡→FollowUp死亡一致チェック: OK(", length(unique(treatment_death[["USUBJID"]])), "件)\n", sep = "")

ae_death <- ae %>% filter(AETOXGR == 5)
# AEドメインで死亡(AETOXGR==5)になっている被験者が、DSドメインのFollowUpでも死亡(DSTERM=="DEATH")
# になっていることを確認する
missing_followup_death <- ae_death %>% anti_join(followup_death, by = "USUBJID")
if (nrow(missing_followup_death) > 0) {
  stop(str_c(
    "AE死亡→DS FollowUp死亡一致チェック: NG(DSのFollowUpに死亡記録が無いUSUBJID: ",
    paste(unique(missing_followup_death[["USUBJID"]]), collapse = ", "), ")"
  ))
}
cat("AE死亡→DS FollowUp死亡一致チェック: OK(", length(unique(ae_death[["USUBJID"]])), "件)\n", sep = "")

# AE死亡日(AETOXGR==5のAEENDTC最小値、R版build_death_date_table()と同じ考え方)が、
# DSドメインのFollowUp死亡日(DSDTC。DSTERM=="DEATH"の実際の死亡日はfinalize_ds_disposition()で
# AE側の値を根拠にDSDTCへ設定される。DSSTDTCは別の意味の列で必ずしも死亡日とは一致しない)と
# 一致することを確認する
ae_death_date <- ae_death %>% group_by(USUBJID) %>% summarise(AEDTHDTC = min(AEENDTC), .groups = "drop")
death_date_mismatch <- ae_death_date %>% inner_join(followup_death, by = "USUBJID") %>% filter(AEDTHDTC != DSDTC)
if (nrow(death_date_mismatch) > 0) {
  detail <- str_c(death_date_mismatch[["USUBJID"]], "(AE:", death_date_mismatch[["AEDTHDTC"]], " / DS:", death_date_mismatch[["DSDTC"]], ")") %>%
    paste(collapse = ", ")
  stop(str_c("AE死亡日=DS FollowUp死亡日チェック: NG(不一致: ", detail, ")"))
}
cat("AE死亡日=DS FollowUp死亡日チェック: OK(", nrow(ae_death_date), "件)\n", sep = "")
# EC
c("ECTRT", "ECMOOD", "VISITNUM") %>%
  walk(~ run_value_equals_checks_from_csv(ec, "EC", .x, fixed_value_checks_csv_path))
# FA
fa %>% filter(FABLFL == "Y" & FAOBJ == "Bulky Mass") %>% check_required_vars("FAORRES", domain_name = "FA")
fa %>% filter(FABLFL == "Y" & FAOBJ == "Tumor Involvement") %>% check_required_vars("FAORRES", domain_name = "FA")
check_fa_baseline_orres(fa, "Bone Marrow Infiltration")
c("FABLFL") %>%
  walk(~ run_value_equals_checks_from_csv(fa, "FA", .x, fixed_value_checks_csv_path, visit=100))
tmp_fa <- fa %>% filter(FATESTCD=="OCCUR")
tmp_fa <- tmp_fa %>% rename_with(~ str_c(.x, "_1"), c(FATEST, FAOBJ, VISITNUM, FAORRES))
c("FATEST_1", "FAOBJ_1", "VISITNUM_1") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))
tmp_fa <- tmp_fa %>% filter(FAOBJ_1=="Bulky Mass")
c("FAORRES_1") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))
tmp_fa <- fa %>% filter(FAOBJ=="Bone Marrow Infiltration")
fa %>% filter(FAOBJ=="Bone Marrow Infiltration" & FASTAT == "NOT DONE") %>% check_blank_vars("FAORRES", domain_name="FA")
tmp_fa <- tmp_fa %>% rename_with(~ str_c(.x, "_2"), c(FALOC, FAORRES))
c("FALOC_2") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))
tmp_fa <- tmp_fa %>% filter(FASTAT != "NOT DONE")
c("FAORRES_2") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))
tmp_fa <- fa %>% filter(FATESTCD=="LESNUM")
tmp_fa <- tmp_fa %>% rename_with(~ str_c(.x, "_3"), c(FATEST, FAOBJ, VISITNUM, FACAT))
c("FATEST_3", "FAOBJ_3", "VISITNUM_3", "FACAT_3") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))
tmp_fa <- fa %>% filter(FATESTCD == "GRADE")
tmp_fa <- tmp_fa %>% rename_with(~ str_c(.x, "_4"), c(FATEST, FACAT, FAORRES, FAOBJ, VISITNUM))
c("FATEST_4", "FACAT_4", "FAORRES_4", "FAOBJ_4", "VISITNUM_4") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))
# LB
c("VISITNUM", "LBBLFL") %>%
  walk(~ run_value_equals_checks_from_csv(lb, "LB", .x, fixed_value_checks_csv_path))
suffix <- 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "WBC", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
tmp_lb %>% check_required_vars("LBORRES", domain_name = "LB")
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "NEUT", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
tmp_lb %>% check_required_vars("LBORRES", domain_name = "LB")
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "LYM", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "PBTCCE", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "RBC", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "HGB", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "HCT", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "PLAT", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
tmp_lb %>% check_required_vars("LBORRES", domain_name = "LB")
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "PROT", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "ALB", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "BILI", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
tmp_lb %>% check_required_vars("LBORRES", domain_name = "LB")
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "AST", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
tmp_lb %>% check_required_vars("LBORRES", domain_name = "LB")
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "ALT", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
tmp_lb %>% check_required_vars("LBORRES", domain_name = "LB")
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "LDH", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
lb %>% filter(LBTESTCD == "CREAT") %>% check_required_vars("LBORRES", domain_name = "LB")
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "ALP", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "GGT", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "UREAN", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "CREAT", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "SODIUM", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "K", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "CL", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "CA", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "CRP", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "IL2SR", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "B2MICG", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "INR", c("LBTEST", "LBCAT", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "APTT", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "DDIMER", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "FIBRINO", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
suffix <- suffix + 1
tmp_lb <- check_lb_testcd_fixed_values(lb, "FDP", c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC"), str_c("_", as.character(suffix)), fixed_value_checks_csv_path)
check_lb_status_orres(lb, "LYM")
check_lb_status_orres(lb, "PBTCCE")
check_lb_status_orres(lb, "RBC")
check_lb_status_orres(lb, "HGB")
check_lb_status_orres(lb, "HCT")
check_lb_status_orres(lb, "PROT")
check_lb_status_orres(lb, "ALB")
check_lb_status_orres(lb, "LDH")
check_lb_status_orres(lb, "ALP")
check_lb_status_orres(lb, "GGT")
check_lb_status_orres(lb, "UREAN")
check_lb_status_orres(lb, "SODIUM")
check_lb_status_orres(lb, "K")
check_lb_status_orres(lb, "CL")
check_lb_status_orres(lb, "CA")
check_lb_status_orres(lb, "PHOS")
check_lb_status_orres(lb, "CRP")
check_lb_status_orres(lb, "IL2SR")
check_lb_status_orres(lb, "B2MICG")
check_lb_status_orres(lb, "INR")
check_lb_status_orres(lb, "APTT")
check_lb_status_orres(lb, "DDIMER")
check_lb_status_orres(lb, "FIBRINO")
check_lb_status_orres(lb, "FDP")
mh %>% filter(MHENTPT == 100) %>% check_required_vars(c("MHTERM", "MHDTC"), domain_name = "MH")
# MHOCCUR
rs_spdevid_1_cr_pr <- rs %>% filter(SPDEVID == 1 & (RSORRES == "CR" | RSORRES == "PR")) %>% select(USUBJID)
rs_spdevid_1_others <- rs %>% filter(SPDEVID == 1 & (RSORRES != "CR" & RSORRES != "PR")) %>% select(USUBJID)
mh_spdevid_1 <- mh %>%  filter(SPDEVID == 1)
mh %>% filter(SPDEVID == 1) %>% inner_join(rs_spdevid_1_cr_pr, by="USUBJID") %>% check_required_vars("MHOCCUR", domain_name = "MH")
mh %>% filter(SPDEVID == 1) %>% inner_join(rs_spdevid_1_others, by="USUBJID") %>% check_blank_vars("MHOCCUR", domain_name = "MH")
tmp_mh <- mh
tmp_mh$VISITNUM <- tmp_mh$MHENTPT
c("MHTERM", "MHCAT", "MHPRESP", "MHENRTPT") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path, visit = 100))
pe %>% check_required_vars("PEORRES", domain_name = "PE")
pe %>% filter(PEORRES != "U") %>% check_required_vars("PEDTC", domain_name = "PE")
c("PETESTCD", "PETEST", "PEORRES") %>%
  walk(~ run_value_equals_checks_from_csv(pe, "PE", .x, fixed_value_checks_csv_path, visit = 50))
c("PETESTCD", "PETEST", "PEORRES", "PEBLFL") %>%
  walk(~ run_value_equals_checks_from_csv(pe, "PE", .x, fixed_value_checks_csv_path, visit = 100))
c("PRENRTPT", "PRENTPT", "PRPRESP") %>%
  walk(~ run_value_equals_checks_from_csv(pr, "PR", .x, fixed_value_checks_csv_path))
tmp_pr <- pr %>% filter(PRCAT == "Autologous") %>% rename(PRTRT_1=PRTRT)
c("PRTRT_1") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr, "PR", .x, fixed_value_checks_csv_path))
tmp_pr <- pr %>% filter(PRCAT == "Allogeneic") %>% rename(PRTRT_2=PRTRT)
c("PRTRT_2") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr, "PR", .x, fixed_value_checks_csv_path))

qs %>% check_required_vars(c("QSORRES", "QSDTC"), domain_name = "QS")
c("QSTESTCD", "QSTEST", "QSCAT", "QSORRES", "QSBLFL") %>%
  walk(~ run_value_equals_checks_from_csv(qs, "QS", .x, fixed_value_checks_csv_path, visit = 100))
# RS
rs %>% filter(SPDEVID == 1) %>% check_required_vars(c("RSORRES", "RSDTC"), domain_name = "RS")
c("RSCAT", "RSEVAL", "RSORRES") %>%
  walk(~ run_value_equals_checks_from_csv(rs, "RS", .x, fixed_value_checks_csv_path))
tmp_rs <- rs %>% filter(RSTESTCD == "BESTRESP")
c("RSTEST", "RSENRTPT", "RSENTPT") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))
tmp_rs <- rs %>% filter(RSTESTCD == "OVRLRESP")
tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, "_1"), c(RSTEST,VISITNUM))
c("RSTEST_1", "VISITNUM_1") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))

# SC
sc %>% check_required_vars("SCORRES", domain_name = "SC")
sc %>% filter(SCTESTCD == "STAGE" & SCORRES != "UNKNOWN") %>% check_required_vars("SCDTC", domain_name = "SC")
tmp_sc <- sc %>% filter(SCTESTCD == "STAGE")
tmp_sc <- tmp_sc %>% rename_with(~ str_c(.x, "_1"), c(SCTEST, SCCAT, SCORRES))
c("SCTEST_1", "SCCAT_1", "SCORRES_1") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_sc, "SC", .x, fixed_value_checks_csv_path))
tmp_sc <- sc %>% filter(SCTESTCD == "PLOTNUM")
tmp_sc <- tmp_sc %>% rename_with(~ str_c(.x, "_2"), c(SCTEST, SCCAT, SCORRES))
c("SCTEST_2", "SCORRES_2") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_sc, "SC", .x, fixed_value_checks_csv_path))
tr %>% check_required_vars(c("TRORRES", "TRDTC"), domain_name = "TR")
c("TRLNKID", "TRTESTCD", "TRTEST", "TRORRESU", "VISITNUM", "TRBLFL") %>%
  walk(~ run_value_equals_checks_from_csv(tr, "TR", .x, fixed_value_checks_csv_path))
c("TULNKID", "TUTESTCD", "TUTEST", "TUORRES","TULOC", "VISITNUM", "TUBLFL") %>%
  walk(~ run_value_equals_checks_from_csv(tu, "TU", .x, fixed_value_checks_csv_path))
pr %>% filter(PRCAT == "Autologous") %>% check_required_vars("PROCCUR", domain_name = "PR")
pr %>% filter(PRCAT == "Autologous" & PROCCUR == "Y") %>% check_required_vars("PRSTDTC", domain_name = "PR")
pr %>% filter(PRCAT == "Allogeneic") %>% check_required_vars("PROCCUR", domain_name = "PR")
pr %>% filter(PRCAT == "Allogeneic" & PROCCUR == "Y") %>% check_required_vars("PRSTDTC", domain_name = "PR")
ec %>% check_required_vars("ECSTDTC", domain_name = "EC")
fa %>% filter(FAOBJ != "Bone Marrow Infiltration" & VISITNUM != 100) %>% check_required_vars("FAORRES", domain_name = "FA")
rs %>% filter(RSENTPT != 100 | (RSENTPT == 100 & SPDEVID == 1)) %>% check_required_vars(c("RSORRES", "RSDTC"), domain_name = "RS")
ce %>% check_required_vars("CEOCCUR", domain_name = "CE")
ce %>% filter(CEOCCUR == "Y") %>% check_required_vars(c("CETERM", "CEDTC"), domain_name = "CE")
ce %>% filter(CEOCCUR == "N") %>% check_blank_vars(c("CETERM", "CEDTC"), domain_name = "CE")
ds %>% check_required_vars(c("DSTERM", "DSDTC", "DSSTDTC"), domain_name = "DS")
ae %>% check_required_vars(c("AETERM", "AETOXGR", "AESTDTC", "AESER", "AEACN", "AEREL", "AEOUT", "AEENDTC"), domain_name = "AE")
ae %>% filter(AESER =="Y") %>% check_required_vars(c("AESDTH", "AESLIFE", "AESHOSP", "AESDISAB", "AESCONG", "AESMIE"), domain_name = "AE")
ae %>% filter(AESER !="Y") %>% check_blank_vars(c("AESDTH", "AESLIFE", "AESHOSP", "AESDISAB", "AESCONG", "AESMIE"), domain_name = "AE")

# ここから1行ずつ実行して、ドメインの中身を1つずつ目視確認する(View()が2枚(生成データ/CSV)開く)。
# 必要な数だけ行をコピーしてindexを変えて追加していく
csv_list <- list()
csv_list$DM <- dm
csv_list$AE <- ae
csv_list$DS <- ds
names(other_domains) <- toupper(names(other_domains))
#compare_domain(csv_list, datasets, "DM", "USUBJID")
#compare_domain(csv_list, datasets, "AE", c("USUBJID", "AESEQ"))
#compare_domain(csv_list, datasets, "DS", c("USUBJID", "DSSEQ"))
#compare_domain_by_index(other_domains, datasets, 1, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 2, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 3, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 4, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 5, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 6, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 7, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 8, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 9, exclude = special_domain_names)
#compare_domain_by_index(other_domains, datasets, 10, exclude = special_domain_names)
# compare_domain_by_index(generated_datasets, datasets, 13, exclude = special_domain_names)

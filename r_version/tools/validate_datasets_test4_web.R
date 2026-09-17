library(here)
rm(list = ls())
source(here("resolve_os_path.R"))

# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、test_config.R(共通)ではなくここで指定する。リポジトリ外の任意の場所でよい
fixed_value_checks_csv_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test4/fixed_value_checks_test4.csv",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test4\\fixed_value_checks_test4.csv"
)

source(here("test_config.R"))
# test_config.Rはjson_path(他テストとの切り替え用)も定義するが、このファイルは上で固定した
# json_pathを優先して使うため、test_config.R側の値で上書きしないよう再度設定し直す
json_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test4/json/fortest4_260826_1501.json",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test4\\json\\fortest4_260826_1501.json"
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

# test4個別チェック
# ここにドメインごとのチェックを追加していく(参考: tools/validate_datasets_test1_web.R・test2_web.R・test3_web.R)

# DM
c("RFICDTC", "BRTHDTC", "SEX", "RACE", "COUNTRY") %>% check_required_vars(dm, ., domain_name = "DM")
dm %>% check_date_before_today(c("BRTHDTC"), domain_name = "DM")
dm %>% check_date_after_var_before_today("RFICDTC", "BRTHDTC", domain_name = "DM")
c("SEX", "RACE", "COUNTRY") %>% walk(~ run_value_equals_checks_from_csv(dm, "DM", .x, fixed_value_checks_csv_path))

#IE
# IETESTCD(field9等、is_invisible)ごとにIETEST/IECAT/IEORRES(field10/11/12相当)が固定値と
# 一致することと、IEORRESが入っている行だけIEDTC(確認日)が必須(入っていない行は空欄)であることを
# 確認する(test2のvalidate_test2_shared.R記載のrun_ie_testcd_checksと同じ考え方)
run_ie_testcd_checks <- function(ie, ietestcd, suffix, fixed_value_checks_csv_path) {
  target_ie <- c("IETEST", "IECAT", "IEORRES")
  tmp_ie <- ie %>% filter(IETESTCD == ietestcd)
  tmp_ie <- tmp_ie %>% rename_with(~ str_c(.x, suffix), all_of(target_ie))
  str_c(target_ie, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ie, "IE", .x, fixed_value_checks_csv_path))
  tmp_ie %>%
    filter(!!str_c("IEORRES", suffix) != "") %>%
    check_required_vars("IEDTC", domain_name = "IE")
  tmp_ie %>%
    filter(!!str_c("IEORRES", suffix) == "") %>%
    check_blank_vars("IEDTC", domain_name = "IE")
}

ie %>% check_date_before_today("IEDTC", domain_name = "IE")
ie %>% run_ie_testcd_checks("IN01", "_1", fixed_value_checks_csv_path)

# IN02(field17)はvalidate_presence_if(age(ref('registration',1), ref('registration',2)) <18 ||
# age(...)>=65)により、IEORRESが他のIETESTCDと違い全行固定値ではなく条件付きになる
# (年齢が18歳未満または65歳以上の行だけ必須・値は"N"固定、18歳以上65歳未満の行は空欄のはず)。
# IETEST/IECATは他と同様に固定値チェックし、IEORRES/IEDTCはこのブロックで個別に確認する
age_years <- function(birth_date, ref_date) {
  birth_date <- as.Date(birth_date)
  ref_date <- as.Date(ref_date)
  years <- as.integer(format(ref_date, "%Y")) - as.integer(format(birth_date, "%Y"))
  had_birthday <- format(ref_date, "%m-%d") >= format(birth_date, "%m-%d")
  years - as.integer(!had_birthday)
}

tmp_ie_in02 <- ie %>% filter(IETESTCD == "IN02")
tmp_ie_in02_renamed <- tmp_ie_in02 %>% rename_with(~ str_c(.x, "_2"), all_of(c("IETEST", "IECAT")))
c("IETEST_2", "IECAT_2") %>% walk(~ run_value_equals_checks_from_csv(tmp_ie_in02_renamed, "IE", .x, fixed_value_checks_csv_path))

tmp_ie_in02_age <- tmp_ie_in02 %>%
  inner_join(dm %>% select(USUBJID, BRTHDTC, RFICDTC), by = "USUBJID") %>%
  mutate(age_at_consent = age_years(BRTHDTC, RFICDTC))

# 年齢が範囲外(18歳未満または65歳以上): IEORRES必須・値は"N"固定
tmp_ie_in02_out_of_range <- tmp_ie_in02_age %>% filter(age_at_consent < 18 | age_at_consent >= 65)
tmp_ie_in02_out_of_range %>% check_required_vars("IEORRES", domain_name = "IE(IN02)")
tmp_ie_in02_out_of_range %>% rename(IEORRES_2 = IEORRES) %>%
  run_value_equals_checks_from_csv("IE", "IEORRES_2", fixed_value_checks_csv_path)

# 年齢が範囲内(18歳以上65歳未満): IEORRESは空欄のはず
tmp_ie_in02_age %>% filter(age_at_consent >= 18 & age_at_consent < 65) %>%
  check_blank_vars("IEORRES", domain_name = "IE(IN02)")

# IEDTC(確認日)は、他のIETESTCDと同様にIEORRESが入っている行だけ必須・入っていない行は空欄のはず
tmp_ie_in02 %>% filter(IEORRES != "") %>% check_required_vars("IEDTC", domain_name = "IE(IN02)")
tmp_ie_in02 %>% filter(IEORRES == "") %>% check_blank_vars("IEDTC", domain_name = "IE(IN02)")

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
ie %>% run_ie_testcd_checks("EX06", "_14", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX07", "_15", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX08", "_16", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX09", "_17", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX10", "_18", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX11", "_19", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX12", "_20", fixed_value_checks_csv_path)
ie %>% run_ie_testcd_checks("EX13", "_21", fixed_value_checks_csv_path)

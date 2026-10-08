library(here)
rm(list = ls())
source(here("resolve_os_path.R"))

# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、test_config.R(共通)ではなくここで指定する。リポジトリ外の任意の場所でよい
fixed_value_checks_csv_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/stat-forge-test/test4/fixed_value_checks_test4.csv",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\stat-forge-test\\test4\\fixed_value_checks_test4.csv"
)

source(here("test_config.R"))
# test_config.Rはjson_path(他テストとの切り替え用)も定義するが、このファイルは上で固定した
# json_pathを優先して使うため、test_config.R側の値で上書きしないよう再度設定し直す
json_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/stat-forge-test/test4/json/fortest4_260826_1501.json",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\stat-forge-test\\test4\\json\\fortest4_260826_1501.json"
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

# AE(ae1-3 / sae_report1-4共通の重篤性・評価項目)
# AESER(Y/N)/AEACN(6択)/AEREL(2択)/AEOUT(5択)/AETOXGR(1-5)は、sae_report1-4・ae1-3のどちらも
# 同じ値域を持つため、test1〜3と同様にsuffix無しでCSV登録する。重篤性サブ基準(AESDTH/AESLIFE/
# AESHOSP/AESDISAB/AESCONG/AESMIE)は、sae_report1-4ではAESER=="Y"の時のみ必須(=="N"なら空欄)、
# ae1-3ではEDC項目自体が無く常に空欄。
# MedDRAコーディング階層(AETERM/AELLT/AELLTCD/AEDECOD/AEPTCD/AEHLT/AEHLTCD/AEHLGT/AEHLGTCD/
# AEBODSYS/AEBDSYCD/AESOC/AESOCCD)は辞書との整合性確認が必要な別種のチェックのため対象外
tmp_ae_sae <- ae %>% filter(str_detect(AESPID, "^sae_report"))
c("AETERM", "AETOXGR", "AESTDTC", "AESER", "AEACN", "AEREL", "AEOUT", "AEENDTC") %>%
  check_required_vars(tmp_ae_sae, ., domain_name = "AE")
c("AESER", "AETOXGR", "AEACN", "AEREL", "AEOUT") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ae_sae, "AE", .x, fixed_value_checks_csv_path))

target_ae_cols <- c("AESDTH", "AESLIFE", "AESHOSP", "AESDISAB", "AESCONG", "AESMIE")
tmp_ae_sae_y <- tmp_ae_sae %>% filter(AESER == "Y")
target_ae_cols %>% walk(~ run_value_equals_checks_from_csv(tmp_ae_sae_y, "AE", .x, fixed_value_checks_csv_path))
tmp_ae_sae_y %>% check_required_vars(target_ae_cols, domain_name = "AE")
tmp_ae_sae %>% filter(AESER != "Y") %>% check_blank_vars(target_ae_cols, domain_name = "AE")

tmp_ae_plain <- ae %>% filter(str_detect(AESPID, "^ae[0-9]"))
c("AETERM", "AETOXGR", "AESTDTC", "AESER", "AEACN", "AEREL", "AEOUT", "AEENDTC") %>%
  check_required_vars(tmp_ae_plain, ., domain_name = "AE")
c("AESER", "AETOXGR", "AEACN", "AEREL", "AEOUT") %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ae_plain, "AE", .x, fixed_value_checks_csv_path))
tmp_ae_plain %>% check_blank_vars(target_ae_cols, domain_name = "AE")

# CE(有害事象的な単一TERM報告: gvhd1/gvhd2/relapse/secondmg)
# 4シートとも、CETERM/CEPRESPが固定値、CEOCCURが必須という共通パターンを持つ。CEOCCURの許容値は
# gvhd1/gvhd2ではY/N/NA(NAは文字列としての"NA"。GVHD評価未実施等)、relapse/secondmgではY/Nのみ。
# CESTDTCは、gvhd1/gvhd2ではCEOCCURによらず常に必須(評価実施日のため)、relapse/secondmgでは
# CEOCCUR=="Y"の時のみ必須(=="N"なら空欄)
target_ce_cols <- c("CETERM", "CEPRESP")

check_ce_term <- function(ce, cespid, suffix, fixed_value_checks_csv_path, occur_values, cestdtc_required_unconditionally = FALSE) {
  label <- str_c("CE(", cespid, ")")
  tmp_ce <- ce %>% filter(CESPID == cespid)

  tmp_ce_renamed <- tmp_ce %>% rename_with(~ str_c(.x, suffix), all_of(target_ce_cols))
  str_c(target_ce_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ce_renamed, "CE", .x, fixed_value_checks_csv_path))

  tmp_ce %>% check_required_vars("CEOCCUR", domain_name = label)
  tmp_ce %>% check_values_subset_of("CEOCCUR", occur_values, domain_name = label)

  if (cestdtc_required_unconditionally) {
    tmp_ce %>% check_required_vars("CESTDTC", domain_name = label)
  } else {
    ce_occur_y <- tmp_ce %>% filter(CEOCCUR == "Y")
    ce_occur_n <- tmp_ce %>% filter(CEOCCUR == "N")
    ce_occur_y %>% check_required_vars("CESTDTC", domain_name = str_c(label, "(CEOCCUR==Y)"))
    ce_occur_n %>% check_blank_vars("CESTDTC", domain_name = str_c(label, "(CEOCCUR==N)"))
  }

  cat(label, "チェック: OK(", nrow(tmp_ce), "件)\n", sep = "")
}

check_ce_term(ce, "gvhd1", "_1", fixed_value_checks_csv_path, c("Y", "N", "NA"), cestdtc_required_unconditionally = TRUE)
check_ce_term(ce, "gvhd2", "_2", fixed_value_checks_csv_path, c("Y", "N", "NA"), cestdtc_required_unconditionally = TRUE)
check_ce_term(ce, "relapse", "_3", fixed_value_checks_csv_path, c("Y", "N"))
check_ce_term(ce, "secondmg", "_4", fixed_value_checks_csv_path, c("Y", "N"))

# CM(screening_100)
# screening_100の先行治療歴はCMTRT(AZACITIDINE/HYDREA/OTHER PRIOR THERAPY)ごとに3ブロックあり、
# いずれもCMCAT/CMENRTPT/CMENTPT/CMPRESPは固定値、CMOCCUR(Y/N/U)は必須(値は被験者ごとに異なる)
target_cm_cols <- c("CMOCCUR", "CMPRESP", "CMCAT", "CMENRTPT", "CMENTPT")

tmp_cm <- cm %>% filter(CMTRT == "AZACITIDINE")
tmp_cm %>% check_required_vars(target_cm_cols, domain_name = "CM")
suffix <- "_1"
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))

tmp_cm <- cm %>% filter(CMTRT == "HYDREA")
tmp_cm %>% check_required_vars(target_cm_cols, domain_name = "CM")
suffix <- "_2"
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))

tmp_cm <- cm %>% filter(CMTRT == "OTHER PRIOR THERAPY")
tmp_cm %>% check_required_vars(target_cm_cols, domain_name = "CM")
suffix <- "_3"
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))

# CM(抗真菌薬予防投与等の固定パネル)
# induction2/3・consoli1(3visit)・consoli2は、抗真菌薬+GCSFの11項目が同一パネルとして繰り返される。
# 同じCMSPID内には被験者ごとに異なる任意の併用薬(CMCAT/CMENRTPT/CMENTPT/CMPRESPは同じ固定値
# パターンだが薬剤名が多岐にわたる)も混在するため、パネル完全性チェックはCMTRT %in% expected_panelで
# 絞り込んで行う。maint2(4visit)/maint3(4visit)は、それに輸血2種(PLATELETS/RED BLOOD CELLS)を
# 加えた13項目パネルになる。Miconazole(WHO Drugコード6290400)は、full_name_enが辞書上未登録で
# generic_name_enへのフォールバックが無いと生成時にランダムな薬剤名になる不具合があったため、
# other_domains.js/build_domain_common.Rの修正後に追加された項目
cm_antifungal_panel <- c(
  "VORICONAZOLE", "MICAFUNGIN NA", "CASPOFUNGIN ACETATE", "POSACONAZOLE",
  "ISAVUCONAZONIUM SULFATE", "FLUCONAZOLE", "ITRACONAZOLE", "FOSFLUCONAZOLE",
  "GCSF", "OTHER ANTIFUNGAL", "Miconazole"
)
cm_maint_panel <- c(cm_antifungal_panel, "PLATELETS", "RED BLOOD CELLS")

# CM(CMSPID==faspid、CMTRT %in% expected_panel)について、被験者ごとにパネル全項目を最低1件ずつ
# 持つこと(同一薬剤の複数行(用量変更等)を許容するため、行数の完全一致ではなく充足チェックにする)、
# CMCAT/CMENRTPTが固定値であること、CMOCCUR/CMPRESPが必須(値域確認込み)であることを確認する
check_cm_panel <- function(cm, faspid, expected_panel, suffix, fixed_value_checks_csv_path) {
  label <- str_c("CM(", faspid, ")")
  tmp_cm <- cm %>% filter(CMSPID == faspid & CMTRT %in% expected_panel)

  actual_cmtrt <- tmp_cm %>% pull(CMTRT) %>% unique() %>% sort()
  if (!setequal(actual_cmtrt, expected_panel)) {
    stop(str_c(
      label, "パネル項目一致チェック: NG(不足: ", paste(setdiff(expected_panel, actual_cmtrt), collapse = ", "),
      " / 想定外: ", paste(setdiff(actual_cmtrt, expected_panel), collapse = ", "), ")"
    ))
  }
  usubjid_n <- n_distinct(tmp_cm[["USUBJID"]])
  missing_items <- tmp_cm %>% distinct(USUBJID, CMTRT) %>% count(USUBJID) %>% filter(n < length(expected_panel))
  if (nrow(missing_items) > 0) {
    stop(str_c(label, "被験者ごとの項目充足チェック: NG(不足あり: ", paste(missing_items[["USUBJID"]], collapse = ", "), ")"))
  }
  cat(label, "パネル一致チェック: OK(", length(expected_panel), "項目 x ", usubjid_n, "名)\n", sep = "")

  target_cm_panel_cols <- c("CMCAT", "CMENRTPT", "CMENTPT")
  tmp_cm_renamed <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_panel_cols))
  str_c(target_cm_panel_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_cm_renamed, "CM", .x, fixed_value_checks_csv_path))

  tmp_cm %>% check_required_vars(c("CMOCCUR", "CMPRESP"), domain_name = label)
  tmp_cm %>% check_values_subset_of("CMOCCUR", c("Y", "N"), domain_name = label)
  tmp_cm %>% check_values_subset_of("CMPRESP", "Y", domain_name = label)
}

for (faspid in c("induction1_200", "induction2_300", "induction3_500", "consoli1_800", "consoli1_1000", "consoli1_1200", "consoli2_1400")) {
  check_cm_panel(cm, faspid, cm_antifungal_panel, "_4", fixed_value_checks_csv_path)
}
for (faspid in c("maint2_1700", "maint2_1800", "maint2_1900", "maint2_2000", "maint3_2300", "maint3_2400", "maint3_2500", "maint3_2600")) {
  check_cm_panel(cm, faspid, cm_maint_panel, "_5", fixed_value_checks_csv_path)
}

# CM(induction1_200: 化学療法レジメン + 自由入力併用薬)
target_cm_cols <- c("CMTRT", "CMCAT", "CMENRTPT", "CMENTPT", "CMOCCUR", "CMPRESP")
tmp_cm <- cm %>% filter(CMSPID == "induction1_200" &CMCAT == "PRIOR TREATMENT" & CMENTPT == "START OF TREATMENT" & CMENRTPT == "BEFORE")
tmp_cm %>% check_required_vars(target_cm_cols, domain_name = "CM")
suffix <- "_6"
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))

target_cm_cols <- c("CMTRT", "CMCAT")
tmp_cm <- cm %>% filter(CMSPID == "hsct_700" | CMSPID == "hsct_1600" | CMSPID == "hsct_2200" )
tmp_cm %>% check_required_vars(target_cm_cols, domain_name = "CM")
suffix <- "_7"
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))

target_cm_cols <- c("CMTRT", "CMCAT", "CMPRESP", "CMOCCUR", "CMSTDTC")
tmp_cm <- cm %>% filter(CMSPID == "posttrt" )
tmp_cm %>% check_required_vars(target_cm_cols, domain_name = "CM")
"CMSTDTC" %>% check_date_before_today(tmp_cm, ., domain_name = "CM")
"CMENDTC" %>% check_date_after_var_before_today(tmp_cm, ., "CMSTDTC", domain_name = "CM")
target_cm_cols <- c("CMTRT", "CMCAT", "CMPRESP", "CMOCCUR")
suffix <- "_8"
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))
tmp_cm <- tmp_cm %>% filter(CMSPID == "posttrt" & CMENRF != "ONGOING")
"CMENDTC" %>% check_required_vars(tmp_cm, ., domain_name = "CM")

target_cm_cols <- c("CMTRT", "CMCAT", "CMPRESP", "CMOCCUR", "CMSTDTC")
tmp_cm <- cm %>% filter(CMSPID %>% str_detect("posttrt_ad"))
tmp_cm %>% check_required_vars(target_cm_cols, domain_name = "CM")
"CMSTDTC" %>% check_date_before_today(tmp_cm, ., domain_name = "CM")
"CMENDTC" %>% check_date_after_var_before_today(tmp_cm, ., "CMSTDTC", domain_name = "CM")
target_cm_cols <- c("CMTRT", "CMCAT", "CMPRESP", "CMOCCUR")
suffix <- "_8"
tmp_cm <- tmp_cm %>% rename_with(~ str_c(.x, suffix), all_of(target_cm_cols))
str_c(target_cm_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_cm, "CM", .x, fixed_value_checks_csv_path))
tmp_cm <- tmp_cm %>% filter(CMENRF != "ONGOING")
"CMENDTC" %>% check_required_vars(tmp_cm, ., domain_name = "CM")

# DD
# DDTESTCD/DDTESTは固定値(PRCDTH/Primary Cause of Death)、DDORRESは3値、DDRESCATは3値から選択、
# いずれも必須。DDドメインには日付列が無い(死因の詳細のみで、対応する死亡日はDS側が持つ)
target_dd_cols <- c("DDTESTCD", "DDTEST")
dd_orres_values <- c("ADVERSE EVENT", "DEATH OF DISEASE", "OTHER")
dd_rescat_values <- c("NONTREATMENT RELATED", "TREATMENT RELATED", "UNDETERMINED")

suffix <- "_1"
dd_renamed <- dd %>% rename_with(~ str_c(.x, suffix), all_of(target_dd_cols))
str_c(target_dd_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(dd_renamed, "DD", .x, fixed_value_checks_csv_path))

dd %>% check_required_vars(c("DDORRES", "DDRESCAT"), domain_name = "DD")
dd %>% check_values_subset_of("DDORRES", dd_orres_values, domain_name = "DD")
dd %>% check_values_subset_of("DDRESCAT", dd_rescat_values, domain_name = "DD")

# DS(allocation/discon/withdrawal)
# 3シートとも、DSCAT/EPOCHが固定値、DSTERMが許容値セットの範囲内、DSDTCが必須という共通パターンを
# 持つ。DSSTDTCは、allocation(マイルストーン報告)では常に空欄、discon/withdrawal(中止・脱落報告)
# では常に必須
target_ds_cols <- c("DSCAT", "EPOCH")
ds_allocation_term_values <- "RANDOMIZED"
ds_discon_term_values <- c(
  "ADVERSE EVENT", "COMPLETED", "DEATH", "DISCONTINUATION BY PHYSICIAN DECISION",
  "DISCONTINUATION BY SUBJECT", "DISEASE RELAPSE", "FAILURE TO MEET CONTINUATION CRITERIA",
  "LACK OF EFFICACY", "LOST TO FOLLOW-UP", "NON-COMPLIANCE WITH STUDY DRUG", "ONGOING",
  "PREGNANCY", "PROTOCOL DEVIATION", "PROTOCOL-SPECIFIED WITHDRAWAL CRITERION MET",
  "SITE TERMINATED BY SPONSOR", "STUDY TERMINATED BY SPONSOR", "WITHDRAWAL BY PHYSICIAN DECISION",
  "WITHDRAWAL BY SUBJECT"
)
ds_withdrawal_term_values <- c(
  "COMPLETED", "DEATH", "LOST TO FOLLOW-UP", "PROTOCOL-SPECIFIED WITHDRAWAL CRITERION MET",
  "SCREEN FAILURE", "SITE TERMINATED BY SPONSOR", "STUDY TERMINATED BY SPONSOR",
  "WITHDRAWAL BY PHYSICIAN DECISION", "WITHDRAWAL BY SUBJECT"
)

check_ds_disposition <- function(ds, dsspid, suffix, fixed_value_checks_csv_path, term_values, dsstdtc_required) {
  label <- str_c("DS(", dsspid, ")")
  tmp_ds <- ds %>% filter(DSSPID == dsspid)

  tmp_ds_renamed <- tmp_ds %>% rename_with(~ str_c(.x, suffix), all_of(target_ds_cols))
  str_c(target_ds_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ds_renamed, "DS", .x, fixed_value_checks_csv_path))

  tmp_ds %>% check_required_vars(c("DSTERM", "DSDTC"), domain_name = label)
  tmp_ds %>% check_values_subset_of("DSTERM", term_values, domain_name = label)

  if (dsstdtc_required) {
    tmp_ds %>% check_required_vars("DSSTDTC", domain_name = label)
  } else {
    tmp_ds %>% check_blank_vars("DSSTDTC", domain_name = label)
  }

  cat(label, "チェック: OK(", nrow(tmp_ds), "件)\n", sep = "")
}

check_ds_disposition(ds, "allocation", "_1", fixed_value_checks_csv_path, ds_allocation_term_values, dsstdtc_required = FALSE)
check_ds_disposition(ds, "discon", "_2", fixed_value_checks_csv_path, ds_discon_term_values, dsstdtc_required = TRUE)
check_ds_disposition(ds, "withdrawal", "_3", fixed_value_checks_csv_path, ds_withdrawal_term_values, dsstdtc_required = TRUE)

# DD/DS USUBJID整合性チェック
# DDが存在する被験者と、DS(discon/withdrawal)でDSTERM=="DEATH"の被験者が完全に一致すること
# (どちらか一方にしか存在するUSUBJIDが無いこと)を確認する。DDには日付列が無いため日付同士の
# 比較はできない
dd_usubjid <- dd %>% distinct(USUBJID) %>% pull(USUBJID)
ds_death_usubjid <- ds %>% filter(DSTERM == "DEATH") %>% distinct(USUBJID) %>% pull(USUBJID)
only_in_dd <- setdiff(dd_usubjid, ds_death_usubjid)
only_in_ds_death <- setdiff(ds_death_usubjid, dd_usubjid)
if (length(only_in_dd) > 0 || length(only_in_ds_death) > 0) {
  stop(str_c(
    "DD/DS USUBJID整合性チェック: NG(DDのみに存在: ", paste(only_in_dd, collapse = ", "),
    " / DS(DSTERM==DEATH)のみに存在: ", paste(only_in_ds_death, collapse = ", "), ")"
  ))
}
cat("DD/DS USUBJID整合性チェック: OK(", length(dd_usubjid), "名)\n", sep = "")

# EC(ECTRT=="QUIZARTINIB HYDROCHLORIDE"の固定用法・用量パターン)
# induction2_300・induction3_500・maint1・quizartinib1では、QUIZARTINIB HYDROCHLORIDEの投与記録が
# ECMOOD/ECPRESP/ECDOSFRM/ECDOSU/ECROUTEについて共通の固定値パターンを持つ(ECTPT/ECTPTNUM/ECTPTREFは
# maint1のみDAY1・DAY15/MAINTENANCE CYCLE1の値を取り、他は空欄)。induction3_500には別途化学療法
# レジメン(Cytarabine系、択一)の行も混在するが、ECTRTでの絞り込みにより対象から除外される。
# consoli1_800/1000/1200・consoli2_1400は、ECTPTNUM有無でCytarabine(_6)/QUIZARTINIB(_7)の
# 2ブロックに分けて別途対応(下記のconsoli1系ブロックを参照)
target_ec_cols <- c("ECMOOD", "ECPRESP", "ECDOSFRM", "ECDOSU", "ECROUTE")

# EC(ECSPID==ecspid、ECTRT=="QUIZARTINIB HYDROCHLORIDE")について、用法・用量関連項目
# (ECMOOD/ECPRESP/ECDOSFRM/ECDOSU/ECROUTE、tpt_cols_checked=TRUEならECTPT/ECTPTNUM/ECTPTREFも)が
# 固定値であること、ECOCCUR/ECADJが必須であること、ECDOSEがECOCCUR=="Y"の時のみ必須(=="N"なら空欄)
# であることを確認する
check_ec_fixed_drug <- function(ec, ecspid, suffix, fixed_value_checks_csv_path, tpt_cols_checked = TRUE) {
  label <- str_c("EC(", ecspid, ")")
  tmp_ec <- ec %>% filter(ECSPID == ecspid & ECTRT == "QUIZARTINIB HYDROCHLORIDE")

  target_cols <- target_ec_cols
  if (tpt_cols_checked) {
    target_cols <- c(target_cols, "ECTPT", "ECTPTNUM", "ECTPTREF")
  }
  tmp_ec_renamed <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_cols))
  str_c(target_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_ec_renamed, "EC", .x, fixed_value_checks_csv_path))

  tmp_ec %>% check_required_vars(c("ECOCCUR", "ECADJ"), domain_name = label)
  tmp_ec %>% check_values_subset_of("ECOCCUR", c("Y", "N"), domain_name = label)

  ec_occur_y <- tmp_ec %>% filter(ECOCCUR == "Y")
  ec_occur_n <- tmp_ec %>% filter(ECOCCUR == "N")
  ec_occur_y %>% check_required_vars("ECDOSE", domain_name = str_c(label, "(ECOCCUR==Y)"))
  ec_occur_n %>% check_blank_vars("ECDOSE", domain_name = str_c(label, "(ECOCCUR==N)"))

  cat(label, "固定用法・用量チェック: OK(", nrow(tmp_ec), "件)\n", sep = "")
}

check_ec_fixed_drug(ec, "induction2_300", "_1", fixed_value_checks_csv_path)
check_ec_fixed_drug(ec, "induction3_500", "_2", fixed_value_checks_csv_path)
check_ec_fixed_drug(ec, "maint1", "_3", fixed_value_checks_csv_path)
check_ec_fixed_drug(ec, "quizartinib1", "_4", fixed_value_checks_csv_path)

tmp_ec <- ec %>% filter(ECSPID == "induction3_500" & ECOCCUR == "")
tmp_ec %>% check_required_vars(c("ECTRT", "ECSTDTC"), domain_name = "EC")
"ECENDTC" %>% check_required_vars(tmp_ec, ., domain_name = "EC")
target_ec_cols <- c("ECTRT")
suffix <- "_5"
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

target_ec_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECDOSU", "ECDOSFRM", "ECROUTE", "ECTPT", "ECTPTNUM", "ECTPTREF", "ECTRT")
tmp_ec <- ec %>% filter(ECSPID %in% c("consoli1_800", "consoli1_1000", "consoli1_1200", "consoli2_1400") & ECTPTNUM != "")
tmp_ec %>% check_required_vars(target_ec_cols, domain_name = "EC")
suffix <- "_6"
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))
tmp_ec <- tmp_ec %>% filter(ECOCCUR_6 == "Y")
target_ec_cols <- c("ECDOSE", "ECSTDTC")
tmp_ec %>% check_required_vars(target_ec_cols, domain_name = "EC")

target_ec_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECDOSU", "ECDOSFRM", "ECROUTE", "ECTRT", "ECENDTC", "ECADJ")
tmp_ec <- ec %>% filter(ECSPID %in% c("consoli1_800", "consoli1_1000", "consoli1_1200", "consoli2_1400") & ECTPTNUM == "")
tmp_ec %>% check_required_vars(target_ec_cols, domain_name = "EC")
suffix <- "_7"
tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))
target_ec_cols <- c("ECMOOD", "ECPRESP", "ECOCCUR", "ECDOSU", "ECDOSFRM", "ECROUTE", "ECTRT", "ECADJ")
str_c(target_ec_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path))

# DM
c("RFICDTC", "BRTHDTC", "SEX", "RACE", "COUNTRY", "ARM") %>% check_required_vars(dm, ., domain_name = "DM")
dm %>% check_date_before_today(c("BRTHDTC"), domain_name = "DM")
dm %>% check_date_after_var_before_today("RFICDTC", "BRTHDTC", domain_name = "DM")
c("SEX", "RACE", "COUNTRY", "ARM") %>% walk(~ run_value_equals_checks_from_csv(dm, "DM", .x, fixed_value_checks_csv_path))

# FA
# test4のFAは、test3(部位別の腫瘍浸潤チェック)と違い、CTCAE有害事象のグレーディングパネル形式。
# GRADE(重症度Grade0-5)・QHCAUSAL(原因薬剤との因果関係)は、screening/induction/consoli/maintの
# ほぼ全シートで同一の30項目パネル(fa_ae_panel)が繰り返し使われる。OCCUR(次コース施行有無に
# 関連する項目、7または8項目)・LT500/1000OM/LT50000/100000OM(好中球数・血小板数の閾値判定、
# 各1項目)・SEVERITY(慢性GVHDの重症度、1項目)は該当シートのみに存在する。
# FATEST/FACATはFATESTCDごとに固定値(FAOBJによらず同一)なので、suffixもFATESTCDごとに1つでよい
fa <- fa %>% inner_join(dm %>% select(USUBJID, BRTHDTC), by = "USUBJID")
fa %>% check_date_after_var_before_today("FADTC", "BRTHDTC", domain_name = "FA")

fa_ae_panel <- c(
  "Abdominal pain", "Alanine aminotransferase increased", "Anemia",
  "Aspartate aminotransferase increased", "Bacteremia", "Blood bilirubin increased",
  "Decreased appetite", "Diarrhea", "Electrocardiogram QT corrected interval prolonged",
  "Febrile neutropenia", "Fungal infection", "Headache", "Herpes virus infection",
  "Hypokalemia", "Intracranial hemorrhage", "Lower gastrointestinal hemorrhage",
  "Lymphocyte count decreased", "Nausea", "Neutrophil count decreased",
  "Oedema", "Platelet count decreased", "Pneumonia", "Pneumonitis",
  "Pulmonary haemorrhage", "Rash", "Sepsis", "Upper gastrointestinal hemorrhage",
  "Upper respiratory infection", "Vomiting", "White blood cell decreased"
)
fa_occur_panel_7 <- c(
  "Bone Marrow Suppression", "CNS involvement", "Hematopoietic Stem Cell Transplantation",
  "Infection", "Non-hematological Toxicity", "Other", "Physician Decision"
)
fa_occur_panel_8 <- c(fa_occur_panel_7, "Performed Maitenance Theraphy")

grade_values <- as.character(0:5)
qhcausal_values <- c("RELATED", "NOT RELATED", "NOT APPLICABLE")
occur_values <- c("Y", "N")
severity_values <- c("Mild", "Moderate", "Severe")

# FA(FASPID==faspid & FATESTCD==fatestcd)について、FAOBJがexpected_faobj(パネル)と被験者ごとに
# 過不足なく一致すること、FATEST/FACATが固定値であること、FAORRESが必須であること
# (orres_values指定時はFAORRESの値域も)を確認する。
# FADTCは、GRADE/OCCUR/QHCAUSAL/SEVERITYでは常に空欄(fadtc_required_when=NULL、既定値)だが、
# LT500等の閾値判定4項目ではFAORRESが特定の値(例: "Y")のときだけ必須になるため、
# fadtc_required_whenにその値を指定する
check_fa_panel <- function(fa, faspid, fatestcd, expected_faobj, suffix, fixed_value_checks_csv_path,
                           orres_values = NULL, fadtc_required_when = NULL) {
  label <- str_c("FA(", faspid, "/", fatestcd, ")")
  tmp_fa <- fa %>% filter(FASPID == faspid & FATESTCD == fatestcd)

  actual_faobj <- tmp_fa %>% pull(FAOBJ) %>% unique() %>% sort()
  if (!setequal(actual_faobj, expected_faobj)) {
    stop(str_c(
      label, "パネル項目一致チェック: NG(不足: ", paste(setdiff(expected_faobj, actual_faobj), collapse = ", "),
      " / 想定外: ", paste(setdiff(actual_faobj, expected_faobj), collapse = ", "), ")"
    ))
  }
  usubjid_n <- n_distinct(tmp_fa[["USUBJID"]])
  if (nrow(tmp_fa) != usubjid_n * length(expected_faobj)) {
    stop(str_c(
      label, "件数チェック: NG(", nrow(tmp_fa), "件 / 期待値: ",
      usubjid_n * length(expected_faobj), "件 = ", usubjid_n, "名 x ", length(expected_faobj), "項目)"
    ))
  }
  cat(label, "パネル一致チェック: OK(", length(expected_faobj), "項目 x ", usubjid_n, "名)\n", sep = "")

  target_fa_cols <- c("FATEST", "FACAT")
  tmp_fa_renamed <- tmp_fa %>% rename_with(~ str_c(.x, suffix), all_of(target_fa_cols))
  str_c(target_fa_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_fa_renamed, "FA", .x, fixed_value_checks_csv_path))

  tmp_fa %>% check_required_vars("FAORRES", domain_name = label)
  if (!is.null(orres_values)) {
    tmp_fa %>% check_values_subset_of("FAORRES", orres_values, domain_name = label)
  }
  if (is.null(fadtc_required_when)) {
    tmp_fa %>% check_blank_vars("FADTC", domain_name = label)
  } else {
    tmp_fa %>% filter(FAORRES == fadtc_required_when) %>% check_required_vars("FADTC", domain_name = label)
    tmp_fa %>% filter(FAORRES != fadtc_required_when) %>% check_blank_vars("FADTC", domain_name = label)
  }
}

# screening/induction1: GRADEのみ(検査値・QHCAUSAL・OCCURは無し)
check_fa_panel(fa, "screening_100", "GRADE", fa_ae_panel, "_1", fixed_value_checks_csv_path, orres_values = grade_values)
check_fa_panel(fa, "induction1_200", "GRADE", fa_ae_panel, "_1", fixed_value_checks_csv_path, orres_values = grade_values)

# induction2/3・consoli1(3visit)・consoli2: GRADE+QHCAUSAL+好中球数/血小板数の閾値判定
for (faspid in c("induction2_300", "induction3_500", "consoli1_800", "consoli1_1000", "consoli1_1200", "consoli2_1400")) {
  check_fa_panel(fa, faspid, "GRADE", fa_ae_panel, "_1", fixed_value_checks_csv_path, orres_values = grade_values)
  check_fa_panel(fa, faspid, "QHCAUSAL", fa_ae_panel, "_2", fixed_value_checks_csv_path, orres_values = qhcausal_values)
  check_fa_panel(fa, faspid, "LT500", "Neutrophil count decreased", "_4", fixed_value_checks_csv_path, orres_values = occur_values, fadtc_required_when = "Y")
  check_fa_panel(fa, faspid, "1000OM", "Neutrophil count decreased", "_5", fixed_value_checks_csv_path, orres_values = occur_values, fadtc_required_when = "Y")
  check_fa_panel(fa, faspid, "LT50000", "Platelet count decreased", "_6", fixed_value_checks_csv_path, orres_values = occur_values, fadtc_required_when = "Y")
  check_fa_panel(fa, faspid, "100000OM", "Platelet count decreased", "_7", fixed_value_checks_csv_path, orres_values = occur_values, fadtc_required_when = "Y")
}
# OCCURはinduction2/3が7項目、consoli1(3visit)が8項目("Performed Maitenance Theraphy"が追加)で
# パネルが異なる。consoli2にはOCCUR自体が無い
check_fa_panel(fa, "induction2_300", "OCCUR", fa_occur_panel_7, "_3", fixed_value_checks_csv_path, orres_values = occur_values)
check_fa_panel(fa, "induction3_500", "OCCUR", fa_occur_panel_7, "_3", fixed_value_checks_csv_path, orres_values = occur_values)
check_fa_panel(fa, "consoli1_800", "OCCUR", fa_occur_panel_8, "_3", fixed_value_checks_csv_path, orres_values = occur_values)
check_fa_panel(fa, "consoli1_1000", "OCCUR", fa_occur_panel_8, "_3", fixed_value_checks_csv_path, orres_values = occur_values)
check_fa_panel(fa, "consoli1_1200", "OCCUR", fa_occur_panel_8, "_3", fixed_value_checks_csv_path, orres_values = occur_values)

# maint2(4visit)/maint3(4visit): GRADE+QHCAUSALのみ(検査値・OCCURは無し)
for (faspid in c("maint2_1700", "maint2_1800", "maint2_1900", "maint2_2000", "maint3_2300", "maint3_2400", "maint3_2500", "maint3_2600")) {
  check_fa_panel(fa, faspid, "GRADE", fa_ae_panel, "_1", fixed_value_checks_csv_path, orres_values = grade_values)
  check_fa_panel(fa, faspid, "QHCAUSAL", fa_ae_panel, "_2", fixed_value_checks_csv_path, orres_values = qhcausal_values)
}

# gvhd1(急性GVHDのGrade)/gvhd2(慢性GVHDのSeverity): それぞれ単一項目パネル
check_fa_panel(fa, "gvhd1", "GRADE", "Acute Graft Versus Host Disease", "_1", fixed_value_checks_csv_path, orres_values = grade_values)
check_fa_panel(fa, "gvhd2", "SEVERITY", "Chronic Graft Versus Host Disease", "_8", fixed_value_checks_csv_path, orres_values = severity_values)

# ae1〜ae3・sae_report1〜4: AE報告に連動する動的なQHCAUSALブロック。FAOBJは実際に報告されたAE名が
# そのまま入るため固定パネルではなく、該当AE報告が無い行はFAOBJ/FAORRESとも空欄になる
fa_ae_linked_faspid <- c("ae1", "ae2", "ae3", "sae_report1", "sae_report2", "sae_report3", "sae_report4")
for (faspid in fa_ae_linked_faspid) {
  label <- str_c("FA(", faspid, ")")
  tmp_fa <- fa %>% filter(FASPID == faspid)
  tmp_fa_reported <- tmp_fa %>% filter(FAOBJ != "")
  tmp_fa_reported %>% check_required_vars("FAORRES", domain_name = label)
  tmp_fa_reported %>% check_values_subset_of("FAORRES", qhcausal_values, domain_name = label)
  tmp_fa %>% filter(FAOBJ == "") %>% check_blank_vars("FAORRES", domain_name = label)
}

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
  if (tmp_ie %>%
    filter(!!str_c("IEORRES", suffix) == "") %>% nrow() > 0) {
    stop(str_c(ietestcd, "エラー"))
  }

}

ie %>% check_date_before_today("IEDTC", domain_name = "IE")
ie %>% run_ie_testcd_checks("IN01", "_1", fixed_value_checks_csv_path)

# IN02(field17)はvalidate_presence_if(age(ref('registration',1), ref('registration',2)) <18 ||
# age(...)>=65)により、IEORRESが他のIETESTCDと違い全行固定値ではなく条件付きになる
# (年齢が18歳未満または65歳以上の行だけ必須・値は"N"固定、18歳以上65歳未満の行は存在しないのが正)。
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
if (tmp_ie_in02_age %>% filter(age_at_consent >= 18 & age_at_consent < 65) %>% nrow() > 0) {
  stop("IE02エラー1")
}

# IEDTC(確認日)は、他のIETESTCDと同様にIEORRESが入っている行だけ必須・入っていない行は空欄のはず
tmp_ie_in02 %>% filter(IEORRES != "") %>% check_required_vars("IEDTC", domain_name = "IE(IN02)")
if (tmp_ie_in02 %>% filter(IEORRES == "") %>% nrow() > 0) {
  stop("IE02エラー2")
}

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

# LB
lb %>% check_date_before_today("LBDTC", domain_name = "LB")
tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == "CYEXAM")
target_lb_cols <- c("LBTEST", "LBCAT", "LBMETHOD")
tmp_lb %>% check_required_vars(target_lb_cols, domain_name = "lb")
suffix <- "_1"
tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))
target_lb_cols <- "LBORRES"
tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == "CYEXAM" & LBSTAT == "NOT DONE")
target_lb_cols %>% check_blank_vars(tmp_lb, ., domain_name = "LB")
tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == "CYEXAM" & LBSTAT != "NOT DONE")
tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))

# 染色体異常チェックリスト項目(LBTESTCD、例: T_8_21)1件分の確認。
# LBTEST/LBCAT/LBMETHOD/LBORRESの固定値チェックに加え、この項目のレコードを持つUSUBJIDと、
# CYEXAM(染色体検査本体)がNOT DONE以外・結果がNORMAL/ABNORMALのいずれかで確定しているUSUBJIDが
# 完全に一致すること(両方向)を確認する
check_lb_chromosomal_abnormality <- function(lb, lbtestcd, suffix, fixed_value_checks_csv_path) {
  target_lb_cols <- c("LBTEST", "LBCAT", "LBMETHOD", "LBORRES")
  tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == lbtestcd)
  tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))

  tmp_lb_usubjid <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == lbtestcd) %>% select(USUBJID)
  tmp_cyexam_usubjid <- lb %>%
    filter(LBSPID == "screening_100" & LBTESTCD == "CYEXAM" & LBSTAT != "NOT DONE" & (LBORRES == "NORMAL" | LBORRES == "ABNORMAL")) %>%
    select(USUBJID)

  if (tmp_lb_usubjid %>% anti_join(tmp_cyexam_usubjid, by = "USUBJID") %>% nrow() > 0) {
    stop(str_c("ERROR ", lbtestcd, "-1"))
  }
  if (tmp_cyexam_usubjid %>% anti_join(tmp_lb_usubjid, by = "USUBJID") %>% nrow() > 0) {
    stop(str_c("ERROR ", lbtestcd, "-2"))
  }
}

check_lb_chromosomal_abnormality(lb, "T_8_21", "_2", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "INV16Q22", "_3", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "T_16_16", "_4", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "T911Q233", "_5", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "TV11Q233", "_6", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "INV3Q262", "_7", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "T33Q262", "_8", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "T122Q133", "_9", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "T69Q341", "_10", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "T_9_22", "_11", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "T_8_16", "_12", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "AMLOTRRT", "_13", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "MSY5", "_14", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "DEL5Q", "_15", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "DEL7Q", "_16", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "TRISOM8", "_17", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "MSY7", "_18", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "ISOCH17Q", "_19", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "DEL20Q", "_20", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "CTA_3KM", "_21", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "OTHCHALT", "_22", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "IDICXQ13", "_23", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "INCDEL12", "_24", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "INCT5Q", "_25", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "INCDEL17", "_26", fixed_value_checks_csv_path)
check_lb_chromosomal_abnormality(lb, "T3Q262MR", "_27", fixed_value_checks_csv_path)

tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == "FLT3_TKD")
target_lb_cols <- c("LBTEST", "LBCAT", "LBORRES", "LBSPEC", "LBDTC", "LBMETHOD")
tmp_lb %>% check_required_vars(target_lb_cols, domain_name = "LB")
target_lb_cols <- c("LBTEST", "LBCAT", "LBORRES", "LBSPEC", "LBMETHOD")
suffix <- "_28"
tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))

tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == "FL3TKDSR")
target_lb_cols <- c("LBTEST", "LBCAT", "LBMETHOD")
suffix <- "_29"
tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))
tmp_lb_not_done <- tmp_lb %>% filter(LBSTAT == "NOT DONE")
target_lb_cols <- c("LBREASND")
tmp_lb_not_done <- tmp_lb_not_done %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb_not_done, "LB", .x, fixed_value_checks_csv_path))
tmp_lb_done <- tmp_lb %>% filter(LBSTAT != "NOT DONE")
tmp_lb_2 <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == "FLT3_TKD" & LBORRES == "POSITIVE") %>% select("USUBJID")
tmp_lb <- tmp_lb_done %>% inner_join(tmp_lb_2, by="USUBJID")
c("LBORRES", "LBDTC") %>% check_required_vars(tmp_lb, ., domain_name = "LB")

tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == "FL3ITDSR")
target_lb_cols <- c("LBTEST", "LBCAT", "LBMETHOD")
suffix <- "_30"
tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))
tmp_lb_not_done <- tmp_lb %>% filter(LBSTAT == "NOT DONE")
target_lb_cols <- c("LBREASND")
target_lb_cols %>% check_required_vars(tmp_lb_not_done, ., domain_name="LB")
tmp_lb_not_done <- tmp_lb_not_done %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb_not_done, "LB", .x, fixed_value_checks_csv_path))
tmp_lb_done <- tmp_lb %>% filter(LBSTAT != "NOT DONE")
c("LBORRES", "LBDTC") %>% check_required_vars(tmp_lb_done, ., domain_name = "LB")

tmp_lb <- lb %>% filter(LBSPID == "screening_100" & LBTESTCD == "NPM1MUT")
target_lb_cols <- c("LBTEST", "LBCAT", "LBMETHOD")
suffix <- "_31"
tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))
tmp_lb_done <- tmp_lb %>% filter(LBSTAT != "NOT DONE")
c("LBORRES", "LBDTC", "LBSPEC") %>% check_required_vars(tmp_lb_done, ., domain_name = "LB")
target_lb_cols <- c("LBORRES", "LBSPEC")
tmp_lb <- tmp_lb_done %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
str_c(target_lb_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path))

# LB(LBTESTCD==lbtestcd)について、LBTEST/LBCAT/LBORRESU/LBSPECの固定値チェックと、
# LBSTAT!="NOT DONE"の行だけLBORRES/LBDTCが必須であることを確認する
check_lb_testcd_fixed_values <- function(lb, lbtestcd, suffix, fixed_value_checks_csv_path, target_lb_cols = c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC")) {
  tmp_lb <- lb %>% filter(LBTESTCD == lbtestcd)
  tmp_lb_renamed <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_lb_renamed, "LB", .x, fixed_value_checks_csv_path))
  tmp_lb %>% filter(LBSTAT != "NOT DONE") %>% check_required_vars(c("LBORRES", "LBDTC"), domain_name = "LB")
}

check_lb_testcd_fixed_values(lb, "WBC", "_32", fixed_value_checks_csv_path)
check_lb_testcd_fixed_values(lb, "BLASTLE", "_33", fixed_value_checks_csv_path)
check_lb_testcd_fixed_values(lb, "MYBLALE", "_34", fixed_value_checks_csv_path, target_lb_cols = c("LBTEST", "LBCAT", "LBORRESU", "LBSPEC", "LBMETHOD"))

# MH
tmp_mh <- mh %>% filter(MHSPID == "screening_100")
tmp_mh <- tmp_mh %>% filter(MHTERM == "Acute myeloid leukaemia")
tmp_mh %>% check_required_vars(c("MHTERM","MHSTDTC"), domain_name = "MH")
target_mh_cols <- c("MHCAT", "MHPRESP", "MHOCCUR")
suffix <- "_1"
tmp_mh <- tmp_mh %>% rename_with(~ str_c(.x, suffix), all_of(target_mh_cols))
str_c(target_mh_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path))

tmp_mh <- mh %>% filter(MHSPID == "screening_100")
tmp_mh <- tmp_mh %>% filter(MHTERM != "Acute myeloid leukaemia")
tmp_mh %>% check_required_vars(c("MHTERM", "MHCAT", "MHSCAT", "MHENRTPT", "MHENTPT"), domain_name = "MH")
target_mh_cols <- c("MHCAT", "MHSCAT", "MHENRTPT", "MHENTPT")
suffix <- "_2"
tmp_mh <- tmp_mh %>% rename_with(~ str_c(.x, suffix), all_of(target_mh_cols))
str_c(target_mh_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_mh, "MH", .x, fixed_value_checks_csv_path))

# MI
target_mi_cols <- c("MITESTCD", "MITEST", "MICAT", "MIORRES", "MIENRTPT", "MIENTPT")
mi %>% check_required_vars(target_mi_cols, domain_name = "MI")
target_mi_cols %>%
  walk(~ run_value_equals_checks_from_csv(mi, "MI", .x, fixed_value_checks_csv_path))

# RS
target_rs_cols <- c("RSORRES", "RSTEST", "RSCAT")
rs %>% check_required_vars(target_rs_cols, domain_name = "RS")
tmp_rs <- rs %>% filter(RSTESTCD == "ETIOCLN")
suffix <- "_1"
tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, suffix), all_of(target_rs_cols))
str_c(target_rs_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))

tmp_rs <- rs %>% filter(RSTESTCD == "ECOG101")
suffix <- "_2"
target_rs_cols <- c("RSORRES", "RSTEST", "RSCAT")
tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, suffix), all_of(target_rs_cols))
str_c(target_rs_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))

# RS(RSTESTCD=="OVRLRESP"の効果判定パネル)
# evaluation1_400等9シートは、いずれもRSTESTCD=="OVRLRESP"1項目のみを持ち、RSTEST/RSCAT/RSEVALが
# 全シート共通の固定値、RSORRESが5値{CR, CRh, CRi, NE, NON-CR/NON-PD}のいずれか、RSORRES/RSDTCが
# 必須という共通パターンを持つため、1つの関数・1つのsuffixで対応する
target_rs_ovrlresp_cols <- c("RSTEST", "RSCAT", "RSEVAL")
rs_ovrlresp_values <- c("CR", "CRh", "CRi", "NE", "NON-CR/NON-PD")

check_rs_ovrlresp <- function(rs, rsspid, suffix, fixed_value_checks_csv_path) {
  label <- str_c("RS(", rsspid, ")")
  tmp_rs <- rs %>% filter(RSSPID == rsspid & RSTESTCD == "OVRLRESP")

  tmp_rs_renamed <- tmp_rs %>% rename_with(~ str_c(.x, suffix), all_of(target_rs_ovrlresp_cols))
  str_c(target_rs_ovrlresp_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_rs_renamed, "RS", .x, fixed_value_checks_csv_path))

  tmp_rs %>% check_required_vars(c("RSORRES", "RSDTC"), domain_name = label)
  tmp_rs %>% check_values_subset_of("RSORRES", rs_ovrlresp_values, domain_name = label)

  cat(label, "効果判定チェック: OK(", nrow(tmp_rs), "件)\n", sep = "")
}

for (rsspid in c(
  "evaluation1_400", "evaluation1_600", "evaluation2_650", "evaluation1_900",
  "evaluation1_1100", "evaluation1_1300", "evaluation1_1500",
  "evaluation2_1550", "evaluation2_2100"
)) {
  check_rs_ovrlresp(rs, rsspid, "_3", fixed_value_checks_csv_path)
}

# PR(移植報告: hsct_700/1600/2200)
# 3シートとも、PRTRT(移植方法3択)/PRCAT(RELATED/UNRELATED)/PRSCAT(Haplo/Matched/Mismatched)が
# 同一の選択肢セットを持ち、PRSTDTCが必須という共通パターンを持つ
pr_hsct_trt_values <- c(
  "Allogeneic Bone Marrow Transplantation",
  "Allogeneic Cord Blood Transplantation",
  "Allogeneic Peripheral Stem Cell Transplantation"
)
pr_hsct_cat_values <- c("RELATED", "UNRELATED")
pr_hsct_scat_values <- c("Haplo", "Matched", "Mismatched")

check_pr_hsct <- function(pr, prspid) {
  label <- str_c("PR(", prspid, ")")
  tmp_pr <- pr %>% filter(PRSPID == prspid)
  tmp_pr %>% check_required_vars(c("PRTRT", "PRCAT", "PRSCAT", "PRSTDTC"), domain_name = label)
  tmp_pr %>% check_values_subset_of("PRTRT", pr_hsct_trt_values, domain_name = label)
  tmp_pr %>% check_values_subset_of("PRCAT", pr_hsct_cat_values, domain_name = label)
  tmp_pr %>% check_values_subset_of("PRSCAT", pr_hsct_scat_values, domain_name = label)
  cat(label, "移植報告チェック: OK(", nrow(tmp_pr), "件)\n", sep = "")
}
for (prspid in c("hsct_700", "hsct_1600", "hsct_2200")) {
  check_pr_hsct(pr, prspid)
}

# PR(discon: 試験治療の完了/中止報告)
# PRTRT/PROCCUR/PRPRESP/PRSTRTPT/PRSTTPTは固定値、PRREASNDは3値の理由コードから1つ、
# PRSTDTC/PRENDTCは常に空欄
target_pr_discon_cols <- c("PRTRT", "PROCCUR", "PRPRESP", "PRSTRTPT", "PRSTTPT")
pr_discon_reasnd_values <- c("Not available donor", "Physician decision", "Subject's request")

tmp_pr <- pr %>% filter(PRSPID == "discon")
tmp_pr %>% check_required_vars("PRREASND", domain_name = "PR(discon)")
tmp_pr %>% check_values_subset_of("PRREASND", pr_discon_reasnd_values, domain_name = "PR(discon)")
tmp_pr %>% check_blank_vars(c("PRSTDTC", "PRENDTC"), domain_name = "PR(discon)")

suffix <- "_1"
tmp_pr <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_discon_cols))
str_c(target_pr_discon_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_pr, "PR", .x, fixed_value_checks_csv_path))

# PR(後療法報告: postpr/postpr_ad1-3)
# PRCAT="AFTER THERAPY"・PROCCUR="Y"・PRPRESP="Y"が固定値、PRTRTは3択、PRSTDTCが必須、
# PRENDTCが空欄の行はPRENRF=="ONGOING"、入力されている行はPRENRFが空欄という相関を持つ
target_pr_postpr_cols <- c("PRCAT", "PROCCUR", "PRPRESP")
pr_postpr_trt_values <- c("Hematopoietic Stem Cell Transplantation", "Radiation Therapy", "Other")

check_pr_postpr <- function(pr, prspid, suffix, fixed_value_checks_csv_path) {
  label <- str_c("PR(", prspid, ")")
  tmp_pr <- pr %>% filter(PRSPID == prspid)

  tmp_pr_renamed <- tmp_pr %>% rename_with(~ str_c(.x, suffix), all_of(target_pr_postpr_cols))
  str_c(target_pr_postpr_cols, suffix) %>%
    walk(~ run_value_equals_checks_from_csv(tmp_pr_renamed, "PR", .x, fixed_value_checks_csv_path))

  tmp_pr %>% check_required_vars(c("PRTRT", "PRSTDTC"), domain_name = label)
  tmp_pr %>% check_values_subset_of("PRTRT", pr_postpr_trt_values, domain_name = label)

  ongoing <- tmp_pr %>% filter(PRENRF == "ONGOING")
  not_ongoing <- tmp_pr %>% filter(PRENRF == "")
  if (nrow(ongoing) + nrow(not_ongoing) != nrow(tmp_pr)) {
    stop(str_c(label, ": PRENRFが想定外の値です"))
  }
  ongoing %>% check_blank_vars("PRENDTC", domain_name = str_c(label, "(PRENRF==ONGOING)"))
  not_ongoing %>% check_required_vars("PRENDTC", domain_name = str_c(label, "(PRENRF==\"\")"))

  cat(label, "後療法報告チェック: OK(", nrow(tmp_pr), "件)\n", sep = "")
}
for (prspid in c("postpr", "postpr_ad1", "postpr_ad2", "postpr_ad3")) {
  check_pr_postpr(pr, prspid, "_2", fixed_value_checks_csv_path)
}

# VS
"VSDTC" %>% check_date_before_today(vs, ., domain_name="VS")
c("VSORRES", "VSDTC") %>% check_required_vars(filter(vs, VSSTAT != "NOT DONE"), ., domain_name = "VS")
target_vs_cols <- c("VSORRESU", "VSTEST")
tmp_vs <- vs %>% filter(VSTESTCD == "HEIGHT")
suffix <- "_1"
tmp_vs <- tmp_vs %>% rename_with(~ str_c(.x, suffix), all_of(target_vs_cols))
str_c(target_vs_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_vs, "VS", .x, fixed_value_checks_csv_path))
tmp_vs <- vs %>% filter(VSTESTCD == "WEIGHT")
suffix <- "_2"
tmp_vs <- tmp_vs %>% rename_with(~ str_c(.x, suffix), all_of(target_vs_cols))
str_c(target_vs_cols, suffix) %>%
  walk(~ run_value_equals_checks_from_csv(tmp_vs, "VS", .x, fixed_value_checks_csv_path))


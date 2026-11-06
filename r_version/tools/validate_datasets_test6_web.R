library(here)
rm(list = ls())
source(here("resolve_os_path.R"))

# check_value_equals(固定値チェック)用のCSV設定ファイルのパス。内容(チェックしたい固定値)は
# 試験ごとに異なるため、test_config.R(共通)ではなくここで指定する。リポジトリ外の任意の場所でよい
fixed_value_checks_csv_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/stat-forge-test/test6/fixed_value_checks_test6.csv",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\stat-forge-test\\test6\\fixed_value_checks_test6.csv"
)

source(here("test_config.R"))
# test_config.Rはjson_path(他テストとの切り替え用)も定義するが、このファイルは上で固定した
# json_pathを優先して使うため、test_config.R側の値で上書きしないよう再度設定し直す
json_path <- resolve_os_path(
  "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/stat-forge-test/test6/json/fortest6_261028_1626.json",
  "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\stat-forge-test\\test6\\json\\fortest6_261028_1626.json"
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

# test6個別チェック
# ここにドメインごとのチェックを追加していく(参考: tools/validate_datasets_test1_web.R〜test5_web.R)
# data(domainドメイン)のtarget列(suffix付き)が、fixed_value_checks_csv_pathの期待値と一致することを確認する
run_target_value_checks <- function(data, domain) {
  str_c(target, suffix) %>% walk(~ run_value_equals_checks_from_csv(data, domain, .x, fixed_value_checks_csv_path))
}

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
  # T02〜T05は不一致になるため、仕様が直るまではエラーではなくwarningにする
  tmp_tu_1 <- tu %>% filter(TUSPID == "screening_100" & TULNKID == trlnkid)
  tr_only <- tmp_tr_1 %>% anti_join(tmp_tu_1, by = c("USUBJID", "TRMETHOD" = "TUMETHOD", "TRDTC" = "TUDTC"))
  tu_only <- tmp_tu_1 %>% anti_join(tmp_tr_1, by = c("USUBJID", "TUMETHOD" = "TRMETHOD", "TUDTC" = "TRDTC"))
  if (nrow(tr_only) > 0 | nrow(tu_only) > 0) {
    warning(str_c(
      label, ": TU/TRリンクチェック: 警告(TUと一致しないTR: ", nrow(tr_only), "件 ", paste(tr_only[["USUBJID"]], collapse = ", "),
      " / TRと一致しないTU: ", nrow(tu_only), "件 ", paste(tu_only[["USUBJID"]], collapse = ", "), ")"
    ))
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

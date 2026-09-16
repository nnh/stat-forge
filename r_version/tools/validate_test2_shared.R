# validate_datasets_test2.R(R版データを比較元にする)とvalidate_datasets_test2_web.R
# (Webツール生成のCSVを比較元にする)で共通の処理。
# 呼び出し元スクリプトが ae/dm/ds/other_domains/cdisc_variable_values/registration_n/who_drug_idf/
# json_path/discontinuation_date を用意した上でこのファイルをsourceすること

# cycle2以降(ec2〜ec65・lab2〜lab65)のVISITNUM一覧。両シートは1対1でサイクルに対応しており
# VISITNUMも共通のため、EC・VSどちらのrun_..._checks_all_cycles()からも参照する。各値は
# JSON(fortest2_260826_1501.json)のec1〜ec65/lab1〜lab65シートのVisit Numberフィールドの
# default_valueから取得したもの(100刻みだが一部200飛びの箇所がある、test2専用の固定リスト)
cycle2_onward_visitnums <- c(
  300, 400, 500, 700, 800, 900, 1000, 1200, 1300, 1400, 1500, 1700, 1800, 1900, 2000,
  2200, 2300, 2400, 2500, 2700, 2800, 2900, 3000, 3200, 3300, 3400, 3500, 3700, 3800, 3900, 4000,
  4200, 4300, 4400, 4500, 4700, 4800, 4900, 5000, 5200, 5300, 5400, 5500, 5700, 5800, 5900, 6000,
  6200, 6300, 6400, 6500, 6700, 6800, 6900, 7000, 7200, 7300, 7400, 7500, 7700, 7800, 7900, 8000, 8200
)

# IE: IETESTCDごとに、IETEST/IECAT/IEORRESをsuffix付き列名にリネームしたうえで
# 固定値チェック(CSV)とIEDTCの要否(IEORRESが空でなければ必須、空なら空欄)を確認する
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

# LB: LBTESTCDごとの個別チェック(妊娠検査(HCG)を除く通常パターン)。指定visitのレコードに絞り込み、
# LBTEST/LBCAT/LBSPEC(+has_blflならLBBLFL、has_unitなら単位LBORRESU)をsuffix付き列名にリネームした
# うえで固定値と一致することを確認する。has_unit=FALSEを指定すると、単位を持たない項目(定性検査等)
# としてLBORRESUのチェックを除外する。has_blfl=FALSEを指定すると、Baseline Flagが定義されていない
# visitの項目としてLBBLFLのチェックを除外する
check_lb_testcd <- function(lb_done, lbtestcd, visit, suffix, fixed_value_checks_csv_path, has_unit = TRUE, has_blfl = TRUE) {
  target_lb_cols <- c("LBTEST", "LBCAT", "LBSPEC")
  if (has_blfl) {
    target_lb_cols <- c(target_lb_cols, "LBBLFL")
  }
  if (has_unit) {
    target_lb_cols <- c(target_lb_cols, "LBORRESU")
  }
  tmp_lb <- lb_done %>% filter(LBTESTCD == lbtestcd & VISITNUM == visit)
  tmp_lb <- tmp_lb %>% rename_with(~ str_c(.x, suffix), all_of(target_lb_cols))
  str_c(target_lb_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_lb, "LB", .x, fixed_value_checks_csv_path, visit = visit))
}

# LB: cycle2以降(VISITNUM>=300)の1visitnum分の通常検査パネル(29項目)個別チェックをまとめて実行する。
# GLUCは血液(LBCAT=="CHEMISTRY")と尿(LBCAT=="URINALYSIS")の2項目があり、同じTESTCDでも
# カテゴリが異なるため、lb_doneをLBCATで絞り込んでから渡す(PROT/OCCBLDも尿検査のみ)(test2専用)
run_lb_testcd_checks_cycle2_onward <- function(lb_done, visitnum, fixed_value_checks_csv_path) {
  check_lb_testcd(lb_done, "RBC", visitnum, "_4", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "HGB", visitnum, "_5", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "HCT", visitnum, "_6", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "WBC", visitnum, "_7", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "NEUT", visitnum, "_8", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "PLAT", visitnum, "_9", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "ALB", visitnum, "_10", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "SODIUM", visitnum, "_11", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "K", visitnum, "_12", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "CL", visitnum, "_13", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "CA", visitnum, "_14", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "PHOS", visitnum, "_15", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(filter(lb_done, LBCAT == "CHEMISTRY"), "GLUC", visitnum, "_16", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "UREAN", visitnum, "_17", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "CYURIAC", visitnum, "_18", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "CREAT", visitnum, "_19", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "BILI", visitnum, "_20", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "AST", visitnum, "_21", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "ALT", visitnum, "_22", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "ALP", visitnum, "_23", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "LDH", visitnum, "_24", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "HBA1C", visitnum, "_25", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "CHOL", visitnum, "_26", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "TRIG", visitnum, "_27", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(lb_done, "INR", visitnum, "_28", fixed_value_checks_csv_path, has_unit = FALSE, has_blfl = FALSE)
  check_lb_testcd(lb_done, "APTT", visitnum, "_29", fixed_value_checks_csv_path, has_blfl = FALSE)
  check_lb_testcd(filter(lb_done, LBCAT == "URINALYSIS"), "PROT", visitnum, "_34", fixed_value_checks_csv_path, has_unit = FALSE, has_blfl = FALSE)
  check_lb_testcd(filter(lb_done, LBCAT == "URINALYSIS"), "GLUC", visitnum, "_35", fixed_value_checks_csv_path, has_unit = FALSE, has_blfl = FALSE)
  check_lb_testcd(filter(lb_done, LBCAT == "URINALYSIS"), "OCCBLD", visitnum, "_36", fixed_value_checks_csv_path, has_unit = FALSE, has_blfl = FALSE)
}

# LB: lab2〜lab65(通常検査パネルを持つ全シート)のVISITNUMについてrun_lb_testcd_checks_cycle2_onward()を
# 実行する。各値はJSON(fortest2_260826_1501.json)のlab2〜lab65シートのVisit Numberフィールドの
# default_valueを取得したもの(ec2〜ec65と同じ値、test2専用の固定リスト)
run_lb_testcd_checks_all_cycles <- function(lb_done, fixed_value_checks_csv_path) {
  lb_visitnums <- c(
    300, 400, 500, 700, 800, 900, 1000, 1200, 1300, 1400, 1500, 1700, 1800, 1900, 2000,
    2200, 2300, 2400, 2500, 2700, 2800, 2900, 3000, 3200, 3300, 3400, 3500, 3700, 3800, 3900, 4000,
    4200, 4300, 4400, 4500, 4700, 4800, 4900, 5000, 5200, 5300, 5400, 5500, 5700, 5800, 5900, 6000,
    6200, 6300, 6400, 6500, 6700, 6800, 6900, 7000, 7200, 7300, 7400, 7500, 7700, 7800, 7900, 8000, 8200
  )
  walk(lb_visitnums, ~ run_lb_testcd_checks_cycle2_onward(lb_done, .x, fixed_value_checks_csv_path))
}

# VS: VSTESTCD×VISITNUM(×VSTPTNUM)ごとの個別チェック。指定visit(/vstptnum)のレコードに絞り込み、
# VSTEST/VSORRESU(+has_blflならVSBLFL)をsuffix付き列名にリネームしたうえで固定値と一致することを
# 確認する。test2のVSTPTNUM==10のブロックはBaseline Flag(VSBLFL)が定義されているが、
# VSTPTNUM==20のブロックには定義が無いため、has_blflで含める/除外するを切り替える。
# WEIGHTのようにVSTPTNUM自体が定義されていない項目ではvstptnum=NULL(既定値)を指定し、
# VSTPTNUMによる絞り込み・メッセージ表示を行わない
check_vs_testcd <- function(vs_done, vstestcd, visit, suffix, fixed_value_checks_csv_path, vstptnum = NULL, has_blfl = TRUE) {
  target_vs_cols <- c("VSTEST", "VSORRESU")
  if (has_blfl) {
    target_vs_cols <- c(target_vs_cols, "VSBLFL")
  }
  tmp_vs <- vs_done %>% filter(VSTESTCD == vstestcd & VISITNUM == visit)
  if (!is.null(vstptnum)) {
    tmp_vs <- tmp_vs %>% filter(VSTPTNUM == vstptnum)
  }
  tmp_vs <- tmp_vs %>% rename_with(~ str_c(.x, suffix), all_of(target_vs_cols))
  extra_label <- if (is.null(vstptnum)) NULL else str_c("VSTPTNUM=", vstptnum)
  str_c(target_vs_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_vs, "VS", .x, fixed_value_checks_csv_path, visit = visit, extra_label = extra_label))
}

# VS: cycle2以降(VISITNUM>=300)の1visitnum分の個別チェックをまとめて実行する。WEIGHT(VSTPTNUMが
# 定義されていないためvstptnum省略)と、TEMP/PULSE/SYSBP/DIABP(VSTPTNUM=10・20の両方)分の
# check_vs_testcd()呼び出しをまとめたもの(test2専用)
run_vs_testcd_checks_cycle2_onward <- function(vs_done, visitnum, fixed_value_checks_csv_path) {
  check_vs_testcd(vs_done, "WEIGHT", visitnum, "_2", fixed_value_checks_csv_path, has_blfl = FALSE)
  for (vstptnum in c(10, 20)) {
    check_vs_testcd(vs_done, "TEMP", visitnum, "_3", fixed_value_checks_csv_path, vstptnum = vstptnum, has_blfl = FALSE)
    check_vs_testcd(vs_done, "PULSE", visitnum, "_4", fixed_value_checks_csv_path, vstptnum = vstptnum, has_blfl = FALSE)
    check_vs_testcd(vs_done, "SYSBP", visitnum, "_5", fixed_value_checks_csv_path, vstptnum = vstptnum, has_blfl = FALSE)
    check_vs_testcd(vs_done, "DIABP", visitnum, "_6", fixed_value_checks_csv_path, vstptnum = vstptnum, has_blfl = FALSE)
  }
}

# VS: cycle2以降(lab2〜lab65)全てのVISITNUMについてrun_vs_testcd_checks_cycle2_onward()を実行する
# (test2専用。VISITNUM一覧はcycle2_onward_visitnums参照。EC側のec1〜ec65と同じサイクル・VISITNUM対応)
run_vs_testcd_checks_all_cycles <- function(vs_done, fixed_value_checks_csv_path) {
  walk(cycle2_onward_visitnums, ~ run_vs_testcd_checks_cycle2_onward(vs_done, .x, fixed_value_checks_csv_path))
}

# EC: ECTRT(×ECROUTE)×VISITNUMごとの個別チェック。指定条件で絞り込み、ECDOSU/ECROUTE/ECADJを
# suffix付き列名にリネームしたうえで、ECDOSU/ECROUTEは固定値チェック、ECADJはECOCCUR=="Y"の
# 行に限定して固定値チェックする。VISITNUMはfilter条件自体で保証済みのため固定値チェックの対象に
# 含めない(サイクルごとにVISITNUMが変わるrun_ec_trt_checks_all_cycles()から呼ぶため)。
# ecrouteを指定した場合はその値でも絞り込み、ECROUTEは絞り込み条件自体で保証済みのため
# 固定値チェックの対象から除く(test2専用)
check_ec_trt <- function(ec, ectrt, visitnum, suffix, fixed_value_checks_csv_path, ecroute = NULL) {
  target_ec_cols <- c("ECDOSU", "ECROUTE", "ECADJ")
  tmp_ec <- ec %>% filter(ECTRT == ectrt & VISITNUM == visitnum)
  if (!is.null(ecroute)) {
    tmp_ec <- tmp_ec %>% filter(ECROUTE == ecroute)
  }
  tmp_ec <- tmp_ec %>% rename_with(~ str_c(.x, suffix), all_of(target_ec_cols))

  extra_label <- if (is.null(ecroute)) ectrt else str_c(ectrt, "/", ecroute)
  value_equals_cols <- if (is.null(ecroute)) c("ECDOSU", "ECROUTE") else c("ECDOSU")
  str_c(value_equals_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_ec, "EC", .x, fixed_value_checks_csv_path, extra_label = extra_label))
  str_c("ECADJ", suffix) %>% walk(~ run_value_equals_checks_from_csv(filter(tmp_ec, ECOCCUR == "Y"), "EC", .x, fixed_value_checks_csv_path, extra_label = extra_label))
}

# EC: cycle1(ec1、VISITNUM=200)時点の5剤(ベバシズマブ/オキサリプラチン/レボホリナート/
# 5-FU(急速静注)/5-FU(持続静注))分のcheck_ec_trt()呼び出しをまとめて実行する。ECADJ(投与状況)の
# 許容値がcycle2以降と異なる(cycle1はまだ減量していないためLevel1/Level2が無い)ため、
# suffix(_1〜_5)をcycle2以降(run_ec_trt_checks_cycle2_onward、_6〜_10)と分けている(test2専用)
run_ec_trt_checks_cycle1 <- function(ec, visitnum, fixed_value_checks_csv_path) {
  check_ec_trt(ec, "BEVACIZUMAB(GENETICAL RECOMBINATION)", visitnum, "_1", fixed_value_checks_csv_path)
  check_ec_trt(ec, "OXALIPLATIN", visitnum, "_2", fixed_value_checks_csv_path)
  check_ec_trt(ec, "LEVOFOLINATE CALCIUM", visitnum, "_3", fixed_value_checks_csv_path)
  check_ec_trt(ec, "5-FU", visitnum, "_4", fixed_value_checks_csv_path, ecroute = "INTRAVENOUS BOLUS")
  check_ec_trt(ec, "5-FU", visitnum, "_5", fixed_value_checks_csv_path, ecroute = "INTRAVENOUS DRIP")
}

# EC: cycle2以降(ec2〜ec65)時点の5剤分のcheck_ec_trt()呼び出しをまとめて実行する。
# run_ec_trt_checks_cycle1()とはsuffix(_6〜_10)を分けており、fixed_value_checks_test2.csv側で
# ECADJの許容値をcycle1と別に(Level1/Level2を含む形で)登録できるようにしている(test2専用)
run_ec_trt_checks_cycle2_onward <- function(ec, visitnum, fixed_value_checks_csv_path) {
  check_ec_trt(ec, "BEVACIZUMAB(GENETICAL RECOMBINATION)", visitnum, "_6", fixed_value_checks_csv_path)
  check_ec_trt(ec, "OXALIPLATIN", visitnum, "_7", fixed_value_checks_csv_path)
  check_ec_trt(ec, "LEVOFOLINATE CALCIUM", visitnum, "_8", fixed_value_checks_csv_path)
  check_ec_trt(ec, "5-FU", visitnum, "_9", fixed_value_checks_csv_path, ecroute = "INTRAVENOUS BOLUS")
  check_ec_trt(ec, "5-FU", visitnum, "_10", fixed_value_checks_csv_path, ecroute = "INTRAVENOUS DRIP")
}

# EC: ec1〜ec65(サイクルごとの投与記録シート)全てのVISITNUMについて、cycle1はrun_ec_trt_checks_cycle1()、
# cycle2以降はrun_ec_trt_checks_cycle2_onward()を実行する(test2専用。VISITNUM一覧はcycle2_onward_visitnums参照)
run_ec_trt_checks_all_cycles <- function(ec, fixed_value_checks_csv_path) {
  run_ec_trt_checks_cycle1(ec, 200, fixed_value_checks_csv_path)
  walk(cycle2_onward_visitnums, ~ run_ec_trt_checks_cycle2_onward(ec, .x, fixed_value_checks_csv_path))
}

# RS: 指定visitnum時点のRSTESTCD=="TRGRESP"(_2)/"NTRGRESP"(_3)/"OVRLRESP"(_4)の3種類について、
# RSTEST/RSCAT/RSORRES/RSEVALをsuffix付き列名にリネームしたうえで固定値と一致することを確認する。
# RSLNKGRPは別途VISITNUM×RSLNKGRP対応チェックで確認するため、ここでは対象に含めない(test2専用)
run_rs_testcd_checks <- function(rs, visitnum, fixed_value_checks_csv_path) {
  target_rs_cols <- c("RSTEST", "RSCAT", "RSORRES", "RSEVAL")

  tmp_rs <- rs %>% filter(RSTESTCD == "TRGRESP" & VISITNUM == visitnum)
  tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, "_2"), all_of(target_rs_cols))
  str_c(target_rs_cols, "_2") %>% walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))

  tmp_rs <- rs %>% filter(RSTESTCD == "NTRGRESP" & VISITNUM == visitnum)
  tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, "_3"), all_of(target_rs_cols))
  str_c(target_rs_cols, "_3") %>% walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))

  tmp_rs <- rs %>% filter(RSTESTCD == "OVRLRESP" & VISITNUM == visitnum)
  tmp_rs <- tmp_rs %>% rename_with(~ str_c(.x, "_4"), all_of(target_rs_cols))
  str_c(target_rs_cols, "_4") %>% walk(~ run_value_equals_checks_from_csv(tmp_rs, "RS", .x, fixed_value_checks_csv_path))
}

# CM: CMSPID=="baseline1"のブロックについて、CMOCCURとCMENDTC/CMSTDTCの関係を確認する
# (1) CMOCCUR=="Y"ならCMENDTCに値がある
# (2) CMOCCUR=="N"ならCMENDTCは空白
# (3) CMOCCURの値に関わらずCMSTDTCは常に空白(baseline1はCMSTDTCを定義していないため)
check_cm_baseline1 <- function(data, dm, cdisc_variable_values) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  baseline1 <- data %>% filter(CMSPID == "baseline1")

  missing_endtc_y <- baseline1 %>%
    filter(CMOCCUR == "Y", is.na(CMENDTC) | CMENDTC == "") %>%
    pull(USUBJID)
  add_check(
    "cmendtc_present_when_occur_Y",
    length(missing_endtc_y) == 0,
    str_c("CMENDTCが空: ", paste(missing_endtc_y, collapse = ", "))
  )

  present_endtc_n <- baseline1 %>%
    filter(CMOCCUR == "N", !is.na(CMENDTC) & CMENDTC != "") %>%
    pull(USUBJID)
  add_check(
    "cmendtc_blank_when_occur_N",
    length(present_endtc_n) == 0,
    str_c("CMENDTCに値あり: ", paste(present_endtc_n, collapse = ", "))
  )

  present_stdtc <- baseline1 %>%
    filter(!is.na(CMSTDTC) & CMSTDTC != "") %>%
    pull(USUBJID)
  add_check(
    "cmstdtc_always_blank",
    length(present_stdtc) == 0,
    str_c("CMSTDTCに値あり: ", paste(present_stdtc, collapse = ", "))
  )

  # CMSPIDが"concomitant_drug_other"で始まる場合はCMDECODが空白、
  # "concomitant_drug"で始まり"other"を含まない場合はCMDECODが空白でないはず
  concomitant_drug_other <- data %>% filter(str_starts(CMSPID, "concomitant_drug_other"))
  present_decod_other <- concomitant_drug_other %>%
    filter(!is.na(CMDECOD) & CMDECOD != "") %>%
    pull(USUBJID)
  add_check(
    "cmdecod_blank_for_concomitant_drug_other",
    length(present_decod_other) == 0,
    str_c("CMDECODに値あり: ", paste(present_decod_other, collapse = ", "))
  )

  # WHO Drug/IDF側にgeneric_name_enが1件も無い薬剤(カテゴリ名など)は、CMDECODが空になるのが
  # 正しい挙動のため、判定対象から除く。who_drug_idfは呼び出し元スクリプトのトップレベルで
  # 定義済みの変数をそのまま参照する(クロージャ)
  has_generic_name <- who_drug_idf %>%
    filter(!is.na(generic_name_en)) %>%
    pull(full_name_en) %>%
    unique()

  concomitant_drug_main <- data %>%
    filter(str_starts(CMSPID, "concomitant_drug"), !str_detect(CMSPID, "other"), CMTRT %in% has_generic_name)
  missing_decod_main <- concomitant_drug_main %>%
    filter(is.na(CMDECOD) | CMDECOD == "") %>%
    pull(USUBJID)
  add_check(
    "cmdecod_present_for_concomitant_drug",
    length(missing_decod_main) == 0,
    str_c("CMDECODが空: ", paste(missing_decod_main, collapse = ", "))
  )

  final <- bind_rows(results)
  if (all(final[["passed"]])) {
    cat("CM baseline1チェック: 問題なし(", nrow(final), "件PASS)\n")
  } else {
    cat("CM baseline1チェック:", sum(!final[["passed"]]), "件NG\n")
  }
  final
}

# TR: 同一USUBJID・LNKID(病変を紐づけるID、TULNKID<->TRLNKID)・VISITNUM(同じ訪問)であれば、
# TRDTC(腫瘍評価日)とTUDTC(腫瘍同定日)が一致するはずであることを確認する。
# TUは病変の同定(baseline)を1回だけ記録し、TRはその後の複数回のフォローアップ評価を含むため、
# 単純にUSUBJIDだけで突き合わせると別訪問同士を比較してしまい、日付が異なって当然のケースを
# 誤検知してしまう。LNKID・VISITNUMも含めて突き合わせることで、同じ訪問の記録同士だけを比較する。
# other_domainsは呼び出し元スクリプトのトップレベルで定義済みの変数をそのまま参照する(クロージャ)
check_tr_tu_dtc <- function(data, dm, cdisc_variable_values) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  tu_dtc <- other_domains[["TU"]] %>% distinct(USUBJID, TULNKID, VISITNUM, TUDTC)
  tr_dtc <- data %>% distinct(USUBJID, TRLNKID, VISITNUM, TRDTC)

  mismatch <- tr_dtc %>%
    inner_join(tu_dtc, by = c("USUBJID", "TRLNKID" = "TULNKID", "VISITNUM")) %>%
    filter(TRDTC != TUDTC)

  add_check(
    "trdtc_matches_tudtc (同一USUBJID・LNKID・VISITNUM)",
    nrow(mismatch) == 0,
    str_c("不一致: ", nrow(mismatch), "件(USUBJID: ", paste(unique(mismatch[["USUBJID"]]), collapse = ", "), ")")
  )

  final <- bind_rows(results)
  if (all(final[["passed"]])) {
    cat("TR/TU DTCチェック: 問題なし(", nrow(final), "件PASS)\n")
  } else {
    cat("TR/TU DTCチェック:", sum(!final[["passed"]]), "件NG\n")
  }
  final
}

# other_domainsのうち、この試験で特に確認したいprefixがあれば、ここにprefix -> チェック関数を追加する
other_domains_special_checks <- list(CM = check_cm_baseline1, TR = check_tr_tu_dtc)

# 比較対象のCSVファイルを格納しているディレクトリ(直下のCSVを全て読み込む)。
# json_pathのファイル名ごとにcsv_dir_by_file(tools/validate_common.R)で管理する
csv_dir <- csv_dir_by_file[[basename(json_path)]]

validation <- run_full_validation(ae, dm, ds, other_domains, cdisc_variable_values, registration_n, csv_dir, other_domains_special_checks, discontinuation_date)

# AE/DM/DSを除いた、両方に共通して存在するドメイン名一覧。以下の1行ずつ実行するとき、
# この並び順の「何番目」かを指定する
generated_datasets <- validation[["generated_datasets"]]
datasets <- validation[["datasets"]]
common_names <- setdiff(intersect(names(generated_datasets), names(datasets)), special_domain_names)
common_names
common_names %>% length()

# ここから1行ずつ実行して、ドメインの中身を1つずつ目視確認する(View()が2枚(生成データ/CSV)開く)。
# 必要な数だけ行をコピーしてindexを変えて追加していく
# compare_domain_by_index(generated_datasets, datasets, 1, exclude = special_domain_names)

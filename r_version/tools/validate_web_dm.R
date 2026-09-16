library(here)

# Web版(JS)のDMドメイン生成が、R版(いつも通りの完全なパイプライン)と一致しているかを確認する。
#
# 比較元(R版)として、先にtest_config.R(json_path)とload_edc_spec.Rをsourceし、
# load_edc_spec(json_path)を実行してdm/cdisc_variable_values/sheets/sheet_groupsを
# 作成しておくこと。テストファイルを切り替えたいときはtest_config.Rのjson_pathを書き換える。
# (source()より前に行うこと。後だと読み込んだ関数まで削除されてしまう)。
# ae/ds/discontinuation_date/presence_conditions/meddraはこのファイルでは使わないが、
# tools/run_web_validation.Rでvalidate_web_ae.Rと連続実行する際に消えてしまわないよう残す
rm(list = setdiff(ls(), c("dm", "ae", "ds", "other_domains", "cdisc_variable_values", "sheets", "sheet_groups", "presence_conditions", "meddra", "discontinuation_date")))

source(here("test_config.R"))
source(here("tools/validate_common.R"))
source(here("tools/validate_dm.R"))

# Webツールで同じJSONを読み込み、被験者数・登録開始日をload_edc_spec.R側(registration_n/
# registration_start_date)と合わせて生成し、「DM_dummy.csvをダウンロード」したものを
# dm_web_csv_path(test_config.R)に指定しておくこと
dm_web <- read_csv(dm_web_csv_path, col_types = cols(.default = "c"), na = character(0))

# 年齢整合性チェック(RFICDTC: 同意日がBRTHDTC: 生年月日からmin_age〜max_age歳の範囲内か)用に、
# 使用したテストJSONファイル名を求める(age_bounds_by_fileでファイルごとの許容範囲を切り替える)。
# json_pathはtest_config.Rで管理する
json_file_name <- json_path %>% basename()
json_file_name

age_bounds_by_file <- list(
  "fortest1_260826_1112.json" = list(min_age = 20, max_age = NA),
  "fortest2_260826_1501.json" = list(min_age = 20, max_age = 80),
  "fortest3_260826_1452.json" = list(min_age = NA, max_age = NA),
  "fortest4_260826_1501.json" = list(min_age = NA, max_age = NA)
)

# defaultグループの割り付けシートが持つcode一覧(+空欄)を返す。ARMの妥当な値の範囲として使う
valid_arm_codes <- function(sheets, sheet_groups) {
  default_alias <- sheet_groups %>%
    keep(~ coalesce(.x[["is_default"]], FALSE)) %>%
    map(~ map_chr(.x[["sheets"]], "alias_name")) %>%
    unlist() %>%
    unique()
  default_allocation_alias <- sheets %>%
    keep(~ identical(.x[["category"]], "allocation") && .x[["alias_name"]] %in% default_alias) %>%
    map_chr(~ .x[["alias_name"]])
  if (length(default_allocation_alias) == 0) {
    return("")
  }
  sheets %>%
    keep(~ .x[["alias_name"]] %in% default_allocation_alias) %>%
    map(~ .x[["allocation"]][["groups"]]) %>%
    purrr::flatten() %>%
    map_chr(~ .x[["code"]]) %>%
    union("")
}

# 列名の一致を確認する
cat("--- 列名の一致 ---\n")
only_r <- setdiff(colnames(dm), colnames(dm_web))
only_web <- setdiff(colnames(dm_web), colnames(dm))
if (length(only_r) == 0 && length(only_web) == 0) {
  cat("一致\n")
} else {
  cat("Rのみ:", if (length(only_r) > 0) paste(only_r, collapse = ", ") else "(なし)", "\n")
  cat("Webのみ:", if (length(only_web) > 0) paste(only_web, collapse = ", ") else "(なし)", "\n")
}

# ARM値の妥当性を確認する
cat("--- ARM値の妥当性 ---\n")
arm_codes <- valid_arm_codes(sheets, sheet_groups)
invalid_arm_r <- setdiff(unique(dm[["ARM"]]), arm_codes)
invalid_arm_web <- setdiff(unique(dm_web[["ARM"]]), arm_codes)
cat("R版 範囲外:", if (length(invalid_arm_r) > 0) paste(invalid_arm_r, collapse = ", ") else "なし", "\n")
cat("Web版 範囲外:", if (length(invalid_arm_web) > 0) paste(invalid_arm_web, collapse = ", ") else "なし", "\n")

# RFICDTC(同意日)がBRTHDTC(生年月日)からmin_age〜max_age歳の範囲内かを確認する
# (Rのage_bounds/apply_age_date_bounds()に対応する制約が、生成データ上で守られているかのチェック)
validate_age_consistency <- function(dm_data, min_age, max_age) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  if (!all(c("BRTHDTC", "RFICDTC") %in% colnames(dm_data))) {
    add_check("age_at_consent_in_range", TRUE, "BRTHDTC/RFICDTC列が無いためスキップ")
    return(bind_rows(results))
  }

  brthdtc <- as.Date(dm_data[["BRTHDTC"]])
  rficdtc <- as.Date(dm_data[["RFICDTC"]])
  valid_pair <- !is.na(brthdtc) & !is.na(rficdtc)
  age_years <- as.numeric(rficdtc - brthdtc) / 365.25

  if (any(valid_pair)) {
    cat(
      "  データ内の年齢(同意時点): 最小=", round(min(age_years[valid_pair]), 1), "歳, 最大=",
      round(max(age_years[valid_pair]), 1), "歳\n",
      sep = ""
    )
  }

  below_min <- valid_pair & !is.na(min_age) & (age_years < min_age)
  above_max <- valid_pair & !is.na(max_age) & (age_years > max_age)

  range_label <- str_c(
    if (is.na(min_age)) "下限なし" else str_c(min_age, "歳以上"), "〜",
    if (is.na(max_age)) "上限なし" else str_c(max_age, "歳以下")
  )
  add_check(
    "age_at_consent_in_range",
    !any(below_min) && !any(above_max),
    str_c("許容範囲: ", range_label, " / 下限未満: ", sum(below_min), "件, 上限超過: ", sum(above_max), "件")
  )

  bind_rows(results)
}

age_bounds <- age_bounds_by_file[[json_file_name]]
if (is.null(age_bounds)) {
  stop(str_c("json_file_name「", json_file_name, "」はage_bounds_by_fileに未登録です"))
}

# R版・Web版それぞれについて、コードリスト範囲内・日付妥当性・年齢整合性を確認する
# (tools/validate_dm.Rを再利用し、年齢整合性チェックを追加する)
cat("--- R版DMのバリデーション(validate_dm) ---\n")
report_dm_validation(bind_rows(
  validate_dm(dm, cdisc_variable_values, nrow(dm)),
  validate_age_consistency(dm, age_bounds[["min_age"]], age_bounds[["max_age"]])
))

cat("--- Web版DMのバリデーション(validate_dm) ---\n")
report_dm_validation(bind_rows(
  validate_dm(dm_web, cdisc_variable_values, nrow(dm_web)),
  validate_age_consistency(dm_web, age_bounds[["min_age"]], age_bounds[["max_age"]])
))

# R版とWeb版を列ごとに直接比較する。R版はpresence_conditions等のゲーティングで空欄になる行が
# あり得るのに対し、Web版はまだそれらを未移植のため一度も空欄にならない、といった差が
# あれば、それがWeb側の未実装箇所を示すのでFAILとして検出する(validate_dm()は「許容範囲内か」
# しか見ないため、「R版にはあるがWeb版に無いパターン」までは検出できない。ここで直接比較する)
compare_r_web_dm <- function(dm_r, dm_web) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  ignore_cols <- c("STUDYID", "DOMAIN", "USUBJID", "SUBJID", "SITEID", "BRTHDTC")
  common_cols <- setdiff(intersect(colnames(dm_r), colnames(dm_web)), ignore_cols)

  for (col in common_cols) {
    # R側はDate型の列がある(as.character()で明示的に変換してから空欄/NA判定する。
    # Date型のまま""と比較するとas.Date("")のパース失敗で全行NAになってしまうため)
    r_col <- as.character(dm_r[[col]])
    web_col <- as.character(dm_web[[col]])
    r_has_blank <- any(is.na(r_col) | r_col == "")
    web_has_blank <- any(is.na(web_col) | web_col == "")
    detail <- str_c("R版に空欄あり=", r_has_blank, " / Web版に空欄あり=", web_has_blank)
    if (r_has_blank && !web_has_blank) {
      detail <- str_c(detail, "(R版は空欄になる場合があるのにWeb版は一度も空欄にならない: ",
        "presence_conditions等によるゲーティングがWeb側で未実装の可能性)")
    }
    add_check(str_c("blank_pattern_match: ", col), r_has_blank == web_has_blank, detail)
  }

  bind_rows(results)
}

report_r_web_comparison <- function(results) {
  print(results, n = nrow(results))
  n_fail <- sum(!results[["passed"]])
  if (n_fail == 0) {
    cat("R/Web比較: 全", nrow(results), "件PASS\n")
  } else {
    stop(str_c("R/Web比較: ", n_fail, "件FAIL(", paste(results[["check"]][!results[["passed"]]], collapse = ", "), ")"))
  }
}

cat("--- R版とWeb版の直接比較 ---\n")
report_r_web_comparison(compare_r_web_dm(dm, dm_web))

library(here)

# Web版(JS)のAEドメイン生成が、R版(いつも通りの完全なパイプライン)と一致しているかを確認する。
#
# 比較元(R版)として、先にtest_config.R(json_path)とload_edc_spec.Rをsourceし、
# load_edc_spec(json_path)を実行してae/dm/cdisc_variable_valuesを作成しておくこと。
# テストファイルを切り替えたいときはtest_config.Rのjson_pathを書き換える。
# (source()より前に行うこと。後だと読み込んだ関数まで削除されてしまう)。
# sheets/sheet_groups/dsはこのファイルでは使わないが、tools/run_web_validation.Rで
# validate_web_dm.R・validate_web_ds.Rと連続実行する際に消えてしまわないよう残す。
# presence_conditions/meddraは必須LLTコードチェック(validate_ae()内)で使う
rm(list = setdiff(ls(), c("ae", "dm", "ds", "other_domains", "cdisc_variable_values", "sheets", "sheet_groups", "presence_conditions", "meddra", "discontinuation_date")))

source(here("test_config.R"))
source(here("tools/validate_common.R"))
source(here("tools/validate_ae.R"))

# Webツールで同じJSONを読み込み、被験者数・AEレコード数・登録開始日をload_edc_spec.R側
# (registration_n/registration_start_date、AEはbuild_ae_domain()のデフォルトn=100)と合わせて生成し、
# 「AE_dummy.csvをダウンロード」したものを ae_web_csv_path(test_config.R)に指定しておくこと
ae_web <- read_csv(ae_web_csv_path, col_types = cols(.default = "c"), na = character(0))

# 列名の一致を確認する
cat("--- 列名の一致 ---\n")
only_r <- setdiff(colnames(ae), colnames(ae_web))
only_web <- setdiff(colnames(ae_web), colnames(ae))
if (length(only_r) == 0 && length(only_web) == 0) {
  cat("一致\n")
} else {
  cat("Rのみ:", if (length(only_r) > 0) paste(only_r, collapse = ", ") else "(なし)", "\n")
  cat("Webのみ:", if (length(only_web) > 0) paste(only_web, collapse = ", ") else "(なし)", "\n")
}

# R版・Web版それぞれについて、コードリスト範囲内・日付妥当性・AESTDTC<=AEENDTC等を確認する
# (tools/validate_ae.Rを再利用)
cat("--- R版AEのバリデーション(validate_ae) ---\n")
report_ae_validation(validate_ae(ae, dm, cdisc_variable_values, presence_conditions, meddra))

cat("--- Web版AEのバリデーション(validate_ae) ---\n")
report_ae_validation(validate_ae(ae_web, dm, cdisc_variable_values, presence_conditions, meddra))

# R版とWeb版を列ごとに直接比較する。R版はpresence_conditions等のゲーティングで空欄になる行が
# あり得るのに対し、Web版で対応するゲーティングが未実装/不完全だと一度も空欄にならない、といった差が
# あれば、それがWeb側の未実装箇所を示すのでFAILとして検出する(validate_ae()は「許容範囲内か」
# しか見ないため、「R版にはあるがWeb版に無いパターン」までは検出できない。ここで直接比較する)
compare_r_web_ae <- function(ae_r, ae_web) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  # STUDYID/DOMAIN/USUBJIDは共通識別子、AESEQ/AESPIDは行ごとに一意な採番のため、
  # 空欄パターンの比較対象から除く
  ignore_cols <- c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AESPID")
  common_cols <- setdiff(intersect(colnames(ae_r), colnames(ae_web)), ignore_cols)

  for (col in common_cols) {
    # R側はDate型の列がある(as.character()で明示的に変換してから空欄/NA判定する)
    r_col <- as.character(ae_r[[col]])
    web_col <- as.character(ae_web[[col]])
    r_has_blank <- any(is.na(r_col) | r_col == "")
    web_has_blank <- any(is.na(web_col) | web_col == "")
    detail <- str_c("R版に空欄あり=", r_has_blank, " / Web版に空欄あり=", web_has_blank)
    if (r_has_blank && !web_has_blank) {
      detail <- str_c(detail, "(R版は空欄になる場合があるのにWeb版は一度も空欄にならない: ",
        "presence_conditions等によるゲーティングがWeb側で未実装/不完全な可能性)")
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
report_r_web_comparison(compare_r_web_ae(ae, ae_web))

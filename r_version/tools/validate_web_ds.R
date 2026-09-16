library(here)

# Web版(JS)のDSドメイン生成が、R版(いつも通りの完全なパイプライン)と一致しているかを確認する。
#
# 比較元(R版)として、先にtest_config.R(json_path)とload_edc_spec.Rをsourceし、
# load_edc_spec(json_path)を実行してds/dm/cdisc_variable_valuesを作成しておくこと。
# テストファイルを切り替えたいときはtest_config.Rのjson_pathを書き換える。
# (source()より前に行うこと。後だと読み込んだ関数まで削除されてしまう)
rm(list = setdiff(ls(), c("ds", "dm", "ae", "other_domains", "cdisc_variable_values", "discontinuation_date")))

source(here("test_config.R"))
source(here("tools/validate_common.R"))
source(here("tools/validate_ds.R"))

# Webツールで同じJSONを読み込み、被験者数・登録開始日をload_edc_spec.R側(registration_n/
# registration_start_date)と合わせて生成し、「DS_dummy.csvをダウンロード」したものを
# ds_web_csv_path(test_config.R)に指定しておくこと
ds_web <- read_csv(ds_web_csv_path, col_types = cols(.default = "c"), na = character(0))

# 列名の一致を確認する
cat("--- 列名の一致 ---\n")
only_r <- setdiff(colnames(ds), colnames(ds_web))
only_web <- setdiff(colnames(ds_web), colnames(ds))
if (length(only_r) == 0 && length(only_web) == 0) {
  cat("一致\n")
} else {
  cat("Rのみ:", if (length(only_r) > 0) paste(only_r, collapse = ", ") else "(なし)", "\n")
  cat("Webのみ:", if (length(only_web) > 0) paste(only_web, collapse = ", ") else "(なし)", "\n")
}

# R版・Web版それぞれについて、USUBJIDカバレッジ・DEATH重複無し・コードリスト範囲内・日付妥当性等を
# 確認する(tools/validate_ds.Rを再利用)
cat("--- R版DSのバリデーション(validate_ds) ---\n")
report_ds_validation(validate_ds(ds, dm, cdisc_variable_values))

cat("--- Web版DSのバリデーション(validate_ds) ---\n")
report_ds_validation(validate_ds(ds_web, dm, cdisc_variable_values))

# R版とWeb版を列ごとに直接比較する。R版はpresence_conditions等のゲーティングで空欄になる行が
# あり得るのに対し、Web版で対応するゲーティングが未実装/不完全だと一度も空欄にならない、といった差が
# あれば、それがWeb側の未実装箇所を示すのでFAILとして検出する(validate_ds()は「許容範囲内か」
# しか見ないため、「R版にはあるがWeb版に無いパターン」までは検出できない。ここで直接比較する)
compare_r_web_ds <- function(ds_r, ds_web) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  # STUDYID/DOMAIN/USUBJIDは共通識別子、DSSEQ/DSSPIDは行ごとに一意な採番のため、
  # 空欄パターンの比較対象から除く
  ignore_cols <- c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSSPID")
  common_cols <- setdiff(intersect(colnames(ds_r), colnames(ds_web)), ignore_cols)

  for (col in common_cols) {
    # R側はDate型の列がある(as.character()で明示的に変換してから空欄/NA判定する)
    r_col <- as.character(ds_r[[col]])
    web_col <- as.character(ds_web[[col]])
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
report_r_web_comparison(compare_r_web_ds(ds, ds_web))

library(here)

# Web版(JS)のother_domains(DM/AE/DS以外の各ドメイン)生成が、R版(いつも通りの完全なパイプライン)と
# 一致しているかを確認する。
#
# 比較元(R版)として、先にtest_config.R(json_path)とload_edc_spec.Rをsourceし、
# load_edc_spec(json_path)を実行してother_domains/dm/cdisc_variable_valuesを作成しておくこと。
# テストファイルを切り替えたいときはtest_config.Rのjson_pathを書き換える。
# (source()より前に行うこと。後だと読み込んだ関数まで削除されてしまう)
rm(list = setdiff(ls(), c("dm", "ae", "ds", "other_domains", "cdisc_variable_values", "discontinuation_date")))

source(here("test_config.R"))
source(here("tools/validate_common.R"))

# Webツールで同じJSONを読み込み、被験者数・登録開始日をload_edc_spec.R側(registration_n/
# registration_start_date)と合わせて生成し、「ZIPで一括ダウンロード」したdummy_data.zipを
# 展開したフォルダを other_domains_web_csv_dir(test_config.R)に指定しておくこと。
# DM/AE/DSはvalidate_web_dm.R/ae.R/ds.Rで既に比較済みのため、ここではそれ以外のみを対象にする。
# Web側は0件のドメインをCSVとして出力しない(main.jsのダウンロード処理が空ドメインをスキップする
# ため)。R側はたまたま0件になった場合でもCSVを出力するため、ごく稀に(乱数次第で)そのドメインだけ
# 「Rのみに存在」という差分が出ることがある。これはバグではなく、run-to-runの乱数差による見た目上の
# 差分なので、その場合は再実行して再現するか確認すること
other_domains_web <- load_csv_datasets(other_domains_web_csv_dir)
# load_csv_datasets()はファイル名から拡張子を除いた名前をそのままキーにするため、
# Webツールの出力ファイル名(例: CE_dummy.csv)の"_dummy"サフィックスを外してprefix名に揃える
names(other_domains_web) <- str_remove(names(other_domains_web), "_dummy$")
other_domains_web <- other_domains_web[setdiff(names(other_domains_web), c("DM", "AE", "DS"))]

# ドメインの過不足・列名diffを確認する
cat("--- ドメインの過不足 ---\n")
compare_dataset_names(other_domains, other_domains_web)

cat("--- 列名の一致 ---\n")
compare_colnames(other_domains, other_domains_web)

# R版・Web版それぞれについて、コードリスト範囲内・日付妥当性・中止日以降レコードの有無等を確認する
# (validate_other_domainsを再利用)。discontinuation_dateは被験者ごとの中止日という「その乱数シードでの
# 生成結果」に依存する値のため、R版・Web版それぞれ自分自身のdsから作ったものを使う(dmのUSUBJID一覧とは
# 違い、これをR/Web間で使い回すと、対応するUSUBJIDの中止日が互いに無関係な値になり誤検知する)
ds_web <- read_csv(ds_web_csv_path, col_types = cols(.default = "c"), na = character(0))
discontinuation_date_web <- build_discontinuation_date_table(ds_web)

cat("--- R版other_domainsのバリデーション ---\n")
report_other_domains_validation(validate_other_domains(other_domains, dm, cdisc_variable_values, discontinuation_date = discontinuation_date))

cat("--- Web版other_domainsのバリデーション ---\n")
report_other_domains_validation(validate_other_domains(other_domains_web, dm, cdisc_variable_values, discontinuation_date = discontinuation_date_web))

# R版とWeb版を、ドメイン×列ごとに直接比較する。R版はpresence_conditions等のゲーティングで空欄になる
# 行があり得るのに対し、Web版で対応するゲーティングが未実装/不完全だと一度も空欄にならない、といった
# 差があれば、それがWeb側の未実装箇所を示すのでFAILとして検出する(validate_other_domains()は
# 「許容範囲内か」しか見ないため、「R版にはあるがWeb版に無いパターン」までは検出できない。
# ここで直接比較する。validate_web_ds.R等のcompare_r_web_ds()と同じ考え方を、全ドメインに適用する)
# test3(fortest3_260826_1452.json)には、R/Web双方に共通する真の乱数由来で、R/Web比較が
# たまたま食い違うことがある既知パターンが複数ある(presence_conditions等のゲーティングの
# 実装差ではない。R自身を複数回実行しても同じ頻度で発生することを確認済み)。
# 該当する場合はFAILではなくワーニング扱いにする:
# - PR:PROCCUR/PRPRESP・CM:CMOCCUR/CMPRESP: それぞれのalias_name一覧のうち"sct1"だけが
#   この2項目を定義しておらず、被験者ごとのalias_name選択抽選で"sct1"が選ばれた場合だけ空欄になる
# - FA:VISITNUM: FATESTCD=="EORTCMSG"(alias_name"deepmycosisz"、category="multiple")・
#   "ASTCTGR"(alias_name"immunomonitoring1/2/3"、category="ordered")はVISITNUMを定義しない
#   alias由来で、レコード自体が生成される件数が乱数依存(0件になる回もある)なため
test3_known_limitation_vars <- list(
  PR = c("PROCCUR", "PRPRESP"),
  CM = c("CMOCCUR", "CMPRESP"),
  FA = c("VISITNUM")
)

compare_r_web_other_domains <- function(other_domains_r, other_domains_web) {
  is_test3 <- identical(basename(json_path), "fortest3_260826_1452.json")
  common_domains <- intersect(names(other_domains_r), names(other_domains_web))
  common_domains %>%
    map_dfr(function(domain_name) {
      data_r <- other_domains_r[[domain_name]]
      data_web <- other_domains_web[[domain_name]]
      ignore_cols <- c("STUDYID", "DOMAIN", "USUBJID", str_c(domain_name, "SEQ"), str_c(domain_name, "SPID"))
      common_cols <- setdiff(intersect(colnames(data_r), colnames(data_web)), ignore_cols)

      common_cols %>%
        map_dfr(function(col) {
          r_col <- as.character(data_r[[col]])
          web_col <- as.character(data_web[[col]])
          r_has_blank <- any(is.na(r_col) | r_col == "")
          web_has_blank <- any(is.na(web_col) | web_col == "")
          passed <- r_has_blank == web_has_blank
          known_limitation <- !passed && is_test3 && col %in% test3_known_limitation_vars[[domain_name]]
          detail <- str_c("R版に空欄あり=", r_has_blank, " / Web版に空欄あり=", web_has_blank)
          if (known_limitation) {
            detail <- str_c(detail, "(alias_nameの一部がこの項目を未定義、またはレコード自体の生成数が乱数依存。乱数次第で発生する既知のパターン)")
          } else if (r_has_blank && !web_has_blank) {
            detail <- str_c(
              detail, "(R版は空欄になる場合があるのにWeb版は一度も空欄にならない: ",
              "presence_conditions等によるゲーティングがWeb側で未実装/不完全な可能性)"
            )
          }
          tibble(
            domain = domain_name, check = str_c("blank_pattern_match: ", col), passed = passed,
            known_limitation = known_limitation, detail = detail
          )
        })
    })
}

report_r_web_comparison <- function(results) {
  print(results, n = nrow(results))
  known_fail <- results %>% filter(!passed, known_limitation)
  real_fail <- results %>% filter(!passed, !known_limitation)
  if (nrow(known_fail) > 0) {
    known_summary <- known_fail %>% mutate(label = str_c(domain, ":", check)) %>% pull(label)
    warning(str_c(
      "R/Web比較: ", nrow(known_fail), "件は既知の乱数パターン(test3)によるワーニング(",
      paste(known_summary, collapse = ", "), ")"
    ), call. = FALSE, immediate. = TRUE)
  }
  if (nrow(real_fail) == 0) {
    cat("R/Web比較: 全", nrow(results), "件PASS", if (nrow(known_fail) > 0) str_c("(うち", nrow(known_fail), "件はワーニング)") else "", "\n")
  } else {
    fail_summary <- real_fail %>% mutate(label = str_c(domain, ":", check)) %>% pull(label)
    stop(str_c("R/Web比較: ", nrow(real_fail), "件FAIL(", paste(fail_summary, collapse = ", "), ")"))
  }
}

cat("--- R版とWeb版の直接比較 ---\n")
report_r_web_comparison(compare_r_web_other_domains(other_domains, other_domains_web))

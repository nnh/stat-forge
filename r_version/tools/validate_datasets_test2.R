library(here)

# 比較元(生成データ)として、先にload_edc_spec.Rを実行してae/dm/ds/other_domainsを作成しておくこと。
# その際、load_edc_spec.Rのjson_pathを下記に変更してから実行すること
# json_path <- "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test2/json/fortest2_260826_1501.json"
# 比較に不要な中間オブジェクトが環境に残らないよう、それら以外は削除する
# (source()より前に行うこと。後だと読み込んだ関数まで削除されてしまう)
rm(list = setdiff(ls(), c("ae", "dm", "ds", "other_domains", "cdisc_variable_values", "registration_n", "who_drug_idf", "json_path", "discontinuation_date")))

source(here("tools/validate_common.R"))

# validate_datasets_test2_web.Rと共通の処理(CM/TR特別チェック・run_full_validation呼び出し・
# ドメイン名一覧の確認)はvalidate_test2_shared.Rにまとめてある
source(here("tools/validate_test2_shared.R"))

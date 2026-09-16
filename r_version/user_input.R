library(tidyverse)

# 登録予定被験者数
registration_n <- 100

# 登録開始日
registration_start_date <- "2024-04-01"

# MedDRAのバージョン(meddra_dir直下のサブフォルダ名。NULLなら先頭のフォルダを使う)
meddra_version <- NULL

# WHO Drug/IDFのバージョン(who_drug_idf_parent_dir直下のフォルダ名。例: "2025 Mar 1")
who_drug_idf_version_folder <- "2025 Mar 1"

# 生成データ(ae/dm/ds/other_domains)をCSVとして出力するディレクトリ
output_csv_dir <- "/Users/mariko/Downloads/test20260826"

# 乱数シード。同じEDC仕様JSON・同じ被験者数等の条件であれば生成結果を再現できる。
# 別のランダムなデータが欲しい場合はこの値を変える(NULLにすると完全ランダムに戻る)
random_seed <- 42

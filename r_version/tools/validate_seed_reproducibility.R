# 乱数シード機能のバリデーションプログラム(R版)。
# Webツール(web_tool)で実際に「ZIPで一括ダウンロード」して出力したCSV一式を比較対象とする
# (このプログラム自体はRで書かれているが、R版自身の乱数再現性は検証しない。
# JS版の検証はweb_tool/tools/validate_seed_reproducibility.jsを参照)。
#
# 事前準備: Webツールで同じEDC仕様JSONを使い、以下の組み合わせでそれぞれ「生成する」→
# 「ZIPで一括ダウンロード」→展開し、下記のパスに設定しておくこと
# (展開先フォルダそのものを指定する。中にDM_dummy.csv等が並んでいる想定)
#   1. 同じシード(例: 777)で3回生成 -> same_seed_dirs(3件)
#   2. 異なるシード(例: 111/222/333)で1回ずつ生成 -> diff_seed_dirs(3件)
#   3. シード値42で生成、シード値空欄で生成 -> seed_42_dir / seed_blank_dir
#
# 実行方法: source(here("tools/validate_seed_reproducibility.R"))

library(here)
library(tidyverse)

same_seed_dirs <- c(
  "/Users/mariko/Downloads/dummy_data",
  '/Users/mariko/Downloads/dummy_data (1)',
  '/Users/mariko/Downloads/dummy_data (2)'
)
diff_seed_dirs <- c(
  '/Users/mariko/Downloads/dummy_data (2)',
  '/Users/mariko/Downloads/dummy_data (3)',
  '/Users/mariko/Downloads/dummy_data (4)'
)
seed_42_dir <- "/Users/mariko/Downloads/dummy_data"
seed_blank_dir <- '/Users/mariko/Downloads/dummy_data (5)'

# 2つのディレクトリが、同じファイル名の集合を持ち、全ファイルの内容が完全一致するかを確認する
dirs_identical <- function(dir_a, dir_b) {
  names_a <- sort(list.files(dir_a))
  names_b <- sort(list.files(dir_b))
  if (!identical(names_a, names_b)) {
    return(list(
      identical = FALSE,
      reason = str_c(
        "ファイル一覧が異なる: ", dir_a, "=[", str_c(names_a, collapse = ","),
        "] / ", dir_b, "=[", str_c(names_b, collapse = ","), "]"
      )
    ))
  }
  for (name in names_a) {
    path_a <- file.path(dir_a, name)
    path_b <- file.path(dir_b, name)
    content_a <- readBin(path_a, "raw", file.info(path_a)[["size"]])
    content_b <- readBin(path_b, "raw", file.info(path_b)[["size"]])
    if (!identical(content_a, content_b)) {
      return(list(identical = FALSE, reason = str_c(name, " の内容が異なる")))
    }
  }
  list(identical = TRUE, reason = "")
}

# 2つのディレクトリを比較し、結果(一致/不一致)を表示する。expect_identicalと実際の結果が
# 一致していればTRUE(OK)を返す
report_pair <- function(label, dir_a, dir_b, expect_identical) {
  result <- dirs_identical(dir_a, dir_b)
  ok <- result[["identical"]] == expect_identical
  detail <- if (result[["identical"]]) "一致" else str_c("不一致(", result[["reason"]], ")")
  cat(label, ": ", detail, " [", if (ok) "OK" else "NG", "]\n", sep = "")
  ok
}

all_pass <- TRUE

cat("\n=== 検証1: 同じシードで3回出力 -> 全て完全一致するか ===\n")
p1 <- report_pair("run1 vs run2", same_seed_dirs[1], same_seed_dirs[2], TRUE)
p2 <- report_pair("run1 vs run3", same_seed_dirs[1], same_seed_dirs[3], TRUE)
test1_pass <- p1 && p2
cat("検証1: ", if (test1_pass) "PASS" else "FAIL", "\n", sep = "")
all_pass <- all_pass && test1_pass

cat("\n=== 検証2: 異なるシードで3回出力 -> 内容が異なるか ===\n")
p3 <- report_pair("run1 vs run2", diff_seed_dirs[1], diff_seed_dirs[2], FALSE)
p4 <- report_pair("run1 vs run3", diff_seed_dirs[1], diff_seed_dirs[3], FALSE)
p5 <- report_pair("run2 vs run3", diff_seed_dirs[2], diff_seed_dirs[3], FALSE)
test2_pass <- p3 && p4 && p5
cat("検証2: ", if (test2_pass) "PASS" else "FAIL", "\n", sep = "")
all_pass <- all_pass && test2_pass

cat("\n=== 検証3: シード値42とシード値空欄 -> 完全一致するか ===\n")
test3_pass <- report_pair("seed=42 vs seed=(空欄)", seed_42_dir, seed_blank_dir, TRUE)
cat("検証3: ", if (test3_pass) "PASS" else "FAIL", "\n", sep = "")
all_pass <- all_pass && test3_pass

cat("\n=== 総合結果: ", if (all_pass) "全件PASS" else "FAILあり", " ===\n", sep = "")

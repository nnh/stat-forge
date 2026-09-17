library(tidyverse)
library(here)

source(here("resolve_os_path.R"))

# DM/DS/AE/other_domainsの構造的な自動チェック(validate_dm()等)は、それぞれ専用ファイルに分けている。
# testN.Rが個別にsourceしなくても済むよう、ここでまとめてsourceしておく
source(here("tools/validate_dm.R"))
source(here("tools/validate_ds.R"))
source(here("tools/validate_ae.R"))
source(here("tools/validate_other_domains.R"))
# run_full_validation()がbuild_discontinuation_date_table()を使うため。testN.R側のrm()で
# 消えている可能性がある(load_edc_spec.R実行時に一度定義されても、testN.R先頭のrm()で
# データ以外の全オブジェクトと一緒に消えてしまうため)、ここで明示的にsourceし直しておく
source(here("build_ds_domain.R"))

# json_path(のファイル名)ごとに、対応する実データ(rawdata)フォルダのパスをべた書きで設定する。
# test1・test2は実データが用意できているのでパスを指定し、test3・test4はまだ実データが無いので
# NULLにしておく(run_full_validation()はcsv_dir=NULLだと実データとの比較をスキップする)
csv_dir_by_file <- list(
  "fortest1_260826_1112.json" = resolve_os_path(
    "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test1/rawdata",
    "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test1\\rawdata"
  ),
  "fortest2_260826_1501.json" = resolve_os_path(
    "/Users/mariko/Library/CloudStorage/Box-Box/Datacenter/Users/ohtsuka/2026/20260826/test2/rawdata",
    "C:\\Users\\c0002691\\Box\\Datacenter\\Users\\ohtsuka\\2026\\20260826\\test2\\rawdata"
  ),
  "fortest3_260826_1452.json" = NULL,
  "fortest4_260826_1501.json" = NULL
)

# 専用のsort_colで個別に確認するため、index指定の対象から除くドメイン名
special_domain_names <- c("AE", "DM", "DS")

# csv_dir直下のCSVを全て読み込む。ファイル名(拡張子なし)をキーにしたnamed listを返す(datasets$AE のように参照できる)。
# 型推定による誤判定(先頭行がT/Fに見えて後方の"NOT DONE"等がパースエラーになる、等)を避けるため、
# 全列を文字列として読み込む。na=character(0)を指定し、空欄も文字列"NA"も自動でRのNAに
# 変換しない(コードリストの選択肢として文字列"NA"が使われているケースがあるため、
# 空欄と文字列"NA"を区別したまま読み込む)
load_csv_datasets <- function(csv_dir) {
  csv_paths <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  csv_paths %>%
    set_names(~ tools::file_path_sans_ext(basename(.x))) %>%
    map(~ read_csv(.x, col_types = cols(.default = "c"), na = character(0)))
}

# load_edc_spec.Rで生成したae/dm/ds/other_domainsを、CSV側(datasets)と同じ
# ドメイン名(AE/DM/DS/FA等)をキーにした1つのnamed listにまとめる
build_generated_datasets <- function(ae, dm, ds, other_domains) {
  c(list(AE = ae, DM = dm, DS = ds), other_domains)
}

# データセットの過不足を確認(片方にしか存在しないドメイン名を洗い出して表示する)
compare_dataset_names <- function(generated_datasets, datasets) {
  only_in_generated <- setdiff(names(generated_datasets), names(datasets))
  only_in_csv <- setdiff(names(datasets), names(generated_datasets))
  cat("生成データのみに存在:", if (length(only_in_generated) > 0) paste(only_in_generated, collapse = ", ") else "(なし)", "\n")
  cat("CSVのみに存在:", if (length(only_in_csv) > 0) paste(only_in_csv, collapse = ", ") else "(なし)", "\n")
  invisible(list(only_in_generated = only_in_generated, only_in_csv = only_in_csv))
}

# 両方に共通して存在するデータセットについて、列名の差分があるものだけを表示する
compare_colnames <- function(generated_datasets, datasets) {
  common_names <- intersect(names(generated_datasets), names(datasets))

  colname_diff <- common_names %>%
    set_names() %>%
    map(function(name) {
      generated_cols <- colnames(generated_datasets[[name]])
      csv_cols <- colnames(datasets[[name]])
      list(
        only_in_generated = setdiff(generated_cols, csv_cols),
        only_in_csv = setdiff(csv_cols, generated_cols)
      )
    })

  diff_only <- colname_diff %>%
    keep(~ length(.x[["only_in_generated"]]) > 0 || length(.x[["only_in_csv"]]) > 0)

  if (length(diff_only) == 0) {
    cat("列名の差分: なし\n")
  } else {
    diff_only %>%
      iwalk(function(diff, name) {
        cat("===", name, "===\n")
        cat("  生成データのみ:", if (length(diff[["only_in_generated"]]) > 0) paste(diff[["only_in_generated"]], collapse = ", ") else "(なし)", "\n")
        cat("  CSVのみ:", if (length(diff[["only_in_csv"]]) > 0) paste(diff[["only_in_csv"]], collapse = ", ") else "(なし)", "\n")
      })
  }

  invisible(colname_diff)
}

# 中身を目視比較しやすいよう列順・行順を揃える。共通の列を先頭(CSV側の並び順)に置き、
# 片方にしか無い列は末尾に残す(削除はしない)。行順はsort_colで昇順に揃える。
# 生成データ側のNAとCSV側の空欄("")は同じ「値が無い」状態とみなし、比較上の見た目の差にならないよう
# どちらも""に統一する(生成データ側はDate等の列も混ざるため、一旦全列を文字列にしてから揃える)
align_for_comparison <- function(generated_df, csv_df, sort_col) {
  common_cols <- intersect(colnames(csv_df), colnames(generated_df))
  generated_col_order <- c(common_cols, setdiff(colnames(generated_df), common_cols))
  csv_col_order <- c(common_cols, setdiff(colnames(csv_df), common_cols))
  list(
    generated = generated_df %>% select(all_of(generated_col_order)) %>%
      # arrange(across(all_of(sort_col))) %>%
      mutate(across(everything(), ~ replace_na(as.character(.x), ""))),
    csv = csv_df %>% select(all_of(csv_col_order)) %>%
      # arrange(across(all_of(sort_col))) %>%
      mutate(across(everything(), ~ replace_na(as.character(.x), "")))
  )
}

# domain_name(例: "DM")について、generated_datasets/datasets双方をalign_for_comparison()で揃え、
# interactiveセッションであればView()で開く。返り値はlist(generated=, csv=)
compare_domain <- function(generated_datasets, datasets, domain_name, sort_col, view = interactive()) {
  aligned <- align_for_comparison(generated_datasets[[domain_name]], datasets[[domain_name]], sort_col)
  if (view) {
    View(aligned[["generated"]], title = str_c(domain_name, "_generated"))
    View(aligned[["csv"]], title = str_c(domain_name, "_csv"))
  }
  aligned
}

# 両方に共通して存在するドメイン名一覧(common_names)のうち、index番目のドメインを比較する。
# 1行ずつ実行してドメインを1つずつ目視確認していく用途で、domain_nameを直接指定する代わりに
# 「何番目か」で指定できるようにしたもの。sort_colは指定が無ければ、{domain_name}SEQ列があれば
# USUBJID+{domain_name}SEQ、無ければUSUBJIDのみを使う。
# exclude(例: AE/DM/DSは専用のsort_colで別途名前指定するため除外したい場合)を指定すると、
# common_names作成時にそのドメイン名を除いてから番号を振る
compare_domain_by_index <- function(generated_datasets, datasets, index, sort_col = NULL, exclude = character(0), view = interactive()) {
  common_names <- setdiff(intersect(names(generated_datasets), names(datasets)), exclude)
  domain_name <- common_names[index]
  if (is.null(sort_col)) {
    seq_col <- str_c(domain_name, "SEQ")
    sort_col <- if (seq_col %in% colnames(generated_datasets[[domain_name]])) c("USUBJID", seq_col) else "USUBJID"
  }
  cat("[", index, "/", length(common_names), "]", domain_name, "\n")
  compare_domain(generated_datasets, datasets, domain_name, sort_col, view = view)
}

# AE(AETOXGR==5、死亡)のUSUBJIDと発生日(AEENDTC。死亡に至ったAEの終了日を発生日とみなす)を返す
build_ae_death_dates <- function(ae) {
  ae %>%
    filter(AETOXGR == "5") %>%
    group_by(USUBJID) %>%
    summarise(DTHDTC = min(AEENDTC), .groups = "drop")
}

# DS(DSTERM=="DEATH")のUSUBJIDと死亡日(DSDTC)を返す
build_ds_death_dates <- function(ds) {
  ds %>%
    filter(DSTERM == "DEATH") %>%
    select(USUBJID, DSDTC)
}

# AEとDSの死亡情報を突き合わせる。片方にしかUSUBJIDが無い(NA)、または両方にあっても
# 日付が一致しない行がないか、match列を見て目視確認できるようにする
check_death_consistency <- function(ae_death_dates, ds_death_dates) {
  full_join(ae_death_dates, ds_death_dates, by = "USUBJID") %>%
    mutate(match = !is.na(DTHDTC) & !is.na(DSDTC) & DTHDTC == DSDTC) %>%
    arrange(USUBJID)
}

# data(1ドメイン分。USUBJID列が必要)の各行について、required_vars(必須になるはずの
# 列名ベクトル)の値がすべて入っている(NAでも空文字列""でもない)ことを確認する。
# dataがCSV由来(Web版)の場合、空欄はNAではなく文字列""のまま読み込まれるため、
# 両方を「値なし」として扱う。domain_nameを指定するとメッセージの先頭に付く。
# 値が無い行がある場合はstop()でエラーにする(変数ごとに件数・USUBJIDを表示)。
# 問題なければチェック内容とOKである旨をcatで表示する
check_required_vars <- function(data, required_vars, domain_name = NULL) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")

  missing_cols <- setdiff(required_vars, colnames(data))
  if (length(missing_cols) > 0) {
    stop(str_c(label, "required_varsチェック: dataに列がありません(", paste(missing_cols, collapse = ", "), ")"))
  }

  problem_usubjid_by_var <- required_vars %>%
    set_names() %>%
    map(function(var) {
      is_blank <- is.na(data[[var]]) | data[[var]] == ""
      data[["USUBJID"]][is_blank]
    }) %>%
    keep(~ length(.x) > 0)

  if (length(problem_usubjid_by_var) > 0) {
    detail <- problem_usubjid_by_var %>%
      imap_chr(~ str_c(.y, "(", length(.x), "件: ", paste(.x, collapse = ", "), ")")) %>%
      paste(collapse = " / ")
    stop(str_c(label, "required_varsチェック: NG - ", detail))
  }

  cat(
    label, "required_varsチェック: OK(", paste(required_vars, collapse = ", "), "が全", nrow(data), "行で値あり)\n",
    sep = ""
  )
}

# check_required_varsの逆。data(1ドメイン分。USUBJID列が必要)の各行について、blank_vars
# (常に空欄のはずの列名ベクトル)の値がすべて空(NAまたは空文字列"")であることを確認する。
# dataがCSV由来(Web版)の場合、空欄はNAではなく文字列""のまま読み込まれるため、
# 両方を「値なし」として扱う。domain_nameを指定するとメッセージの先頭に付く。
# 値が入っている行がある場合はstop()でエラーにする(変数ごとに件数・USUBJIDを表示)。
# 問題なければチェック内容とOKである旨をcatで表示する
check_blank_vars <- function(data, blank_vars, domain_name = NULL) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")

  missing_cols <- setdiff(blank_vars, colnames(data))
  if (length(missing_cols) > 0) {
    stop(str_c(label, "blank_varsチェック: dataに列がありません(", paste(missing_cols, collapse = ", "), ")"))
  }

  problem_usubjid_by_var <- blank_vars %>%
    set_names() %>%
    map(function(var) {
      is_present <- !is.na(data[[var]]) & data[[var]] != ""
      data[["USUBJID"]][is_present]
    }) %>%
    keep(~ length(.x) > 0)

  if (length(problem_usubjid_by_var) > 0) {
    detail <- problem_usubjid_by_var %>%
      imap_chr(~ str_c(.y, "(", length(.x), "件: ", paste(.x, collapse = ", "), ")")) %>%
      paste(collapse = " / ")
    stop(str_c(label, "blank_varsチェック: NG - ", detail))
  }

  cat(
    label, "blank_varsチェック: OK(", paste(blank_vars, collapse = ", "), "が全", nrow(data), "行で空欄)\n",
    sep = ""
  )
}

# data(1ドメイン分。USUBJID列が必要)のvar列(数値の文字列)がmin_value以上max_value以下かを
# 確認する。min_value/max_valueはNA(既定値)にすると片側無制限にできる。
# varが無い(NAまたは空文字列"")行は判定対象から除く(値の有無自体はcheck_required_vars等の
# 別チェックで見る)。空欄ではないが数値に変換できない値(例: "DUMMY")が1件でもある場合は、
# 範囲チェックがその行を素通りしてしまう(NG判定から除外される)ことに気づけるよう、
# 警告(warning())を出す。domain_nameを指定するとメッセージの先頭に付く。
# 範囲外の値がある場合はstop()でエラーにする。問題なければチェック内容とOKである旨をcatで表示する
check_numeric_range <- function(data, var, min_value = NA, max_value = NA, domain_name = NULL) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")
  raw <- data[[var]]
  values <- suppressWarnings(as.numeric(raw))
  valid <- !is.na(values)
  is_blank <- is.na(raw) | raw == ""
  non_numeric <- !valid & !is_blank

  invalid <- valid & ((!is.na(min_value) & values < min_value) | (!is.na(max_value) & values > max_value))
  invalid_usubjid <- data[["USUBJID"]][invalid]

  range_label <- str_c(
    if (is.na(min_value)) "下限なし" else str_c(min_value, "以上"), "〜",
    if (is.na(max_value)) "上限なし" else str_c(max_value, "以下")
  )

  if (length(invalid_usubjid) > 0) {
    stop(str_c(
      label, var, "範囲チェック: ", length(invalid_usubjid), "件NG(", range_label,
      "の範囲外。USUBJID: ", paste(invalid_usubjid, collapse = ", "), ")"
    ))
  }
  if (any(non_numeric)) {
    non_numeric_usubjid <- data[["USUBJID"]][non_numeric]
    warning(str_c(
      label, var, "範囲チェック: 警告 - ", sum(non_numeric), "件が数値に変換できません(例: \"DUMMY\"等)。",
      "範囲チェックから除外されています。USUBJID: ", paste(non_numeric_usubjid, collapse = ", ")
    ))
  }
  cat(
    label, var, "範囲チェック: OK(", var, "が", range_label, "であることを確認、", sum(valid), "件)\n",
    sep = ""
  )
}

# data(1ドメイン分。USUBJID列が必要)のdate_var列(日付の文字列)が今日以前かを確認する。
# date_varが無い(NAまたは空文字列"")行は判定対象から除く(値の有無自体はcheck_required_vars等の
# 別チェックで見る)。domain_nameを指定するとメッセージの先頭に付く。
# dataが0行の場合はstop()でエラーにする(絞り込み条件に該当する行が1件も無いことの検知用)。
# 問題なければチェック内容とOKである旨をcatで表示する
check_not_empty <- function(data, check_name, domain_name = NULL) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")

  if (nrow(data) == 0) {
    stop(str_c(label, check_name, ": 該当する行が0件です"))
  }
  cat(label, check_name, ": OK(", nrow(data), "件)\n", sep = "")
}

# 今日より後の値がある場合はstop()でエラーにする。問題なければチェック内容とOKである旨をcatで表示する
check_date_before_today <- function(data, date_var, domain_name = NULL) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")
  date_values <- as.Date(data[[date_var]])
  today <- Sys.Date()
  valid <- !is.na(date_values)

  invalid <- valid & date_values > today
  invalid_usubjid <- data[["USUBJID"]][invalid]

  if (length(invalid_usubjid) > 0) {
    stop(str_c(
      label, date_var, "範囲チェック: ", length(invalid_usubjid), "件NG(今日(", as.character(today),
      ")より後。USUBJID: ", paste(invalid_usubjid, collapse = ", "), ")"
    ))
  }
  cat(
    label, date_var, "範囲チェック: OK(", date_var, "が今日(", as.character(today), ")以前であることを確認、",
    sum(valid), "件)\n",
    sep = ""
  )
}

# DMのRFICDTC(同意取得日)が、BRTHDTC(生年月日)以降・今日以前の範囲内かを確認する。
# BRTHDTC/RFICDTCのどちらかが無い行は判定対象から除く(値の有無自体は別の構造チェックで見る)。
# 範囲外がある場合はstop()でエラーにする。範囲内ならチェック内容とOKである旨をcatで表示する
# data(1ドメイン分。USUBJID列が必要)のdate_var列(日付の文字列)が、ref_date_var列以降・
# 今日以前の範囲内かを確認する。date_var/ref_date_varのどちらかが無い行は判定対象から除く
# (値の有無自体はcheck_required_vars等の別チェックで見る)。domain_nameを指定するとメッセージの
# 先頭に付く。範囲外がある場合はstop()でエラーにする。問題なければチェック内容とOKである旨をcatで表示する。
# offset_days(既定0): EDC仕様のref('sheet',N)+N.days/-N.daysのような日数オフセット付き参照の場合、
# 下限をref_date_var + offset_daysにする(例: -28ならref_date_varの28日前以降を許容)
check_date_after_var_before_today <- function(data, date_var, ref_date_var, domain_name = NULL, offset_days = 0) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")
  ref_dates <- as.Date(data[[ref_date_var]]) + offset_days
  dates <- as.Date(data[[date_var]])
  today <- Sys.Date()
  valid_pair <- !is.na(ref_dates) & !is.na(dates)

  invalid <- valid_pair & (dates < ref_dates | dates > today)
  invalid_usubjid <- data[["USUBJID"]][invalid]

  ref_label <- if (offset_days == 0) ref_date_var else str_c(ref_date_var, if (offset_days > 0) "+" else "", offset_days, "日")

  if (length(invalid_usubjid) > 0) {
    stop(str_c(
      label, date_var, "範囲チェック: ", length(invalid_usubjid), "件NG(", ref_label, "以降・今日(",
      as.character(today), ")以前の範囲外。USUBJID: ", paste(invalid_usubjid, collapse = ", "), ")"
    ))
  }
  cat(
    label, date_var, "範囲チェック: OK(", date_var, "が", ref_label, "以降・今日(", as.character(today),
    ")以前であることを確認、", sum(valid_pair), "件)\n",
    sep = ""
  )
}

# data(1ドメイン分。USUBJID列が必要)のvalue_var列の値が、全てallowed_values(例: 施設一覧CSVのcode列)に
# 含まれているかを確認する(参照整合性チェック。例: DMのSITEIDがfacilities_dummy.csvのcodeの範囲内か)。
# value_varが無い(NAまたは空文字列"")行は判定対象から除く。domain_nameを指定するとメッセージの先頭に付く。
# allowed_valuesに含まれない値がある場合はstop()でエラーにする。問題なければチェック内容とOKである旨をcatで表示する
check_values_subset_of <- function(data, value_var, allowed_values, domain_name = NULL) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")
  values <- data[[value_var]]
  valid <- !is.na(values) & values != ""

  invalid <- valid & !(values %in% allowed_values)
  invalid_usubjid <- data[["USUBJID"]][invalid]

  if (length(invalid_usubjid) > 0) {
    stop(str_c(
      label, value_var, "整合性チェック: ", length(invalid_usubjid), "件NG(想定される値一覧に含まれない値。USUBJID: ",
      paste(invalid_usubjid, collapse = ", "), ")"
    ))
  }
  cat(
    label, value_var, "整合性チェック: OK(", value_var, "が想定される値一覧に含まれることを確認、",
    sum(valid), "件)\n",
    sep = ""
  )
}

# data(1ドメイン分)のSTUDYID列が、全行expected_studyidと一致するかを確認する。
# expected_studyidはハードコードせず、正しいjson_pathから生成したR版データのSTUDYID
# (例: dm[["STUDYID"]][1])を呼び出し元で渡す想定。json_pathの設定間違い(意図しない試験の
# JSONを指している)を、比較対象のCSVの中身からも検知できるようにするためのチェック。
# 不一致がある場合はstop()でエラーにする
check_studyid_matches <- function(data, expected_studyid, domain_name = NULL) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")
  observed <- unique(data[["STUDYID"]])
  unexpected <- setdiff(observed, expected_studyid)

  if (length(unexpected) > 0) {
    stop(str_c(
      label, "STUDYID整合性チェック: 期待値「", expected_studyid, "」に対し、実際の値に「",
      paste(unexpected, collapse = ", "), "」が含まれています(json_pathの設定間違いの可能性があります)"
    ))
  }
  cat(label, "STUDYID整合性チェック: OK(期待値「", expected_studyid, "」と一致)\n", sep = "")
}

# 値のベクトルを、1つずつダブルクォートで囲んでからカンマ区切りで連結する。値そのものにカンマを
# 含む文字列(例: "PTCL, NOS")がある場合に、メッセージ上で値の区切りのカンマなのか値の一部なのか
# 区別できるようにするため(check_value_equals・run_value_equals_checks_from_csvのメッセージで使う)
quote_join <- function(x) {
  str_c('"', x, '"', collapse = ", ")
}

# data(1ドメイン分。USUBJID列が必要)のvar列の値が、全行expected_value(単一値、または許容する
# 複数値のベクトル)のいずれかと一致することを確認する。domain_nameを指定するとメッセージの先頭に付く。
# 一致しない値がある行がある場合はstop()でエラーにする(USUBJIDと実際の値を表示)。
# 問題なければチェック内容とOKである旨をcatで表示する
check_value_equals <- function(data, var, expected_value, domain_name = NULL) {
  label <- if (is.null(domain_name)) "" else str_c(domain_name, ": ")

  if (!(var %in% colnames(data))) {
    stop(str_c(label, var, "一致チェック: dataに列がありません"))
  }

  invalid <- !(data[[var]] %in% expected_value)
  invalid_usubjid <- data[["USUBJID"]][invalid]

  if (length(invalid_usubjid) > 0) {
    detail <- str_c(invalid_usubjid, '("', data[[var]][invalid], '")') %>% paste(collapse = ", ")
    stop(str_c(
      label, var, "一致チェック: ", length(invalid_usubjid), "件NG(期待値: ",
      quote_join(expected_value), "。実際の値 - ", detail, ")"
    ))
  }
  cat(
    label, var, "一致チェック: OK(", var, "が全", nrow(data), "行で",
    quote_join(expected_value), "のいずれかであることを確認)\n",
    sep = ""
  )
}

# data(1ドメイン分。USUBJID列が必要。例: lb/qs等、testN_web.R側で既に取り出し済みの変数を
# そのまま渡す)のvar列の値が、CSV(csv_path。domain, var, expected_value、任意でvisit列を持つ)に
# 登録されている期待値と全行一致することを確認する。domain・varでCSVを絞り込み、該当行の
# expected_valueを「許容する複数値」として使う(同じdomain・var・visitの行が複数あれば、
# それらをまとめて許容値とする)。visit列が空欄の行は「どのvisitでも使える値」を表す:
# visit未指定の呼び出しでは空欄の行だけが対象になり、visit指定の呼び出しでは、その
# visit専用の行に加えて空欄の行も許容値に含める。visitを指定した場合は、dataをVISITNUM==visitで
# 絞り込んでからチェックする(dataにVISITNUM列が必要)。
# CSVに該当する行が無い場合、またはvisit指定があるのにdataにVISITNUM列が無い場合はstop()でエラーにする。
# また、CSVで許容値として登録したexpected_valueのうち、実際のdataのvar列に一度も出現しなかった
# 値があれば、CSVの設定ミス・不要な行に気づけるようwarning()で知らせる(チェック自体はOKのまま)。
# extra_labelは、CSVの絞り込み(domain/var/visit一致)には使わず、結果メッセージの表示にのみ
# 追加情報(例: VSTPTNUMの値)を含めたい場合に指定する
run_value_equals_checks_from_csv <- function(data, domain, var, csv_path, visit = NULL, extra_label = NULL) {
  # na = character(0): expected_valueに選択肢コードとして文字列"NA"が使われているケースがあるため、
  # readrのデフォルトのNA文字列判定("NA"等を欠測値として扱う)を無効にし、そのまま文字列として読む
  config <- read_csv(csv_path, col_types = cols(.default = "c"), na = character(0))
  missing_cols <- setdiff(c("domain", "var", "expected_value"), colnames(config))
  if (length(missing_cols) > 0) {
    stop(str_c("固定値チェックCSV: 必要な列がありません(", paste(missing_cols, collapse = ", "), ")"))
  }
  if (!("visit" %in% colnames(config))) {
    config[["visit"]] <- NA_character_
  }
  config[["visit"]] <- na_if(config[["visit"]], "")

  label <- if (is.null(visit)) domain else str_c(domain, "(VISITNUM=", visit, ")")
  if (!is.null(extra_label)) {
    label <- str_c(label, "(", extra_label, ")")
  }

  # visit未指定の呼び出しはvisit空欄の行(全visit共通の値)だけが対象。visit指定の呼び出しは、
  # そのvisit専用の行に加えて、visit空欄の行(どのvisitでも使える値)も許容値に含める
  visit_match <- if (is.null(visit)) is.na(config[["visit"]]) else is.na(config[["visit"]]) | config[["visit"]] == visit
  matched <- config[config[["domain"]] == domain & config[["var"]] == var & visit_match, ]
  if (nrow(matched) == 0) {
    stop(str_c(label, ": ", var, "一致チェック: CSVに該当する行がありません(", csv_path, ")"))
  }

  if (!is.null(visit)) {
    if (!("VISITNUM" %in% colnames(data))) {
      stop(str_c(label, ": dataにVISITNUM列がありません"))
    }
    data <- data %>% filter(VISITNUM == visit)
  }

  check_value_equals(data, var, matched[["expected_value"]], label)

  unused_values <- setdiff(matched[["expected_value"]], data[[var]])
  if (length(unused_values) > 0) {
    warning(str_c(
      label, ": ", var, "一致チェック: CSVのexpected_valueのうち、実際の値に一度も出現しなかったものがあります(",
      quote_join(unused_values), ")"
    ))
  }
}

# load_edc_spec.Rで生成したae/dm/ds/other_domainsと、csv_dir直下のCSVを一括で比較・検証する。
# データセットの過不足確認、列名diff、DM/DS/AE/other_domainsの構造的な自動チェック、AE/DSの死亡情報の
# 整合性チェックまでをまとめて実行する。testN.R側は csv_dir と other_domains_special_checks
# (この試験固有の追加チェック)を用意してこの関数を呼ぶだけでよい。
# csv_dirはNULL可(既定値もNULL)。対応する実データ(rawdata)がまだ用意できていない試験
# (例: test3・test4)では、csv_dirを指定しない(またはNULLを渡す)ことで、実データとの比較
# (データセットの過不足・列名diff)をスキップし、生成データ自体の構造チェック(DM/DS/AE/
# other_domainsの自動チェック・AE/DSの死亡情報整合性)だけを実行できる。無関係な実データ
# (別試験のrawdata等)と比較して構造差分を誤検知するより、比較自体を行わない方が安全なため
# discontinuation_dateもNULL可。渡さなければこの関数の中でds(引数)から作り直すが、
# load_edc_spec.Rはadd_randomization_ds_rows()でRANDOMIZED行を追加した後のdsをこの関数に渡すため、
# ここで作り直すとRANDOMIZED行(DSTERM!="COMPLETED"だが中止ではない)が混ざり、中止日が実際より
# 早い誤った日付になってしまう。testN.R側でload_edc_spec.R実行時にできる、RANDOMIZED行を混ぜる前の
# 正しいdiscontinuation_date(グローバル環境にコピーされている)を明示的に渡すこと
# other_domains_special_checksの各チェック関数がwho_drug_idf等の他の変数を参照したい場合は、
# この関数の引数としてではなく、testN.R側のトップレベル変数をクロージャとして直接参照すればよい
# (special_checksの関数はtestN.R側で定義されるため、testN.R側の変数がそのまま見える)。
# 生成した各オブジェクトをlistで返す(1行ずつの目視確認(compare_domain_by_index)はtestN.R側で行う)
run_full_validation <- function(ae, dm, ds, other_domains, cdisc_variable_values, registration_n, csv_dir = NULL, other_domains_special_checks = list(), discontinuation_date = NULL, view = interactive()) {
  generated_datasets <- build_generated_datasets(ae, dm, ds, other_domains)

  if (is.null(csv_dir)) {
    cat("csv_dir未指定のため、実データとの比較(データセットの過不足・列名diff)はスキップします\n")
    datasets <- list()
  } else {
    datasets <- load_csv_datasets(csv_dir)
    compare_dataset_names(generated_datasets, datasets)
    compare_colnames(generated_datasets, datasets)
  }

  dm_result <- validate_dm(dm, cdisc_variable_values, registration_n)
  report_dm_validation(dm_result)
  ds_result <- validate_ds(ds, dm, cdisc_variable_values)
  report_ds_validation(ds_result)
  ae_result <- validate_ae(ae, dm, cdisc_variable_values)
  report_ae_validation(ae_result)
  if (is.null(discontinuation_date)) {
    cat("discontinuation_date未指定のため、この時点のds(RANDOMIZED行を含む)から作り直します。\n")
    cat("RANDOMIZED行が誤って中止日として扱われる可能性があるため、可能ならload_edc_spec.R実行時の\n")
    cat("discontinuation_dateを明示的に渡すことを推奨します\n")
    discontinuation_date <- build_discontinuation_date_table(ds)
  }
  other_domains_result <- validate_other_domains(other_domains, dm, cdisc_variable_values, other_domains_special_checks, discontinuation_date)
  report_other_domains_validation(other_domains_result)

  ae_death_dates <- build_ae_death_dates(ae)
  ds_death_dates <- build_ds_death_dates(ds)
  death_consistency <- check_death_consistency(ae_death_dates, ds_death_dates)
  if (view) {
    View(death_consistency, title = "death_consistency")
  }

  list(
    datasets = datasets,
    generated_datasets = generated_datasets,
    dm_result = dm_result,
    ds_result = ds_result,
    ae_result = ae_result,
    other_domains_result = other_domains_result,
    ae_death_dates = ae_death_dates,
    ds_death_dates = ds_death_dates,
    death_consistency = death_consistency
  )
}

library(tidyverse)
library(lubridate)

# 先にload_edc_spec.Rを実行してother_domains/dm/cdisc_variable_valuesを作成しておくこと。
# DM/DS/AEで個別に書いていた「コードリスト範囲内・日付の妥当性」等の汎用チェックを、
# prefixを問わず適用できるようにしたもの。other_domainsの全ドメインに一括で適用する

# prefix(例: "FA")について、乱数に依存しない構造的な条件を汎用的にチェックする。
# dmを渡すと、USUBJIDがdmの範囲内に収まっているかも確認する(渡さなければスキップ)。
# discontinuation_date(build_discontinuation_date_table(ds)の結果、USUBJID/DISCONDTC)を渡すと、
# date型の列に中止日より後の値が無いかも確認する(渡さなければスキップ)
validate_domain_generic <- function(data, prefix, cdisc_variable_values, dm = NULL, discontinuation_date = NULL) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  if ("DOMAIN" %in% colnames(data)) {
    add_check("domain_is_correct", all(data[["DOMAIN"]] == prefix), str_c("値: ", paste(unique(data[["DOMAIN"]]), collapse = ", ")))
  }

  if (!is.null(dm) && "USUBJID" %in% colnames(data)) {
    extra_usubjid <- setdiff(data[["USUBJID"]], dm[["USUBJID"]])
    add_check("usubjid_subset_of_dm", length(extra_usubjid) == 0, str_c("DM以外: ", paste(extra_usubjid, collapse = ", ")))
  }

  # radio_button/check_box型の列は、コードリスト(空欄含む)の範囲内の値のみを持つ。
  # check_boxは複数選択がカンマ区切りで1つの文字列になるため、カンマで分割してから判定する
  choice_spec <- cdisc_variable_values %>% filter(prefix == !!prefix, field_type %in% c("radio_button", "check_box"))
  for (var_name in intersect(unique(choice_spec[["cdisc_variable"]]), colnames(data))) {
    # 同じcdisc_variable名が別のalias_nameでmeddra/drug等の別field_typeとしても定義されている場合、
    # 単一のコードリストでは判定できないためスキップする
    # (例: FAOBJがAETERMをコピーするalias_nameではmeddra型、別のalias_nameではradio_button型)
    all_field_types <- cdisc_variable_values %>% filter(prefix == !!prefix, cdisc_variable == var_name) %>% pull(field_type) %>% unique()
    if (!all(all_field_types %in% c("radio_button", "check_box"))) {
      next
    }

    var_spec <- choice_spec %>% filter(cdisc_variable == var_name)
    # str_trim(): CSV経由(read_csvのtrim_ws=TRUEが既定)でdataを読み込んだ場合、コードリスト側の
    # 値にEDC仕様JSON由来の前後空白(例: "TP53 mutations "のような入力ミス)が残っていると、
    # 生成データ側だけ空白が落ちてしまい実際は正しい値なのに不一致と誤判定するため、
    # 両側とも前後空白を落としてから比較する
    valid_codes <- var_spec %>%
      mutate(code = ifelse(is.na(code), default_value, code)) %>%
      pull(code) %>%
      str_trim() %>%
      unique() %>%
      union("")

    observed <- data[[var_name]][!is.na(data[[var_name]])]
    if (any(var_spec[["field_type"]] == "check_box")) {
      observed <- unique(unlist(str_split(observed, ",")))
    }
    invalid <- setdiff(unique(str_trim(observed)), valid_codes)

    add_check(
      str_c("valid_codes: ", var_name),
      length(invalid) == 0,
      if (length(invalid) > 0) str_c("コードリスト外の値: ", paste(invalid, collapse = ", ")) else ""
    )
  }

  # date型の列は、日付(YYYY-MM-DD)としてパースでき、未来日でない。
  # as.Date()は完全に書式が崩れた文字列(数値がそのまま文字列化されてしまった等)だとエラーで
  # 停止してしまうため、パースできない値はNAを返すlubridate::ymd()を使う。
  # dataがCSV経由(na=character(0)で読み込み)の場合は空欄が""になるため、NAと""の両方を
  # 「値なし」として除外する。rawがDate型のことがあり、Date型のまま""と比較すると
  # as.Date("")のパース失敗で全行NAになってしまうため、先にas.character()で文字列化してから判定する
  date_vars <- cdisc_variable_values %>% filter(prefix == !!prefix, field_type == "date") %>% pull(cdisc_variable) %>% unique()
  for (var_name in intersect(date_vars, colnames(data))) {
    raw <- as.character(data[[var_name]])
    non_na <- raw[!is.na(raw) & raw != ""]
    parsed <- suppressWarnings(ymd(non_na))
    unparsable <- non_na[is.na(parsed)]
    future_dates <- parsed[!is.na(parsed) & parsed > Sys.Date()]

    add_check(
      str_c("valid_date: ", var_name),
      length(unparsable) == 0 && length(future_dates) == 0,
      str_c("パース不可: ", length(unparsable), "件(", paste(head(unparsable, 5), collapse = ", "), "), 未来日: ", length(future_dates), "件")
    )
  }

  # discontinuation_dateが渡された場合、date型の列の値がその被験者の中止日(DISCONDTC)より
  # 後になっていないか確認する(中止後に検査等のレコードが発生している、という矛盾を検出する)。
  # DISCONDTCがNAの被験者(対象レコードのいずれかにDSDTCが無かった等)は比較できないため対象外にする
  if (!is.null(discontinuation_date) && nrow(discontinuation_date) > 0 && "USUBJID" %in% colnames(data) && length(date_vars) > 0) {
    discon_by_usubjid <- discontinuation_date %>% filter(!is.na(DISCONDTC)) %>% distinct(USUBJID, DISCONDTC)
    for (var_name in intersect(date_vars, colnames(data))) {
      raw <- as.character(data[[var_name]])
      parsed <- suppressWarnings(ymd(raw))
      violations <- tibble(USUBJID = data[["USUBJID"]], value = parsed) %>%
        inner_join(discon_by_usubjid, by = "USUBJID") %>%
        filter(!is.na(value), value > DISCONDTC)

      add_check(
        str_c("no_records_after_discontinuation: ", var_name),
        nrow(violations) == 0,
        str_c(
          "中止日より後の行数: ", nrow(violations),
          if (nrow(violations) > 0) str_c(" (USUBJID例: ", paste(head(unique(violations[["USUBJID"]]), 3), collapse = ", "), ")") else ""
        )
      )
    }
  }

  bind_rows(results)
}

# other_domains(prefixをキーにしたnamed list)の全ドメインにvalidate_domain_generic()を適用し、
# domain列を付けて1つのtibbleにまとめる。
# special_checks(名前付きlist、prefix -> function(data, dm, cdisc_variable_values) -> tibble(check,passed,detail))を
# 渡すと、そのprefixだけ試験固有の追加チェックを実行して結果に加える(未指定のprefixは汎用チェックのみ)。
# discontinuation_date(build_discontinuation_date_table(ds)の結果)を渡すと、各ドメインの
# date型列に中止日より後の値が無いかも確認する(渡さなければスキップ)
validate_other_domains <- function(other_domains, dm, cdisc_variable_values, special_checks = list(), discontinuation_date = NULL) {
  other_domains %>%
    imap_dfr(function(data, prefix) {
      generic <- validate_domain_generic(data, prefix, cdisc_variable_values, dm, discontinuation_date)
      extra <- if (prefix %in% names(special_checks)) {
        special_checks[[prefix]](data, dm, cdisc_variable_values)
      } else {
        tibble(check = character(0), passed = logical(0), detail = character(0))
      }
      bind_rows(generic, extra) %>% mutate(domain = prefix, .before = 1)
    })
}

# チェック結果を表示する。1件でもFAILがあればstopでエラーにする
report_other_domains_validation <- function(results) {
  print(results, n = nrow(results))
  n_fail <- sum(!results[["passed"]])
  if (n_fail == 0) {
    cat("other_domainsバリデーション: 全", nrow(results), "件PASS\n")
  } else {
    fail_summary <- results %>% filter(!passed) %>% mutate(label = str_c(domain, ":", check)) %>% pull(label)
    stop(str_c("other_domainsバリデーション: ", n_fail, "件FAIL(", paste(fail_summary, collapse = ", "), ")"))
  }
}

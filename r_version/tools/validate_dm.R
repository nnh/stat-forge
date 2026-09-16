library(tidyverse)
library(lubridate)

# 先にload_edc_spec.Rを実行してdm/cdisc_variable_values/registration_nを作成しておくこと。
# 乱数で値が変わるDMドメインを、値そのものではなく「満たすべき構造的な条件」で自動チェックする
# (行数・USUBJIDの一意性・コードリスト範囲内・日付の妥当性など)。
# 目視確認と異なり、今後の修正で出方がおかしくなっていないかをそのまま再実行して確認できる

# DMドメインが満たすべき条件をチェックし、結果をtibble(check, passed, detail)で返す
validate_dm <- function(dm, cdisc_variable_values, registration_n) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  # 行数がregistration_nと一致する
  add_check("row_count", nrow(dm) == registration_n, str_c("nrow=", nrow(dm), ", registration_n=", registration_n))

  # USUBJIDが重複していない
  dup_usubjid <- dm[["USUBJID"]][duplicated(dm[["USUBJID"]])]
  add_check("usubjid_unique", length(dup_usubjid) == 0, str_c("重複: ", paste(unique(dup_usubjid), collapse = ", ")))

  # DOMAIN列が全て"DM"
  add_check("domain_is_dm", all(dm[["DOMAIN"]] == "DM"), "")

  # STUDYIDが全行で同一
  add_check("studyid_consistent", length(unique(dm[["STUDYID"]])) <= 1, str_c("値: ", paste(unique(dm[["STUDYID"]]), collapse = ", ")))

  # radio_button/check_box型の列は、コードリスト(空欄含む)の範囲内の値のみを持つ。
  # check_boxは複数選択がカンマ区切りで1つの文字列になるため、カンマで分割してから判定する
  dm_choice_spec <- cdisc_variable_values %>% filter(prefix == "DM", field_type %in% c("radio_button", "check_box"))
  for (var_name in intersect(unique(dm_choice_spec[["cdisc_variable"]]), colnames(dm))) {
    # 同じcdisc_variable名が別のalias_nameでmeddra/drug等の別field_typeとしても定義されている場合、
    # 単一のコードリストでは判定できないためスキップする
    all_field_types <- cdisc_variable_values %>% filter(prefix == "DM", cdisc_variable == var_name) %>% pull(field_type) %>% unique()
    if (!all(all_field_types %in% c("radio_button", "check_box"))) {
      next
    }

    var_spec <- dm_choice_spec %>% filter(cdisc_variable == var_name)
    valid_codes <- var_spec %>%
      mutate(code = ifelse(is.na(code), default_value, code)) %>%
      pull(code) %>%
      unique() %>%
      union("")

    observed <- dm[[var_name]][!is.na(dm[[var_name]])]
    if (any(var_spec[["field_type"]] == "check_box")) {
      observed <- unique(unlist(str_split(observed, ",")))
    }
    invalid <- setdiff(unique(observed), valid_codes)

    add_check(
      str_c("valid_codes: ", var_name),
      length(invalid) == 0,
      if (length(invalid) > 0) str_c("コードリスト外の値: ", paste(invalid, collapse = ", ")) else ""
    )
  }

  # date型の列は、日付(YYYY-MM-DD)としてパースでき、未来日でない。
  # as.Date()は完全に書式が崩れた文字列(数値がそのまま文字列化されてしまった等)だとエラーで
  # 停止してしまうため、パースできない値はNAを返すlubridate::ymd()を使う
  dm_date_vars <- cdisc_variable_values %>% filter(prefix == "DM", field_type == "date") %>% pull(cdisc_variable) %>% unique()
  for (var_name in intersect(dm_date_vars, colnames(dm))) {
    raw <- dm[[var_name]]
    non_na <- raw[!is.na(raw)]
    parsed <- suppressWarnings(ymd(non_na))
    unparsable <- non_na[is.na(parsed)]
    future_dates <- parsed[!is.na(parsed) & parsed > Sys.Date()]

    add_check(
      str_c("valid_date: ", var_name),
      length(unparsable) == 0 && length(future_dates) == 0,
      str_c("パース不可: ", length(unparsable), "件(", paste(head(unparsable, 5), collapse = ", "), "), 未来日: ", length(future_dates), "件")
    )
  }

  bind_rows(results)
}

# チェック結果を表示する。1件でもFAILがあればstopでエラーにする
report_dm_validation <- function(results) {
  print(results, n = nrow(results))
  n_fail <- sum(!results[["passed"]])
  if (n_fail == 0) {
    cat("DMバリデーション: 全", nrow(results), "件PASS\n")
  } else {
    stop(str_c("DMバリデーション: ", n_fail, "件FAIL(", paste(results[["check"]][!results[["passed"]]], collapse = ", "), ")"))
  }
}

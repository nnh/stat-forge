library(tidyverse)
library(lubridate)

# 先にload_edc_spec.Rを実行してae/dm/cdisc_variable_valuesを作成しておくこと。
# 乱数で値が変わるAEドメインを、値そのものではなく「満たすべき構造的な条件」で自動チェックする
# (USUBJIDがdmの範囲内・AESTDTC<=AEENDTC・コードリスト範囲内・日付の妥当性など)。
# 目視確認と異なり、今後の修正で出方がおかしくなっていないかをそのまま再実行して確認できる

# AEドメインが満たすべき条件をチェックし、結果をtibble(check, passed, detail)で返す。
# presence_conditions/meddraを指定すると、必須LLTコード(inject_required_llt_codes対象)の
# 出現チェックも行う(省略した場合はこのチェックをスキップする)
validate_ae <- function(ae, dm, cdisc_variable_values, presence_conditions = NULL, meddra = NULL) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  # DOMAIN列が全て"AE"
  add_check("domain_is_ae", all(ae[["DOMAIN"]] == "AE"), "")

  # STUDYIDが全行で同一
  add_check("studyid_consistent", length(unique(ae[["STUDYID"]])) <= 1, str_c("値: ", paste(unique(ae[["STUDYID"]]), collapse = ", ")))

  # AEにdm以外のUSUBJIDが混ざっていない(AEは被験者ごとに0件でもよいため、全USUBJIDのカバレッジは求めない)
  extra_usubjid <- setdiff(ae[["USUBJID"]], dm[["USUBJID"]])
  add_check("usubjid_subset_of_dm", length(extra_usubjid) == 0, str_c("DM以外: ", paste(extra_usubjid, collapse = ", ")))

  # AESTDTC(開始日)がAEENDTC(終了日)以前であること(同日は許容、両方値がある行のみ対象)。
  # 日付以外の時刻成分が万一残っていても影響しないよう、文字列経由でDate型に変換してから比較する
  if (all(c("AESTDTC", "AEENDTC") %in% colnames(ae))) {
    both_present <- !is.na(ae[["AESTDTC"]]) & !is.na(ae[["AEENDTC"]])
    aestdtc_date <- as.Date(as.character(ae[["AESTDTC"]]))
    aeendtc_date <- as.Date(as.character(ae[["AEENDTC"]]))
    reversed <- both_present & (aestdtc_date > aeendtc_date)
    add_check("start_before_end", !any(reversed), str_c("逆転している行数: ", sum(reversed)))
  }

  # AETOXGR=5(死亡)のAEENDTC(被験者ごとの最も早い日)より後にAESTDTCが開始する他のAEレコードが
  # 残っていないか確認する(死亡後に新たな有害事象が発生している、という矛盾を検出する)
  if (all(c("AETOXGR", "AESTDTC", "AEENDTC") %in% colnames(ae))) {
    ae_dates <- ae %>%
      mutate(
        AESTDTC_date = as.Date(as.character(AESTDTC)),
        AEENDTC_date = as.Date(as.character(AEENDTC))
      )
    death_dates <- ae_dates %>%
      filter(AETOXGR == "5", !is.na(AEENDTC_date)) %>%
      group_by(USUBJID) %>%
      summarise(DTHDTC = min(AEENDTC_date), .groups = "drop")

    violations <- ae_dates %>%
      inner_join(death_dates, by = "USUBJID") %>%
      filter(!is.na(AESTDTC_date), AESTDTC_date > DTHDTC)

    add_check(
      "no_records_after_death",
      nrow(violations) == 0,
      str_c(
        "死亡日より後に開始している行数: ", nrow(violations),
        if (nrow(violations) > 0) str_c(" (USUBJID例: ", paste(head(unique(violations[["USUBJID"]]), 3), collapse = ", "), ")") else ""
      )
    )
  }

  # presence_conditionsのうち、ref_cdisc_variableが"*LLTCD"(meddra参照)かつcondition_type=="equals"な
  # 行のexpected_valueは、必ずどこかのレコードに混ぜ込まれるべきLLTコード(inject_required_llt_codes対象)。
  # 少なくとも1件は実際のAELLTCDに出現しているか確認する(死亡日フィルタ等で一部が偶然除外されることは
  # あり得るため、全件出現までは求めない)。該当する行が無ければチェック自体をスキップする
  if (!is.null(presence_conditions) && "AELLTCD" %in% colnames(ae)) {
    required_llt_codes <- presence_conditions %>%
      filter(condition_type == "equals", str_detect(ref_cdisc_variable, "LLTCD$")) %>%
      pull(expected_value) %>%
      discard(~ is.na(.x) || .x == "") %>%
      unique()

    if (length(required_llt_codes) > 0) {
      observed_codes <- unique(ae[["AELLTCD"]])
      hit_codes <- intersect(required_llt_codes, observed_codes)
      missing_codes <- setdiff(required_llt_codes, observed_codes)

      # 対象コードをLLT日本語病名に変換してコンソールに表示する
      if (!is.null(meddra) && "llt_name_j" %in% colnames(meddra)) {
        code_to_name_j <- meddra %>% distinct(llt_code, llt_name_j)
        describe_codes <- function(codes) {
          if (length(codes) == 0) return("(なし)")
          names_j <- code_to_name_j[["llt_name_j"]][match(codes, code_to_name_j[["llt_code"]])]
          str_c(codes, "(", coalesce(names_j, "不明"), ")", collapse = ", ")
        }
        cat("  必須LLTコード 出現(", length(hit_codes), "/", length(required_llt_codes), "件): ", describe_codes(hit_codes), "\n", sep = "")
        if (length(missing_codes) > 0) {
          cat("  必須LLTコード 未出現: ", describe_codes(missing_codes), "\n", sep = "")
        }
      }

      add_check(
        "required_llt_codes_present",
        length(hit_codes) > 0,
        str_c("必須", length(required_llt_codes), "件中、出現: ", length(hit_codes), "件")
      )
    }
  }

  # radio_button/check_box型の列は、コードリスト(空欄含む)の範囲内の値のみを持つ。
  # check_boxは複数選択がカンマ区切りで1つの文字列になるため、カンマで分割してから判定する
  ae_choice_spec <- cdisc_variable_values %>% filter(prefix == "AE", field_type %in% c("radio_button", "check_box"))
  for (var_name in intersect(unique(ae_choice_spec[["cdisc_variable"]]), colnames(ae))) {
    # 同じcdisc_variable名が別のalias_nameでmeddra/drug等の別field_typeとしても定義されている場合、
    # 単一のコードリストでは判定できないためスキップする
    all_field_types <- cdisc_variable_values %>% filter(prefix == "AE", cdisc_variable == var_name) %>% pull(field_type) %>% unique()
    if (!all(all_field_types %in% c("radio_button", "check_box"))) {
      next
    }

    var_spec <- ae_choice_spec %>% filter(cdisc_variable == var_name)
    valid_codes <- var_spec %>%
      mutate(code = ifelse(is.na(code), default_value, code)) %>%
      pull(code) %>%
      unique() %>%
      union("")

    observed <- ae[[var_name]][!is.na(ae[[var_name]])]
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
  ae_date_vars <- cdisc_variable_values %>% filter(prefix == "AE", field_type == "date") %>% pull(cdisc_variable) %>% unique()
  for (var_name in intersect(ae_date_vars, colnames(ae))) {
    raw <- ae[[var_name]]
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
report_ae_validation <- function(results) {
  print(results, n = nrow(results))
  n_fail <- sum(!results[["passed"]])
  if (n_fail == 0) {
    cat("AEバリデーション: 全", nrow(results), "件PASS\n")
  } else {
    stop(str_c("AEバリデーション: ", n_fail, "件FAIL(", paste(results[["check"]][!results[["passed"]]], collapse = ", "), ")"))
  }
}

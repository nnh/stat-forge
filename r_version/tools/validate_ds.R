library(tidyverse)
library(lubridate)

# 先にload_edc_spec.Rを実行してds/dm/cdisc_variable_valuesを作成しておくこと。
# 乱数で値が変わるDSドメインを、値そのものではなく「満たすべき構造的な条件」で自動チェックする
# (USUBJIDのカバレッジ・DEATHの重複無し・コードリスト範囲内・日付の妥当性など)。
# 目視確認と異なり、今後の修正で出方がおかしくなっていないかをそのまま再実行して確認できる

# DSドメインが満たすべき条件をチェックし、結果をtibble(check, passed, detail)で返す
validate_ds <- function(ds, dm, cdisc_variable_values) {
  results <- list()
  add_check <- function(name, passed, detail = "") {
    results[[length(results) + 1]] <<- tibble(check = name, passed = passed, detail = detail)
  }

  # DOMAIN列が全て"DS"
  add_check("domain_is_ds", all(ds[["DOMAIN"]] == "DS"), "")

  # STUDYIDが全行で同一
  add_check("studyid_consistent", length(unique(ds[["STUDYID"]])) <= 1, str_c("値: ", paste(unique(ds[["STUDYID"]]), collapse = ", ")))

  # dmの全USUBJIDがDSに少なくとも1行存在し、DSにdm以外のUSUBJIDが混ざっていない
  missing_usubjid <- setdiff(dm[["USUBJID"]], ds[["USUBJID"]])
  extra_usubjid <- setdiff(ds[["USUBJID"]], dm[["USUBJID"]])
  add_check(
    "usubjid_coverage",
    length(missing_usubjid) == 0 && length(extra_usubjid) == 0,
    str_c("DSに無い: ", paste(missing_usubjid, collapse = ", "), " / DM以外: ", paste(extra_usubjid, collapse = ", "))
  )

  # DSTERM=="DEATH"が同一USUBJID×同一EPOCH内では高々1件(同じEPOCH内で死亡が重複しない)。
  # 異なるEPOCH(例: TREATMENT・FOLLOW-UP)にまたがって複数DEATH行を持つのは、
  # finalize_ds_disposition()のearly_death_probによる正当なケースのため許容する
  if ("DSTERM" %in% colnames(ds)) {
    group_cols <- if ("EPOCH" %in% colnames(ds)) c("USUBJID", "EPOCH") else "USUBJID"
    death_counts <- ds %>% filter(DSTERM == "DEATH") %>% count(across(all_of(group_cols)))
    dup_death <- death_counts %>% filter(n > 1) %>% pull(USUBJID) %>% unique()
    add_check("death_not_duplicated", length(dup_death) == 0, str_c("重複: ", paste(dup_death, collapse = ", ")))
  }

  # add_randomization_ds_rows(): 割り付け(ARM)がある被験者には"RANDOMIZED"のマイルストーン行が
  # 過不足なく追加されているか(ARMありなら必ず追加され、ARM無しには追加されない)を確認する。
  # dmにARM列自体が無い場合はスキップする
  if ("DSTERM" %in% colnames(ds) && "ARM" %in% colnames(dm)) {
    arm_usubjid <- dm %>% filter(ARM != "") %>% pull(USUBJID) %>% unique()
    randomized_usubjid <- ds %>% filter(DSTERM == "RANDOMIZED") %>% pull(USUBJID) %>% unique()

    missing_randomized <- setdiff(arm_usubjid, randomized_usubjid)
    add_check(
      "randomized_row_added_for_arm_subjects",
      length(missing_randomized) == 0,
      str_c(
        "ARMありなのにRANDOMIZED行が無い被験者数: ", length(missing_randomized),
        if (length(missing_randomized) > 0) str_c(" (例: ", paste(head(missing_randomized, 5), collapse = ", "), ")") else ""
      )
    )

    unexpected_randomized <- setdiff(randomized_usubjid, arm_usubjid)
    add_check(
      "randomized_row_not_added_without_arm",
      length(unexpected_randomized) == 0,
      str_c(
        "ARM無しなのにRANDOMIZED行がある被験者数: ", length(unexpected_randomized),
        if (length(unexpected_randomized) > 0) str_c(" (例: ", paste(head(unexpected_randomized, 5), collapse = ", "), ")") else ""
      )
    )
  }

  # add_randomization_ds_rows()がEDC仕様のコードリストとは無関係に固定挿入する
  # 無作為化マイルストーン行の値(EDCフォーム上には存在しない標準SDTM値)。valid_codesチェックの
  # 誤検知を避けるため、該当するcdisc_variableの許容値にあらかじめ加えておく
  randomization_allowed_codes <- list(DSCAT = "PROTOCOL MILESTONE", DSTERM = "RANDOMIZED", DSDECOD = "RANDOMIZED")

  # radio_button/check_box型の列は、コードリスト(空欄含む)の範囲内の値のみを持つ。
  # check_boxは複数選択がカンマ区切りで1つの文字列になるため、カンマで分割してから判定する
  ds_choice_spec <- cdisc_variable_values %>% filter(prefix == "DS", field_type %in% c("radio_button", "check_box"))
  for (var_name in intersect(unique(ds_choice_spec[["cdisc_variable"]]), colnames(ds))) {
    # 同じcdisc_variable名が別のalias_nameでmeddra/drug等の別field_typeとしても定義されている場合、
    # 単一のコードリストでは判定できないためスキップする
    all_field_types <- cdisc_variable_values %>% filter(prefix == "DS", cdisc_variable == var_name) %>% pull(field_type) %>% unique()
    if (!all(all_field_types %in% c("radio_button", "check_box"))) {
      next
    }

    var_spec <- ds_choice_spec %>% filter(cdisc_variable == var_name)
    valid_codes <- var_spec %>%
      mutate(code = ifelse(is.na(code), default_value, code)) %>%
      pull(code) %>%
      unique() %>%
      union("") %>%
      union(randomization_allowed_codes[[var_name]])

    observed <- ds[[var_name]][!is.na(ds[[var_name]])]
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
  # DSSTDTC等は、add_randomization_ds_rows()が追加するRANDOMIZED行のように正当に空欄になる行が
  # あり得る。R版(in-memory)では未設定列はNAだが、Web版はCSV経由(na=character(0)で読み込むため
  # 空文字列"")なので、NAと空文字列の両方を「値なし」として除外してからパースする。
  # raw自体はR版だとDate型のことがあり、Date型のまま""と比較するとas.Date("")のパース失敗で
  # 全行NAになってしまうため、先にas.character()で文字列化してから判定する
  ds_date_vars <- cdisc_variable_values %>% filter(prefix == "DS", field_type == "date") %>% pull(cdisc_variable) %>% unique()
  for (var_name in intersect(ds_date_vars, colnames(ds))) {
    raw <- as.character(ds[[var_name]])
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

  bind_rows(results)
}

# チェック結果を表示する。1件でもFAILがあればstopでエラーにする
report_ds_validation <- function(results) {
  print(results, n = nrow(results))
  n_fail <- sum(!results[["passed"]])
  if (n_fail == 0) {
    cat("DSバリデーション: 全", nrow(results), "件PASS\n")
  } else {
    stop(str_c("DSバリデーション: ", n_fail, "件FAIL(", paste(results[["check"]][!results[["passed"]]], collapse = ", "), ")"))
  }
}

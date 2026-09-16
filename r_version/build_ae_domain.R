library(tidyverse)
library(here)

source(here("build_domain_common.R"))

build_ae_domain <- function(dm, n = 100) {
  ae <- tibble(
    USUBJID = sample(dm[["USUBJID"]], n, replace = TRUE)
  )
  # RFSTDTC(症例登録日)も結合しておく。AEの日付項目は明示的なref()参照を持たず、
  # 従来はregistration_start_date(試験共通の定数)を下限にしていたため、稀に被験者本人の
  # 登録日より前の日付が生成され得た。populate_ae_domain()側でこれを下限として使う
  # (最終的な出力列には含めない。populate_ae_domain()末尾で取り除く)
  ae <- ae %>% left_join(dm %>% select(USUBJID, STUDYID, any_of("RFSTDTC")), by = "USUBJID")
  ae[["DOMAIN"]] <- "AE"

  ae %>% select(STUDYID, DOMAIN, USUBJID, any_of("RFSTDTC"))
}

populate_ae_domain <- function(ae, cdisc_variable_values, registration_start_date, meddra, presence_conditions, numeric_bounds = NULL, field_ref_bounds = NULL, required_llt_codes = character(0), who_drug_idf = NULL, active_sheet_table = NULL, date_ref_bounds = NULL, built_domains = list(), cdisc_variable_to_prefix = NULL) {
  ae_spec <- cdisc_variable_values %>% filter(prefix == "AE")

  # レコードごとにalias_nameを割り当てる。active_sheet_table(USUBJID, alias_name)が指定されている場合、
  # その行のUSUBJIDにとって実際に有効な(そのシートが表示される)alias_nameだけから選ぶ
  # (どのalias_nameも有効でない被験者の行は、AE報告自体が存在しないとみなして除外する)
  alias_names <- ae_spec[["alias_name"]] %>% unique()
  if (!is.null(active_sheet_table)) {
    eligible <- active_sheet_table %>% filter(alias_name %in% alias_names)
    eligible_pool <- split(eligible[["alias_name"]], eligible[["USUBJID"]])
    ae[["alias_name"]] <- map_chr(ae[["USUBJID"]], function(usubjid) {
      pool <- eligible_pool[[usubjid]]
      if (is.null(pool) || length(pool) == 0) NA_character_ else sample(pool, 1)
    })
    ae <- ae %>% filter(!is.na(alias_name))
  } else {
    ae[["alias_name"]] <- sample(alias_names, size = nrow(ae), replace = TRUE)
  }

  target_vars <- compute_target_vars(ae, ae_spec)
  ae <- ae %>% populate_radio_button_fields(ae_spec, target_vars, numeric_bounds)

  # date: AEENDTC>=AESTDTCの前後関係や、sae_reportのAESTDTC<-MH(registration)のMHSTDTC(診断日)の
  # ような他ドメイン参照はdate_ref_boundsに定義されている。以前はAEENDTC>=AESTDTCの関係だけを
  # ハードコードし、他ドメイン参照は一切考慮していなかった(sae_reportのAESTDTCが診断日より前に
  # なり得るバグの原因)。他の汎用ドメイン(build_generic_domain/build_repeated_domain)と同じく
  # inject_cross_domain_refs()で参照先の値を結合してからpopulate_date_fields()に通すことで、
  # date_ref_boundsに定義された全ての制約(AEENDTC>=AESTDTCを含む)を一律に反映する
  ae_date_vars <- ae_spec %>%
    filter(field_type == "date") %>%
    pull(cdisc_variable) %>%
    unique() %>%
    intersect(target_vars)
  # date_ref_boundsは全ドメイン分を含む共通テーブルのため、AE自身のcdisc_variableに関する行だけに絞る
  ae_date_ref_bounds <- if (!is.null(date_ref_bounds)) date_ref_bounds %>% filter(cdisc_variable %in% ae_spec[["cdisc_variable"]]) else NULL
  date_injected <- inject_cross_domain_refs(ae, NULL, NULL, built_domains, cdisc_variable_to_prefix, NULL, ae_date_ref_bounds, own_prefix = "AE")
  ae <- date_injected[["data"]]
  # RFSTDTC(症例登録日)がある場合、明示的なref()参照(date_ref_bounds)を持たない日付項目の下限を
  # registration_start_date(試験共通の定数)ではなく被験者本人のRFSTDTCにする(build_ae_domain()で
  # 結合済み。populate_date_fields()内のhas_rfstdtc分岐が判定する)
  ae <- populate_date_fields(ae, ae_spec, ae_date_vars, registration_start_date, ae_date_ref_bounds)

  # AE報告が複数のalias(シート、例: "sae_report"/"ae2")にまたがる場合、シートの本来の並び順
  # (sheet_seq)に沿うようalias単位でまとめて日付をシフトする(同じ行のAESTDTC<=AEENDTCの関係は保つ)。
  # このシフトはae自身の日付列だけをまとめて動かすため、他ドメイン参照(date_ref_bounds、例:
  # sae_reportのAESTDTC<-MHSTDTC)の下限が再び崩れる場合がある(同じUSUBJIDが"ae"と"sae_report"の
  # 両方を持つ場合など)。build_generic_domain等はreorder後にclamp_dates_to_discontinuation()を
  # 再度呼んで修復しているが、AEはこの時点でDS/中止日情報をまだ持たないため、その簡易版
  # (reclamp_ae_dates_to_ref_bounds、discon超過は扱わずdate_ref_boundsのmin_date違反のみ対象)で
  # 修復する。参照列(MHSTDTC等)が必要なため、injected_colsを取り除くのはこの後にする
  ae <- reorder_dates_by_sheet_seq(ae, ae_date_vars, ae_spec, registration_start_date, date_ref_bounds = ae_date_ref_bounds)
  ae <- reclamp_ae_dates_to_ref_bounds(ae, ae_date_vars, ae_date_ref_bounds)
  ae <- ae %>% select(-any_of(date_injected[["injected_cols"]]))

  # meddra: field_type=="meddra"に該当する変数はLLT名を直接格納し、MedDRAコーディングブロック(LLT〜SOC)を追加
  meddra_vars <- compute_meddra_vars(ae_spec, target_vars)
  meddra_sample <- sample_meddra_rows(meddra, nrow(ae)) %>%
    inject_required_llt_codes(meddra, required_llt_codes)
  ae <- ae %>%
    populate_meddra_fields(ae_spec, meddra_vars, meddra, meddra_sample) %>%
    add_meddra_coding_block(meddra_sample, "AE")

  # "ae"シートのように、AE報告と同じフォーム上に他prefix(例: FA)のブロックがある場合、
  # そのフィールドも同じ行に追加する。presence_conditionsが同じ行内で完結するようにするため、
  # apply_presence_conditionsの前に行う
  linked <- populate_linked_blocks(ae, cdisc_variable_values, "AE", registration_start_date, meddra, who_drug_idf, date_ref_bounds)
  ae <- linked[["data"]]
  linked_spec <- linked[["linked_spec"]]

  # 上記以外のfield_type: とりあえずダミー値を格納
  ae <- ae %>%
    populate_dummy_fields(target_vars) %>%
    apply_presence_conditions(presence_conditions) %>%
    apply_field_ref_bounds(ae_spec, field_ref_bounds)

  # AETOXGR=5(死亡)のAEENDTCより後に開始する他のAEは矛盾するため除外
  if (all(c("AETOXGR", "AESTDTC", "AEENDTC") %in% colnames(ae))) {
    death_dates <- ae %>%
      filter(AETOXGR == "5") %>%
      group_by(USUBJID) %>%
      summarise(DTHDTC = min(AEENDTC), .groups = "drop")

    ae <- ae %>%
      left_join(death_dates, by = "USUBJID") %>%
      filter(is.na(DTHDTC) | AESTDTC <= DTHDTC) %>%
      select(-DTHDTC)
  }

  # AESPIDはUSUBJID内の通番 (例: sae_report1, sae_report2)
  ae <- ae %>%
    group_by(USUBJID) %>%
    mutate(AESPID = str_c(alias_name, row_number())) %>%
    ungroup()

  # AESEQはUSUBJID・AESTDTC・AESPIDの昇順で振る(同日にAETOXGR=5(死亡)と他のAEがある場合の
  # 前後関係は問わない)
  ae <- ae %>%
    arrange(USUBJID, AESTDTC, AESPID) %>%
    add_seq("AESEQ")

  # populate_linked_blocks()で同じ行に追加した他prefix(例: FA)の列を、対応するドメインの
  # 断片テーブルに分離する(AESPIDをそのままprefixSPIDとして引き継ぎ、どのAE報告に対応するか分かるようにする)。
  # AE自身の返り値には、リンク先prefixの列とalias_nameは含めない
  linked_domains <- split_linked_domains(ae, linked_spec, "AESPID")
  ae <- ae %>% select(-alias_name, -any_of("RFSTDTC"), -any_of(linked_spec[["cdisc_variable"]] %>% unique()))

  # 列順を整理: STUDYID/DOMAIN/USUBJID/AESEQ/AESPID -> meddra項目 -> MedDRAコーディングブロック -> その他 -> AETOXGR/AESTDTC/AEENDTC
  ae <- ae %>%
    reorder_domain_columns(
      front_cols = c(domain_front_cols("AE"), meddra_vars, meddra_coding_cols("AE")),
      end_cols = c("AETOXGR", "AESTDTC", "AEENDTC")
    )

  list(ae = ae, linked = linked_domains)
}

# reorder_dates_by_sheet_seq()による同日ブロックの入れ替えでdate_ref_bounds(他ドメイン参照。例:
# sae_reportのAESTDTC<-MHSTDTC)のmin_date制約が再び崩れた行だけ、参照値以降になるよう最小限で
# 再生成する。build_domain_common.Rのclamp_dates_to_discontinuation()と同じ考え方の簡易版だが、
# AEはこの時点でDS/中止日情報をまだ持たないため、discon超過は扱わずmin_date違反のみを対象にする
reclamp_ae_dates_to_ref_bounds <- function(ae, date_vars, date_ref_bounds) {
  if (is.null(date_ref_bounds) || length(date_vars) == 0) {
    return(ae)
  }
  # date_vars同士の依存(AEENDTC>=AESTDTC)がある場合、参照先(AESTDTC)を先に直してから
  # 参照元(AEENDTC)を判定しないと関係が崩れるため、populate_date_fields()と同じ理由で並べ替える
  ordered_date_vars <- date_vars
  date_deps <- date_ref_bounds %>% filter(cdisc_variable %in% date_vars, ref_cdisc_variable %in% date_vars)
  if (nrow(date_deps) > 0) {
    sorted_date_vars <- character(0)
    remaining <- date_vars
    while (length(remaining) > 0) {
      unresolved <- date_deps %>% filter(ref_cdisc_variable %in% remaining) %>% pull(cdisc_variable) %>% unique()
      ready <- setdiff(remaining, unresolved)
      if (length(ready) == 0) {
        sorted_date_vars <- c(sorted_date_vars, remaining)
        break
      }
      sorted_date_vars <- c(sorted_date_vars, ready)
      remaining <- setdiff(remaining, ready)
    }
    ordered_date_vars <- sorted_date_vars
  }

  for (var_name in ordered_date_vars) {
    if (!(var_name %in% colnames(ae))) next
    min_ref_vals <- resolve_date_ref_bound_vals(ae, date_ref_bounds, var_name, "min_date")
    if (is.null(min_ref_vals)) next
    current <- as.Date(as.character(ae[[var_name]]))
    violated <- !is.na(current) & !is.na(min_ref_vals) & current < min_ref_vals
    if (!any(violated)) next
    lower <- min_ref_vals[violated]
    upper <- pmax(lower, Sys.Date())
    ae[[var_name]][violated] <- as.character(lower + floor(runif(sum(violated), 0, as.numeric(upper - lower) + 1)))
  }
  ae
}

build_death_date_table <- function(ae) {
  ae %>%
    filter(AETOXGR == "5") %>%
    group_by(USUBJID) %>%
    summarise(DTHDTC = min(AEENDTC), .groups = "drop")
}

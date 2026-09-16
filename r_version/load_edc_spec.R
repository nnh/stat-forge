library(jsonlite)
library(tidyverse)
library(here)

source(here("constant.R"))
source(here("user_input.R"))
source(here("generate_random_date.R"))
source(here("generate_brthdtc.R"))
source(here("build_dm_domain.R"))
source(here("build_ae_domain.R"))
source(here("build_meddra_soc_pt_llt.R"))
source(here("build_ds_domain.R"))
source(here("build_validator_table.R"))
source(here("build_cdisc_variable_values.R"))
source(here("build_generation_constraints.R"))
source(here("build_field_reference_table.R"))
source(here("lb_reference_ranges.R"))
source(here("tr_orres_values.R"))
source(here("vs_orres_values.R"))
source(here("fa_orres_values.R"))
source(here("read_who_drug_idf.R"))

# json_path(EDC仕様JSON)からDM/AE/DS/その他ドメインのダミーデータ一式を生成する。
# テスト・バリデーション専用(json_pathはtest_config.Rで管理する)。生成過程の全ての中間オブジェクト
# (edc_spec, sheets, cdisc_variable_values, presence_conditions, dm, ae, ds, other_domains等)を
# 呼び出し元のグローバル環境に代入するため、呼んだ後はそれらをそのまま参照できる
# (例: source(here("test_config.R")); source(here("load_edc_spec.R")); load_edc_spec(json_path))
load_edc_spec <- function(json_path) {
  if (!is.null(random_seed)) set.seed(random_seed)
  edc_spec <- jsonlite::read_json(json_path)
  sheets <- edc_spec[["sheets"]]
  sheet_groups <- edc_spec[["sheet_groups"]]

  cdisc <- build_cdisc_variable_values(edc_spec, sheets)
  df_cdisc <- cdisc[["df_cdisc"]]
  cdisc_variable_values <- cdisc[["cdisc_variable_values"]]

  validator_table <- build_validator_table(sheets)
  field_reference_table <- build_field_reference_table(sheets)

  # sheetsのcategoryが"ae_report"または"multiple"のalias_name一覧。
  # 該当するドメインのSPIDはAEドメインと同じ形式(alias_name + USUBJID内の連番)にする
  multi_record_alias_names <- sheets %>%
    keep(~ !is.null(.x[["category"]]) && .x[["category"]] %in% c("ae_report", "multiple")) %>%
    map_chr(~ .x[["alias_name"]])

  constraints <- build_generation_constraints(validator_table, df_cdisc, field_reference_table)
  presence_conditions <- constraints[["presence_conditions"]]
  required_vars <- constraints[["required_vars"]]
  required_var_instances <- constraints[["required_var_instances"]]
  numeric_bounds <- constraints[["numeric_bounds"]]
  field_numeric_bounds <- constraints[["field_numeric_bounds"]]
  field_ref_bounds <- constraints[["field_ref_bounds"]]
  date_ref_bounds <- constraints[["date_ref_bounds"]]
  age_bounds <- constraints[["age_bounds"]]

  # cdisc_variable_values(各生成関数にspecとして渡されるテーブル)に、そのalias_name/label/
  # cdisc_variableのインスタンスが実際にpresenceバリデータを持つかどうか(is_required)を付与する。
  # required_vars(cdisc_variable名だけでunique化したフラット版)と違い、同じcdisc_variable名が
  # 複数のalias_name/labelに定義されていても、インスタンスごとに正確に必須/非必須を判定できる。
  # labelがNAの行同士はleft_joinでマッチしないため、joinキーとしては空文字列に揃えてから結合する
  cdisc_variable_values <- cdisc_variable_values %>%
    mutate(join_label = coalesce(label, "")) %>%
    left_join(
      required_var_instances %>% mutate(join_label = coalesce(label, ""), is_required = TRUE) %>% select(-label),
      by = c("alias_name", "join_label", "cdisc_variable")
    ) %>%
    mutate(is_required = coalesce(is_required, FALSE)) %>%
    select(-join_label)

  # MedDRA
  meddra <- build_meddra_hierarchy(meddra_version)

  # WhoDrug/IDF
  who_drug_idf <- build_who_drug_idf(who_drug_idf_parent_dir, who_drug_idf_version_folder)

  # DM
  # STUDYIDはEDC仕様JSONのname(試験名)に"_dummy"を付けたものにする。固定のダミー値だと
  # どのJSONから生成したデータか分からなくなるため、生成データを見ただけで試験を判別できるようにする
  studyid <- str_c(edc_spec[["name"]], "_dummy")
  dm_result <- build_dm_domain(sheets, sheet_groups, n = registration_n, age_bounds = age_bounds, studyid = studyid)
  dm <- dm_result[["dm"]]
  active_sheet_table <- active_sheet_membership_table(dm_result[["active_sheets"]])
  visit_lookup <- build_visit_lookup(sheets, edc_spec[["visits"]])
  dm <- populate_dm_domain(dm, cdisc_variable_values, registration_start_date, meddra, presence_conditions, numeric_bounds, field_ref_bounds, age_bounds, date_ref_bounds)
  cdisc_variable_to_prefix <- build_cdisc_variable_to_prefix(cdisc_variable_values)

  # MH(registration、診断日ブロック)を他ドメインより先に生成しておく。sae_reportシートのAESTDTCは
  # このブロックのMHSTDTC(診断日)を下限として参照する(date_ref_bounds)が、MHドメイン全体は本来
  # build_other_domains側(AEより後)で生成される。MH全体を早めるのは影響範囲が大きいため、
  # 他ドメインに依存しない(DMのBRTHDTCのみに依存する)registrationブロックだけを先行生成し、
  # 後段のbuild_other_domains呼び出しにexisting_dataとして渡して二重生成を避ける(pre_built_domains)
  mh_registration_spec <- cdisc_variable_values %>% filter(prefix == "MH", alias_name == "registration")
  mh_registration <- if (nrow(mh_registration_spec) > 0) {
    build_repeated_domain(
      dm, mh_registration_spec, "MH", registration_start_date, meddra, presence_conditions, required_var_instances,
      add_coding_block = TRUE, built_domains = list(DM = dm), cdisc_variable_to_prefix = cdisc_variable_to_prefix,
      age_bounds = age_bounds, active_sheet_table = active_sheet_table, date_ref_bounds = date_ref_bounds,
      finalize = FALSE
    )
  } else {
    NULL
  }

  # AE
  ae <- dm %>% build_ae_domain()
  ae_result <- populate_ae_domain(
    ae, cdisc_variable_values, registration_start_date, meddra, presence_conditions, numeric_bounds, field_ref_bounds,
    required_ae_llt_codes, who_drug_idf, active_sheet_table, date_ref_bounds,
    built_domains = list(DM = dm, MH = mh_registration), cdisc_variable_to_prefix = cdisc_variable_to_prefix
  )
  ae <- ae_result[["ae"]]
  ae_linked_domains <- ae_result[["linked"]]
  death_date <- build_death_date_table(ae)
  # DS
  # discon/withdrawalシートのDSSTDTC(field6)はDM.RFSTDTCを参照する下限バリデータ(ref('registration',12))を
  # 持つため、built_domains/cdisc_variable_to_prefixを渡してDM側の値を結合できるようにする
  # (結合しないと下限が適用されず、DISCONDTCがRFSTDTCより前になり得る)
  ds <- build_ds_domain(dm, cdisc_variable_values)
  ds <- populate_ds_domain(ds, cdisc_variable_values, registration_start_date, meddra, presence_conditions, numeric_bounds, field_ref_bounds, date_ref_bounds, built_domains = list(DM = dm), cdisc_variable_to_prefix = cdisc_variable_to_prefix)
  ds <- finalize_ds_disposition(ds, death_date, cdisc_variable_values)
  discontinuation_date <- build_discontinuation_date_table(ds)

  # discon/withdrawalシートのDTC(中止日)はDM.RFSTDTC/RFICDTCを一切参照せずに独立生成されるため、
  # 稀に中止日がRFSTDTC(症例登録日)より前になる、という時系列上ありえない矛盾が生じることがある
  # (RFSTDTCを参照する他ドメイン(例: CEのCEDTC)で「中止日超過」として検出される)。
  # DSが確定した後のこの時点で、RFSTDTC(・その前提であるべきRFICDTC)が中止日を超えている被験者だけ、
  # 中止日以前になるよう遡って補正する
  if (all(c("USUBJID", "RFSTDTC") %in% colnames(dm))) {
    discon_lookup <- discontinuation_date %>% filter(!is.na(DISCONDTC)) %>% distinct(USUBJID, .keep_all = TRUE)
    discon_map <- set_names(discon_lookup[["DISCONDTC"]], discon_lookup[["USUBJID"]])
    discon_for_dm <- discon_map[dm[["USUBJID"]]]
    rfstdtc_over <- !is.na(discon_for_dm) & as.Date(dm[["RFSTDTC"]]) > discon_for_dm
    dm[["RFSTDTC"]][rfstdtc_over] <- as.character(discon_for_dm[rfstdtc_over])
    if ("RFICDTC" %in% colnames(dm)) {
      rficdtc_over <- as.Date(dm[["RFICDTC"]]) > as.Date(dm[["RFSTDTC"]])
      rficdtc_over[is.na(rficdtc_over)] <- FALSE
      dm[["RFICDTC"]][rficdtc_over] <- dm[["RFSTDTC"]][rficdtc_over]
    }
  }

  # registrationブロックはAEより前(discontinuation_dateが確定する前)に先行生成したため、
  # そのブロック自身の日付項目(例: 試験によってはMHSTDTCではなくMHDTCという名前のこともある)が
  # 中止日を超えていてもクランプされないまま残っている可能性がある。build_other_domains側の
  # existing_data経路はexisting_data自体を再クランプしない仕様のため、ここでdiscontinuation_date・
  # 上のRFSTDTC補正の両方が確定した時点で明示的にクランプしておく(RFSTDTC補正より前に行うと、
  # 中止日を超えたままの補正前RFSTDTCがデフォルト下限として使われ、クランプが効かなくなる)
  if (!is.null(mh_registration)) {
    mh_registration_reclamp_data <- inject_dm_rfstdtc(mh_registration, list(DM = dm))[["data"]]
    if ("BRTHDTC" %in% colnames(dm) && !("BRTHDTC" %in% colnames(mh_registration_reclamp_data))) {
      mh_registration_reclamp_data <- mh_registration_reclamp_data %>% left_join(dm %>% select(USUBJID, BRTHDTC), by = "USUBJID")
    }
    mh_registration_date_vars <- mh_registration_spec %>% filter(field_type == "date") %>% pull(cdisc_variable) %>% unique()
    mh_registration_date_ref_bounds <- date_ref_bounds %>% filter(cdisc_variable %in% mh_registration_spec[["cdisc_variable"]])
    mh_registration_presence_conditions <- presence_conditions %>% filter(cdisc_variable %in% mh_registration_spec[["cdisc_variable"]])
    mh_registration <- clamp_dates_to_discontinuation(
      mh_registration_reclamp_data, mh_registration_date_vars, registration_start_date, discontinuation_date,
      mh_registration_date_ref_bounds, existing_data = NULL, presence_conditions = mh_registration_presence_conditions
    ) %>% select(-any_of(c("RFSTDTC", "BRTHDTC")))
  }

  ds <- add_randomization_ds_rows(ds, dm, registration_start_date)

  # ae/sae_reportのように、AE報告と同じフォーム上の他prefixブロック(例: FA)は、
  # 既にpopulate_ae_domain側で(AE報告と同じ行として)生成済みのため、
  # build_other_domains側では二重生成しないよう該当のprefix/alias_nameを除外する
  cdisc_variable_values_for_others <- exclude_ae_linked_prefixes(cdisc_variable_values, ae_linked_domains)

  # その他のドメイン(DM/AE/DS以外)。同じalias_name内でcdisc_variableが複数labelを持つドメインは自動判定される。
  # 他ドメイン(DM/AE/DS含む)の変数を参照するpresence_conditions/field_ref_boundsがある場合は、
  # 依存順に生成し、built_domainsで既存のDM/AE/DSも参照できるようにする
  other_domains <- build_other_domains(
    dm, cdisc_variable_values_for_others, registration_start_date, meddra, presence_conditions, required_var_instances, numeric_bounds, field_ref_bounds,
    built_domains = list(DM = dm, AE = ae, DS = ds), age_bounds = age_bounds, multi_record_alias_names = multi_record_alias_names, who_drug_idf = who_drug_idf,
    active_sheet_table = active_sheet_table, visit_lookup = visit_lookup, discontinuation_date = discontinuation_date, date_ref_bounds = date_ref_bounds,
    pre_built_domains = list(MH = mh_registration), pre_built_alias_names = list(MH = "registration")
  )

  # alias_name/label/sheet_seqは他ドメイン生成時の突き合わせキーやDSSEQ並び替えに使い終わったため、
  # 最終出力からは取り除く
  ds <- ds %>% select(-any_of(c("alias_name", "label", "sheet_seq")))

  # AE報告と同じ行として生成したリンク先ブロック(例: FA)を、対応するドメインにマージする
  other_domains <- merge_linked_domains(other_domains, ae_linked_domains)

  # LB/TR/VS/FAのORRESを、EDC仕様の数値バリデーション(min/max)に基づいたそれらしい数値に置き換える
  # (対応するドメインが存在しない、またはTESTCD/ORRES列が無い場合は何もしない)
  other_domains <- apply_orres_populators(other_domains, list(
    LB = function(d) populate_lb_orres(d, cdisc_variable_values, field_numeric_bounds),
    TR = function(d) populate_tr_orres(d, cdisc_variable_values, field_numeric_bounds),
    VS = function(d) populate_vs_orres(d, cdisc_variable_values, field_numeric_bounds),
    FA = function(d) populate_fa_orres(d, cdisc_variable_values, field_numeric_bounds)
  ))

  # DD(死因)は死亡した被験者のみのレコードにする(DDTEST/DDTESTCDのような固定値の列ではなく、
  # presence_conditionsで条件付けされている列(例: DDORRES)が全てNAの行を除外)
  if ("DD" %in% names(other_domains)) {
    dd_gated_vars <- presence_conditions %>% filter(cdisc_variable %in% colnames(other_domains[["DD"]])) %>% pull(cdisc_variable) %>% unique()
    other_domains[["DD"]] <- drop_empty_domain_rows(other_domains[["DD"]], dd_gated_vars)
  }

  # prefixSEQ列を持つドメインは、その列で行を並べ替えておく(mergeやfilter等で崩れた行順を
  # 最終出力前に揃えるため)
  ae <- sort_by_seq(ae, "AE")
  dm <- sort_by_seq(dm, "DM")
  ds <- sort_by_seq(ds, "DS")
  other_domains <- other_domains %>% imap(sort_by_seq)

  # 生成データ(ae/dm/ds/other_domains)をCSVとして出力する(ドメイン名の大文字+"_dummy.csv"、例: DM_dummy.csv)。
  # RのNAは空欄として書き出す(コードリストの選択肢として文字列"NA"が使われているケースがあるため、
  # 空欄と文字列としての"NA"を区別できるようにするため)
  if (!dir.exists(output_csv_dir)) dir.create(output_csv_dir, recursive = TRUE)
  export_datasets <- c(list(AE = ae, DM = dm, DS = ds), other_domains)
  iwalk(export_datasets, ~ write_csv(.x, file.path(output_csv_dir, str_c(.y, "_dummy.csv")), na = ""))

  # 生成過程の全オブジェクト(dm/ae/ds/cdisc_variable_values/presence_conditions等)を、
  # 呼び出し元のグローバル環境に代入する(source()実行後の状態と同様に直接参照できるようにするため)
  list2env(as.list(environment()), envir = .GlobalEnv)
  invisible(NULL)
}

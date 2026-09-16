library(tidyverse)
library(here)

source(here("constant.R"))
source(here("generate_brthdtc.R"))
source(here("build_domain_common.R"))

# sheet_groupsのうちis_default==TRUEのグループに含まれるsheetのalias_name一覧を返す。
# defaultグループは全被験者が共通して持つシート集合を表す。
# allocation2/3/4のように治療経過の途中(defaultグループに含まれない、特定の群/時点でのみ表示される)の
# 「実施した/しなかった」報告を、無作為化群(ARM)の判定に誤って使わないようにするため
default_sheet_alias_names <- function(sheet_groups) {
  sheet_groups %>%
    keep(~ coalesce(.x[["is_default"]], FALSE)) %>%
    map(~ map_chr(.x[["sheets"]], "alias_name")) %>%
    unlist() %>%
    unique()
}

# category=="visit"のシートは、name(シート表示名)の末尾に"(VisitName)"という形でedc_spec$visitsの
# nameが含まれている(例: "効果判定報告(End of Induction Cycle1)")。これを抽出してvisitsと突き合わせ、
# (alias_name, VISIT, VISITNUM)のテーブルを作る。一致しない行(末尾が"(...)"形式でない等)は含めない
build_visit_lookup <- function(sheets, visits) {
  if (length(visits) == 0) {
    return(tibble(alias_name = character(0), VISIT = character(0), VISITNUM = character(0)))
  }
  visit_table <- visits %>% map_dfr(~ tibble(VISIT = .x[["name"]], VISITNUM = .x[["num"]]))

  sheets %>%
    keep(~ coalesce(.x[["category"]] == "visit", FALSE)) %>%
    map_dfr(~ tibble(
      alias_name = .x[["alias_name"]],
      VISIT = str_match(.x[["name"]], "\\(([^()]+)\\)$")[, 2]
    )) %>%
    filter(!is.na(VISIT)) %>%
    inner_join(visit_table, by = "VISIT")
}

# 被験者ごとに、有効なalias_name(そのシートが実際にその被験者に表示される)集合と、
# 割り付け系シート(category=="allocation")ごとに割り当てたcodeを計算する。
# defaultグループのシートを起点に、割り付け結果に応じて追加で有効になる非defaultグループのシートを
# 連鎖的にたどる(sheet_groupsの非defaultグループは、allocation_sheet/allocation_groupが指す
# 割り付け結果に応じて追加で表示されるシート集合を表す。そのシートの中に別の割り付けシートが
# 含まれる場合、その結果も再帰的に評価することで、実際の臨床上の前後関係を再現する)。
# 戻り値: list(active_sheets = USUBJID -> alias_name一覧のnamed list,
#              assigned_codes = USUBJID -> (alloc_alias -> code)のnamed list)
build_subject_active_sheets <- function(sheets, sheet_groups, usubjids) {
  default_alias <- default_sheet_alias_names(sheet_groups)

  # sheet_groups(defaultも非defaultも含む)のどこにも登場しないシート(例: registration)は、
  # 割り付けによる条件分岐の対象外(=常に表示される)とみなし、defaultと同様に全被験者の
  # 初期有効集合に含める
  all_grouped_alias_names <- sheet_groups %>%
    map(~ map_chr(.x[["sheets"]], "alias_name")) %>%
    unlist() %>%
    unique()
  all_sheet_alias_names <- map_chr(sheets, ~ .x[["alias_name"]])
  ungrouped_alias_names <- setdiff(all_sheet_alias_names, all_grouped_alias_names)
  initial_active <- union(default_alias, ungrouped_alias_names)

  allocation_sheets <- sheets %>% keep(~ coalesce(.x[["category"]] == "allocation", FALSE))
  allocation_lookup <- allocation_sheets %>% set_names(map_chr(., ~ .x[["alias_name"]]))
  non_default_groups <- sheet_groups %>% discard(~ coalesce(.x[["is_default"]], FALSE))

  active <- map(usubjids, ~ initial_active) %>% set_names(usubjids)
  assigned_codes <- map(usubjids, ~ list()) %>% set_names(usubjids)

  if (length(allocation_lookup) == 0) {
    return(list(active_sheets = active, assigned_codes = assigned_codes))
  }

  repeat {
    changed <- FALSE
    for (usubjid in usubjids) {
      reachable_allocs <- intersect(active[[usubjid]], names(allocation_lookup))
      unassigned <- setdiff(reachable_allocs, names(assigned_codes[[usubjid]]))
      for (alloc_alias in unassigned) {
        codes <- allocation_lookup[[alloc_alias]][["allocation"]][["groups"]] %>% map_chr(~ .x[["code"]])
        if (length(codes) == 0) next
        code <- sample(codes, 1)
        assigned_codes[[usubjid]][[alloc_alias]] <- code
        changed <- TRUE

        matching_groups <- non_default_groups %>%
          keep(~ identical(.x[["allocation_sheet"]][["alias_name"]], alloc_alias) && identical(.x[["allocation_group"]], code))
        for (g in matching_groups) {
          active[[usubjid]] <- union(active[[usubjid]], map_chr(g[["sheets"]], "alias_name"))
        }
      }
    }
    if (!changed) break
  }

  list(active_sheets = active, assigned_codes = assigned_codes)
}

# active_sheets(USUBJID -> alias_name一覧のnamed list)を(USUBJID, alias_name)の縦持りテーブルにする。
# ドメイン生成側で、被験者ごとに有効なalias_nameへ絞り込むための結合キーとして使う
active_sheet_membership_table <- function(active_sheets) {
  active_sheets %>% imap_dfr(~ tibble(USUBJID = .y, alias_name = .x))
}

build_dm_domain <- function(sheets, sheet_groups, n = 100, age_bounds = NULL, studyid = "dummy-studyid") {
  dm <- tibble(
    SITEID = sample(dummy_site$SITEID, n, replace = TRUE),
    SUBJID = str_pad(1:n, width = 4, pad = "0")
  )
  dm[["STUDYID"]] <- studyid
  dm[["DOMAIN"]] <- "DM"
  dm[["USUBJID"]] <- str_c(dm[["STUDYID"]], dm[["SUBJID"]], sep = "-")
  birth_age_range <- compute_birth_age_range(age_bounds)
  dm <- generate_brthdtc(dm, var_name = "BRTHDTC", min_age = birth_age_range[["min_age"]], max_age = birth_age_range[["max_age"]])

  active_result <- build_subject_active_sheets(sheets, sheet_groups, dm[["USUBJID"]])

  # ARMは、defaultグループに属する割り付けシート(通常1つ)に割り当てられたcodeとする
  default_alias <- default_sheet_alias_names(sheet_groups)
  default_allocation_alias <- sheets %>%
    keep(~ identical(.x[["category"]], "allocation") && .x[["alias_name"]] %in% default_alias) %>%
    map_chr(~ .x[["alias_name"]])

  dm[["ARM"]] <- if (length(default_allocation_alias) == 0) {
    ""
  } else {
    map_chr(dm[["USUBJID"]], function(usubjid) {
      codes <- active_result[["assigned_codes"]][[usubjid]][default_allocation_alias]
      codes <- codes[!map_lgl(codes, is.null)]
      if (length(codes) == 0) "" else codes[[1]]
    })
  }

  list(
    dm = dm %>% select(STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, BRTHDTC, ARM),
    active_sheets = active_result[["active_sheets"]]
  )
}

populate_dm_domain <- function(dm, cdisc_variable_values, registration_start_date, meddra, presence_conditions, numeric_bounds = NULL, field_ref_bounds = NULL, age_bounds = NULL, date_ref_bounds = NULL) {
  dm_spec <- cdisc_variable_values %>% filter(prefix == "DM")
  target_vars <- compute_target_vars(dm, dm_spec)

  dm <- dm %>%
    populate_radio_button_fields(dm_spec, target_vars, numeric_bounds) %>%
    populate_date_fields(dm_spec, target_vars, registration_start_date, date_ref_bounds) %>%
    populate_dummy_fields(target_vars)

  # RFSTDTC(症例登録日)は、populate_date_fields()による独立生成のままだとRFICDTC(同意取得日)や
  # 実際の無作為化(DS RANDOMIZED)と無関係な日付になり、稀にRFSTDTCが被験者の中止日より後になる
  # という時系列上の矛盾が生じる(discon以降の日付を禁止する他ドメインのチェックで検出される)。
  # add_randomization_ds_rows()のRANDOMIZEDレコードと同じ考え方で、RFICDTCがあればその数日以内、
  # 無ければ登録開始日から数日以内にすることで、無作為化のタイミングと整合させる
  if ("RFSTDTC" %in% colnames(dm)) {
    if ("RFICDTC" %in% colnames(dm)) {
      base_date <- as.Date(dm[["RFICDTC"]])
      base_date[is.na(base_date)] <- as.Date(registration_start_date)
      offset <- sample(0:3, nrow(dm), replace = TRUE)
    } else {
      base_date <- as.Date(registration_start_date)
      offset <- sample(0:7, nrow(dm), replace = TRUE)
    }
    dm[["RFSTDTC"]] <- as(pmin(base_date + offset, Sys.Date()), class(dm[["RFSTDTC"]]))
  }

  meddra_vars <- compute_meddra_vars(dm_spec, target_vars)
  if (length(meddra_vars) > 0) {
    meddra_sample <- sample_meddra_rows(meddra, nrow(dm))
    dm <- dm %>% populate_meddra_fields(dm_spec, meddra_vars, meddra, meddra_sample)
  }

  dm %>%
    apply_presence_conditions(presence_conditions) %>%
    apply_field_ref_bounds(dm_spec, field_ref_bounds) %>%
    apply_age_date_bounds(age_bounds, registration_start_date)
}

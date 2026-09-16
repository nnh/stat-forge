library(tidyverse)
library(here)

source(here("build_domain_common.R"))

build_ds_domain <- function(dm, cdisc_variable_values) {
  ds_spec <- cdisc_variable_values %>% filter(prefix == "DS")

  # DSEPOCHが存在する場合はそのdefault_value(エポック)ごとに、USUBJID×エポックのレコードを作る。
  # エポックはsheet_seqの昇順に並べ、その順にレコードを積み上げることで
  # 同一USUBJID内での行の並びがシートの登場順(=経過順)と一致するようにする。
  # 出力列名はSDTM標準に合わせてDSEPOCHではなくEPOCHにする。
  # DSSPIDには、そのエポックの元になったシートのalias_nameを入れる。
  # alias_name/labelも保持しておく(他ドメインが「discon」シートのDSTERMのように特定のDSブロックを
  # 参照する場合、inject_cross_domain_refs()がそのインスタンスを正しく特定できるようにするため。
  # 最終的な出力からは、load_edc_spec.R側で他ドメイン生成に使い終わった後に取り除く)
  if ("DSEPOCH" %in% ds_spec[["cdisc_variable"]]) {
    epoch_table <- ds_spec %>%
      filter(cdisc_variable == "DSEPOCH") %>%
      distinct(alias_name, label, default_value, sheet_seq) %>%
      arrange(sheet_seq)
    ds <- epoch_table %>%
      pmap_dfr(function(alias_name, label, default_value, sheet_seq) {
        dm %>% select(USUBJID, STUDYID) %>% mutate(EPOCH = default_value, DSSPID = alias_name, alias_name = alias_name, label = label, sheet_seq = sheet_seq)
      })
  } else {
    ds <- dm %>% select(USUBJID, STUDYID)
  }

  ds[["DOMAIN"]] <- "DS"
  # sheet_seq(シートの本来の並び順)はDSSEQを振る際の並び替えキーとして使う。alias_name/labelと
  # 同様、最終的な出力からはload_edc_spec.R側で使い終わった後に取り除く
  ds %>% select(STUDYID, DOMAIN, USUBJID, any_of(c("DSSPID", "EPOCH", "alias_name", "label", "sheet_seq")))
}

# DSSEQを振るための並び替え。USUBJID・DSSTDTC・sheet_seq(シートの本来の並び順)の順で昇順にする。
# DSSTDTCが無い行(例: RANDOMIZED)は、実際の日付より前に来るよう非常に早い日付として扱う
# (その被験者の他の全行より前に来ることを表す。sheet_seqが無い行があれば、それも他の全sheet_seq
# より前として扱う)。DSSTDTC/sheet_seqのどちらの列も無ければUSUBJIDのみで並べる
sort_ds_for_seq <- function(ds) {
  has_dsstdtc <- "DSSTDTC" %in% colnames(ds)
  has_sheet_seq <- "sheet_seq" %in% colnames(ds)
  if (!has_dsstdtc && !has_sheet_seq) {
    return(ds %>% arrange(USUBJID))
  }
  if (has_dsstdtc) {
    ds[[".sort_dsstdtc"]] <- coalesce(ds[["DSSTDTC"]], as.Date("1900-01-01"))
  }
  if (has_sheet_seq) {
    ds[[".sort_sheet_seq"]] <- coalesce(ds[["sheet_seq"]], -Inf)
  }
  sorted <- if (has_dsstdtc && has_sheet_seq) {
    ds %>% arrange(USUBJID, .sort_dsstdtc, .sort_sheet_seq)
  } else if (has_dsstdtc) {
    ds %>% arrange(USUBJID, .sort_dsstdtc)
  } else {
    ds %>% arrange(USUBJID, .sort_sheet_seq)
  }
  sorted %>% select(-any_of(c(".sort_dsstdtc", ".sort_sheet_seq")))
}

populate_ds_domain <- function(ds, cdisc_variable_values, registration_start_date, meddra, presence_conditions, numeric_bounds = NULL, field_ref_bounds = NULL, date_ref_bounds = NULL, built_domains = list(), cdisc_variable_to_prefix = NULL) {
  # DSEPOCHはbuild_ds_domain()側でEPOCHという列名として既に生成済みのため、
  # spec上のcdisc_variable名のままtarget_varsに残ると別列として重複生成されてしまう。ここで除外する
  ds_spec <- cdisc_variable_values %>% filter(prefix == "DS", cdisc_variable != "DSEPOCH")
  target_vars <- compute_target_vars(ds, ds_spec)

  ds_date_vars <- ds_spec %>% filter(field_type == "date") %>% pull(cdisc_variable) %>% unique()

  # discon/withdrawalシートのfield6(DSSTDTC)は"ref('registration',12)"(DM.RFSTDTC)以降という
  # 他ドメイン参照の下限バリデータを持つが、date_ref_boundsのref_cdisc_variable(RFSTDTC)は
  # このds自身の列には存在しないため、これを結合しておかないとpopulate_date_fields()で
  # 下限が適用されないまま(=RFSTDTCより前の日付も)生成されてしまう
  ds_date_ref_bounds <- if (!is.null(date_ref_bounds)) date_ref_bounds %>% filter(cdisc_variable %in% ds_spec[["cdisc_variable"]]) else NULL
  date_injected <- inject_cross_domain_refs(ds, NULL, NULL, built_domains, cdisc_variable_to_prefix, NULL, ds_date_ref_bounds)
  ds <- date_injected[["data"]]

  ds <- ds %>%
    populate_radio_button_fields(ds_spec, target_vars, numeric_bounds) %>%
    populate_date_fields(ds_spec, target_vars, registration_start_date, date_ref_bounds) %>%
    # DSが複数のalias(シート、例: "discon"/"withdrawal")にまたがる場合、シートの本来の並び順
    # (sheet_seq)に沿うようalias単位でまとめて日付をシフトする
    reorder_dates_by_sheet_seq(ds_date_vars, ds_spec, registration_start_date, date_ref_bounds = ds_date_ref_bounds) %>%
    populate_dummy_fields(target_vars) %>%
    select(-any_of(date_injected[["injected_cols"]]))

  # DSSEQはUSUBJID・DSSTDTC・sheet_seq(シートの本来の並び順)の昇順で振る
  ds <- ds %>% sort_ds_for_seq() %>% add_seq("DSSEQ")

  meddra_vars <- compute_meddra_vars(ds_spec, target_vars)
  if (length(meddra_vars) > 0) {
    meddra_sample <- sample_meddra_rows(meddra, nrow(ds))
    ds <- ds %>% populate_meddra_fields(ds_spec, meddra_vars, meddra, meddra_sample)
  }

  ds <- ds %>%
    apply_presence_conditions(presence_conditions) %>%
    apply_field_ref_bounds(ds_spec, field_ref_bounds)

  ds %>% reorder_domain_columns(front_cols = domain_front_cols("DS"))
}

# DSTERMの最終判定を確定する。death_date(AE由来の死亡日)と矛盾しないようDEATHを設定し、
# 死亡していない被験者は最後のレコードの約completed_rateをCOMPLETEDにする。DSTERMが無ければ何もしない。
# cdisc_variable_valuesが渡され、DSTERMの選択肢に"DEATH"を含むalias_name(例: discon)が判別できる場合は、
# そのブロックの行にDEATHを設定する(単に時系列上最後の行に設定すると、DEATHを選択肢に持たない
# 別ブロック(例: allocation/withdrawal)の行になってしまい、DEATHを参照する他ドメインの判定が
# 常に不一致になるため)。判別できない場合は、従来通り各被験者の最後の行に設定する
# 死亡確定行の候補(同一USUBJIDの複数行)から、DEATHとする行を選ぶ。最後の行(=sheet_seqが
# 最も遅いブロック、例: FOLLOW-UP)は常にDEATHとする(死亡した被験者は最終的にFollowUpでも
# DEATHとして記録されるのが正しいため)。それに加えて、ごく低い確率(early_death_prob)で、
# 最後以外の行(例: TREATMENT中のdiscon)もランダムに1つDEATHにする(TREATMENT中に死亡が
# 判明していたケースを表す)。候補が1行しかない場合は常にその行のみを返す
pick_death_rows <- function(rows, early_death_prob) {
  last_row <- rows %>% slice_tail(n = 1)
  if (nrow(rows) <= 1 || runif(1) >= early_death_prob) {
    return(last_row)
  }
  extra_row <- rows %>% slice(seq_len(n() - 1)) %>% slice_sample(n = 1)
  bind_rows(last_row, extra_row)
}

finalize_ds_disposition <- function(ds, death_date, cdisc_variable_values = NULL, completed_rate = 0.6, early_death_prob = 0.1) {
  if (!"DSTERM" %in% colnames(ds)) {
    return(ds)
  }

  died_usubjid <- death_date[["USUBJID"]]
  has_dsdtc <- "DSDTC" %in% colnames(ds)

  # 既存のDEATH表記は一旦すべて解除(重複・矛盾を避けるため)。どの行が元々DEATHだったかは、
  # 実際には死亡していない被験者がランダムでDEATHを選んでいた場合などに、後で(DEATH以外の
  # 選択肢から)埋め直すために記録しておく
  was_death <- ds[["DSTERM"]] == "DEATH"
  ds <- ds %>% mutate(DSTERM = if_else(DSTERM == "DEATH", NA_character_, DSTERM))

  ds_with_row_id <- ds %>% mutate(.row_id = row_number())

  death_alias_names <- if (!is.null(cdisc_variable_values) && "alias_name" %in% colnames(ds)) {
    cdisc_variable_values %>%
      filter(prefix == "DS", cdisc_variable == "DSTERM", code == "DEATH") %>%
      pull(alias_name) %>%
      unique()
  } else {
    character(0)
  }

  # death_dateにある被験者は、DEATHを選択肢に持つブロック(あれば)の行、無ければ最後のレコードをDEATHとして確定させる
  died_data <- ds_with_row_id %>% filter(USUBJID %in% died_usubjid)
  if (length(death_alias_names) > 0) {
    preferred <- died_data %>% filter(alias_name %in% death_alias_names)
    fallback <- died_data %>% filter(!(USUBJID %in% unique(preferred[["USUBJID"]])))
    death_row_ids <- bind_rows(preferred, fallback) %>%
      group_by(USUBJID) %>%
      group_modify(~ pick_death_rows(.x, early_death_prob)) %>%
      ungroup() %>%
      pull(.row_id)
  } else {
    death_row_ids <- died_data %>%
      group_by(USUBJID) %>%
      group_modify(~ pick_death_rows(.x, early_death_prob)) %>%
      ungroup() %>%
      pull(.row_id)
  }

  ds$DSTERM[death_row_ids] <- "DEATH"
  if (has_dsdtc) {
    dthdtc_map <- death_date$DTHDTC[match(ds$USUBJID[death_row_ids], died_usubjid)]
    ds$DSDTC[death_row_ids] <- dthdtc_map

    # DSDTC(死亡日、AE側の実際の死亡日が根拠)をここで上書きすると、date_ref_boundsが期待する
    # DSDTC>=DSSTDTC(同じ行)の関係が崩れる場合がある(DSSTDTCは死亡日を知らずに生成されているため)。
    # 死亡日は動かせない事実なので、矛盾する場合はDSSTDTC側を死亡日に合わせて引き戻す
    if ("DSSTDTC" %in% colnames(ds)) {
      violates <- !is.na(ds$DSSTDTC[death_row_ids]) & !is.na(dthdtc_map) & ds$DSSTDTC[death_row_ids] > dthdtc_map
      ds$DSSTDTC[death_row_ids][violates] <- dthdtc_map[violates]
    }
  }

  # 元々DEATHだったがdeath_row_idsに選ばれなかった行(実際には死亡していない被験者がランダムで
  # DEATHを選んでいた、または同じ被験者の別ブロックがDEATH行に選ばれた)は、解除されたまま空白になって
  # しまうため、DEATH以外の選択肢から改めて値を入れ直す
  leftover_ids <- setdiff(which(was_death), death_row_ids)
  if (length(leftover_ids) > 0 && !is.null(cdisc_variable_values) && "alias_name" %in% colnames(ds)) {
    dsterm_choices <- cdisc_variable_values %>%
      filter(prefix == "DS", cdisc_variable == "DSTERM") %>%
      mutate(code = if_else(is.na(code), default_value, code)) %>%
      filter(!is.na(code), code != "DEATH") %>%
      distinct(alias_name, code)
    for (row_id in leftover_ids) {
      choices <- dsterm_choices %>% filter(alias_name == ds$alias_name[row_id]) %>% pull(code)
      if (length(choices) > 0) {
        ds$DSTERM[row_id] <- sample(choices, 1)
      }
    }
  }

  # DSTERMの選択肢に"COMPLETED"を持たないalias_name(例: discon_ind)を判別する。
  # そのブロックはCOMPLETED状態を表現できないため、対象USUBJIDについてはCOMPLETEDを書き込まず、
  # 後段でブロックのレコード自体を除外する
  no_completed_alias_names <- character(0)
  if (!is.null(cdisc_variable_values) && "alias_name" %in% colnames(ds)) {
    no_completed_alias_names <- cdisc_variable_values %>%
      filter(prefix == "DS", cdisc_variable == "DSTERM") %>%
      mutate(code = if_else(is.na(code), default_value, code)) %>%
      group_by(alias_name) %>%
      summarise(has_completed = "COMPLETED" %in% code, .groups = "drop") %>%
      filter(!has_completed) %>%
      pull(alias_name)
  }

  # 死亡していない被験者は、最後のレコードの約completed_rateをCOMPLETEDにする
  alive_last_row_ids <- ds_with_row_id %>%
    filter(!(USUBJID %in% died_usubjid)) %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    pull(.row_id)

  completed_row_ids <- sample(alive_last_row_ids, size = round(length(alive_last_row_ids) * completed_rate))
  completed_usubjid <- ds$USUBJID[completed_row_ids]
  no_completed_mask <- if ("alias_name" %in% colnames(ds)) ds$alias_name %in% no_completed_alias_names else rep(FALSE, nrow(ds))
  # 最終的にCOMPLETEDとなった被験者は、途中経過のレコードもすべてCOMPLETEDにする
  # (選択肢に"COMPLETED"を持たないブロックは除く。そのブロックのレコードは後段で除外する)
  ds$DSTERM[ds$USUBJID %in% completed_usubjid & !no_completed_mask] <- "COMPLETED"

  # ここまででDEATH/COMPLETEDに確定した行を除いた「自由な」行(まだランダムな理由が入りうる行)について、
  # DEATH/COMPLETED以外の選択肢が一度も出現していなければ、可能な範囲でランダムな自由行に反映させる
  # (populate_radio_button_fields()のカバレッジ保証と同じ考え方を、DEATH/COMPLETED上書き後に
  # 残った行に対して適用する。DEATH/COMPLETED上書きでカバレッジが崩れることがあるため)
  if (!is.null(cdisc_variable_values) && "alias_name" %in% colnames(ds)) {
    free_ids <- setdiff(seq_len(nrow(ds)), c(death_row_ids, which(ds[["USUBJID"]] %in% completed_usubjid)))
    if (length(free_ids) > 0) {
      for (an in unique(ds[["alias_name"]][free_ids])) {
        an_free_ids <- free_ids[ds[["alias_name"]][free_ids] == an]
        choices <- cdisc_variable_values %>%
          filter(prefix == "DS", cdisc_variable == "DSTERM", alias_name == an) %>%
          mutate(code = if_else(is.na(code), default_value, code)) %>%
          filter(!is.na(code), !(code %in% c("DEATH", "COMPLETED"))) %>%
          pull(code) %>%
          unique()
        if (length(choices) == 0) next
        present <- unique(ds[["DSTERM"]][an_free_ids])
        missing <- setdiff(choices, present)
        if (length(missing) == 0) next
        # 上書きする行は、値が重複している(=他にも同じ値を持つ行がある)行を優先して選ぶ。
        # ユニークな値を持つ行を上書きすると、その値が新たに欠落してしまうため
        value_counts <- table(ds[["DSTERM"]][an_free_ids])
        is_dup_or_blank <- vapply(an_free_ids, function(i) {
          v <- ds[["DSTERM"]][i]
          is.na(v) || v == "" || value_counts[[v]] > 1
        }, logical(1))
        safe_shuffle <- function(x) if (length(x) <= 1) x else sample(x)
        ordered_ids <- c(safe_shuffle(an_free_ids[is_dup_or_blank]), safe_shuffle(an_free_ids[!is_dup_or_blank]))
        target <- ordered_ids[seq_len(min(length(missing), length(ordered_ids)))]
        ds[["DSTERM"]][target] <- missing[seq_along(target)]
      }
    }
  }

  # COMPLETEDの選択肢を持たないブロック(例: discon_ind)は、実質COMPLETEDとなった被験者について
  # 選択肢にない値を書き込むことになるため、レコード自体を出力しない
  if (length(no_completed_alias_names) > 0) {
    ds <- ds %>% filter(!(USUBJID %in% completed_usubjid & alias_name %in% no_completed_alias_names))
  }

  # DEATH確定行のDSSTDTCを死亡日に合わせてクランプした影響で、populate_ds_domain()側で
  # 既に確定していたDSSEQ(USUBJID・DSSTDTC・sheet_seq昇順)の並びが崩れることがあるため、
  # ここで振り直す
  ds <- ds %>% sort_ds_for_seq()
  if ("DSSEQ" %in% colnames(ds)) {
    ds <- ds %>% add_seq("DSSEQ")
  }

  ds
}

# 割り付け(群)があるUSUBJIDに対して、DSドメインにランダム化のマイルストーンレコード
# (DSCAT="PROTOCOL MILESTONE", DSDECOD/DSTERM="RANDOMIZED")を追加する。
# DM$ARMが全員空("")の場合(単群、割り付けなし)は何もしない。
# ランダム化は同意取得〜適格性確認の直後(初回投与前)に行われるのが一般的なため、
# DSDTCはDMにRFICDTC(同意取得日)があればその数日以内、無ければ登録開始日から数日以内とする。
# 中止日判定(build_discontinuation_date_table)に混ざらないよう、それより後に呼び出すこと
add_randomization_ds_rows <- function(ds, dm, registration_start_date) {
  randomized_usubjid <- dm %>% filter(ARM != "") %>% pull(USUBJID)
  if (length(randomized_usubjid) == 0) {
    return(ds)
  }

  randomization_rows <- dm %>%
    filter(USUBJID %in% randomized_usubjid) %>%
    select(USUBJID, STUDYID, any_of("RFICDTC")) %>%
    mutate(DOMAIN = "DS")

  if ("DSSPID" %in% colnames(ds)) randomization_rows[["DSSPID"]] <- "allocation"
  if ("DSCAT" %in% colnames(ds)) randomization_rows[["DSCAT"]] <- "PROTOCOL MILESTONE"
  if ("DSDECOD" %in% colnames(ds)) randomization_rows[["DSDECOD"]] <- "RANDOMIZED"
  if ("DSTERM" %in% colnames(ds)) randomization_rows[["DSTERM"]] <- "RANDOMIZED"
  if ("DSDTC" %in% colnames(ds)) {
    n <- nrow(randomization_rows)
    if ("RFICDTC" %in% colnames(randomization_rows)) {
      base_date <- as.Date(randomization_rows[["RFICDTC"]])
      base_date[is.na(base_date)] <- as.Date(registration_start_date)
      offset <- sample(0:3, n, replace = TRUE)
    } else {
      base_date <- as.Date(registration_start_date)
      offset <- sample(0:7, n, replace = TRUE)
    }
    # RFICDTCが今日に近い被験者だと、オフセットを足した結果が未来日になり得るため、今日でクランプする
    randomization_rows[["DSDTC"]] <- as(pmin(base_date + offset, Sys.Date()), class(ds[["DSDTC"]]))
  }
  randomization_rows <- randomization_rows %>% select(-any_of("RFICDTC"))

  combined <- bind_rows(randomization_rows, ds)
  # DSSEQはUSUBJID・DSSTDTC・sheet_seq(シートの本来の並び順)の昇順で振る。RANDOMIZED行は
  # DSSTDTC・sheet_seqのどちらも持たないため、sort_ds_for_seq()により各被験者の他の全行より
  # 前に来る(無作為化は治療開始前のイベントのため)
  combined <- combined %>% sort_ds_for_seq()
  combined %>%
    add_seq("DSSEQ") %>%
    reorder_domain_columns(front_cols = domain_front_cols("DS"))
}

# USUBJIDごとの中止日テーブルを作る(DSTERM!="COMPLETED"のレコード。DEATHも含む)。
# RANDOMIZED(add_randomization_ds_rows()が追加する無作為化マイルストーン行)も、中止理由ではなく
# 通常は治療開始前の早い日付のため除外する(呼び出し側がadd_randomization_ds_rows()より後のds
# (RANDOMIZED行を含む)を渡してしまっても、無作為化日が誤って中止日として扱われないようにするため)。
# 他ドメイン(EX/LBなど)で中止日以降のレコードが発生していないかのチェックに使う
build_discontinuation_date_table <- function(ds) {
  if (!all(c("DSTERM", "DSDTC") %in% colnames(ds))) {
    return(tibble(USUBJID = character(), DISCONDTC = as.Date(character())))
  }

  ds %>%
    filter(!(DSTERM %in% c("COMPLETED", "RANDOMIZED"))) %>%
    group_by(USUBJID) %>%
    summarise(DISCONDTC = min(DSDTC), .groups = "drop")
}

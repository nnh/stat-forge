library(tidyverse)
library(here)

source(here("generate_random_date.R"))

# specに定義されているがdataにまだ存在しないcdisc_variableを対象変数として抽出
compute_target_vars <- function(data, spec) {
  setdiff(unique(spec[["cdisc_variable"]]), colnames(data))
}

# check_box: radio_buttonと異なり複数選択が可能なため、実際の選択肢("" を除く)から1個以上を
# ランダムに選び、カンマ区切りで1つの文字列に結合する(選ぶ個数自体もランダムにすることで、
# 単一選択と複数選択が混在するようにする)。""が選択肢に含まれる場合(必須でない項目)は、
# 未選択(空欄)になることもある
sample_check_box_values <- function(choices, n) {
  real_choices <- setdiff(choices, "")
  has_blank <- "" %in% choices
  if (length(real_choices) == 0) {
    return(rep(if (has_blank) "" else NA_character_, n))
  }
  map_chr(seq_len(n), function(i) {
    if (has_blank && sample(c(TRUE, FALSE), 1)) {
      return("")
    }
    k <- sample(length(real_choices), size = sample(seq_len(length(real_choices)), 1))
    str_c(real_choices[k], collapse = ",")
  })
}

# radio_button用: choicesからn件選ぶ際、単純なランダムサンプリング(重複あり)だと選択肢数が
# 多い場合に一部の選択肢が一度も出現しないことがある。ダミーデータとして全選択肢が実際に
# 出現することが望ましいため、可能な限り全選択肢を含めるようにする。
# n<=length(choices)なら重複無しでn件選ぶ(入るだけ全種類異なる値、入りきらない分は諦める)。
# n>length(choices)なら全選択肢を最低1回ずつ含め、残りは通常通りランダム(重複あり)で埋めてから
# 順序をシャッフルする(inject_required_llt_codes()と同じ「必須値を混ぜ込む」考え方)
sample_values_with_coverage <- function(choices, n) {
  if (length(choices) == 0 || n == 0) {
    return(character(0))
  }
  if (n <= length(choices)) {
    return(sample(choices, size = n, replace = FALSE))
  }
  extra <- sample(choices, size = n - length(choices), replace = TRUE)
  sample(c(choices, extra))
}

# check_box用: sample_check_box_values()で生成した後、一度も出現しなかった選択肢があれば、
# ランダムな行に追記して全選択肢が最低1回は出現するようにする(空欄""は選択肢としてカウントしない。
# has_blankにより既に自然に出現しうるため)
sample_check_box_values_with_coverage <- function(choices, n) {
  values <- sample_check_box_values(choices, n)
  real_choices <- setdiff(choices, "")
  if (length(real_choices) == 0 || n == 0) {
    return(values)
  }
  present <- unique(unlist(str_split(values, ","))) %>% discard(~ is.na(.x) | .x == "")
  missing <- setdiff(real_choices, present)
  if (length(missing) == 0) {
    return(values)
  }
  target_rows <- sample(seq_len(n), size = length(missing), replace = length(missing) > n)
  for (i in seq_along(missing)) {
    row_i <- target_rows[i]
    existing <- values[row_i]
    parts <- if (is.na(existing) || existing == "") character(0) else str_split(existing, ",")[[1]]
    values[row_i] <- str_c(union(parts, missing[i]), collapse = ",")
  }
  values
}

# radio_button/check_box: 全codeパターン(codeが無ければdefault_value)からランダムに割り振り。
# そのalias_name/labelのインスタンスがis_required(presence型のvalidatorを持つ)でなく、かつ
# is_invisibleがFALSE(可視項目)の場合は必須ではないため、空白("")も選択肢に加える。
# is_requiredはcdisc_variable名単位ではなくalias_name/label単位の判定(build_generation_constraints.R
# のrequired_var_instances由来)のため、同じcdisc_variable名でもインスタンスによって必須/非必須が異なりうる。
# numeric_bounds(cdisc_variable, min_value, max_value)がある場合、数値として範囲外のcodeは選択肢から除く。
# dataにalias_name列がある場合(build_generic_domainなど)は、同じcdisc_variableでも
# 定義しているalias_nameが違えばcodeを混ぜず、そのalias_nameの行だけ自分のcodeから選ぶ
populate_radio_button_fields <- function(data, spec, target_vars, numeric_bounds = NULL) {
  options_spec <- spec %>% filter(field_type %in% c("radio_button", "check_box"))
  options_spec[["code"]] <- ifelse(is.na(options_spec[["code"]]), options_spec[["default_value"]], options_spec[["code"]])
  options_target_vars <- intersect(unique(options_spec[["cdisc_variable"]]), target_vars)
  has_alias_name <- "alias_name" %in% colnames(data)

  build_choices <- function(rows, var_name) {
    choices <- rows %>% pull(code) %>% unique()
    is_visible <- !any(rows[["is_invisible"]], na.rm = TRUE)
    is_required <- any(rows[["is_required"]], na.rm = TRUE)
    if (!is_required && is_visible) {
      choices <- union(choices, "")
    }
    if (!is.null(numeric_bounds)) {
      bound_row <- numeric_bounds %>% filter(cdisc_variable == var_name)
      if (nrow(bound_row) > 0) {
        min_value <- bound_row[["min_value"]][1]
        max_value <- bound_row[["max_value"]][1]
        numeric_choices <- suppressWarnings(as.numeric(choices))
        within_bounds <- is.na(numeric_choices) |
          ((is.na(min_value) | numeric_choices >= min_value) & (is.na(max_value) | numeric_choices <= max_value))
        choices <- choices[within_bounds]
      }
    }
    choices
  }

  for (var_name in options_target_vars) {
    var_rows <- options_spec %>% filter(cdisc_variable == var_name)

    if (has_alias_name) {
      data[[var_name]] <- NA_character_
      for (an in unique(var_rows[["alias_name"]])) {
        an_rows <- var_rows %>% filter(alias_name == an)
        choices <- build_choices(an_rows, var_name)
        target <- data[["alias_name"]] == an
        if (length(choices) > 0 && any(target)) {
          data[[var_name]][target] <- if (any(an_rows[["field_type"]] == "check_box")) {
            sample_check_box_values_with_coverage(choices, sum(target))
          } else {
            sample_values_with_coverage(choices, sum(target))
          }
        }
      }
    } else {
      choices <- build_choices(var_rows, var_name)
      if (length(choices) > 0) {
        data[[var_name]] <- if (any(var_rows[["field_type"]] == "check_box")) {
          sample_check_box_values_with_coverage(choices, nrow(data))
        } else {
          sample_values_with_coverage(choices, nrow(data))
        }
      }
    }
  }
  data
}

# date_ref_bounds(cdisc_variable, alias_name, label, ref_cdisc_variable, ref_alias_name, ref_label,
# bound_type)のうち、var_name/bound_typeに該当する行から、dataの各行に対応する参照先の値(Date)の
# ベクトルを返す(該当行が無ければNULL)。
# 通常のケース(参照先が別のcdisc_variable。同じ行(同じUSUBJID・同じブロック)から直接参照できる、
# 例: ECENDTC>=ECSTDTC)は、単純にdata[[ref_var]]を使う。
# 参照先がvar_name自身(同じcdisc_variable名を、別のalias_name/labelが参照している。例:
# inductionのSVSTDTCがprephaseのSVSTDTCを参照する、ref('prephase', 825)のようなケース)の場合は、
# 単純な同じ行からの参照ができない(自分自身を参照することになってしまう)ため、USUBJID単位で
# 参照先ブロック(ref_alias_name/ref_label)の値を引く。
# 同じvar_nameに対して複数の行(alias_nameごとに異なる参照先を持つ)がある場合は、それぞれ自分の
# alias_nameの行だけに適用し、bound_typeに応じて(min_dateはpmax、max_dateはpmin)組み合わせる
resolve_date_ref_bound_vals <- function(data, date_ref_bounds, var_name, bound_type_val, existing_data = NULL) {
  bound_rows <- date_ref_bounds %>%
    filter(cdisc_variable == var_name, bound_type == bound_type_val, ref_cdisc_variable %in% colnames(data))
  if (nrow(bound_rows) == 0) {
    return(NULL)
  }
  has_alias_name <- "alias_name" %in% colnames(data)
  has_label <- has_alias_name && "label" %in% colnames(data)
  has_existing <- !is.null(existing_data) && nrow(existing_data) > 0 && "alias_name" %in% colnames(existing_data)
  existing_has_label <- has_existing && "label" %in% colnames(existing_data)
  combine <- if (bound_type_val == "min_date") pmax else pmin

  result <- rep(as.Date(NA), nrow(data))
  for (i in seq_len(nrow(bound_rows))) {
    # unname(): 一部の行で"names"属性付きの文字列が紛れ込むことがあり(joinの経路によって発生)、
    # identical()がnames属性の違いだけでFALSEになってしまう(値としては同じでも別物と判定される)
    # のを防ぐため、比較・キーとして使う前に必ず名前を落とす
    ref_var <- unname(bound_rows[["ref_cdisc_variable"]][i])
    # ref('sheet_alias', N)+150.days / -28.daysのような符号付き日数オフセット
    # (extract_date_cross_ref_offset_days()由来)。無指定(NA)の場合は0として扱う
    offset_days_i <- if ("offset_days" %in% colnames(bound_rows)) unname(bound_rows[["offset_days"]][i]) else NA_real_
    if (is.na(offset_days_i)) offset_days_i <- 0
    own_alias <- if ("alias_name" %in% colnames(bound_rows)) unname(bound_rows[["alias_name"]][i]) else NA_character_
    own_label <- if ("label" %in% colnames(bound_rows)) unname(bound_rows[["label"]][i]) else NA_character_
    ref_alias <- if ("ref_alias_name" %in% colnames(bound_rows)) unname(bound_rows[["ref_alias_name"]][i]) else NA_character_
    ref_label_i <- if ("ref_label" %in% colnames(bound_rows)) unname(bound_rows[["ref_label"]][i]) else NA_character_
    # 同じcdisc_variable名を参照する自己参照(ref_var==var_name)には、alias_nameが異なる場合
    # (例: inductionのSVSTDTCがprephaseのSVSTDTCを参照)だけでなく、同一alias内でlabelだけが
    # 異なる場合(例: BLASTLE(005)がWBC(006)のLBDTCを参照)も含める。後者を素通りさせて下のelse節
    # (values = data[[ref_var]]、つまり自分自身の現在値)に落ちると、常に「自分自身と等しい」という
    # 無意味な比較になり、参照先(WBC)が後続のclampで動いても追従できなくなる(実際に発生したバグ:
    # WBC/BLASTLEの等号制約が崩れた)
    is_self_ref_across_alias_or_label <- identical(ref_var, var_name) && has_alias_name &&
      !is.na(own_alias) && !is.na(ref_alias) &&
      (!identical(own_alias, ref_alias) || (has_label && !is.na(own_label) && !is.na(ref_label_i) && !identical(own_label, ref_label_i)))

    target_rows <- if (has_alias_name && !is.na(own_alias)) data[["alias_name"]] == own_alias else rep(TRUE, nrow(data))
    # labelがある場合、この制約はown_label(bound_rows$label)の行にだけ適用すべき。フィルタしないと、
    # 同じaliasの他label(例: WBC自身の行)にまで「BLASTLE用の下限」が誤って適用されてしまう
    if (has_label && !is.na(own_label)) {
      target_rows <- target_rows & (data[["label"]] == own_label)
    }
    if (!any(target_rows)) next

    values <- if (is_self_ref_across_alias_or_label) {
      ref_target_rows <- if (has_label && !is.na(ref_label_i)) {
        data[["alias_name"]] == ref_alias & data[["label"]] == ref_label_i
      } else {
        data[["alias_name"]] == ref_alias
      }
      # このdata(1つのbuild呼び出し=1 wave分)に参照先aliasの行が無い場合、既にドメイン単位/シート単位の
      # 依存順序解決(wave分割)で先に確定済みのexisting_data(前waveの結果)から探す。これにより、
      # 「ドメイン(prefix)単位では循環に見えるが、実際にはシート単位では循環でない」参照チェーン
      # (例: SV(hr3fisrt)→RS→LB→FA→SV(prephase))を正しく解決できる
      if (!any(ref_target_rows) && has_existing && ref_var %in% colnames(existing_data)) {
        ext_target_rows <- if (existing_has_label && !is.na(ref_label_i)) {
          existing_data[["alias_name"]] == ref_alias & existing_data[["label"]] == ref_label_i
        } else {
          existing_data[["alias_name"]] == ref_alias
        }
        if (any(ext_target_rows)) {
          ref_map <- set_names(as.Date(as.character(existing_data[[ref_var]][ext_target_rows])), existing_data[["USUBJID"]][ext_target_rows])
          unname(ref_map[data[["USUBJID"]]])
        } else {
          rep(as.Date(NA), nrow(data))
        }
      } else {
        ref_map <- set_names(as.Date(as.character(data[[ref_var]][ref_target_rows])), data[["USUBJID"]][ref_target_rows])
        unname(ref_map[data[["USUBJID"]]])
      }
    } else {
      as.Date(as.character(data[[ref_var]]))
    }
    if (offset_days_i != 0) values <- values + offset_days_i
    result[target_rows] <- combine(result[target_rows], values[target_rows], na.rm = TRUE)
  }
  if (all(is.na(result))) NULL else result
}

# date: registration_start_date〜今日の間でランダムな日付を生成。
# dataにalias_name列がある場合(build_generic_domainなど)は、同じcdisc_variableでも
# 定義しているalias_nameが違えば日付を入れず、そのcdisc_variableを実際に定義しているalias_nameの
# 行だけに絞って生成する(例: CMドメインで"concomitant_drug"にしか無いCMSTDTCが、
# それを定義していない"baseline1"の行にまで入ってしまうのを防ぐ)
populate_date_fields <- function(data, spec, target_vars, registration_start_date, date_ref_bounds = NULL, existing_data = NULL) {
  date_vars <- spec %>%
    filter(field_type == "date") %>%
    pull(cdisc_variable) %>%
    unique() %>%
    intersect(target_vars)
  has_alias_name <- "alias_name" %in% colnames(data)

  # date_vars同士がvalidate_date_after_or_equal_to/validate_date_before_or_equal_to(他フィールド参照)で
  # 数珠つなぎに依存し合う場合、参照先が先に生成されていないと値を引けない。date_ref_bounds
  # (このdate_vars同士の依存だけ)を使って依存が無いものから順に並べ替える
  # (トポロジカルソート。循環参照があれば残りは元の順のまま追加する)
  if (!is.null(date_ref_bounds) && length(date_vars) > 1) {
    date_deps <- date_ref_bounds %>% filter(cdisc_variable %in% date_vars, ref_cdisc_variable %in% date_vars)
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
    date_vars <- sorted_date_vars
  }

  # BRTHDTC(生年月日)列がある場合(DM等)、生成する日付がBRTHDTCより前にならないよう、
  # 下限を「登録開始日とBRTHDTCの遅い方」にする(小児等でBRTHDTCが登録開始日より後になる場合、
  # 「生まれる前に同意している」といった矛盾が生じるのを防ぐ)。generate_random_date()の
  # start_date引数は列名の文字列も受け付けるため、計算結果を一時列として持たせて渡す
  has_brthdtc <- "BRTHDTC" %in% colnames(data)
  if (has_brthdtc) {
    data[["__date_lower_bound"]] <- as.character(pmax(as.Date(registration_start_date), as.Date(data[["BRTHDTC"]]), na.rm = TRUE))
  }
  start_bound <- if (has_brthdtc) "__date_lower_bound" else registration_start_date
  # RFSTDTC(症例登録日、他ドメインではinject_cross_domain_refs等で結合されている場合がある)は、
  # 明示的なref()参照(min_ref_vals)を持たない変数だけのデフォルト下限として使う(下のループ内)。
  # min_ref_vals(例: MHSTDTCのBRTHDTC基準)がある変数にまで一律にRFSTDTCを加えると、
  # その変数本来の(RFSTDTCより前を許容する)意味を壊してしまうため
  has_rfstdtc <- "RFSTDTC" %in% colnames(data)

  for (var_name in date_vars) {
    # var_nameにvalidate_date_after_or_equal_to/validate_date_before_or_equal_to(他フィールド参照。
    # 例: "field5")があれば、一律のstart_bound/今日ではなく、その参照先フィールドの値(同じ行)と
    # 一律の下限/上限の厳しい方を使うための一時列を作る(参照先列がまだ無い場合は一律のままにする)
    var_start_bound <- start_bound
    var_end_bound <- Sys.Date()
    if (!is.null(date_ref_bounds)) {
      min_ref_vals <- resolve_date_ref_bound_vals(data, date_ref_bounds, var_name, "min_date", existing_data)
      default_lower <- if (is.character(start_bound) && length(start_bound) == 1 && start_bound %in% colnames(data)) {
        as.Date(data[[start_bound]])
      } else {
        as.Date(start_bound)
      }
      if (!is.null(min_ref_vals) || has_rfstdtc) {
        lower_val <- pmax(default_lower, min_ref_vals, na.rm = TRUE)
        # RFSTDTCは、行ごとに明示的な参照値(min_ref_vals)が無い(NA)場合だけ補う
        # (他の行でmin_ref_valsが解決できていれば、その変数本来の意味を尊重してRFSTDTCは加えない)
        if (has_rfstdtc) {
          no_explicit_ref <- if (is.null(min_ref_vals)) rep(TRUE, nrow(data)) else is.na(min_ref_vals)
          rfstdtc_vals <- as.Date(data[["RFSTDTC"]])
          lower_val[no_explicit_ref] <- pmax(lower_val[no_explicit_ref], rfstdtc_vals[no_explicit_ref], na.rm = TRUE)
        }
        lower_col <- str_c("__date_lower_bound__", var_name)
        data[[lower_col]] <- as.character(lower_val)
        var_start_bound <- lower_col
      }
      max_ref_vals <- resolve_date_ref_bound_vals(data, date_ref_bounds, var_name, "max_date", existing_data)
      if (!is.null(max_ref_vals)) {
        upper_col <- str_c("__date_upper_bound__", var_name)
        data[[upper_col]] <- as.character(pmin(Sys.Date(), max_ref_vals, na.rm = TRUE))
        var_end_bound <- upper_col
      }
    }

    if (has_alias_name) {
      date_alias_names <- spec %>%
        filter(field_type == "date", cdisc_variable == var_name) %>%
        pull(alias_name) %>%
        unique()
      target_rows <- data[["alias_name"]] %in% date_alias_names
      data[[var_name]] <- as.Date(NA)
      if (any(target_rows)) {
        generated <- generate_random_date(data[target_rows, , drop = FALSE], var_start_bound, var_end_bound, var_name)
        data[[var_name]][target_rows] <- generated[[var_name]]
      }
    } else {
      data <- generate_random_date(data, var_start_bound, var_end_bound, var_name)
    }
  }

  if (has_brthdtc) {
    data[["__date_lower_bound"]] <- NULL
  }
  data <- data %>% select(-starts_with("__date_lower_bound__"), -starts_with("__date_upper_bound__"))
  data
}

# 変数名が"DOSE"で終わる場合(例: CMDOSE, ECDOSE)、それらしい用量の数値を入れる
dose_value_choices <- c("50", "100", "150", "200", "250", "300", "400", "500")

populate_dose_fields <- function(data, target_vars) {
  dose_vars <- target_vars[str_detect(target_vars, "DOSE$")] %>% setdiff(colnames(data))
  for (var_name in dose_vars) {
    data[[var_name]] <- sample(dose_value_choices, size = nrow(data), replace = TRUE)
  }
  data
}

# 上記のいずれでも埋まらなかった対象変数はとりあえずダミー値を格納
populate_dummy_fields <- function(data, target_vars) {
  remaining_vars <- setdiff(target_vars, colnames(data))
  for (var_name in remaining_vars) {
    data[[var_name]] <- "DUMMY"
  }
  data
}

# presence_conditions(cdisc_variable, label, ref_cdisc_variable, ref_alias_name, ref_label, expected_value,
# condition_type)に基づき、条件を満たさないレコードのcdisc_variableをNAにする。インデックス代入を使うことで、
# 日付型など列の型を問わず安全に適用できる。
# condition_type=="equals": ref_cdisc_variableの値がexpected_value(複数行ならOR)と一致しない場合NAにする
# condition_type=="not_blank": ref_cdisc_variableが空白/NAの場合NAにする(expected_valueは使わない)
# condition_type=="copy": cdisc_variableの値をref_cdisc_variableの値でそのまま上書きする
# (FieldItem::Referenceのような「他フィールドの値をそのまま使う」項目向け)
#
# 同じcdisc_variableでも、ref_alias_nameが異なる複数の条件行がある場合(例: MHTERMのうち
# thrombophiliaブロックだけがMHOCCUR=='Y'でゲーティングされ、registrationブロックには無関係)、
# dataがalias_name列を持ち、そのref_alias_nameがdata自身のalias_nameのいずれかと一致するなら、
# その行(そのブロック)だけにゲーティングを適用する。一致しない(または不明)場合は全行に適用する
# (真に外部の固定参照とみなす。inject_cross_domain_refs()のスコープ判定と対になる)。
# さらに、labelはこの条件が対象とするcdisc_variable自身のインスタンス(label)を表す。
# 同じcdisc_variable名が複数labelに繰り返し定義され、それぞれ別々の条件を持つ場合(例: CM/baselineの
# 5つのCMTRTが、各々異なる閾値でゲーティングされているケース)、labelでも絞り込むことで
# 他インスタンスの条件を巻き込まないようにする
apply_presence_conditions <- function(data, presence_conditions, cdisc_variable_to_prefix = NULL) {
  applicable <- presence_conditions %>%
    filter(cdisc_variable %in% colnames(data), ref_cdisc_variable %in% colnames(data))
  if (!("ref_alias_name" %in% names(applicable))) {
    applicable[["ref_alias_name"]] <- NA_character_
  }
  if (!("ref_label" %in% names(applicable))) {
    applicable[["ref_label"]] <- NA_character_
  }
  if (!("label" %in% names(applicable))) {
    applicable[["label"]] <- NA_character_
  }
  if (!("alias_name" %in% names(applicable))) {
    applicable[["alias_name"]] <- NA_character_
  }

  has_data_alias_name <- "alias_name" %in% colnames(data)
  has_data_alias <- all(c("alias_name", "label") %in% colnames(data))
  data_alias_names <- if (has_data_alias_name) unique(data[["alias_name"]]) else character(0)

  # ref_alias_nameがdata自身のalias_nameのいずれかと一致する行だけに絞る(一致しなければ真に外部の
  # 固定参照とみなして全行を対象にする)。さらに、dataがlabelも持っており、かつref_labelが
  # そのalias_name内でdata自身が実際に持っているlabelの1つでもある場合は、同じalias_name内の他labelを
  # 巻き込まないようlabelでも絞り込む(例: thrombophilia内のlabel="006"のゲーティング条件を、
  # 同じalias_nameの他label(000〜005)に誤って適用しないため)。
  # 一方、ref_labelがdata自身のlabel群に存在しない場合(例: PC(label=111〜114)がEC側のlabel="054"を
  # 参照するような、別prefixの別の繰り返し軸を参照するケース)は、label不一致で全行が対象外になってしまうのを
  # 避けるため、alias_nameのみで絞り込む(=そのalias_name内の全labelに同じ参照値を適用する)。
  # own_label/own_alias_nameが指定されている場合は、それとは独立に、cdisc_variable自身のインスタンス
  # (data自身のalias_name・label)でも絞り込む。labelはシートをまたいで重複しうる(例: 別々のシートが
  # どちらも"006"というlabel番号を使う)ため、own_alias_nameも必ず合わせて絞り込むことで、
  # 同じlabel番号を使う無関係な別シートの行を誤って巻き込まないようにする
  # own_target_rows: ゲーティング対象(値をNA化する側)の行を、自分自身のalias_name/labelだけで絞り込む。
  # resolve_ref_vals: 参照先(ref)の値を、ref_alias_name/ref_labelが自分自身と異なる場合は
  # USUBJID単位でref側の行から引き直す(同一alias内の別labelを参照するequals/not_blank条件で、
  # 「対象行かつ参照元行」を同時に満たす行を求めようとすると該当行が存在せず常に空集合になってしまう
  # 旧target_rows_forの不具合を避けるため、対象行の判定と参照値の解決を分離する)
  own_target_rows <- function(own_alias_name = NA_character_, own_label = NA_character_) {
    rows <- rep(TRUE, nrow(data))
    if (has_data_alias_name && !is.na(own_alias_name)) {
      rows <- rows & data[["alias_name"]] == own_alias_name
    }
    if (has_data_alias && !is.na(own_label)) {
      rows <- rows & data[["label"]] == own_label
    }
    rows
  }

  # ref_alias_name/ref_labelがdata自身の実際の行の組み合わせ(ref_alias_name内に実在するlabel)と
  # 一致しない場合、真に外部(別prefix)の固定参照であり、inject_cross_domain_refs()が既にUSUBJID
  # 単位で正しい値をref_var列としてその行(own_alias/own_labelの行)に結合済みなので、そのまま
  # data[[ref_var]]を読めばよい。ここでさらに同じdata内をUSUBJIDベースで検索し直すと、同じUSUBJIDの
  # 他の行(inject時のpin対象外だった行。例: ゲーティング条件を持たない別labelの行はref_varが
  # NAのまま)を誤って拾ってしまう(実際に発生したバグ: CM(baseline)のCMTRT(SPDEVID2〜5)が
  # SC(baseline)のSCORRES(label=003)を参照する際、CM自身にはlabel="003"の行が存在しないため、
  # alias_name一致の全行(SPDEVID=1の行も含む)から検索してしまい、SCORRES未注入(NA)のlabel="010"行が
  # 名前重複により先にマッチしてCMTRTが常にNA化されていた)
  ref_row_exists <- function(ref_alias_name, ref_label) {
    has_data_alias_name && !is.na(ref_alias_name) && ref_alias_name %in% data_alias_names &&
      (is.na(ref_label) || (has_data_alias && ref_label %in% data[["label"]][data[["alias_name"]] == ref_alias_name]))
  }

  resolve_ref_vals <- function(ref_var, ref_alias_name = NA_character_, ref_label = NA_character_, own_alias_name = NA_character_, own_label = NA_character_) {
    ref_is_own_row <- (is.na(ref_alias_name) || identical(ref_alias_name, own_alias_name)) &&
      (is.na(ref_label) || identical(ref_label, own_label))
    if (ref_is_own_row || !("USUBJID" %in% colnames(data)) || !ref_row_exists(ref_alias_name, ref_label)) {
      return(data[[ref_var]])
    }
    ref_rows <- data[["alias_name"]] == ref_alias_name
    if (has_data_alias && !is.na(ref_label)) {
      ref_rows <- ref_rows & data[["label"]] == ref_label
    }
    ref_lookup <- set_names(data[[ref_var]][ref_rows], data[["USUBJID"]][ref_rows])
    unname(ref_lookup[data[["USUBJID"]]])
  }

  # copyは先に適用する。同じcdisc_variableにequals/not_blankのゲーティング条件も併せて
  # 存在する場合(例: FAOBJがAETERMをコピーしつつ、AELLTCDが特定コードのときだけ値を持つ)、
  # 先にコピーしてから後段のゲーティングでNA化できるようにするため
  copy_conditions <- applicable %>%
    filter(condition_type == "copy") %>%
    distinct(cdisc_variable, ref_cdisc_variable, alias_name, label, ref_alias_name, ref_label)
  for (i in seq_len(nrow(copy_conditions))) {
    var_name <- copy_conditions[["cdisc_variable"]][i]
    ref_var <- copy_conditions[["ref_cdisc_variable"]][i]
    own_label <- copy_conditions[["label"]][i]
    own_alias_name <- copy_conditions[["alias_name"]][i]
    ref_label_i <- copy_conditions[["ref_label"]][i]
    ref_alias_name_i <- copy_conditions[["ref_alias_name"]][i]

    target_rows <- rep(TRUE, nrow(data))
    if (has_data_alias_name && !is.na(own_alias_name)) {
      target_rows <- target_rows & data[["alias_name"]] == own_alias_name
    }
    if (has_data_alias && !is.na(own_label)) {
      target_rows <- target_rows & data[["label"]] == own_label
    }

    # 参照元がref_var(別prefixの変数、例: TU側のTUDTC)である場合、この関数が呼ばれる前の
    # inject_cross_domain_refs()が既にUSUBJID単位で正しい値をref_var列としてdataに結合済みのため、
    # target_rowsの位置でそのまま読めばよい(ここでさらにalias_name/labelで突き合わせようとすると、
    # ref_label/ref_alias_nameは参照先(別prefix)自身のラベル空間の値であり、data(このprefix自身の
    # 行)のalias_name/labelとは無関係な値のため、誤って一致してしまう/一致せず空になるおそれがある)。
    # 一方、参照元が自分自身と同じprefixの場合、同じcdisc_variable列を複数labelブロックが共有しているため、
    # (alias_name, label)が自分自身と一致する場合(例: FAOBJがAETERMをコピーする、同じ行の別フィールドを
    # 参照する)はtarget_rowsの値をそのまま読めばよいが、別の(alias_name, label)ブロックを参照する場合
    # (例: SAXISのTRDTCがLDIAMのTRDTCをコピーする)は、コピー元・コピー先が別々の行になるため、
    # 同じ行のインデックスをそのまま使うと自分自身(まだ値が入っていない)を読んでしまう。USUBJIDで
    # 対応付けてから値を引く
    own_prefix <- if (!is.null(cdisc_variable_to_prefix)) {
      cdisc_variable_to_prefix %>% filter(cdisc_variable == var_name) %>% pull(prefix) %>% first()
    } else {
      NA_character_
    }
    ref_prefix <- if (!is.null(cdisc_variable_to_prefix)) {
      cdisc_variable_to_prefix %>% filter(cdisc_variable == ref_var) %>% pull(prefix) %>% first()
    } else {
      NA_character_
    }
    is_cross_prefix <- !is.na(own_prefix) && !is.na(ref_prefix) && !identical(own_prefix, ref_prefix)

    same_alias <- is.na(ref_alias_name_i) || (!is.na(own_alias_name) && identical(ref_alias_name_i, own_alias_name))
    same_label <- is.na(ref_label_i) || (!is.na(own_label) && identical(ref_label_i, own_label))
    is_same_block <- is_cross_prefix || (same_alias && same_label)

    # コピー元(ref_var)がDate型の場合、文字列型のvar_nameへインデックス代入すると内部の数値表現が
    # そのまま文字列化されてしまうため、as.character()で明示的に変換してから代入する
    if (is_same_block || !("USUBJID" %in% colnames(data))) {
      data[[var_name]][target_rows] <- as.character(data[[ref_var]][target_rows])
    } else {
      source_rows <- rep(TRUE, nrow(data))
      if (has_data_alias_name && !is.na(ref_alias_name_i)) {
        source_rows <- source_rows & data[["alias_name"]] == ref_alias_name_i
      }
      if (has_data_alias && !is.na(ref_label_i)) {
        source_rows <- source_rows & data[["label"]] == ref_label_i
      }
      ref_lookup <- set_names(as.character(data[[ref_var]][source_rows]), data[["USUBJID"]][source_rows])
      data[[var_name]][target_rows] <- unname(ref_lookup[data[["USUBJID"]][target_rows]])
    }
  }

  # age_gt/age_ge/age_lt/age_le: age(ref1, ref2)(2つの日付の経過年数)がoperator/expected_value(閾値)を
  # 満たさない行をNA化する(validate_presence_ifのage(ref('sheet',N), ref('sheet',M)) OP 閾値に対応)。
  # copyと同様、他のequals/not_blank条件がこの変数自身を参照している場合がある(例: FASTATのage_gt条件で
  # NA化された後の値をFAORRESのequals条件("FASTATが空欄のときだけ値を持つ")が読む)ため、
  # equals/not_blankより先に適用する。ref_cdisc_variable/ref2_cdisc_variableという2つの参照先を持つ点が
  # 通常のequals/not_blankと異なるため、専用の処理にする
  if (!("ref2_cdisc_variable" %in% names(applicable))) {
    applicable[["ref2_cdisc_variable"]] <- NA_character_
  }
  if (!("ref2_alias_name" %in% names(applicable))) {
    applicable[["ref2_alias_name"]] <- NA_character_
  }
  if (!("ref2_label" %in% names(applicable))) {
    applicable[["ref2_label"]] <- NA_character_
  }
  age_conditions <- applicable %>%
    filter(condition_type %in% c("age_gt", "age_ge", "age_lt", "age_le"), ref2_cdisc_variable %in% colnames(data)) %>%
    distinct(cdisc_variable, ref_cdisc_variable, ref_alias_name, ref_label, ref2_cdisc_variable, ref2_alias_name, ref2_label, alias_name, label, condition_type, expected_value)
  for (i in seq_len(nrow(age_conditions))) {
    var_name <- age_conditions[["cdisc_variable"]][i]
    own_label <- age_conditions[["label"]][i]
    own_alias_name <- age_conditions[["alias_name"]][i]
    ref_var <- age_conditions[["ref_cdisc_variable"]][i]
    ref_label_i <- age_conditions[["ref_label"]][i]
    ref_alias_name_i <- age_conditions[["ref_alias_name"]][i]
    ref2_var <- age_conditions[["ref2_cdisc_variable"]][i]
    ref2_label_i <- age_conditions[["ref2_label"]][i]
    ref2_alias_name_i <- age_conditions[["ref2_alias_name"]][i]
    op <- age_conditions[["condition_type"]][i]
    threshold <- suppressWarnings(as.numeric(age_conditions[["expected_value"]][i]))

    target_rows <- own_target_rows(own_alias_name, own_label)
    date1 <- as.Date(resolve_ref_vals(ref_var, ref_alias_name_i, ref_label_i, own_alias_name, own_label))
    date2 <- as.Date(resolve_ref_vals(ref2_var, ref2_alias_name_i, ref2_label_i, own_alias_name, own_label))
    age_years <- as.numeric(date1 - date2) / 365.25
    satisfied <- switch(op,
      age_gt = age_years > threshold,
      age_ge = age_years >= threshold,
      age_lt = age_years < threshold,
      age_le = age_years <= threshold,
      rep(NA, length(age_years))
    )
    satisfied[is.na(satisfied)] <- FALSE
    mismatch <- target_rows & !satisfied
    data[[var_name]][mismatch] <- NA
  }

  # numeric_ge/numeric_le/numeric_gt/numeric_lt: ref_cdisc_variableの値を数値としてexpected_value(閾値)と
  # 比較し、満たさない行をNA化する(validate_presence_ifの"fieldN>=数値"のような同一シート内の別
  # フィールドとの数値不等号比較に対応。例: QSORRESの"f16>=2&&STAT.blank?"のうちf16>=2の部分)。
  # age_gt等と同様、他のequals/not_blank条件がこの変数自身を参照している場合があるため先に適用する
  numeric_cmp_conditions <- applicable %>%
    filter(condition_type %in% c("numeric_ge", "numeric_le", "numeric_gt", "numeric_lt")) %>%
    distinct(cdisc_variable, ref_cdisc_variable, ref_alias_name, ref_label, alias_name, label, condition_type, expected_value)
  for (i in seq_len(nrow(numeric_cmp_conditions))) {
    var_name <- numeric_cmp_conditions[["cdisc_variable"]][i]
    ref_var <- numeric_cmp_conditions[["ref_cdisc_variable"]][i]
    own_label <- numeric_cmp_conditions[["label"]][i]
    own_alias_name <- numeric_cmp_conditions[["alias_name"]][i]
    ref_label_i <- numeric_cmp_conditions[["ref_label"]][i]
    ref_alias_name_i <- numeric_cmp_conditions[["ref_alias_name"]][i]
    op <- numeric_cmp_conditions[["condition_type"]][i]
    threshold <- suppressWarnings(as.numeric(numeric_cmp_conditions[["expected_value"]][i]))

    target_rows <- own_target_rows(own_alias_name, own_label)
    ref_vals <- suppressWarnings(as.numeric(resolve_ref_vals(ref_var, ref_alias_name_i, ref_label_i, own_alias_name, own_label)))
    satisfied <- switch(op,
      numeric_ge = ref_vals >= threshold,
      numeric_le = ref_vals <= threshold,
      numeric_gt = ref_vals > threshold,
      numeric_lt = ref_vals < threshold,
      rep(NA, length(ref_vals))
    )
    satisfied[is.na(satisfied)] <- FALSE
    mismatch <- target_rows & !satisfied
    data[[var_name]][mismatch] <- NA
  }

  equals_conditions <- applicable %>%
    filter(condition_type == "equals") %>%
    group_by(cdisc_variable, ref_cdisc_variable, ref_alias_name, ref_label, alias_name, label) %>%
    summarise(expected_values = list(unique(expected_value)), .groups = "drop")

  # equals条件同士に依存関係がある場合(あるcdisc_variable/alias_name/labelの組がグループAの対象で
  # あると同時にグループBの参照先でもある場合)、Aを先に処理してから値を確定させないと、
  # Bがまだゲーティング前のAの初期乱数値を参照してしまう(挿入順=source配列の並び順のままでは
  # 依存順序が保証されない)。そのため、対象(target)と参照(ref)のキーが一致するグループ間に
  # 依存エッジを張り、DFSでトポロジカル順に並べ替えてから適用する
  group_node_key <- function(alias, label, v) {
    str_c(if (is.na(alias)) "" else alias, "::", if (is.na(label)) "" else label, "::", if (is.na(v)) "" else v)
  }
  n_groups <- nrow(equals_conditions)
  target_keys <- map_chr(seq_len(n_groups), function(i) {
    group_node_key(equals_conditions[["alias_name"]][i], equals_conditions[["label"]][i], equals_conditions[["cdisc_variable"]][i])
  })
  ref_keys <- map_chr(seq_len(n_groups), function(i) {
    group_node_key(equals_conditions[["ref_alias_name"]][i], equals_conditions[["ref_label"]][i], equals_conditions[["ref_cdisc_variable"]][i])
  })
  target_key_to_index <- set_names(seq_len(n_groups), target_keys)
  depends_on <- map_int(seq_len(n_groups), function(i) {
    idx <- unname(target_key_to_index[ref_keys[i]])
    if (!is.na(idx) && idx != i) idx else NA_integer_
  })

  visited <- rep(FALSE, n_groups)
  visiting <- rep(FALSE, n_groups)
  ordered_idx <- integer(0)
  visit <- function(i) {
    if (visited[i] || visiting[i]) {
      return(invisible(NULL))
    }
    visiting[i] <<- TRUE
    if (!is.na(depends_on[i])) visit(depends_on[i])
    visiting[i] <<- FALSE
    visited[i] <<- TRUE
    ordered_idx <<- c(ordered_idx, i)
  }
  for (i in seq_len(n_groups)) visit(i)

  for (i in ordered_idx) {
    var_name <- equals_conditions[["cdisc_variable"]][i]
    ref_var <- equals_conditions[["ref_cdisc_variable"]][i]
    expected_values <- equals_conditions[["expected_values"]][[i]]
    own_label <- equals_conditions[["label"]][i]
    own_alias_name <- equals_conditions[["alias_name"]][i]
    ref_label_i <- equals_conditions[["ref_label"]][i]
    ref_alias_name_i <- equals_conditions[["ref_alias_name"]][i]
    target_rows <- own_target_rows(own_alias_name, own_label)
    ref_vals <- resolve_ref_vals(ref_var, ref_alias_name_i, ref_label_i, own_alias_name, own_label)
    # expected_valuesが""(空欄)を含む場合(例: "field.blank?"由来の条件)、参照先列は他の
    # presence_conditionsで既にNA化されていることがあり、その場合ref_varの値は""ではなくNAになっている。
    # %in%だけで判定すると(NAは""と一致しないため)「空欄のはずが空欄と認識されない」まま誤って
    # NG扱いになってしまうため、""が期待値に含まれる場合はNAも空欄として一致させる
    blank_ok <- "" %in% expected_values
    is_match <- (ref_vals %in% expected_values) | (blank_ok & (is.na(ref_vals) | ref_vals == ""))
    mismatch <- target_rows & !is_match
    data[[var_name]][mismatch] <- NA
  }

  not_blank_conditions <- applicable %>%
    filter(condition_type == "not_blank") %>%
    distinct(cdisc_variable, ref_cdisc_variable, ref_alias_name, ref_label, alias_name, label)
  for (i in seq_len(nrow(not_blank_conditions))) {
    var_name <- not_blank_conditions[["cdisc_variable"]][i]
    ref_var <- not_blank_conditions[["ref_cdisc_variable"]][i]
    own_label <- not_blank_conditions[["label"]][i]
    own_alias_name <- not_blank_conditions[["alias_name"]][i]
    ref_label_i <- not_blank_conditions[["ref_label"]][i]
    ref_alias_name_i <- not_blank_conditions[["ref_alias_name"]][i]
    target_rows <- own_target_rows(own_alias_name, own_label)
    ref_vals <- resolve_ref_vals(ref_var, ref_alias_name_i, ref_label_i, own_alias_name, own_label)
    mismatch <- target_rows & (is.na(ref_vals) | ref_vals == "")
    data[[var_name]][mismatch] <- NA
  }

  data
}

# target_vars(このドメイン自身の対象列)のうちrequired_var_instances(alias_name/label単位の必須
# 判定テーブル。build_generation_constraints.Rのrequired_var_instancesに対応)に含まれる列を、
# 各行が属するalias_name(・label)ごとに求め、その行にとって必須な列が1つ以上あり、かつそれらが
# 全て空(NAまたは空文字列"")であるレコードを削除する。同じcdisc_variable名でもalias_name/labelの
# インスタンスによって必須/非必須が異なりうるため(例: MHTERMは主診断labelでは必須だが、
# 再発診断labelでは必須でない)、cdisc_variable名だけで一律に判定しない。
# presence_conditions等のゲーティングにより、そのインスタンス(行)が実質「存在しない」もの
# (必須項目も含め何も入力されていない)になった場合、ダミーデータとしてもプレースホルダー行を
# 残さず削除するために使う。alias_name列が無い、またはこのドメインに必須列が1つも無い場合は何もしない。
# ただしxxSTAT(xxはprefix。例: LBSTAT)が"NOT DONE"の行は、必須列が全て空でも削除しない
# (未実施を示す正当な状態のため)
drop_all_blank_required_records <- function(data, target_vars, required_var_instances, prefix) {
  if (is.null(required_var_instances) || nrow(required_var_instances) == 0) {
    return(data)
  }
  candidate_vars <- intersect(unique(required_var_instances[["cdisc_variable"]]), target_vars) %>% intersect(colnames(data))
  if (length(candidate_vars) == 0 || !("alias_name" %in% colnames(data))) {
    return(data)
  }
  has_label <- "label" %in% colnames(data)

  is_required_mat <- matrix(FALSE, nrow = nrow(data), ncol = length(candidate_vars), dimnames = list(NULL, candidate_vars))
  is_blank_mat <- matrix(FALSE, nrow = nrow(data), ncol = length(candidate_vars), dimnames = list(NULL, candidate_vars))
  for (v in candidate_vars) {
    req_rows <- required_var_instances %>% filter(cdisc_variable == v)
    if (!has_label) {
      is_required_mat[, v] <- data[["alias_name"]] %in% unique(req_rows[["alias_name"]])
    } else {
      # labelがNA(そのfieldにlabelが無い)場合は、alias_name全体を必須とみなす(緩い一致)。
      # labelがある場合は(alias_name, label)の完全一致のみ必須とみなす
      alias_only <- req_rows %>% filter(is.na(label)) %>% pull(alias_name) %>% unique()
      alias_label <- req_rows %>% filter(!is.na(label)) %>% distinct(alias_name, label)
      matched <- data[["alias_name"]] %in% alias_only
      if (nrow(alias_label) > 0) {
        matched <- matched | (str_c(data[["alias_name"]], "", data[["label"]]) %in% str_c(alias_label[["alias_name"]], "", alias_label[["label"]]))
      }
      is_required_mat[, v] <- matched
    }
    col <- data[[v]]
    is_blank_mat[, v] <- is.na(col) | (is.character(col) & col == "")
  }

  stat_var <- str_c(prefix, "STAT")
  not_done <- if (stat_var %in% colnames(data)) data[[stat_var]] == "NOT DONE" & !is.na(data[[stat_var]]) else FALSE

  has_any_required <- apply(is_required_mat, 1, any)
  all_required_blank <- apply(!is_required_mat | is_blank_mat, 1, all)
  should_delete <- has_any_required & all_required_blank & !not_done

  data %>% filter(!should_delete)
}

# field_ref_bounds(cdisc_variable, ref_cdisc_variable, bound_type)に基づき、
# cdisc_variableの値がref_cdisc_variableの値との大小関係(max_value/min_value/exact_value)を
# 満たさない場合、条件を満たすradio_button選択肢から選び直す。
# 空白("")や、ref_cdisc_variableが数値でない場合は対象外(そのまま)とする
apply_field_ref_bounds <- function(data, spec, field_ref_bounds) {
  if (is.null(field_ref_bounds) || nrow(field_ref_bounds) == 0) {
    return(data)
  }
  applicable <- field_ref_bounds %>%
    filter(cdisc_variable %in% colnames(data), ref_cdisc_variable %in% colnames(data))

  for (i in seq_len(nrow(applicable))) {
    var_name <- applicable[["cdisc_variable"]][i]
    ref_var <- applicable[["ref_cdisc_variable"]][i]
    bound_type <- applicable[["bound_type"]][i]

    choices <- spec %>%
      filter(cdisc_variable == var_name, field_type == "radio_button") %>%
      mutate(code = ifelse(is.na(code), default_value, code)) %>%
      pull(code) %>%
      unique()
    numeric_choices <- suppressWarnings(as.numeric(choices))
    ref_values <- suppressWarnings(as.numeric(data[[ref_var]]))

    data[[var_name]] <- map2_chr(data[[var_name]], ref_values, function(current, ref_value) {
      if (is.na(current) || current == "" || is.na(ref_value)) {
        return(current)
      }
      valid <- switch(bound_type,
        max_value = choices[!is.na(numeric_choices) & numeric_choices <= ref_value],
        min_value = choices[!is.na(numeric_choices) & numeric_choices >= ref_value],
        exact_value = choices[!is.na(numeric_choices) & numeric_choices == ref_value],
        choices
      )
      if (length(valid) == 0 || current %in% valid) {
        return(current)
      }
      sample(valid, 1)
    })
  }
  data
}

# age_bounds(cdisc_variable, ref_cdisc_variable, min_age, max_age)に基づき、
# cdisc_variable(日付)をref_cdisc_variable(日付)からの経過年数がmin_age〜max_ageに収まるよう
# 生成し直す(片方だけ、あるいは両方無い場合もある)。生成範囲はregistration_start_date〜今日にも収める。
# ref_cdisc_variableがNA、またはcdisc_variableが既にNA(そのlabelに存在しない等)の行は変更しない
apply_age_date_bounds <- function(data, age_bounds, registration_start_date) {
  if (is.null(age_bounds) || nrow(age_bounds) == 0) {
    return(data)
  }
  applicable <- age_bounds %>%
    filter(cdisc_variable %in% colnames(data), ref_cdisc_variable %in% colnames(data))

  for (i in seq_len(nrow(applicable))) {
    var_name <- applicable[["cdisc_variable"]][i]
    ref_var <- applicable[["ref_cdisc_variable"]][i]
    min_age <- applicable[["min_age"]][i]
    max_age <- applicable[["max_age"]][i]

    ref_dates <- as.Date(data[[ref_var]])
    raw_lower <- if (!is.na(min_age)) ref_dates + round(min_age * 365.25) else as.Date(registration_start_date)
    raw_upper <- if (!is.na(max_age)) ref_dates + round(max_age * 365.25) else Sys.Date()

    # registration_start_date〜今日でクランプすると逆転してしまう行(高齢のため年齢条件と
    # 登録期間が両立しない等)は、年齢条件を優先してクランプせずそのまま使う
    lower <- pmax(raw_lower, as.Date(registration_start_date))
    upper <- pmin(raw_upper, Sys.Date())
    invalid <- lower > upper
    lower[invalid] <- raw_lower[invalid]
    upper[invalid] <- raw_upper[invalid]
    upper <- pmax(upper, lower)

    current <- data[[var_name]]
    target <- !is.na(current) & !is.na(ref_dates)
    if (any(target)) {
      new_dates <- as.Date(floor(runif(sum(target), as.numeric(lower[target]), as.numeric(upper[target]))), origin = "1970-01-01")
      # data[[var_name]]は文字列型のため、Date型のままインデックス代入すると
      # (YYYY-MM-DD形式ではなく)内部の数値表現が文字列化されてしまう。as.character()で明示的に変換する
      data[[var_name]][target] <- as.character(new_dates)
    }
  }
  data
}

# cdisc_variable_valuesから、cdisc_variable名 -> prefix の対応表を作る。
# MedDRAコーディングブロックの列(例: AELLTCD)はEDC仕様(cdisc_variable_values)には存在せず
# add_meddra_coding_block()でこちらが独自に追加する列のため、この対応表にも明示的に加えておく
# (そうしないとinject_cross_domain_refs()がprefixを解決できず、これらの列を参照する
# presence_conditions等が他ドメインから結合されないまま無視されてしまう)
build_cdisc_variable_to_prefix <- function(cdisc_variable_values) {
  base <- cdisc_variable_values %>% distinct(cdisc_variable, prefix)
  prefixes <- unique(cdisc_variable_values[["prefix"]])
  coding_block <- prefixes %>% map_dfr(~ tibble(prefix = .x, cdisc_variable = meddra_coding_cols(.x)))
  bind_rows(base, coding_block) %>% distinct(cdisc_variable, prefix)
}

# field_numeric_bounds(alias_name, label単位のcdisc_variable別min/max。build_generation_constraints.R
# 参照)を、testcd_var(例: LBTESTCD)の値をキーにした対応表に変換する。testcd_varは(alias_name, label)
# ごとに1つの固定値(default_value)を持つradio_button項目であるため、これをブリッジとして使うことで、
# alias_name/label情報が失われた最終出力後のデータ(populate_lb_orres等)からでもtestcd値だけで
# 対応するmin/maxを引けるようにする(LB/TR/VSのORRES生成で共通して使う)
build_testcd_numeric_bounds <- function(cdisc_variable_values, field_numeric_bounds, testcd_var, orres_var) {
  testcd_map <- cdisc_variable_values %>%
    filter(cdisc_variable == testcd_var, !is.na(default_value)) %>%
    distinct(alias_name, label, testcd = default_value)
  field_numeric_bounds %>%
    filter(cdisc_variable == orres_var) %>%
    inner_join(testcd_map, by = c("alias_name", "label")) %>%
    distinct(testcd, min_value, max_value)
}

# testcd_varごとに、対応するorres_var(例: TRORRES)がradio_button/check_box(選択式)で
# 定義されているtestcdの集合を返す。これらのtestcdは元々コードリストから正しい値(例:
# ABSENT/PRESENT)が生成されているため、generate_orres_value()による数値上書きの対象から除外する
# (TR domainにLDIAM/SAXISのような数値項目とTUMSTATEのような選択式項目が混在しているため必要)
build_testcd_categorical_set <- function(cdisc_variable_values, testcd_var, orres_var) {
  testcd_map <- cdisc_variable_values %>%
    filter(cdisc_variable == testcd_var, !is.na(default_value)) %>%
    distinct(alias_name, label, testcd = default_value)
  cdisc_variable_values %>%
    filter(cdisc_variable == orres_var, field_type %in% c("radio_button", "check_box")) %>%
    distinct(alias_name, label) %>%
    inner_join(testcd_map, by = c("alias_name", "label")) %>%
    pull(testcd) %>%
    unique()
}

# testcdごとに、testcd_bounds(build_testcd_numeric_bounds()の結果)にある範囲内でランダムな数値を
# 生成する。バリデーション(min/max)が定義されていないtestcd(testcd_boundsに無い)は0〜100の
# ランダムな整数にする(未知のtestcd・バリデーション未定義の既知testcdの両方をこれでカバーする)。
# 範囲の片方だけ定義されている場合、無い方はこのフォールバックと同じ0(下限)・100(上限)を使う
generate_orres_value <- function(testcd, testcd_bounds) {
  bound_idx <- match(testcd, testcd_bounds[["testcd"]])
  has_bound <- !is.na(bound_idx)
  value <- numeric(length(testcd))
  if (any(has_bound)) {
    min_v <- coalesce(testcd_bounds[["min_value"]][bound_idx[has_bound]], 0)
    max_v <- coalesce(testcd_bounds[["max_value"]][bound_idx[has_bound]], 100)
    value[has_bound] <- round(runif(sum(has_bound), min_v, max_v), 2)
  }
  if (any(!has_bound)) {
    value[!has_bound] <- sample(0:100, sum(!has_bound), replace = TRUE)
  }
  value
}

# presence_conditions/field_ref_boundsのうち、cdisc_variableとref_cdisc_variableのprefixが異なる
# (=ドメインをまたぐ参照)行から、(from, to)の依存エッジ一覧を作る。fromはtoに依存する(toを先に生成する必要がある)
build_cross_prefix_edges <- function(presence_conditions, field_ref_bounds, cdisc_variable_to_prefix, age_bounds = NULL, date_ref_bounds = NULL) {
  if (is.null(age_bounds)) {
    age_bounds <- tibble(cdisc_variable = character(0), ref_cdisc_variable = character(0))
  }
  if (is.null(date_ref_bounds)) {
    date_ref_bounds <- tibble(cdisc_variable = character(0), ref_cdisc_variable = character(0))
  }
  if (!("ref2_cdisc_variable" %in% names(presence_conditions))) {
    presence_conditions[["ref2_cdisc_variable"]] <- NA_character_
  }
  bind_rows(
    presence_conditions %>% select(cdisc_variable, ref_cdisc_variable),
    presence_conditions %>% select(cdisc_variable, ref2_cdisc_variable) %>% rename(ref_cdisc_variable = ref2_cdisc_variable),
    field_ref_bounds %>% select(cdisc_variable, ref_cdisc_variable),
    age_bounds %>% select(cdisc_variable, ref_cdisc_variable),
    date_ref_bounds %>% select(cdisc_variable, ref_cdisc_variable)
  ) %>%
    distinct() %>%
    left_join(cdisc_variable_to_prefix, by = "cdisc_variable") %>%
    left_join(
      cdisc_variable_to_prefix %>% rename(ref_cdisc_variable = cdisc_variable, ref_prefix = prefix),
      by = "ref_cdisc_variable"
    ) %>%
    filter(!is.na(prefix), !is.na(ref_prefix), prefix != ref_prefix) %>%
    distinct(from = prefix, to = ref_prefix)
}

# prefixes を、edges(from依存toの依存関係)に基づいて依存先が先に来るように並べ替える(トポロジカルソート)。
# 循環参照がある場合は、それ以上並べ替えできない分をそのまま残りの順序で追加する
topo_sort_prefixes <- function(prefixes, edges) {
  edges <- edges %>% filter(from %in% prefixes, to %in% prefixes)
  remaining <- prefixes
  ordered <- character(0)
  while (length(remaining) > 0) {
    ready <- remaining[!vapply(remaining, function(p) any(edges[["from"]] == p & edges[["to"]] %in% remaining), logical(1))]
    if (length(ready) == 0) {
      ordered <- c(ordered, remaining)
      break
    }
    ordered <- c(ordered, ready)
    remaining <- setdiff(remaining, ready)
  }
  ordered
}

# topo_sort_prefixes()と同じアルゴリズムだが、循環(依存が解決できず「ready」が空になる時点)に
# 達したら、そこで打ち切ってordered(そこまでに確定した順序)とremaining(未解決のまま残った、
# 循環に関与する・またはそれに依存しているprefix集合)を分けて返す。build_other_domains()が、
# remainingの部分だけをシート(alias_name)単位で改めて依存解決するために使う
topo_sort_prefixes_with_leftover <- function(prefixes, edges) {
  edges <- edges %>% filter(from %in% prefixes, to %in% prefixes)
  remaining <- prefixes
  ordered <- character(0)
  while (length(remaining) > 0) {
    ready <- remaining[!vapply(remaining, function(p) any(edges[["from"]] == p & edges[["to"]] %in% remaining), logical(1))]
    if (length(ready) == 0) {
      break
    }
    ordered <- c(ordered, ready)
    remaining <- setdiff(remaining, ready)
  }
  list(ordered = ordered, remaining = remaining)
}

# presence_conditions/field_ref_bounds/age_bounds/date_ref_boundsから、(prefix, alias_name)を
# ノードとする依存エッジ一覧を作る(build_cross_prefix_edges()のシート単位版)。
# prefix単位のbuild_cross_prefix_edges()と異なり、同じprefix内の別alias_name(シート)への参照
# (例: inductionのSVSTDTCがprephaseのSVSTDTCを参照する)も含める。これにより、
# 「prefix単位で見ると循環だが、実際にはシート単位では循環でない」参照チェーン
# (例: SV(hr3fisrt)→RS→LB→FA→SV(prephase))を正しく順序付けできる
# (同じprefix内の参照は、通常は各build_*_domain()呼び出し内の自己参照機構で解決されるため、
# ここに含めても実害は無い。単にトポロジカルソートの精度が上がるだけ)
build_alias_level_edges <- function(presence_conditions, field_ref_bounds, cdisc_variable_to_prefix, age_bounds = NULL, date_ref_bounds = NULL) {
  if (is.null(age_bounds)) {
    age_bounds <- tibble(alias_name = character(0), cdisc_variable = character(0), ref_cdisc_variable = character(0), ref_alias_name = character(0))
  }
  if (is.null(date_ref_bounds)) {
    date_ref_bounds <- tibble(alias_name = character(0), cdisc_variable = character(0), ref_cdisc_variable = character(0), ref_alias_name = character(0))
  }
  if (!("alias_name" %in% colnames(field_ref_bounds))) {
    field_ref_bounds <- field_ref_bounds %>% mutate(alias_name = NA_character_)
  }
  if (!("ref2_cdisc_variable" %in% names(presence_conditions))) {
    presence_conditions[["ref2_cdisc_variable"]] <- NA_character_
  }
  if (!("ref2_alias_name" %in% names(presence_conditions))) {
    presence_conditions[["ref2_alias_name"]] <- NA_character_
  }
  bind_rows(
    presence_conditions %>% select(alias_name, cdisc_variable, ref_cdisc_variable, ref_alias_name),
    presence_conditions %>%
      select(alias_name, cdisc_variable, ref2_cdisc_variable, ref2_alias_name) %>%
      rename(ref_cdisc_variable = ref2_cdisc_variable, ref_alias_name = ref2_alias_name),
    # field_ref_bounds(formula参照)は必ず同一シート内の参照のため、ref_alias_nameは自分自身と同じ
    field_ref_bounds %>% transmute(alias_name, cdisc_variable, ref_cdisc_variable, ref_alias_name = alias_name),
    age_bounds %>% select(alias_name, cdisc_variable, ref_cdisc_variable, ref_alias_name),
    date_ref_bounds %>% select(alias_name, cdisc_variable, ref_cdisc_variable, ref_alias_name)
  ) %>%
    filter(!is.na(alias_name), !is.na(ref_alias_name)) %>%
    distinct() %>%
    left_join(cdisc_variable_to_prefix, by = "cdisc_variable") %>%
    left_join(
      cdisc_variable_to_prefix %>% rename(ref_cdisc_variable = cdisc_variable, ref_prefix = prefix),
      by = "ref_cdisc_variable"
    ) %>%
    filter(!is.na(prefix), !is.na(ref_prefix)) %>%
    distinct(from_prefix = prefix, from_alias = alias_name, to_prefix = ref_prefix, to_alias = ref_alias_name)
}

# nodes(prefix, alias_name)を、edges(from_prefix, from_alias, to_prefix, to_alias。fromはtoに依存する)に
# 基づいてトポロジカルソートする。topo_sort_prefixes()の(prefix, alias_name)複合キー版。
# 循環参照が残った場合は、それ以上並べ替えできない分をそのまま残りの順序で追加する(安全策)
topo_sort_prefix_aliases <- function(nodes, edges) {
  node_key <- function(prefix, alias) str_c(prefix, "", alias)
  nodes <- nodes %>% distinct(prefix, alias_name) %>% mutate(.key = node_key(prefix, alias_name))
  edges <- edges %>%
    mutate(.from_key = node_key(from_prefix, from_alias), .to_key = node_key(to_prefix, to_alias)) %>%
    filter(.from_key %in% nodes[[".key"]], .to_key %in% nodes[[".key"]], .from_key != .to_key)

  remaining <- nodes[[".key"]]
  ordered_keys <- character(0)
  while (length(remaining) > 0) {
    ready <- remaining[!vapply(remaining, function(k) any(edges[[".from_key"]] == k & edges[[".to_key"]] %in% remaining), logical(1))]
    if (length(ready) == 0) {
      ordered_keys <- c(ordered_keys, remaining)
      break
    }
    ordered_keys <- c(ordered_keys, ready)
    remaining <- setdiff(remaining, ready)
  }
  nodes[match(ordered_keys, nodes[[".key"]]), c("prefix", "alias_name")]
}

# presence_conditions/field_ref_boundsが参照するcdisc_variableのうち、dataにまだ無いものを、
# 既に生成済みのbuilt_domainsから探して結合する(他ドメイン参照)。
# ref_alias_name/ref_label(参照先フィールド自身が属する固定のブロック、例: RSがSCの特定labelを参照する場合)が
# 分かっていればそのインスタンスに固定して結合する(USUBJIDのみ)。
# 無指定(NA)の場合は、両者がalias_name/labelを持てばそれも突き合わせキーにする(同じブロック内の参照)。
# どちらの情報も無ければUSUBJIDのみで結合する(参照元に複数レコードあると最初の1件を使う)。
#
# 同じref_cdisc_variableに対して複数の異なる(ref_alias_name, ref_label)の組み合わせがある場合
# (例: DDORRESが、discon由来の行はdiscon自身のDSTERM、withdrawal由来の行はwithdrawal自身のDSTERMを
# それぞれ参照する、という"同じ変数名だが参照元シートごとに別インスタンス"のケース)、
# 各組み合わせをdataの該当行(dataのalias_nameがそのref_alias_nameと一致する行)だけに絞って注入する。
# dataがalias_nameを持たない場合や、そのref_alias_nameがdata自身のalias_nameのどれとも一致しない場合
# (=真に外部の固定参照)は、全行に対して適用する
# own_prefixが指定されている場合、ref_prefix(参照先変数のドメイン)がown_prefixと同じ(=自分自身の
# ドメインを参照している)ものは注入しない。wave分割時、built_domains[[own_prefix]]には前waveまでの
# 未finalizeな結果が既に入っているため、素通りさせるとdata自身に同名列が「既にある」ことになり、
# このあとの通常の値生成(populate_date_fields等)がその変数をまるごとスキップしてしまう
# (このケースはresolve_date_ref_bound_vals()のexisting_dataフォールバックで別途正しく処理される)
# 戻り値はlist(data=結合後のdata, injected_cols=このために追加した列名)
inject_cross_domain_refs <- function(data, presence_conditions, field_ref_bounds, built_domains, cdisc_variable_to_prefix, age_bounds = NULL, date_ref_bounds = NULL, own_prefix = NULL) {
  if (is.null(presence_conditions)) {
    presence_conditions <- tibble(ref_cdisc_variable = character(0))
  }
  if (is.null(field_ref_bounds)) {
    field_ref_bounds <- tibble(ref_cdisc_variable = character(0))
  }
  if (is.null(age_bounds)) {
    age_bounds <- tibble(ref_cdisc_variable = character(0))
  }
  if (is.null(date_ref_bounds)) {
    date_ref_bounds <- tibble(ref_cdisc_variable = character(0))
  }
  # age_gt/age_ge/age_lt/age_le型のpresence_conditions(build_generation_constraints.Rのage_ref_presence_
  # conditions)は、通常のref_cdisc_variableに加えてref2_cdisc_variable(もう一方の参照先)を持つ。
  # 無ければ全行NAの列として補い、下記のref_instancesでref2側も別途pinとして扱えるようにする
  if (!("ref2_cdisc_variable" %in% names(presence_conditions))) {
    presence_conditions[["ref2_cdisc_variable"]] <- NA_character_
  }
  if (!("ref2_alias_name" %in% names(presence_conditions))) {
    presence_conditions[["ref2_alias_name"]] <- NA_character_
  }
  if (!("ref2_label" %in% names(presence_conditions))) {
    presence_conditions[["ref2_label"]] <- NA_character_
  }
  # labelは、この参照条件が定義されている側(dataになる予定のドメイン自身)のインスタンス(label)。
  # 同じref_cdisc_variable(例: RSORRES)でも、参照元のlabelブロックごとに参照先のref_labelが
  # 異なる場合(例: MHの5つのSPDEVIDブロックが、それぞれ別のRSブロック(034/035/036/...)を参照する)、
  # このlabelを保持しておかないと、後段でどのpinをdataのどの行に適用すべきか判定できない
  # (field_ref_bounds/age_boundsはlabelを持たないため、その場合はNAのままになる)
  # date_ref_boundsのref_alias_nameは、ref('sheet_alias', N)形式の他シート参照があればその
  # 参照先シート、無ければ自分自身と同じalias_name(build_generation_constraints.Rのdate_ref_bounds
  # 構築時に補われている)
  ref_instances <- bind_rows(
    presence_conditions %>% select(any_of(c("alias_name", "label", "ref_cdisc_variable", "ref_alias_name", "ref_label"))),
    presence_conditions %>%
      select(any_of(c("alias_name", "label", "ref2_cdisc_variable", "ref2_alias_name", "ref2_label"))) %>%
      rename(ref_cdisc_variable = ref2_cdisc_variable, ref_alias_name = ref2_alias_name, ref_label = ref2_label),
    field_ref_bounds %>% select(any_of("ref_cdisc_variable")),
    age_bounds %>% select(any_of(c("alias_name", "label", "ref_cdisc_variable", "ref_alias_name", "ref_label"))),
    date_ref_bounds %>% select(any_of(c("alias_name", "label", "ref_cdisc_variable", "ref_label", "ref_alias_name")))
  ) %>%
    filter(!is.na(ref_cdisc_variable)) %>%
    distinct()
  if (!("alias_name" %in% names(ref_instances))) {
    ref_instances[["alias_name"]] <- NA_character_
  }
  if (!("label" %in% names(ref_instances))) {
    ref_instances[["label"]] <- NA_character_
  }
  if (!("ref_alias_name" %in% names(ref_instances))) {
    ref_instances[["ref_alias_name"]] <- NA_character_
  }
  if (!("ref_label" %in% names(ref_instances))) {
    ref_instances[["ref_label"]] <- NA_character_
  }

  # target_rows(pinごとの行の絞り込み)にはdataのalias_name列だけあれば十分(labelは不要。
  # build_generic_domain由来のdataはlabel列を持たないため、labelまで要求すると絞り込みが常に無効化されてしまう)。
  # 一方、has_data_alias(同じブロックのlabelで突き合わせるフォールバック)はalias_nameとlabelの両方が必要
  has_data_alias_name <- "alias_name" %in% colnames(data)
  has_data_alias <- all(c("alias_name", "label") %in% colnames(data))
  data_alias_names <- if (has_data_alias_name) unique(data[["alias_name"]]) else character(0)

  injected_cols <- character(0)
  for (ref_var in unique(ref_instances[["ref_cdisc_variable"]])) {
    if (ref_var %in% colnames(data)) {
      next
    }
    ref_prefix <- cdisc_variable_to_prefix %>% filter(cdisc_variable == ref_var) %>% pull(prefix) %>% first()
    if (is.na(ref_prefix) || !(ref_prefix %in% names(built_domains))) {
      next
    }
    if (!is.null(own_prefix) && ref_prefix == own_prefix) {
      next
    }
    ref_data <- built_domains[[ref_prefix]]
    if (!(ref_var %in% colnames(ref_data))) {
      next
    }
    has_ref_alias <- all(c("alias_name", "label") %in% colnames(ref_data))
    # 参照先がbuild_generic_domain由来(例: SV)の場合、alias_nameはあってもlabelが無い
    # (繰り返し項目を持たないため)。has_ref_aliasはlabelも必須なのでこのケースではFALSEになるが、
    # alias_name自体は参照先の絞り込みに使えるので別途保持しておく
    has_ref_alias_only <- "alias_name" %in% colnames(ref_data)

    # 型をref_data側に合わせた全NA列を用意し、pinごとに該当行だけ値を埋めていく
    result_col <- ref_data[[ref_var]][rep(NA_integer_, nrow(data))]

    pins <- ref_instances %>% filter(ref_cdisc_variable == ref_var) %>% distinct(alias_name, label, ref_alias_name, ref_label)
    for (i in seq_len(nrow(pins))) {
      own_alias <- pins[["alias_name"]][i]
      pin_alias <- pins[["ref_alias_name"]][i]
      pin_label <- pins[["ref_label"]][i]
      own_label <- pins[["label"]][i]

      # 絞り込みはown_alias/own_label(この条件が定義されているdata自身のインスタンス)を最優先する。
      # own_aliasが分かっている場合、それがdata自身のalias_nameのいずれかと一致するならその行だけに
      # 絞る。一致しない場合(wave分割で別waveに分かれていて、このdataにown_aliasの行がそもそも無い)は
      # 対象0件とする(全行を対象にするとdataに含まれる別aliasの行にまで誤って値を書き込んでしまう。
      # 実際に発生したバグ: wave分割によりdate_ref_boundsが同じprefix内の全alias分を含むようになり、
      # own_alias/pin_aliasのどちらも今回のdataに無いpinが多数生じ、最後に処理されたpinの値が
      # 無関係なaliasの行にまで書き込まれていた)。own_aliasが無い場合(field_ref_bounds由来、
      # 真に外部の固定参照)のみ、従来通りpin_aliasで判定するか、それも無ければ全行を対象にする
      target_rows <- if (has_data_alias_name && !is.na(own_alias)) {
        if (own_alias %in% data_alias_names) {
          rows <- data[["alias_name"]] == own_alias
          if (has_data_alias && !is.na(own_label)) {
            rows <- rows & data[["label"]] == own_label
          }
          rows
        } else {
          rep(FALSE, nrow(data))
        }
      } else if (has_data_alias_name && !is.na(pin_alias) && pin_alias %in% data_alias_names) {
        rows <- data[["alias_name"]] == pin_alias
        if (has_data_alias && !is.na(pin_label) && pin_label %in% data[["label"]][rows]) {
          rows <- rows & data[["label"]] == pin_label
        }
        rows
      } else {
        rep(TRUE, nrow(data))
      }
      if (!any(target_rows)) {
        next
      }

      if (!is.na(pin_label) && has_ref_alias) {
        # 参照先の特定のlabelインスタンスに固定する(参照元自身のlabelとは無関係)
        ref_slice <- ref_data %>%
          filter(alias_name == pin_alias, label == pin_label) %>%
          select(USUBJID, !!ref_var) %>%
          distinct(USUBJID, .keep_all = TRUE)
        value_map <- set_names(ref_slice[[ref_var]], ref_slice[["USUBJID"]])
        result_col[target_rows] <- value_map[data[["USUBJID"]][target_rows]]
      } else if (!is.na(pin_alias) && has_ref_alias_only) {
        # 参照先にlabelが無い(build_generic_domain由来、例: SV)場合、alias_nameだけで絞り込む。
        # ここで絞り込まずUSUBJIDだけで結合すると、参照先ドメインの中で最初に出現したalias
        # (実際に参照したいaliasとは無関係な、ビルド順が早いだけの別シート)の値を拾ってしまう
        ref_slice <- ref_data %>%
          filter(alias_name == pin_alias) %>%
          select(USUBJID, !!ref_var) %>%
          distinct(USUBJID, .keep_all = TRUE)
        value_map <- set_names(ref_slice[[ref_var]], ref_slice[["USUBJID"]])
        result_col[target_rows] <- value_map[data[["USUBJID"]][target_rows]]
      } else if (has_data_alias && has_ref_alias) {
        # labelが不明(同じブロック内の述語参照など): 参照元自身の(alias_name, label)で突き合わせる
        ref_slice <- ref_data %>%
          select(USUBJID, alias_name, label, !!ref_var) %>%
          distinct(USUBJID, alias_name, label, .keep_all = TRUE)
        matched <- data[target_rows, ] %>%
          select(USUBJID, alias_name, label) %>%
          left_join(ref_slice, by = c("USUBJID", "alias_name", "label"))
        result_col[target_rows] <- matched[[ref_var]]
      } else {
        ref_slice <- ref_data %>%
          select(USUBJID, !!ref_var) %>%
          distinct(USUBJID, .keep_all = TRUE)
        value_map <- set_names(ref_slice[[ref_var]], ref_slice[["USUBJID"]])
        result_col[target_rows] <- value_map[data[["USUBJID"]][target_rows]]
      }
    }

    data[[ref_var]] <- result_col
    injected_cols <- c(injected_cols, ref_var)
  }
  list(data = data, injected_cols = injected_cols)
}

# LLTコードを少数に絞り、Zipf的な重みでサンプリングすることで、頻出病名と稀な病名が混在するようにする
# 1レコードにつき1つのLLT〜SOCの階層をまとめて返すため、各コード値の対応関係が崩れない
sample_meddra_rows <- function(meddra, n, pool_size = 20) {
  pool <- meddra %>%
    distinct(llt_code, .keep_all = TRUE) %>%
    slice_sample(n = min(pool_size, n_distinct(meddra[["llt_code"]])))
  weights <- 1 / seq_len(nrow(pool))
  pool[sample(seq_len(nrow(pool)), size = n, replace = TRUE, prob = weights), ]
}

# meddra_sample(1行=1つのLLT〜SOC階層)の一部の行を、required_llt_codes(必ずデータに含めたいLLTコード)の
# 値で上書きする。コードごとに1行を選び、そのLLTコードに対応する階層一式に丸ごと差し替える。
# required_llt_codesが空、meddra_sampleが0行、または該当コードがmeddraに存在しない場合は何もしない(そのコードは無視される)
inject_required_llt_codes <- function(meddra_sample, meddra, required_llt_codes) {
  required_llt_codes <- required_llt_codes[!is.na(required_llt_codes) & required_llt_codes != ""]
  if (length(required_llt_codes) == 0 || nrow(meddra_sample) == 0) {
    return(meddra_sample)
  }
  n <- nrow(meddra_sample)
  target_rows <- sample(seq_len(n), size = length(required_llt_codes), replace = length(required_llt_codes) > n)
  for (i in seq_along(required_llt_codes)) {
    hierarchy_row <- meddra %>% filter(llt_code == required_llt_codes[i])
    if (nrow(hierarchy_row) == 0) {
      next
    }
    meddra_sample[target_rows[i], ] <- hierarchy_row[1, ]
  }
  meddra_sample
}

# field_type=="meddra"に該当する変数名を抽出
compute_meddra_vars <- function(spec, target_vars) {
  spec %>%
    filter(field_type == "meddra") %>%
    pull(cdisc_variable) %>%
    unique() %>%
    intersect(target_vars)
}

# meddra変数にLLT名を格納する。default_valueが8桁数字の場合はllt_codeとみなし、
# 対応するllt_nameを固定値として使う。それ以外はmeddra_sampleのllt_nameを使う
populate_meddra_fields <- function(data, spec, meddra_vars, meddra, meddra_sample) {
  for (var_name in meddra_vars) {
    llt_cd <- spec %>%
      filter(field_type == "meddra", cdisc_variable == var_name, str_detect(default_value, "^[0-9]{8}$")) %>%
      pull(default_value) %>%
      unique()
    if (length(llt_cd) == 1) {
      llt_name <- meddra %>% filter(llt_code == llt_cd) %>% pull(llt_name) %>% unique()
      data[[var_name]] <- llt_name[1]
    } else {
      data[[var_name]] <- meddra_sample[["llt_name"]]
    }
  }
  data
}

# field_type=="drug"に該当する変数名を抽出
compute_drug_vars <- function(spec, target_vars) {
  spec %>%
    filter(field_type == "drug") %>%
    pull(cdisc_variable) %>%
    unique() %>%
    intersect(target_vars)
}

# drug_vars(field_type=="drug"な変数)のspec行が、1つでもdefault_value(固定コード)無し
# (=ランダムサンプリングされ、実際にwho_drug_idfとの一致を確認する意味がある)場合はTRUE。
# 全て固定コードで値が確定している場合はFALSE(この場合、xxDECOD列は生成しない)
drug_vars_need_decod <- function(spec, drug_vars) {
  drug_rows <- spec %>% filter(field_type == "drug", cdisc_variable %in% drug_vars)
  any(is.na(drug_rows[["default_value"]]) | drug_rows[["default_value"]] == "")
}

# drug変数に薬剤名を格納する。default_valueが数値の場合はwho_drug_idf$drug_codeとみなし、
# 対応するfull_name_enを固定値として使う。それ以外はwho_drug_idf$full_name_enからランダムにサンプリングする。
# 同じcdisc_variableでも、field_type=="drug"と定義されているalias_nameの行だけを対象にする
# (同じcdisc_variableが別のalias_nameでは固定値/別のfield_typeとして定義されている場合、その行は変更しない)
populate_drug_fields <- function(data, spec, drug_vars, who_drug_idf) {
  drug_names <- who_drug_idf[["full_name_en"]] %>% discard(is.na) %>% unique()
  if (length(drug_names) == 0) {
    return(data)
  }
  for (var_name in drug_vars) {
    drug_spec_rows <- spec %>% filter(field_type == "drug", cdisc_variable == var_name)
    for (an in unique(drug_spec_rows[["alias_name"]])) {
      target <- data[["alias_name"]] == an
      if (!any(target)) {
        next
      }
      default_value <- drug_spec_rows %>% filter(alias_name == an) %>% pull(default_value) %>% discard(~ is.na(.x) | .x == "") %>% unique()
      fixed_name <- if (length(default_value) == 1 && str_detect(default_value, "^[0-9]+$")) {
        who_drug_idf %>% filter(drug_code == default_value) %>% pull(full_name_en) %>% discard(is.na) %>% unique()
      } else {
        character(0)
      }
      if (length(fixed_name) >= 1) {
        data[[var_name]][target] <- fixed_name[1]
      } else {
        data[[var_name]][target] <- sample(drug_names, size = sum(target), replace = TRUE)
      }
    }
  }
  data
}

# drug変数の値がwho_drug_idf$full_name_enに完全一致する場合、対応するgeneric_name_enを
# prefixDECOD(例: CMDECOD)に格納する(一致しない場合はNA)。field_type=="drug"と定義されている
# alias_nameの行だけを対象にする(他のalias_nameの値がたまたま薬剤名と一致しても対象にしない)。
# drug_varsが複数ある場合は、最初に一致した変数の値を採用する
add_drug_decod <- function(data, spec, drug_vars, who_drug_idf, prefix) {
  if (length(drug_vars) == 0) {
    return(data)
  }
  # 同じfull_name_enが複数行あり、一部だけgeneric_name_enが空のことがあるため、
  # distinct()で先頭行を無条件に採用すると本来値があるはずのケースまで空になってしまう。
  # generic_name_enが空でない行を優先して残すよう、先に並べ替えてからdistinct()する
  lookup <- who_drug_idf %>%
    filter(!is.na(full_name_en)) %>%
    arrange(is.na(generic_name_en)) %>%
    distinct(full_name_en, .keep_all = TRUE)

  decod_var <- str_c(prefix, "DECOD")
  data[[decod_var]] <- drug_vars %>%
    map(function(var_name) {
      drug_alias_names <- spec %>% filter(field_type == "drug", cdisc_variable == var_name) %>% pull(alias_name) %>% unique()
      is_drug_row <- data[["alias_name"]] %in% drug_alias_names
      matched <- lookup[["generic_name_en"]][match(data[[var_name]], lookup[["full_name_en"]])]
      if_else(is_drug_row, matched, NA_character_)
    }) %>%
    reduce(coalesce)
  data
}

# MedDRAコーディングブロック(LLT〜SOC)の列名 (例: prefix="MH" -> MHLLT, MHLLTCD, ...)
meddra_coding_cols <- function(prefix) {
  str_c(prefix, c("LLT", "LLTCD", "DECOD", "PTCD", "HLT", "HLTCD", "HLGT", "HLGTCD", "BODSYS", "BDSYCD", "SOC", "SOCCD"))
}

# MedDRAコーディングブロック(LLT〜SOC)を追加。meddra_sampleと同じ階層を使い、コード間の対応関係を保つ
add_meddra_coding_block <- function(data, meddra_sample, prefix) {
  data[[str_c(prefix, "LLT")]] <- meddra_sample[["llt_name"]]
  data[[str_c(prefix, "LLTCD")]] <- meddra_sample[["llt_code"]]
  data[[str_c(prefix, "DECOD")]] <- meddra_sample[["pt_name"]]
  data[[str_c(prefix, "PTCD")]] <- meddra_sample[["pt_code"]]
  data[[str_c(prefix, "HLT")]] <- meddra_sample[["hlt_name"]]
  data[[str_c(prefix, "HLTCD")]] <- meddra_sample[["hlt_code"]]
  data[[str_c(prefix, "HLGT")]] <- meddra_sample[["hlgt_name"]]
  data[[str_c(prefix, "HLGTCD")]] <- meddra_sample[["hlgt_code"]]
  data[[str_c(prefix, "SOC")]] <- meddra_sample[["soc_name"]]
  data[[str_c(prefix, "SOCCD")]] <- meddra_sample[["soc_code"]]
  data[[str_c(prefix, "BODSYS")]] <- meddra_sample[["soc_name"]]
  data[[str_c(prefix, "BDSYCD")]] <- meddra_sample[["soc_code"]]
  data
}

# データセット全体の通番を付与 (xxSEQ)
add_seq <- function(data, seq_var) {
  data %>% mutate(!!seq_var := row_number())
}

# prefixSEQ列(例: AESEQ)を持つドメインは、出力直前にその列でソートする。add_seq()の時点では
# 行順=SEQ順だが、その後のmerge/filter等で行順が崩れることがあるため、最終出力前に明示的に揃える。
# prefixSEQ列が無いドメイン(例: DM)は何もしない
sort_by_seq <- function(data, prefix) {
  seq_var <- str_c(prefix, "SEQ")
  if (seq_var %in% colnames(data)) {
    data <- data %>% arrange(.data[[seq_var]])
  }
  data
}

# 列順を整理: front_cols -> その他 -> end_cols。存在しない列はエラーにならず無視する
reorder_domain_columns <- function(data, front_cols = character(0), end_cols = character(0)) {
  data %>%
    select(any_of(front_cols), everything(), -any_of(end_cols), any_of(end_cols))
}

# 全ドメイン共通で先頭に固定したい列 (STUDYID, DOMAIN, USUBJID, prefixSEQ, prefixSPID)
# reorder_domain_columns()のfront_colsにそのまま渡す想定。存在しない列は無視される
domain_front_cols <- function(prefix) {
  c("STUDYID", "DOMAIN", "USUBJID", str_c(prefix, "SEQ"), str_c(prefix, "SPID"))
}

# alias_nameがmulti_record_alias_names(sheetsのcategoryが"ae_report"または"multiple"のalias_name一覧)に
# 該当する行だけ、AEドメインと同じ形式(alias_name + USUBJID内の連番、例: concomitant_drug1)でSPIDを
# 付与し直す。該当しない行のSPIDは変更しない
apply_multi_record_spid <- function(data, spid_var, multi_record_alias_names) {
  if (length(multi_record_alias_names) == 0 || !("alias_name" %in% colnames(data))) {
    return(data)
  }
  # USUBJID×alias_nameでグループ化することで、連番はalias_nameごとに独立してリセットされる
  # (対象のalias_nameが複数あっても互いに混ざらない)
  data %>%
    group_by(USUBJID, alias_name) %>%
    mutate(!!spid_var := if (alias_name[1] %in% multi_record_alias_names) str_c(alias_name, row_number()) else .data[[spid_var]]) %>%
    ungroup()
}

# visit_lookup(alias_name, VISIT, VISITNUM)をalias_nameで結合し、VISIT/VISITNUM列を追加する。
# category=="visit"のシート由来でない行(一致しない行)はNAのまま
add_visit_columns <- function(data, visit_lookup) {
  if (is.null(visit_lookup) || nrow(visit_lookup) == 0 || !("alias_name" %in% colnames(data))) {
    return(data)
  }
  data <- data %>% left_join(visit_lookup, by = "alias_name")
  # このドメインにvisitカテゴリのシート由来の行が1件も無ければ(全行NA)、
  # 意味の無い空列を出さないようVISIT/VISITNUM列自体を削除する
  if (all(is.na(data[["VISIT"]]))) {
    data <- data %>% select(-VISIT, -VISITNUM)
  }
  data
}

# gated_vars(presence_conditionsで条件付けされている変数)が全てNAの行を除外する。
# DD(死因)のように、DDTEST/DDTESTCDのような固定値の列は常に埋まっているため、
# 「ドメインの全列がNA」ではなく「条件付きの列(例: DDORRES)が全てNA」で判定する必要がある。
# gated_varsが空、またはdomainに1つも存在しない場合は何もしない
drop_empty_domain_rows <- function(domain, gated_vars) {
  gated_vars <- intersect(gated_vars, colnames(domain))
  if (length(gated_vars) == 0) {
    return(domain)
  }
  all_na <- domain %>% select(all_of(gated_vars)) %>% apply(1, function(row) all(is.na(row)))
  domain[!all_na, ]
}

# candidates(USUBJID, alias_name)の各USUBJIDについて、1つのalias_nameを選ぶ。
# presence_conditions(このドメイン自身のcdisc_variableに絞り込み済み)から、候補のalias_nameが
# 実際にゲーティング条件を満たす(=値が入る)ものであれば、それを優先して選ぶ
# (例: DD(死因)がdiscon/withdrawalのどちらかを選ぶ際、実際にDSTERM=="DEATH"になっている方を選ぶことで、
# ランダムに無関係な方を選んでしまい値が常にNAになる、という事態を避ける)。
# 条件を満たす候補が無い、またはpresence_conditionsに該当するequals条件が無い場合はランダムに1つ選ぶ
resolve_preferred_alias_name <- function(candidates, presence_conditions, built_domains, cdisc_variable_to_prefix) {
  equals_conditions <- presence_conditions %>%
    filter(condition_type == "equals", !is.na(ref_alias_name))

  if (nrow(equals_conditions) == 0) {
    return(
      candidates %>%
        group_by(USUBJID) %>%
        slice_sample(n = 1) %>%
        ungroup()
    )
  }

  satisfied <- equals_conditions %>%
    group_by(ref_cdisc_variable, ref_alias_name, ref_label) %>%
    summarise(expected_values = list(unique(expected_value)), .groups = "drop") %>%
    pmap_dfr(function(ref_cdisc_variable, ref_alias_name, ref_label, expected_values) {
      ref_prefix <- cdisc_variable_to_prefix %>% filter(cdisc_variable == ref_cdisc_variable) %>% pull(prefix) %>% first()
      if (is.na(ref_prefix) || is.null(built_domains) || !(ref_prefix %in% names(built_domains))) {
        return(tibble())
      }
      ref_data <- built_domains[[ref_prefix]]
      if (!all(c("USUBJID", ref_cdisc_variable) %in% colnames(ref_data))) {
        return(tibble())
      }
      ref_slice <- if (all(c("alias_name", "label") %in% colnames(ref_data)) && !is.na(ref_label)) {
        ref_data %>% filter(alias_name == ref_alias_name, label == ref_label)
      } else {
        ref_data
      }
      ref_slice %>%
        filter(.data[[ref_cdisc_variable]] %in% expected_values) %>%
        distinct(USUBJID) %>%
        mutate(alias_name = ref_alias_name)
    })

  if (nrow(satisfied) == 0) {
    return(
      candidates %>%
        group_by(USUBJID) %>%
        slice_sample(n = 1) %>%
        ungroup()
    )
  }

  candidates %>%
    left_join(satisfied %>% mutate(.satisfied = TRUE) %>% distinct(USUBJID, alias_name, .satisfied), by = c("USUBJID", "alias_name")) %>%
    mutate(.satisfied = coalesce(.satisfied, FALSE)) %>%
    group_by(USUBJID) %>%
    group_modify(~ {
      pool <- if (any(.x[[".satisfied"]])) filter(.x, .satisfied) else .x
      slice_sample(pool, n = 1)
    }) %>%
    ungroup() %>%
    select(-.satisfied)
}

# 同じcdisc_variable(date型)が複数のalias_name(シート)にまたがって定義されているドメイン
# (例: AEが"sae_report"/"ae2"の2シートに分かれる、EC/LB/VSが来院ごとに多数のシートに分かれる)では、
# 各シートの日付が互いに独立に生成されるため、シートの本来の並び順(sheet_orders$seq、
# EDC仕様上のフォーム表示順)と生成された日付の前後関係が矛盾することがある
# (例: 来院1のLBDTCが来院3のLBDTCより後になる)。
# 被験者ごとに、シート(alias_name)ブロック単位でまとめて日付をシフトすることでこれを解消する:
# 各(USUBJID, alias_name)ブロックの代表日付(そのブロック内で最も早い非NA日付)を求め、
# sheet_seq昇順に並べたブロックに、代表日付を昇順に並べ替えたものを割り当て直す。
# ブロック内の全date型列を同じ日数分シフトすることで、ブロック内の関係(同じ行の開始日<=終了日、
# 同じalias内のlabelを跨ぐ連鎖等)は変えずに保つ。シフト後の値は被験者の中止日(discontinuation_date、
# 無ければ今日)を上限にする(この関数はclamp_dates_to_discontinuationより後に呼ぶこと。
# シフトが中止日を超えないようこの関数自身で保証するため、これより後に他の日付再生成処理を
# 挟むと、その処理がシフト結果を独立に書き換えてalias間の順序を崩してしまう可能性がある)。
# alias_name列を持たない、またはdate_varsが複数aliasにまたがらないドメインでは何もしない
reorder_dates_by_sheet_seq <- function(data, date_vars, cdisc_variable_values, registration_start_date, discontinuation_date = NULL, date_ref_bounds = NULL) {
  if (!("alias_name" %in% colnames(data)) || !("USUBJID" %in% colnames(data))) {
    return(data)
  }
  date_vars <- intersect(date_vars, colnames(data))
  if (length(date_vars) == 0) {
    return(data)
  }

  alias_seq_map <- cdisc_variable_values %>%
    filter(!is.na(sheet_seq)) %>%
    distinct(alias_name, sheet_seq)
  if (nrow(alias_seq_map) == 0) {
    return(data)
  }

  reg_start <- as.Date(registration_start_date)
  today <- Sys.Date()
  discon_lookup <- NULL
  if (!is.null(discontinuation_date) && nrow(discontinuation_date) > 0) {
    discon_map <- discontinuation_date %>% filter(!is.na(DISCONDTC)) %>% distinct(USUBJID, .keep_all = TRUE)
    discon_lookup <- set_names(as.Date(discon_map[["DISCONDTC"]]), discon_map[["USUBJID"]])
  }

  date_long <- data %>%
    mutate(.row_id = row_number()) %>%
    select(.row_id, USUBJID, alias_name, all_of(date_vars)) %>%
    pivot_longer(cols = all_of(date_vars), names_to = "var", values_to = "val") %>%
    filter(!is.na(val))
  if (nrow(date_long) == 0) {
    return(data)
  }

  anchors <- date_long %>%
    group_by(USUBJID, alias_name) %>%
    summarise(anchor = min(as.Date(val)), .groups = "drop") %>%
    inner_join(alias_seq_map, by = "alias_name")

  multi_usubjid <- anchors %>% count(USUBJID) %>% filter(n > 1) %>% pull(USUBJID)
  anchors <- anchors %>% filter(USUBJID %in% multi_usubjid)
  if (nrow(anchors) == 0) {
    return(data)
  }

  delta_table <- anchors %>%
    group_by(USUBJID) %>%
    arrange(sheet_seq, .by_group = TRUE) %>%
    mutate(new_anchor = sort(anchor)) %>%
    ungroup() %>%
    mutate(delta = as.numeric(new_anchor - anchor)) %>%
    filter(delta != 0) %>%
    select(USUBJID, alias_name, delta)
  if (nrow(delta_table) == 0) {
    return(data)
  }

  data <- data %>%
    left_join(delta_table, by = c("USUBJID", "alias_name"))

  row_upper_bound <- rep(today, nrow(data))
  if (!is.null(discon_lookup)) {
    row_discon <- discon_lookup[data[["USUBJID"]]]
    row_upper_bound <- pmin(row_upper_bound, row_discon, na.rm = TRUE)
  }
  # BRTHDTC(生年月日)は、明示的なref()参照の有無によらず常に守るべき生物学的な下限のため、
  # シフト後もこれより前にならないようにする(乳児コホート等ではBRTHDTCがregistration_start_dateより
  # 後になり得るため、registration_start_dateだけでは生年月日より前の日付になり得る)
  row_lower_bound_base <- rep(reg_start, nrow(data))
  if ("BRTHDTC" %in% colnames(data)) {
    row_lower_bound_base <- pmax(row_lower_bound_base, as.Date(data[["BRTHDTC"]]), na.rm = TRUE)
  }
  # RFSTDTC(症例登録日)は、明示的なref()参照(date_ref_bounds)を持たない行にのみ、
  # populate_date_fields/build_repeated_domain内の日付生成ループと同じくデフォルトの下限として
  # 適用する。以前はこのシフトでRFSTDTCが一切考慮されておらず、同一被験者が複数alias(シート)を
  # 持つ場合にシフトでRFSTDTCより前の日付になってしまうバグがあった(例: 維持療法シートの開始日が
  # 症例登録日より前になる)。同じcdisc_variable名を複数のalias(シート)が定義しており、一部の
  # aliasだけが明示的な参照を持つ場合があるため(例: ECSTDTCはerwaspチェーンでは参照を持つが
  # maintenance6mpでは持たない)、cdisc_variable名単位ではなく、resolve_date_ref_bound_vals()で
  # 行(alias)単位に判定する
  has_rfstdtc <- "RFSTDTC" %in% colnames(data)

  for (var_name in date_vars) {
    has_val <- !is.na(data[[var_name]]) & !is.na(data[["delta"]])
    if (!any(has_val)) next
    row_lower_bound <- row_lower_bound_base
    if (has_rfstdtc) {
      min_ref_vals <- if (!is.null(date_ref_bounds)) resolve_date_ref_bound_vals(data, date_ref_bounds, var_name, "min_date") else NULL
      no_explicit_ref <- if (is.null(min_ref_vals)) rep(TRUE, nrow(data)) else is.na(min_ref_vals)
      rfstdtc_vals <- as.Date(data[["RFSTDTC"]])
      row_lower_bound[no_explicit_ref] <- pmax(row_lower_bound[no_explicit_ref], rfstdtc_vals[no_explicit_ref], na.rm = TRUE)
    }
    new_dates <- as.Date(data[[var_name]][has_val]) + data[["delta"]][has_val]
    new_dates <- pmin(pmax(new_dates, row_lower_bound[has_val]), row_upper_bound[has_val])
    data[[var_name]][has_val] <- as.character(new_dates)
  }

  data %>% select(-delta)
}

# other_domainsのdate型項目は登録開始日〜今日の範囲でランダムに生成されるが、被験者の中止日
# (DISCONDTC)は考慮しないため、中止後に検査等が発生しているように見えてしまうことがある
# (validate_other_domains.Rのno_records_after_discontinuationチェックで検出される)。
# 中止日情報がある被験者については、登録開始日〜中止日の範囲に収まるよう日付を再生成することで
# この矛盾を解消する。中止日情報が無い(NAまたはdiscontinuation_dateに無い)被験者は対象外
# (今まで通り登録開始日〜今日の範囲のまま)。registration_start_date > 中止日の場合(通常は
# 起こらないはずだが念のため)は中止日そのものにする
clamp_dates_to_discontinuation <- function(data, date_vars, registration_start_date, discontinuation_date, date_ref_bounds = NULL, existing_data = NULL, presence_conditions = NULL) {
  if (is.null(discontinuation_date) || nrow(discontinuation_date) == 0 || !("USUBJID" %in% colnames(data))) {
    return(data)
  }
  date_vars <- intersect(date_vars, colnames(data))
  if (length(date_vars) == 0) {
    return(data)
  }

  discon_map <- discontinuation_date %>% filter(!is.na(DISCONDTC)) %>% distinct(USUBJID, .keep_all = TRUE)
  discon_lookup <- set_names(as.Date(discon_map[["DISCONDTC"]]), discon_map[["USUBJID"]])
  reg_start <- as.Date(registration_start_date)

  # date_vars同士がvalidate_date_after_or_equal_to/validate_date_before_or_equal_to(他フィールド参照)で
  # 依存し合う場合(例: ECENDTCがECSTDTC以降)、ここで独立に再サンプルすると関係が崩れてしまう。
  # populate_date_fields/build_repeated_domain等と同じ理由で、参照先が先に処理されるよう並べ替える
  ordered_date_vars <- date_vars
  if (!is.null(date_ref_bounds) && length(date_vars) > 1) {
    date_deps <- date_ref_bounds %>% filter(cdisc_variable %in% date_vars, ref_cdisc_variable %in% date_vars)
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

  has_alias_name <- "alias_name" %in% colnames(data)
  has_label <- has_alias_name && "label" %in% colnames(data)
  # labelがある場合(build_repeated_domain由来)は(alias_name, label)単位、無い場合
  # (build_generic_domain由来)はalias_name単位をノードとして依存順序を組む。labelがある場合に
  # alias_name単位のままだと、同一alias内で別labelを参照するケース(例: BLASTLE(005)がWBC(006)の
  # 値を参照)を見分けられず、両方が同じclamp_rows()呼び出しの中で独立に再サンプルされて
  # 値が食い違ってしまう
  node_key_for <- function(alias_name_val, label_val) {
    if (has_label) str_c(alias_name_val, "", label_val) else alias_name_val
  }

  for (var_name in ordered_date_vars) {
    # target_rows(このvar_nameを持つ行のうち、今回の対象)について、discon超過・参照関係違反
    # (min/max_ref。参照先の値はdataの"現在の"状態から都度計算するため、先に確定した値を反映できる)を
    # 判定し、該当行だけ新しい日付を再生成してdataに書き戻す
    clamp_rows <- function(data, target_rows) {
      if (!any(target_rows)) {
        return(data)
      }
      current <- as.Date(as.character(data[[var_name]]))
      discon <- discon_lookup[data[["USUBJID"]]]

      min_ref_vals <- NULL
      max_ref_vals <- NULL
      if (!is.null(date_ref_bounds)) {
        min_ref_vals <- resolve_date_ref_bound_vals(data, date_ref_bounds, var_name, "min_date", existing_data)
        max_ref_vals <- resolve_date_ref_bound_vals(data, date_ref_bounds, var_name, "max_date", existing_data)
      }

      # discon超過に加えて、同一行内の他日付フィールド(先に処理済み)との参照関係(ECSTDTC<=ECENDTC等)が
      # 崩れている行も再サンプル対象にする。参照先の値が先の反復で更新されている可能性があるため。
      discon_over <- !is.na(current) & !is.na(discon) & current > discon
      ref_violation <- rep(FALSE, length(current))
      if (!is.null(min_ref_vals)) {
        ref_violation <- ref_violation | (!is.na(current) & !is.na(min_ref_vals) & current < min_ref_vals)
      }
      if (!is.null(max_ref_vals)) {
        ref_violation <- ref_violation | (!is.na(current) & !is.na(max_ref_vals) & current > max_ref_vals)
      }
      over <- target_rows & (discon_over | ref_violation)
      if (!any(over)) {
        return(data)
      }

      lower <- rep(reg_start, sum(over))
      # RFSTDTC(症例登録日)は、この変数のこの行に明示的なmin_date参照(min_ref_vals)が無い場合の
      # デフォルト下限としてのみ使う。明示的な参照(例: MHSTDTCのBRTHDTC基準)がある行にまで
      # 一律にRFSTDTCを下限に加えると、その変数本来の(RFSTDTCより前を許容する)意味を壊してしまうため。
      # 同じcdisc_variable名を複数のalias(シート)が共有し、一部のaliasだけが明示的な参照を持つ場合が
      # あるため(例: ECSTDTCはerwaspチェーンでは参照を持つがmaintenance6mpでは持たない)、
      # min_ref_vals全体がNULLかどうかではなく、行ごとにNAかどうかで判定する(以前はmin_ref_vals
      # 全体がNULLの場合しかRFSTDTCを適用しておらず、参照を持たない他aliasの行に一切RFSTDTCが
      # 適用されず、再クランプで登録日より前の日付が生成されてしまうバグがあった)
      if ("RFSTDTC" %in% colnames(data)) {
        no_explicit_ref <- if (is.null(min_ref_vals)) rep(TRUE, sum(over)) else is.na(min_ref_vals[over])
        rfstdtc_vals_over <- as.Date(as.character(data[["RFSTDTC"]][over]))
        lower[no_explicit_ref] <- pmax(lower[no_explicit_ref], rfstdtc_vals_over[no_explicit_ref], na.rm = TRUE)
      }
      # BRTHDTC(生年月日)は、明示的なref()参照の有無によらず常に守るべき生物学的な下限のため、
      # RFSTDTCと異なり全行に適用する(build_repeated_domain内の日付生成ループと同じ理由)
      if ("BRTHDTC" %in% colnames(data)) {
        lower <- pmax(lower, as.Date(as.character(data[["BRTHDTC"]][over])), na.rm = TRUE)
      }
      if (!is.null(min_ref_vals)) {
        lower <- pmax(lower, min_ref_vals[over], na.rm = TRUE)
      }
      # discon(中止日)が無い被験者は中止日による上限は課さず、参照先の日付関係のみを尊重する
      discon_over_vals <- discon[over]
      # hard_upper: 実際に許容できる上限(中止日、無ければ今日)。max_ref_valsがあればさらに絞る。
      # 通常の再クランプでは(discon無し時)upperの初期値をcurrent(置き換え前の既存値)にしており、
      # ref_violationがmin側で発生した行はcurrent<lower(=min_ref_vals)が前提のため、upper<lowerは
      # 「今日/中止日を超えて本当に有効な範囲が無い」ことを意味しない(単に既存値を置き換えようと
      # しているだけ)。真に有効な範囲が無いかどうかはhard_upperとlowerで判定する
      hard_upper <- as.Date(ifelse(is.na(discon_over_vals), as.character(rep(Sys.Date(), sum(over))), as.character(pmax(discon_over_vals, reg_start))))
      if (!is.null(max_ref_vals)) {
        hard_upper <- pmin(hard_upper, max_ref_vals[over], na.rm = TRUE)
      }
      # min_ref_vals(date_ref_boundsの明示的なmin_date参照)が実際に効いてlowerを押し上げている行
      # だけ「有効な範囲が無い」と判定する。RFSTDTC/BRTHDTCのデフォルト下限だけでhard_upperを超える
      # 場合(例: 登録日が中止日より後という、この日付項目固有の参照とは無関係な実データ上の事情)は
      # 対象にせず、従来通りhard_upperまで切り詰める
      has_min_ref <- if (!is.null(min_ref_vals)) !is.na(min_ref_vals[over]) else rep(FALSE, sum(over))
      # 有効な日付範囲が存在しない行(オフセット付き参照の参照先が生成順序上まだ結合されておらず、
      # 生成時点では下限が緩く見えていたが、後段でクロスドメイン参照が解決されて厳しい下限が判明し、
      # それが今日/中止日を超えてしまった)は、無理に未来日等で上書きせず未入力(NA)に戻す。
      # presenceの起点となっている同じ行の他フィールド(例: FAORRES)があれば、そちらもNAにして
      # 連鎖的に後段のapply_presence_conditions()で下位項目も未入力扱いになるようにする
      infeasible <- (hard_upper < lower) & has_min_ref
      if (any(infeasible)) {
        infeasible_mask <- over
        infeasible_mask[over] <- infeasible
        data[[var_name]][infeasible_mask] <- NA_character_
        if (!is.null(presence_conditions)) {
          drivers <- presence_conditions %>%
            filter(cdisc_variable == var_name, condition_type == "not_blank", !is.na(ref_cdisc_variable))
          if (nrow(drivers) > 0) {
            for (j in seq_len(nrow(drivers))) {
              drv <- drivers[j, ]
              row_mask <- infeasible_mask
              if ("alias_name" %in% colnames(drv) && !is.na(drv[["alias_name"]]) && "alias_name" %in% colnames(data)) {
                row_mask <- row_mask & (data[["alias_name"]] == drv[["alias_name"]])
              }
              if ("label" %in% colnames(drv) && !is.na(drv[["label"]]) && "label" %in% colnames(data)) {
                row_mask <- row_mask & (data[["label"]] == drv[["label"]])
              }
              if (drv[["ref_cdisc_variable"]] %in% colnames(data)) {
                data[[drv[["ref_cdisc_variable"]]]][row_mask] <- NA
              }
            }
          }
        }
        lower <- lower[!infeasible]
        hard_upper <- hard_upper[!infeasible]
        discon_over_vals <- discon_over_vals[!infeasible]
        over <- over & !infeasible_mask
      }
      if (length(lower) > 0) {
        # 通常のケース(有効な範囲は存在する): 上限はhard_upperを超えない範囲で、可能な限り既存値
        # (current)を尊重する(discon超過のみが理由の場合、既存値に近い日付に再サンプルするため)
        upper <- as.Date(ifelse(is.na(discon_over_vals), as.character(current[over]), as.character(hard_upper)))
        upper <- pmin(upper, hard_upper)
        upper <- pmax(upper, lower)
        new_dates <- lower + floor(runif(length(lower), 0, as.numeric(upper - lower) + 1))
        data[[var_name]][over] <- as.character(new_dates)
      }
      data
    }

    # 同じcdisc_variable名を、別のノード(alias_name、labelがあれば(alias_name, label))が参照している
    # 場合(例: inductionのSVSTDTCがprephaseのSVSTDTCを参照する、あるいは同一alias内でBLASTLE(005)が
    # WBC(006)の値を参照する)、参照先を先に確定させてから参照元を判定しないと、同じ呼び出しの中で
    # 参照先だけが後から再生成されて関係が崩れてしまう。そのため、そのようなノード間の依存がある場合だけ、
    # 依存関係順(参照されている側が先)にノードごとに処理する。依存が無ければ従来通り全行まとめて処理する
    self_ref_edges <- if (!is.null(date_ref_bounds) && has_alias_name) {
      date_ref_bounds %>%
        filter(
          cdisc_variable == var_name, ref_cdisc_variable == var_name,
          !is.na(alias_name), !is.na(ref_alias_name),
          if (has_label) !(alias_name == ref_alias_name & label == ref_label) else alias_name != ref_alias_name
        ) %>%
        transmute(
          from_key = node_key_for(alias_name, label),
          to_key = node_key_for(ref_alias_name, ref_label)
        ) %>%
        filter(from_key != to_key) %>%
        distinct(from_key, to_key)
    } else {
      tibble(from_key = character(0), to_key = character(0))
    }

    if (nrow(self_ref_edges) == 0) {
      data <- clamp_rows(data, rep(TRUE, nrow(data)))
    } else {
      all_keys <- unique(node_key_for(data[["alias_name"]], if (has_label) data[["label"]] else NA))
      ordered_keys <- character(0)
      remaining <- all_keys
      while (length(remaining) > 0) {
        unresolved <- self_ref_edges %>% filter(to_key %in% remaining) %>% pull(from_key) %>% unique()
        ready <- setdiff(remaining, unresolved)
        if (length(ready) == 0) {
          ordered_keys <- c(ordered_keys, remaining)
          break
        }
        ordered_keys <- c(ordered_keys, ready)
        remaining <- setdiff(remaining, ready)
      }
      data_keys <- node_key_for(data[["alias_name"]], if (has_label) data[["label"]] else NA)
      for (key in ordered_keys) {
        data <- clamp_rows(data, data_keys == key)
      }
    }
  }
  data
}

# 1つのalias_name内で、同じcdisc_variable名(例: ECSTDTC/ECENDTC)がlabel(繰り返しの1回分、例: 投与1回目・2回目...)
# ごとに複数回登場し、「同じlabel内での開始日<=終了日」と「次のlabelの開始日>=前のlabelの終了日」のような
# label内参照とlabelを跨ぐ参照が交互に連なるケース(例: test4のEC複数回投与)向けの日付生成。
# date_ref_boundsのうちこのalias_nameかつchain_varsに関する行(label内・label跨ぎの両方)から
# (label, cdisc_variable)をノードとする依存グラフを作り、トポロジカル順に1ノードずつ値を確定させていく。
# build_repeated_domain()の通常の列単位生成(同じ行=同じlabelの参照しか扱えない)を、
# このalias_nameのchain_varsに関してだけ上書きする形で使う
regenerate_date_chain <- function(data, alias_name_val, date_ref_bounds, chain_vars, registration_start_date, discon_lookup = NULL) {
  reg_start <- as.Date(registration_start_date)
  bounds <- date_ref_bounds %>%
    filter(alias_name == alias_name_val, cdisc_variable %in% chain_vars, !is.na(label), !is.na(ref_label))
  if (nrow(bounds) == 0) {
    return(data)
  }

  nodes <- bind_rows(
    bounds %>% distinct(label, cdisc_variable),
    bounds %>% distinct(label = ref_label, cdisc_variable = ref_cdisc_variable)
  ) %>% distinct()

  node_key <- function(label_vec, var_vec) str_c(label_vec, "::", var_vec)
  all_keys <- node_key(nodes$label, nodes$cdisc_variable)
  edge_from <- node_key(bounds$label, bounds$cdisc_variable)
  edge_to <- node_key(bounds$ref_label, bounds$ref_cdisc_variable)

  remaining <- all_keys
  ordered_keys <- character(0)
  while (length(remaining) > 0) {
    unresolved <- edge_from[edge_to %in% remaining]
    ready <- setdiff(remaining, unresolved)
    if (length(ready) == 0) {
      ordered_keys <- c(ordered_keys, remaining)
      break
    }
    ordered_keys <- c(ordered_keys, ready)
    remaining <- setdiff(remaining, ready)
  }
  ordered_nodes <- nodes[match(ordered_keys, all_keys), ]

  for (i in seq_len(nrow(ordered_nodes))) {
    label_val <- ordered_nodes$label[i]
    var_name <- ordered_nodes$cdisc_variable[i]
    if (!(var_name %in% colnames(data))) next
    mask <- data[["alias_name"]] == alias_name_val & data[["label"]] == label_val
    if (!any(mask)) next

    row_bounds <- bounds %>% filter(label == label_val, cdisc_variable == var_name)
    min_row <- row_bounds %>% filter(bound_type == "min_date") %>% slice(1)
    max_row <- row_bounds %>% filter(bound_type == "max_date") %>% slice(1)

    ref_value_for <- function(ref_label_val, ref_var) {
      if (is.na(ref_label_val) || is.na(ref_var) || !(ref_var %in% colnames(data))) {
        return(rep(NA_real_, sum(mask)))
      }
      ref_rows <- data %>%
        filter(.data[["alias_name"]] == alias_name_val, .data[["label"]] == ref_label_val) %>%
        transmute(USUBJID, ref_val = as.Date(as.character(.data[[ref_var]])))
      joined <- data[mask, "USUBJID", drop = FALSE] %>% left_join(ref_rows, by = "USUBJID")
      as.numeric(joined[["ref_val"]])
    }

    lower <- rep(as.numeric(reg_start), sum(mask))
    # RFSTDTC(症例登録日)は、この変数にchain内の明示的なmin_date参照(min_row)が無い場合の
    # デフォルト下限としてのみ使う(他の変数に明示的な参照がある場合にまで一律に加えると、
    # その変数本来の意味を壊してしまうため)
    if (nrow(min_row) == 0 && "RFSTDTC" %in% colnames(data)) {
      lower <- pmax(lower, as.numeric(as.Date(as.character(data[["RFSTDTC"]][mask]))), na.rm = TRUE)
    }
    if (nrow(min_row) > 0) {
      # ref('sheet_alias', N)+N.days/-N.daysの符号付き日数オフセット(offset_days。無指定ならNA=0)
      min_offset <- coalesce(min_row[["offset_days"]][1], 0)
      lower <- pmax(lower, ref_value_for(min_row[["ref_label"]], min_row[["ref_cdisc_variable"]]) + min_offset, na.rm = TRUE)
    }
    # BRTHDTC(生年月日)は、明示的なref()参照の有無によらず常に守るべき生物学的な下限のため、
    # RFSTDTCと異なり全行に適用する(build_repeated_domain内の日付生成ループと同じ理由)。
    # このchainに含まれるノード(例: WBCのように自分自身は他alias参照でchain対象外だが、
    # 同一alias内の他labelから参照されているためregenerate_date_chain側でも再生成される変数)も、
    # ここで再生成される際にBRTHDTCより前にならないようにする
    if ("BRTHDTC" %in% colnames(data)) {
      lower <- pmax(lower, as.numeric(as.Date(as.character(data[["BRTHDTC"]][mask]))), na.rm = TRUE)
    }
    upper <- rep(as.numeric(Sys.Date()), sum(mask))
    if (!is.null(discon_lookup)) {
      discon_vals <- as.numeric(discon_lookup[data[["USUBJID"]][mask]])
      upper <- ifelse(is.na(discon_vals), upper, pmin(upper, discon_vals))
    }
    if (nrow(max_row) > 0) {
      max_offset <- coalesce(max_row[["offset_days"]][1], 0)
      upper <- pmin(upper, ref_value_for(max_row[["ref_label"]], max_row[["ref_cdisc_variable"]]) + max_offset, na.rm = TRUE)
    }
    upper <- pmax(upper, lower)
    new_dates <- as.Date(floor(runif(sum(mask), lower, upper + 1)), origin = "1970-01-01")
    data[[var_name]][mask] <- as.character(new_dates)
  }
  data
}

# built_domains$DM(既に生成済みのDM)のRFSTDTCをUSUBJID単位で結合する。既にRFSTDTC列がある場合
# (自分自身がDMの場合など)やDMがまだ無い場合は何もしない。明示的なref()参照を持たない日付項目でも、
# populate_date_fields()のデフォルト下限(registration_start_date)ではなく被験者本人の登録日を
# 下限にできるようにするため(戻り値のinjectedで、呼び出し側が最終出力から取り除く列に加えられるようにする)
inject_dm_rfstdtc <- function(data, built_domains) {
  if ("RFSTDTC" %in% colnames(data)) {
    return(list(data = data, injected = FALSE))
  }
  dm <- built_domains[["DM"]]
  if (is.null(dm) || !all(c("USUBJID", "RFSTDTC") %in% colnames(dm))) {
    return(list(data = data, injected = FALSE))
  }
  dm_rfstdtc <- dm %>% select(USUBJID, RFSTDTC) %>% distinct(USUBJID, .keep_all = TRUE)
  list(data = data %>% left_join(dm_rfstdtc, by = "USUBJID"), injected = TRUE)
}

# DM/AE/DSのような個別ロジックを持たないドメイン向けの汎用生成。
# alias_nameがmulti_record_alias_namesに該当しない場合はUSUBJIDごとに1レコード、
# 該当する場合(AE報告のように被験者ごとに複数件記録されうるシート)はAEドメインと同様、
# 被験者に対してランダムな件数(0件を含む)のレコードを作る。
# radio_button/date/ダミーの共通パターンで項目を埋め、prefixSEQ(例: CMSEQ)をデータセット全体の通番として、
# prefixSPID(例: CMSPID)にalias_name(該当する場合はUSUBJID×alias_name内の連番付き)を付与する
# existing_data/finalizeは、依存関係の循環(prefix単位では循環に見えるがシート単位では循環でない
# 参照チェーン)のためにこのprefixを複数wave(alias_nameの集合)に分けてビルドする場合に使う
# (build_other_domains()のwave分割ロジックから渡される)。existing_dataは前waveまでに確定済みの、
# このprefixの行(alias_name列を保持したまま)。finalize=FALSE の場合はDSSEQ相当の連番付与や
# 列順整理などprefix全体に対して1回だけ行うべき処理をスキップし、alias_name列を保持したまま
# (existing_dataと結合した)全行を返す。通常(wave分割しない場合)は両方とも既定値のままでよく、
# 挙動は従来と完全に同じになる
build_generic_domain <- function(dm, spec, prefix, registration_start_date, meddra, presence_conditions, required_var_instances = NULL, numeric_bounds = NULL, field_ref_bounds = NULL, add_coding_block = FALSE, built_domains = list(), cdisc_variable_to_prefix = NULL, age_bounds = NULL, multi_record_alias_names = character(0), who_drug_idf = NULL, active_sheet_table = NULL, visit_lookup = NULL, discontinuation_date = NULL, date_ref_bounds = NULL, is_exclusive = FALSE, existing_data = NULL, finalize = TRUE) {
  # presence_conditions/field_ref_bounds/age_bounds/date_ref_boundsは全ドメイン分を含む共通テーブルのため、
  # 同じref_cdisc_variableを別ドメインが別のlabelで参照しているとinject_cross_domain_refs()が混同してしまう。
  # このドメイン自身のcdisc_variableに関する行だけに絞ってから使う
  presence_conditions <- presence_conditions %>% filter(cdisc_variable %in% spec[["cdisc_variable"]])
  if (!is.null(field_ref_bounds)) {
    field_ref_bounds <- field_ref_bounds %>% filter(cdisc_variable %in% spec[["cdisc_variable"]])
  }
  if (!is.null(age_bounds)) {
    age_bounds <- age_bounds %>% filter(cdisc_variable %in% spec[["cdisc_variable"]])
  }
  if (!is.null(date_ref_bounds)) {
    date_ref_bounds <- date_ref_bounds %>% filter(cdisc_variable %in% spec[["cdisc_variable"]])
  }

  alias_names <- spec[["alias_name"]] %>% unique()
  single_alias_names <- setdiff(alias_names, multi_record_alias_names)
  multi_alias_names <- intersect(alias_names, multi_record_alias_names)

  # active_sheet_table(USUBJID, alias_name)が指定されている場合、被験者ごとに実際に有効な
  # (=そのシートが表示される)alias_nameだけを対象にする。指定が無い場合は全alias_nameを対象にする(従来通り)。
  # 同じcdisc_variableを複数のalias_nameが定義している場合、is_exclusive==TRUEなら
  # resolve_preferred_alias_name()で(その被験者について)候補から1つだけ選ぶ(例: DD。discon/withdrawalの
  # どちらか一方にしか本当の死因が記録されないような、真に排他的な事象を表すドメイン向け)。
  # is_exclusive==FALSE(既定)の場合は、有効な(USUBJID, alias_name)の組み合わせごとに1行を作る
  # (例: SV/PR/PC。被験者が実際に複数のalias_name(治療フェーズ等)を経過することがあり、
  # そのどれもが独立して正しいレコードであるドメイン向け。互いに競合させず全て残す)
  single_rows <- if (length(single_alias_names) > 0) {
    candidates <- if (!is.null(active_sheet_table)) {
      active_sheet_table %>% filter(alias_name %in% single_alias_names)
    } else {
      tidyr::crossing(USUBJID = dm[["USUBJID"]], alias_name = single_alias_names)
    }
    if (is_exclusive) {
      resolve_preferred_alias_name(candidates, presence_conditions, built_domains, cdisc_variable_to_prefix)
    } else {
      candidates
    }
  } else {
    tibble(USUBJID = character(0), alias_name = character(0))
  }

  multi_rows <- if (length(multi_alias_names) > 0) {
    multi_alias_names %>%
      map_dfr(function(an) {
        eligible_usubjids <- if (!is.null(active_sheet_table)) {
          active_sheet_table %>% filter(alias_name == an) %>% pull(USUBJID) %>% unique()
        } else {
          dm[["USUBJID"]]
        }
        if (length(eligible_usubjids) == 0) {
          return(tibble(USUBJID = character(0), alias_name = character(0)))
        }
        tibble(USUBJID = sample(eligible_usubjids, size = length(eligible_usubjids), replace = TRUE), alias_name = an)
      })
  } else {
    tibble(USUBJID = character(0), alias_name = character(0))
  }

  data <- bind_rows(single_rows, multi_rows) %>%
    left_join(dm %>% select(USUBJID, STUDYID), by = "USUBJID")
  data[["DOMAIN"]] <- prefix

  spid_var <- str_c(prefix, "SPID")
  data[[spid_var]] <- data[["alias_name"]]
  data <- data %>% apply_multi_record_spid(spid_var, multi_record_alias_names)

  target_vars <- compute_target_vars(data %>% select(-alias_name), spec)
  seq_var <- str_c(prefix, "SEQ")

  date_vars <- spec %>% filter(field_type == "date") %>% pull(cdisc_variable) %>% unique() %>% intersect(target_vars)

  # date_ref_boundsが他ドメインの日付列を参照する場合、populate_date_fields()より前に
  # built_domainsから該当列を結合しておく(そうしないと生成時点でref_cdisc_variableが
  # colnames(data)に無く、下限/上限制約が適用されないまま日付が生成されてしまう)
  date_injected <- inject_cross_domain_refs(data, NULL, NULL, built_domains, cdisc_variable_to_prefix, NULL, date_ref_bounds, own_prefix = prefix)
  data <- date_injected[["data"]]
  date_injected_cols <- date_injected[["injected_cols"]]

  # 明示的なref()参照を持たない日付項目は、下のpopulate_date_fields()内でregistration_start_date
  # (試験共通の定数)を下限にしてしまう。RFSTDTCを結合しておくことで、被験者本人の登録日を
  # デフォルトの下限にできるようにする
  rfstdtc_injected <- inject_dm_rfstdtc(data, built_domains)
  data <- rfstdtc_injected[["data"]]
  if (rfstdtc_injected[["injected"]]) date_injected_cols <- c(date_injected_cols, "RFSTDTC")

  data <- data %>%
    populate_radio_button_fields(spec, target_vars, numeric_bounds) %>%
    populate_date_fields(spec, target_vars, registration_start_date, date_ref_bounds, existing_data) %>%
    clamp_dates_to_discontinuation(date_vars, registration_start_date, discontinuation_date, date_ref_bounds, existing_data, presence_conditions) %>%
    # 同じcdisc_variableが複数alias(シート)にまたがる場合、シートの本来の並び順(sheet_seq)に沿うよう
    # alias単位でまとめて日付をシフトする。clampより後に行うことで、シフト結果を最終的な値として保つ
    # (この関数自体が被験者の中止日を上限にするため、clampが先に行った中止日調整と矛盾しない)
    reorder_dates_by_sheet_seq(date_vars, spec, registration_start_date, discontinuation_date, date_ref_bounds) %>%
    # reorder_dates_by_sheet_seqは同一alias内の複数labelをまとめて一律にシフトするため、
    # 他ドメイン参照(date_ref_bounds)の下限/上限が再び崩れる場合がある。ここでもう一度
    # clampして修復する(discon_over判定は既に満たされているはずなので実質ref_violationのみ効く)
    clamp_dates_to_discontinuation(date_vars, registration_start_date, discontinuation_date, date_ref_bounds, existing_data, presence_conditions) %>%
    populate_dose_fields(target_vars) %>%
    populate_dummy_fields(target_vars)

  # wave分割していない(existing_data無し)通常時は、従来通りここでDSSEQ相当の連番を振る。
  # wave分割時は、後段でexisting_data(前wave分)と結合してから、finalize=TRUEのタイミングで
  # まとめて振る(通しの連番にするため)
  if (is.null(existing_data)) {
    data <- data %>% add_seq(seq_var)
  }

  meddra_vars <- compute_meddra_vars(spec, target_vars)
  coding_cols <- character(0)
  if (length(meddra_vars) > 0) {
    meddra_sample <- sample_meddra_rows(meddra, nrow(data))
    data <- data %>% populate_meddra_fields(spec, meddra_vars, meddra, meddra_sample)
    if (add_coding_block) {
      data <- data %>% add_meddra_coding_block(meddra_sample, prefix)
      coding_cols <- meddra_coding_cols(prefix)
    }
  }

  # drug変数(field_type=="drug")には、who_drug_idfから薬剤名をサンプリングして格納する。
  # alias_nameでスコープを絞る(同じcdisc_variableが別alias_nameで固定値等の場合はそちらを変更しない)
  drug_vars <- compute_drug_vars(spec, target_vars)
  if (length(drug_vars) > 0 && !is.null(who_drug_idf)) {
    data <- data %>% populate_drug_fields(spec, drug_vars, who_drug_idf)
  }

  # presence_conditions/field_ref_bounds/age_boundsが他ドメインの変数を参照している場合、
  # built_domains(既に生成済みのドメイン)から値を結合してから条件を適用し、結合用に追加した列は最後に外す
  injected <- inject_cross_domain_refs(data, presence_conditions, field_ref_bounds, built_domains, cdisc_variable_to_prefix, age_bounds, date_ref_bounds, own_prefix = prefix)
  data <- injected[["data"]] %>%
    apply_presence_conditions(presence_conditions, cdisc_variable_to_prefix) %>%
    drop_all_blank_required_records(target_vars, required_var_instances, prefix) %>%
    apply_field_ref_bounds(spec, field_ref_bounds) %>%
    apply_age_date_bounds(age_bounds, registration_start_date) %>%
    select(-any_of(c(injected[["injected_cols"]], date_injected_cols)))

  # drug変数の値がwho_drug_idfの薬剤名(full_name_en)に完全一致する場合、prefixDECODに
  # generic_name_enを格納する(presence_conditions等で値が変わった後の最終状態を見る)。
  # 全て固定コード(default_value)で値が確定している場合は、一致確認する意味が無いのでDECOD列自体を作らない。
  # alias_nameはここまでで役目を終えるため、最後にまとめて落とす
  if (length(drug_vars) > 0 && !is.null(who_drug_idf) && drug_vars_need_decod(spec, drug_vars)) {
    data <- data %>% add_drug_decod(spec, drug_vars, who_drug_idf, prefix)
  }
  data <- data %>% add_visit_columns(visit_lookup)

  # wave分割時(existing_data指定あり)は、ここでこのwaveの新規行を前waveまでの確定済み行と結合する
  # (existing_dataがNULLなら何もしない=従来通り)。まだfinalizeでなければ、次waveのexisting_dataとして
  # 使えるようalias_name列を保持したまま返す
  data <- bind_rows(existing_data, data)
  if (!is.null(existing_data) && finalize) {
    data <- data %>% add_seq(seq_var)
  }
  if (!finalize) {
    return(data)
  }

  # alias_nameはここでは落とさない。finalize=TRUEはこのprefix自身の最後のwaveというだけで、
  # 他のleftover_prefix(例: SV)がこのprefix(例: RS)をより後のwaveでalias単位に参照する場合が
  # あるため、build_other_domains側の最終ステップ(built_domainsを返り値に変換する直前)で
  # 全prefixまとめて落とすまで保持しておく必要がある(実際に発生したバグ: RSが一度きりのwaveで
  # finalize=TRUEになりここでalias_nameを落としてしまい、後からRSをalias単位で参照する
  # SV(hdm等)のinject_cross_domain_refsが参照先aliasを絞り込めず、別aliasの値を誤って
  # 拾ってしまっていた)
  data %>%
    reorder_domain_columns(front_cols = c(domain_front_cols(prefix), meddra_vars, coding_cols))
}

# TRのように、同じcdisc_variableが同じalias_name内で複数のlabel(繰り返しフィールド)に対応するドメイン向け。
# USUBJID×(alias_name, label)の組み合わせごとに1レコード作り、各変数は自分のlabelに対応するspec行だけを見て
# 値を生成する(対応するlabelが無ければNAのまま)。radio_button/date/meddra/dummyの基本パターンに対応
# existing_data/finalizeの意味はbuild_generic_domain()と同じ(prefix単位では循環に見える依存関係を
# 複数waveに分けて解決するため。通常は既定値のままでよく、挙動は従来と完全に同じになる)
build_repeated_domain <- function(dm, spec, prefix, registration_start_date, meddra, presence_conditions, required_var_instances = NULL, add_coding_block = FALSE, built_domains = list(), cdisc_variable_to_prefix = NULL, age_bounds = NULL, multi_record_alias_names = character(0), who_drug_idf = NULL, active_sheet_table = NULL, visit_lookup = NULL, discontinuation_date = NULL, date_ref_bounds = NULL, existing_data = NULL, finalize = TRUE) {
  drug_names <- if (!is.null(who_drug_idf)) who_drug_idf[["full_name_en"]] %>% discard(is.na) %>% unique() else character(0)
  # presence_conditions/age_bounds/date_ref_boundsは全ドメイン分を含む共通テーブルのため、
  # 同じref_cdisc_variableを別ドメインが別のlabelで参照しているとinject_cross_domain_refs()が
  # 混同してしまう。このドメイン自身のcdisc_variableに関する行だけに絞ってから使う
  presence_conditions <- presence_conditions %>% filter(cdisc_variable %in% spec[["cdisc_variable"]])
  if (!is.null(age_bounds)) {
    age_bounds <- age_bounds %>% filter(cdisc_variable %in% spec[["cdisc_variable"]])
  }
  if (!is.null(date_ref_bounds)) {
    date_ref_bounds <- date_ref_bounds %>% filter(cdisc_variable %in% spec[["cdisc_variable"]])
  }

  repeat_units <- spec %>% distinct(alias_name, label) %>% filter(!is.na(label))

  # active_sheet_table(USUBJID, alias_name)が指定されている場合、被験者ごとに実際に有効な
  # (=そのシートが表示される)alias_nameのlabelだけを対象にする。指定が無い場合は全被験者×全labelを対象にする(従来通り)
  data <- if (!is.null(active_sheet_table)) {
    active_sheet_table %>%
      inner_join(repeat_units, by = "alias_name", relationship = "many-to-many") %>%
      left_join(dm %>% select(USUBJID, STUDYID), by = "USUBJID")
  } else {
    dm %>%
      select(USUBJID, STUDYID) %>%
      tidyr::crossing(repeat_units)
  }
  data[["DOMAIN"]] <- prefix

  spid_var <- str_c(prefix, "SPID")
  data[[spid_var]] <- data[["alias_name"]]
  data <- data %>% apply_multi_record_spid(spid_var, multi_record_alias_names)

  # date_ref_boundsが他ドメインの日付列を参照する場合、このあとの日付生成ループより前に
  # built_domainsから該当列を結合しておく(そうしないと生成時点でref_cdisc_variableが
  # colnames(data)に無く、下限/上限制約が適用されないまま日付が生成されてしまう)
  date_injected <- inject_cross_domain_refs(data, NULL, NULL, built_domains, cdisc_variable_to_prefix, NULL, date_ref_bounds, own_prefix = prefix)
  data <- date_injected[["data"]]
  date_injected_cols <- date_injected[["injected_cols"]]

  # 明示的なref()参照を持たない日付項目は、下の日付生成でregistration_start_date(試験共通の定数)を
  # 下限にしてしまう。RFSTDTCを結合しておくことで、被験者本人の登録日をデフォルトの下限にできるようにする
  rfstdtc_injected <- inject_dm_rfstdtc(data, built_domains)
  data <- rfstdtc_injected[["data"]]
  if (rfstdtc_injected[["injected"]]) date_injected_cols <- c(date_injected_cols, "RFSTDTC")

  # BRTHDTC(生年月日)より前の日付が生成されないよう、dmから直接結合しておく。乳児コホート等では
  # BRTHDTCがregistration_start_date/RFSTDTCより後になり得るため、明示的なref()参照の有無に
  # よらず常に適用すべき下限(生物学的制約)として扱う。追加した列は他のinjected_colsと同様、
  # 最後に取り除く。
  # BRTHDTCが既に列として存在する場合(直前のinject_cross_domain_refs()が、date_ref_boundsで
  # 特定のalias/labelだけを対象にBRTHDTCを部分的に結合済みのケース。例: FAのbaselineアリアス
  # のFADTCがBRTHDTCを下限参照している場合、そのaliasの行だけ埋まる)は、そのまま素通りすると
  # 他のalias(例: osteonecrosis1)の行がBRTHDTC=NAのまま残ってしまう。列自体は残しつつ、
  # 未充填(NA)の行だけUSUBJID単位で埋める
  if ("BRTHDTC" %in% colnames(dm)) {
    if (!("BRTHDTC" %in% colnames(data))) {
      data <- data %>% left_join(dm %>% select(USUBJID, BRTHDTC), by = "USUBJID")
      date_injected_cols <- c(date_injected_cols, "BRTHDTC")
    } else if (anyNA(data[["BRTHDTC"]])) {
      brthdtc_map <- set_names(dm[["BRTHDTC"]], dm[["USUBJID"]])
      missing_brthdtc <- is.na(data[["BRTHDTC"]])
      data[["BRTHDTC"]][missing_brthdtc] <- unname(brthdtc_map[data[["USUBJID"]][missing_brthdtc]])
    }
  }

  target_vars <- compute_target_vars(data %>% select(-alias_name, -label), spec)

  # date型の変数同士が、同じ行(同一alias_name×label)の中でvalidate_date_after_or_equal_to/
  # validate_date_before_or_equal_to(他フィールド参照)によって数珠つなぎに依存し合う場合
  # (例: field101がfield90を下限にし、field90がfield89を下限にする)、参照先が先に生成されて
  # いないと値を引けない。date_ref_bounds(このドメインのdate_vars同士の依存だけ)を使って
  # 依存が無いものから順に並べ替える(トポロジカルソート。循環参照があれば残りは元の順のまま追加する)
  date_vars <- spec %>% filter(field_type == "date") %>% pull(cdisc_variable) %>% unique() %>% intersect(target_vars)
  if (!is.null(date_ref_bounds) && length(date_vars) > 1) {
    date_deps <- date_ref_bounds %>% filter(cdisc_variable %in% date_vars, ref_cdisc_variable %in% date_vars)
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
    target_vars <- c(setdiff(target_vars, date_vars), sorted_date_vars)
  }

  # (alias_name, label)ごとにdplyr::filter()/which()で行を探すと「組み合わせ数×行数」のスキャンになり、
  # labelの種類が多いドメインで遅くなる。group_by()のハッシュ化されたグループ処理に任せることで、
  # スキャンを行わずに値を割り振る
  for (var_name in target_vars) {
    var_spec <- spec %>% filter(cdisc_variable == var_name)

    # var_nameにvalidate_date_after_or_equal_to/validate_date_before_or_equal_to(他フィールド参照)が
    # あり、かつ参照先が既に生成済み(このforループの前の反復で追加された列)なら、そのfield名(列名)を
    # 使う。無ければNAのままにし、mutate内では従来通りの一律の範囲で生成する。offset_days
    # (ref('sheet_alias', N)+150.days等の符号付き日数オフセット、無指定ならNA)も併せて取り出す
    date_min_bound_row <- if (!is.null(date_ref_bounds)) {
      date_ref_bounds %>%
        filter(cdisc_variable == var_name, bound_type == "min_date", ref_cdisc_variable %in% colnames(data)) %>%
        distinct(ref_cdisc_variable, offset_days)
    } else {
      tibble(ref_cdisc_variable = character(0), offset_days = numeric(0))
    }
    date_min_ref <- if (nrow(date_min_bound_row) > 0) date_min_bound_row[["ref_cdisc_variable"]][1] else NA_character_
    date_min_offset <- if (nrow(date_min_bound_row) > 0) coalesce(date_min_bound_row[["offset_days"]][1], 0) else 0
    date_max_bound_row <- if (!is.null(date_ref_bounds)) {
      date_ref_bounds %>%
        filter(cdisc_variable == var_name, bound_type == "max_date", ref_cdisc_variable %in% colnames(data)) %>%
        distinct(ref_cdisc_variable, offset_days)
    } else {
      tibble(ref_cdisc_variable = character(0), offset_days = numeric(0))
    }
    date_max_ref <- if (nrow(date_max_bound_row) > 0) date_max_bound_row[["ref_cdisc_variable"]][1] else NA_character_
    date_max_offset <- if (nrow(date_max_bound_row) > 0) coalesce(date_max_bound_row[["offset_days"]][1], 0) else 0

    lookup <- var_spec %>%
      group_by(alias_name, label) %>%
      summarise(
        field_type = first(field_type),
        default_value = first(default_value),
        codes = list(unique(ifelse(is.na(code), default_value, code))),
        is_invisible_any = any(is_invisible, na.rm = TRUE),
        is_required_any = any(is_required, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codes = pmap(list(codes, is_invisible_any, is_required_any), function(cs, inv, req) {
        if (!req && !inv) union(cs, "") else cs
      }))

    # case_when()は条件に関係なく全分岐のRHSを評価してしまい、labelが一致しないグループで
    # codes=NAのままsample()を呼んでエラーになるため、if/elseで短絡評価する
    data <- data %>%
      left_join(lookup, by = c("alias_name", "label")) %>%
      group_by(alias_name, label) %>%
      mutate(!!var_name := {
        ft <- field_type[1]
        nn <- n()
        if (is.na(ft)) {
          rep(NA_character_, nn)
        } else if (ft %in% c("radio_button", "check_box")) {
          cs <- codes[[1]]
          if (length(cs) == 0) {
            rep(NA_character_, nn)
          } else if (ft == "check_box") {
            sample_check_box_values_with_coverage(cs, nn)
          } else {
            sample_values_with_coverage(cs, nn)
          }
        } else if (ft == "date") {
          if (is.na(date_min_ref) && is.na(date_max_ref) && !("RFSTDTC" %in% colnames(data)) && !("BRTHDTC" %in% colnames(data))) {
            as.character(sample(seq(as.Date(registration_start_date), Sys.Date(), by = "day"), nn, replace = TRUE))
          } else {
            # RFSTDTC(症例登録日、明示的なref()参照が無い日付項目のデフォルト下限として
            # inject_dm_rfstdtc()で結合されている場合がある)は被験者ごとに異なるため、
            # 一律のseq()ではなく行ごとにrunif()で生成する。この変数にlabelを跨ぐ連鎖参照がある場合
            # (例: EC複数回投与の2回目以降)、行によっては参照先(前labelの値)が実際に存在するため、
            # そのような行ではRFSTDTCを加えず参照値のみ尊重する。参照値が無い行(連鎖の先頭等)だけ
            # RFSTDTCを補う
            # ref('sheet_alias', N)+N.days/-N.daysの符号付き日数オフセット(date_min_offset/
            # date_max_offset。無指定なら0)を参照先の値に加味してから下限/上限として使う
            ref_vals <- if (!is.na(date_min_ref)) as.Date(.data[[date_min_ref]]) + date_min_offset else rep(as.Date(NA), nn)
            lower <- pmax(rep(as.Date(registration_start_date), nn), ref_vals, na.rm = TRUE)
            if ("RFSTDTC" %in% colnames(data)) {
              no_explicit_ref <- is.na(ref_vals)
              rfstdtc_vals <- as.Date(.data[["RFSTDTC"]])
              lower[no_explicit_ref] <- pmax(lower[no_explicit_ref], rfstdtc_vals[no_explicit_ref], na.rm = TRUE)
            }
            # BRTHDTC(生年月日)は、明示的なref()参照の有無によらず常に守るべき生物学的な下限のため、
            # RFSTDTCと異なり全行に適用する(乳児コホート等ではBRTHDTCがregistration_start_date/RFSTDTCより
            # 後になり得るため、それらのデフォルト下限だけでは生年月日より前の日付が生成されてしまう)
            if ("BRTHDTC" %in% colnames(data)) {
              brthdtc_vals <- as.Date(.data[["BRTHDTC"]])
              lower <- pmax(lower, brthdtc_vals, na.rm = TRUE)
            }
            upper <- rep(Sys.Date(), nn)
            if (!is.na(date_max_ref)) upper <- pmin(upper, as.Date(.data[[date_max_ref]]) + date_max_offset, na.rm = TRUE)
            upper <- pmax(upper, lower)
            as.character(as.Date(floor(runif(nn, as.numeric(lower), as.numeric(upper) + 1)), origin = "1970-01-01"))
          }
        } else if (ft == "meddra") {
          dv <- default_value[1]
          if (!is.na(dv) && str_detect(dv, "^[0-9]{8}$")) {
            llt_name <- meddra %>% filter(llt_code == dv) %>% pull(llt_name) %>% unique()
            rep(llt_name[1], nn)
          } else {
            sample_meddra_rows(meddra, nn)[["llt_name"]]
          }
        } else if (ft == "drug") {
          dv <- default_value[1]
          fixed_name <- if (!is.na(dv) && str_detect(dv, "^[0-9]+$")) {
            who_drug_idf %>% filter(drug_code == dv) %>% pull(full_name_en) %>% discard(is.na) %>% unique()
          } else {
            character(0)
          }
          if (length(fixed_name) >= 1) {
            rep(fixed_name[1], nn)
          } else if (length(drug_names) > 0) {
            sample(drug_names, nn, replace = TRUE)
          } else {
            rep(NA_character_, nn)
          }
        } else if (str_detect(var_name, "DOSE$")) {
          sample(dose_value_choices, nn, replace = TRUE)
        } else {
          rep("DUMMY", nn)
        }
      }) %>%
      ungroup() %>%
      select(-field_type, -default_value, -codes, -is_invisible_any, -is_required_any)
  }

  # オフセット付き参照(例: ref('sct1',16)+150.days)等により、下限が上限(今日)を超えてしまい、
  # 上の生成で未来日のまま埋められてしまった日付項目(=有効な日付が実在しない)を未入力に戻す。
  # あわせて、この変数のpresenceを「同じ行の他フィールドが値を持つこと」で条件づけている
  # presence_conditions(not_blank条件、例: "ORRES.present?"→この日付項目の下限参照元)があれば、
  # そのref_cdisc_variable(presenceの起点、例: FAORRES)も同じ行でNAにする。これにより後段の
  # apply_presence_conditions()で連鎖的に下位項目(GRADE等)も正しく未入力扱いになる
  {
    date_vars_for_future_check <- spec %>% filter(field_type == "date") %>% pull(cdisc_variable) %>% unique() %>% intersect(colnames(data))
    for (v in date_vars_for_future_check) {
      future_mask <- !is.na(data[[v]]) & as.Date(data[[v]]) > Sys.Date()
      if (!any(future_mask)) next
      data[[v]][future_mask] <- NA_character_
      drivers <- presence_conditions %>%
        filter(cdisc_variable == v, condition_type == "not_blank", !is.na(ref_cdisc_variable))
      if (nrow(drivers) > 0) {
        for (j in seq_len(nrow(drivers))) {
          drv <- drivers[j, ]
          row_mask <- future_mask
          if ("alias_name" %in% colnames(drv) && !is.na(drv[["alias_name"]]) && "alias_name" %in% colnames(data)) {
            row_mask <- row_mask & (data[["alias_name"]] == drv[["alias_name"]])
          }
          if ("label" %in% colnames(drv) && !is.na(drv[["label"]]) && "label" %in% colnames(data)) {
            row_mask <- row_mask & (data[["label"]] == drv[["label"]])
          }
          if (drv[["ref_cdisc_variable"]] %in% colnames(data)) {
            data[[drv[["ref_cdisc_variable"]]]][row_mask] <- NA
          }
        }
      }
    }
  }

  date_vars <- spec %>% filter(field_type == "date") %>% pull(cdisc_variable) %>% unique() %>% intersect(target_vars)

  # 1つのalias内でcdisc_variable名がlabel(繰り返しの1回分)を跨いで連鎖する行(例: 次回投与の開始日が
  # 前回投与の終了日を参照する)がある場合、通常の列単位生成ではlabelを跨いだ参照を扱えないため、
  # regenerate_date_chain()でそのalias・その変数だけ生成し直す。それ以外の変数はclamp_dates_to_discontinuationに任せる。
  # ref_cdisc_variableも自ドメインのdate_vars内にある場合のみ対象にする(regenerate_date_chain()は
  # 同じdataフレーム内にref_labelの行がある前提のため、ref_cdisc_variableが他ドメインの変数
  # (例: RSDTCがCMSTDTCを参照)の場合はref_labelの行が存在せず機能しない。他ドメイン参照は
  # inject_cross_domain_refs()で既にref_cdisc_variable列自体がdataに結合済みなので、通常の
  # 列単位生成・下のclamp_dates_to_discontinuationのref_violation判定に任せればよい)。
  # alias_name == ref_alias_nameも必須にする(同一ドメイン内で別alias(シート)を参照するケース、
  # 例: evaluationtp1のLBDTCがinductionlabのLBDTCを参照、はlabelが一致しないだけでこのフィルタに
  # 誤って引っかかっていた。regenerate_date_chain()は同じaliasのlabel行しか見ないため参照先が
  # 見つからずref無視のまま生成され、下限が緩すぎる日付が生成されてしまうバグがあった)
  chain_bounds <- if (!is.null(date_ref_bounds)) {
    date_ref_bounds %>% filter(cdisc_variable %in% date_vars, ref_cdisc_variable %in% date_vars, !is.na(label), !is.na(ref_label), label != ref_label, alias_name == ref_alias_name)
  } else {
    date_ref_bounds
  }
  if (!is.null(chain_bounds) && nrow(chain_bounds) > 0) {
    chain_vars <- union(chain_bounds[["cdisc_variable"]], chain_bounds[["ref_cdisc_variable"]]) %>% intersect(date_vars)
    discon_lookup <- NULL
    if (!is.null(discontinuation_date) && nrow(discontinuation_date) > 0 && "USUBJID" %in% colnames(data)) {
      discon_map <- discontinuation_date %>% filter(!is.na(DISCONDTC)) %>% distinct(USUBJID, .keep_all = TRUE)
      discon_lookup <- set_names(as.Date(discon_map[["DISCONDTC"]]), discon_map[["USUBJID"]])
    }
    for (alias_val in unique(chain_bounds[["alias_name"]])) {
      # date_ref_bounds(このドメイン全体分)ではなくchain_boundsを渡す。regenerate_date_chain内で
      # cdisc_variable単独でしか絞り込んでいないと、alias_name==aliasNameValかつcdisc_variableが
      # chain_varsに含まれるが実際は他ドメイン参照(ref_cdisc_variableがchain_vars外、例:
      # WBCのLBDTCがSVSTDTCを参照)の行まで拾ってしまい、そのref先がこのalias内に存在しないため
      # 値を解決できないままminRow相当が非NULLになり、本来効くはずのRFSTDTC等のデフォルト下限が
      # 適用されなくなる(実際に発生したバグ: WBC/BLASTLEの等号制約が崩れた)
      data <- regenerate_date_chain(data, alias_val, chain_bounds, chain_vars, registration_start_date, discon_lookup)
    }
  }
  # clamp_dates_to_discontinuation()はself_ref_edges(labelがあれば(alias_name, label)単位)で依存順に
  # 処理するため、labelを跨ぐ連鎖(regenerate_date_chain()で既に処理済みの同一alias内のものも含む)を
  # そのまま渡してよい。中止日超過などでchain regen後に値が再サンプルされる場合でも、
  # 参照元・参照先の順序が正しく守られる
  data <- clamp_dates_to_discontinuation(data, date_vars, registration_start_date, discontinuation_date, date_ref_bounds, existing_data, presence_conditions)
  # 同じcdisc_variableが複数alias(シート)にまたがる場合(例: 来院ごとに繰り返すEC/LB/VS)、
  # シートの本来の並び順(sheet_seq)に沿うようalias単位でまとめて日付をシフトする。
  # alias内の関係(同じ行の開始日<=終了日、labelを跨ぐ連鎖)は保ったまま動くため、上の
  # regenerate_date_chain()・clampより後に行う(この関数自体が中止日を上限にするため矛盾しない)
  data <- reorder_dates_by_sheet_seq(data, date_vars, spec, registration_start_date, discontinuation_date, date_ref_bounds)
  # reorder_dates_by_sheet_seqは同一alias内の複数labelをまとめて一律にシフトするため、
  # 参照関係(date_ref_bounds)の下限/上限が再び崩れる場合がある。ここでもう一度
  # clampして修復する(discon_over判定は既に満たされているはずなので実質ref_violationのみ効く)
  data <- clamp_dates_to_discontinuation(data, date_vars, registration_start_date, discontinuation_date, date_ref_bounds, existing_data, presence_conditions)

  # meddra型の変数がある場合、コーディングブロック(LLT〜SOC)を追加する。
  # field_type=="meddra"に該当しない行(そのlabelにmeddra型の変数が無い行)は、
  # 対応するmeddra_varsの値がNAのままなのでコード列も自動的にNAになる
  coding_cols <- character(0)
  if (add_coding_block) {
    meddra_type_vars <- spec %>%
      filter(field_type == "meddra") %>%
      distinct(cdisc_variable) %>%
      pull(cdisc_variable) %>%
      intersect(colnames(data))
    if (length(meddra_type_vars) > 0) {
      representative_llt_name <- exec(coalesce, !!!as.list(data[meddra_type_vars]))
      llt_lookup <- meddra %>% distinct(llt_name, .keep_all = TRUE)
      meddra_sample <- tibble(llt_name = representative_llt_name) %>% left_join(llt_lookup, by = "llt_name")
      data <- data %>% add_meddra_coding_block(meddra_sample, prefix)
      coding_cols <- meddra_coding_cols(prefix)
    }
  }

  # presence_conditions/age_boundsが他ドメインの変数を参照している場合、built_domainsから値を結合してから適用し、
  # 結合用に追加した列は最後に外す(alias_name/labelが揃っている場合はそれも突き合わせキーに使う)
  injected <- inject_cross_domain_refs(data, presence_conditions, NULL, built_domains, cdisc_variable_to_prefix, age_bounds, date_ref_bounds, own_prefix = prefix)
  data <- injected[["data"]] %>%
    apply_presence_conditions(presence_conditions, cdisc_variable_to_prefix) %>%
    drop_all_blank_required_records(target_vars, required_var_instances, prefix) %>%
    apply_age_date_bounds(age_bounds, registration_start_date) %>%
    select(-any_of(c(injected[["injected_cols"]], date_injected_cols)))

  # drug変数の値がwho_drug_idfの薬剤名(full_name_en)に完全一致する場合、prefixDECODに
  # generic_name_enを格納する(presence_conditions等で値が変わった後の最終状態を見る)。
  # 全て固定コード(default_value)で値が確定している場合は、一致確認する意味が無いのでDECOD列自体を作らない
  drug_vars <- spec %>% filter(field_type == "drug") %>% distinct(cdisc_variable) %>% pull(cdisc_variable) %>% intersect(colnames(data))
  if (length(drug_vars) > 0 && !is.null(who_drug_idf) && drug_vars_need_decod(spec, drug_vars)) {
    data <- data %>% add_drug_decod(spec, drug_vars, who_drug_idf, prefix)
  }

  # alias_name/labelはここでは落とさない(他ドメインからの参照で突き合わせキーとして使うため)。
  # build_other_domains側で、返り値を作る最後の段階で取り除く
  data <- data %>% add_visit_columns(visit_lookup)

  # wave分割時(existing_data指定あり)は、ここでこのwaveの新規行を前waveまでの確定済み行と結合する
  # (existing_dataがNULLなら何もしない=従来通り)。まだfinalizeでなければ、次waveのexisting_dataとして
  # 使えるようそのまま返す(DSSEQ相当の連番はfinalizeのタイミングでまとめて振る)
  data <- bind_rows(existing_data, data)
  if (!finalize) {
    return(data)
  }

  data %>%
    add_seq(str_c(prefix, "SEQ")) %>%
    reorder_domain_columns(front_cols = c(domain_front_cols(prefix), coding_cols))
}

# dataの各行が持つalias_name(例: AEドメインの"ae"/"sae_report")について、exclude_prefix以外に
# 同じalias_nameでフィールドを定義しているprefix(例: FA)がある場合、そのフィールドを同じ行に
# 直接追加する(FieldItem的な意味で「同じフォーム上の別ブロック」を表す)。
# これにより、"ae"シートのようにAE報告と同一フォーム上にあるFA項目が同じ行(=同じ報告インスタンス)として
# 扱われ、ブロックをまたぐpresence_conditions(例: FAOBJがAELLTCDを参照)がドメインをまたぐ結合なしに
# 正しく判定できるようになる。戻り値のlinked_specは、実際に追加したprefix/alias_nameの一覧
# (呼び出し側で、二重生成を避けるための除外や、後でsplit_linked_domains()に分離する際に使う)
populate_linked_blocks <- function(data, cdisc_variable_values, exclude_prefix, registration_start_date, meddra, who_drug_idf = NULL, date_ref_bounds = NULL) {
  own_alias_names <- data[["alias_name"]] %>% unique()
  linked_spec <- cdisc_variable_values %>%
    filter(prefix != exclude_prefix, alias_name %in% own_alias_names)

  if (nrow(linked_spec) == 0) {
    return(list(data = data, linked_spec = linked_spec))
  }

  drug_names <- if (!is.null(who_drug_idf)) who_drug_idf[["full_name_en"]] %>% discard(is.na) %>% unique() else character(0)
  linked_vars <- linked_spec %>% distinct(cdisc_variable) %>% pull(cdisc_variable)

  if (!is.null(date_ref_bounds)) {
    date_ref_bounds <- date_ref_bounds %>% filter(cdisc_variable %in% linked_spec[["cdisc_variable"]])
  }
  # date型の変数同士が他フィールド参照で数珠つなぎに依存し合う場合(build_repeated_domain()と同じ理由)、
  # 参照先が先に生成されるよう並べ替える
  linked_date_vars <- linked_spec %>% filter(field_type == "date") %>% pull(cdisc_variable) %>% unique() %>% intersect(linked_vars)
  if (!is.null(date_ref_bounds) && length(linked_date_vars) > 1) {
    date_deps <- date_ref_bounds %>% filter(cdisc_variable %in% linked_date_vars, ref_cdisc_variable %in% linked_date_vars)
    sorted_date_vars <- character(0)
    remaining <- linked_date_vars
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
    linked_vars <- c(setdiff(linked_vars, linked_date_vars), sorted_date_vars)
  }

  for (var_name in linked_vars) {
    var_spec <- linked_spec %>% filter(cdisc_variable == var_name)

    date_min_bound_row <- if (!is.null(date_ref_bounds)) {
      date_ref_bounds %>%
        filter(cdisc_variable == var_name, bound_type == "min_date", ref_cdisc_variable %in% colnames(data)) %>%
        distinct(ref_cdisc_variable, offset_days)
    } else {
      tibble(ref_cdisc_variable = character(0), offset_days = numeric(0))
    }
    date_min_ref <- if (nrow(date_min_bound_row) > 0) date_min_bound_row[["ref_cdisc_variable"]][1] else NA_character_
    date_min_offset <- if (nrow(date_min_bound_row) > 0) coalesce(date_min_bound_row[["offset_days"]][1], 0) else 0
    date_max_bound_row <- if (!is.null(date_ref_bounds)) {
      date_ref_bounds %>%
        filter(cdisc_variable == var_name, bound_type == "max_date", ref_cdisc_variable %in% colnames(data)) %>%
        distinct(ref_cdisc_variable, offset_days)
    } else {
      tibble(ref_cdisc_variable = character(0), offset_days = numeric(0))
    }
    date_max_ref <- if (nrow(date_max_bound_row) > 0) date_max_bound_row[["ref_cdisc_variable"]][1] else NA_character_
    date_max_offset <- if (nrow(date_max_bound_row) > 0) coalesce(date_max_bound_row[["offset_days"]][1], 0) else 0

    lookup <- var_spec %>%
      group_by(alias_name) %>%
      summarise(
        field_type = first(field_type),
        default_value = first(default_value),
        codes = list(unique(ifelse(is.na(code), default_value, code))),
        is_invisible_any = any(is_invisible, na.rm = TRUE),
        is_required_any = any(is_required, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codes = pmap(list(codes, is_invisible_any, is_required_any), function(cs, inv, req) {
        if (!req && !inv) union(cs, "") else cs
      }))

    data <- data %>%
      left_join(lookup, by = "alias_name") %>%
      group_by(alias_name) %>%
      mutate(!!var_name := {
        ft <- field_type[1]
        nn <- n()
        if (is.na(ft)) {
          rep(NA_character_, nn)
        } else if (ft %in% c("radio_button", "check_box")) {
          cs <- codes[[1]]
          if (length(cs) == 0) {
            rep(NA_character_, nn)
          } else if (ft == "check_box") {
            sample_check_box_values_with_coverage(cs, nn)
          } else {
            sample_values_with_coverage(cs, nn)
          }
        } else if (ft == "date") {
          if (is.na(date_min_ref) && is.na(date_max_ref) && !("RFSTDTC" %in% colnames(data))) {
            as.character(sample(seq(as.Date(registration_start_date), Sys.Date(), by = "day"), nn, replace = TRUE))
          } else {
            # RFSTDTC(症例登録日、明示的なref()参照が無い日付項目のデフォルト下限として
            # inject_dm_rfstdtc()で結合されている場合がある)は被験者ごとに異なるため、
            # 一律のseq()ではなく行ごとにrunif()で生成する。この変数にlabelを跨ぐ連鎖参照がある場合
            # (例: EC複数回投与の2回目以降)、行によっては参照先(前labelの値)が実際に存在するため、
            # そのような行ではRFSTDTCを加えず参照値のみ尊重する。参照値が無い行(連鎖の先頭等)だけ
            # RFSTDTCを補う。ref('sheet_alias', N)+N.days/-N.daysの符号付き日数オフセット
            # (date_min_offset/date_max_offset。無指定なら0)を参照先の値に加味してから下限/上限として使う
            ref_vals <- if (!is.na(date_min_ref)) as.Date(.data[[date_min_ref]]) + date_min_offset else rep(as.Date(NA), nn)
            lower <- pmax(rep(as.Date(registration_start_date), nn), ref_vals, na.rm = TRUE)
            if ("RFSTDTC" %in% colnames(data)) {
              no_explicit_ref <- is.na(ref_vals)
              rfstdtc_vals <- as.Date(.data[["RFSTDTC"]])
              lower[no_explicit_ref] <- pmax(lower[no_explicit_ref], rfstdtc_vals[no_explicit_ref], na.rm = TRUE)
            }
            upper <- rep(Sys.Date(), nn)
            if (!is.na(date_max_ref)) upper <- pmin(upper, as.Date(.data[[date_max_ref]]) + date_max_offset, na.rm = TRUE)
            upper <- pmax(upper, lower)
            as.character(as.Date(floor(runif(nn, as.numeric(lower), as.numeric(upper) + 1)), origin = "1970-01-01"))
          }
        } else if (ft == "meddra") {
          dv <- default_value[1]
          if (!is.na(dv) && str_detect(dv, "^[0-9]{8}$")) {
            llt_name <- meddra %>% filter(llt_code == dv) %>% pull(llt_name) %>% unique()
            rep(llt_name[1], nn)
          } else {
            sample_meddra_rows(meddra, nn)[["llt_name"]]
          }
        } else if (ft == "drug") {
          dv <- default_value[1]
          fixed_name <- if (!is.na(dv) && str_detect(dv, "^[0-9]+$")) {
            who_drug_idf %>% filter(drug_code == dv) %>% pull(full_name_en) %>% discard(is.na) %>% unique()
          } else {
            character(0)
          }
          if (length(fixed_name) >= 1) {
            rep(fixed_name[1], nn)
          } else if (length(drug_names) > 0) {
            sample(drug_names, nn, replace = TRUE)
          } else {
            rep(NA_character_, nn)
          }
        } else if (str_detect(var_name, "DOSE$")) {
          sample(dose_value_choices, nn, replace = TRUE)
        } else {
          rep("DUMMY", nn)
        }
      }) %>%
      ungroup() %>%
      select(-field_type, -default_value, -codes, -is_invisible_any, -is_required_any)
  }

  list(data = data, linked_spec = linked_spec)
}

# populate_linked_blocks()で同じ行に追加した列を、prefixごとの別テーブルに分離する。
# source_spid_col(例: AESPID)の値をそのままprefixSPID(例: FASPID)として引き継ぐことで、
# どのAE報告インスタンスに対応するリンク行かが分かるようにする
split_linked_domains <- function(data, linked_spec, source_spid_col) {
  if (nrow(linked_spec) == 0) {
    return(list())
  }
  linked_alias_by_prefix <- linked_spec %>% distinct(prefix, alias_name)
  prefixes <- unique(linked_spec[["prefix"]])

  prefixes %>%
    set_names() %>%
    map(function(px) {
      px_alias_names <- linked_alias_by_prefix %>% filter(prefix == px) %>% pull(alias_name)
      px_vars <- linked_spec %>% filter(prefix == px) %>% distinct(cdisc_variable) %>% pull(cdisc_variable)
      spid_var <- str_c(px, "SPID")
      data %>%
        filter(alias_name %in% px_alias_names) %>%
        mutate(DOMAIN = px, !!spid_var := .data[[source_spid_col]]) %>%
        select(STUDYID, DOMAIN, USUBJID, alias_name, all_of(spid_var), any_of(px_vars))
    })
}

# 同じalias_name内で同じcdisc_variableが複数のlabelを持つ行が存在するかどうか(TR/LBなどの繰り返し項目判定)
has_repeated_labels <- function(spec) {
  label_counts <- spec %>%
    filter(!is.na(label)) %>%
    distinct(alias_name, cdisc_variable, label) %>%
    count(alias_name, cdisc_variable)
  nrow(label_counts) > 0 && any(label_counts[["n"]] > 1)
}

# cdisc_variable_valuesに含まれるprefixのうち、個別ロジックを持つドメイン(既定でDM/AE/DS)を除いた
# 全てについてbuild_generic_domain()を適用し、prefixをキーにした名前付きリストで返す。
# MedDRAコーディングブロック(LLT〜SOC)はcoding_block_prefixes(既定でMH)に該当するドメインのみ付与する。
# 同じalias_name内でcdisc_variableが複数labelを持つドメイン(TR/LBなど)は、
# ドメインを限定せず自動判定してbuild_repeated_domain()で生成する。
# repeated_prefixesは自動判定に加えて明示的に強制したい場合に使う。
# presence_conditions/field_ref_boundsがドメインをまたいで参照している場合(例: MHOCCURがRSORRESを参照)は、
# 参照先のprefixを先に生成してから参照元を生成するよう順序を並べ替え、既に生成済みのドメイン(built_domains、
# 引数built_domainsでDM/AE/DSなどを追加で渡せる)の値を結合してから条件判定する
build_other_domains <- function(dm, cdisc_variable_values, registration_start_date, meddra, presence_conditions, required_var_instances = NULL, numeric_bounds = NULL, field_ref_bounds = NULL,
                                 exclude_prefixes = c("DM", "AE", "DS"), coding_block_prefixes = c("MH"), repeated_prefixes = character(0), exclusive_prefixes = c("DD"), built_domains = list(), age_bounds = NULL, multi_record_alias_names = character(0), who_drug_idf = NULL, active_sheet_table = NULL, visit_lookup = NULL, discontinuation_date = NULL, date_ref_bounds = NULL, pre_built_domains = list(), pre_built_alias_names = list()) {
  prefixes <- setdiff(unique(cdisc_variable_values[["prefix"]]), exclude_prefixes)

  cdisc_variable_to_prefix <- build_cdisc_variable_to_prefix(cdisc_variable_values)
  edges <- build_cross_prefix_edges(presence_conditions, field_ref_bounds, cdisc_variable_to_prefix, age_bounds, date_ref_bounds)
  sort_result <- topo_sort_prefixes_with_leftover(prefixes, edges)
  ordered_prefixes <- sort_result[["ordered"]]
  leftover_prefixes <- sort_result[["remaining"]]

  build_one_prefix <- function(px, spec, existing_data = NULL, finalize = TRUE) {
    if (px %in% repeated_prefixes || has_repeated_labels(cdisc_variable_values %>% filter(prefix == px))) {
      build_repeated_domain(
        dm, spec, px, registration_start_date, meddra, presence_conditions, required_var_instances,
        add_coding_block = px %in% coding_block_prefixes,
        built_domains = built_domains, cdisc_variable_to_prefix = cdisc_variable_to_prefix, age_bounds = age_bounds,
        multi_record_alias_names = multi_record_alias_names, who_drug_idf = who_drug_idf, active_sheet_table = active_sheet_table,
        visit_lookup = visit_lookup, discontinuation_date = discontinuation_date, date_ref_bounds = date_ref_bounds,
        existing_data = existing_data, finalize = finalize
      )
    } else {
      build_generic_domain(
        dm, spec, px, registration_start_date, meddra, presence_conditions, required_var_instances, numeric_bounds, field_ref_bounds,
        add_coding_block = px %in% coding_block_prefixes,
        built_domains = built_domains, cdisc_variable_to_prefix = cdisc_variable_to_prefix, age_bounds = age_bounds,
        multi_record_alias_names = multi_record_alias_names, who_drug_idf = who_drug_idf, active_sheet_table = active_sheet_table,
        visit_lookup = visit_lookup, discontinuation_date = discontinuation_date, date_ref_bounds = date_ref_bounds,
        is_exclusive = px %in% exclusive_prefixes, existing_data = existing_data, finalize = finalize
      )
    }
  }

  # 循環に関与しないprefix(大多数)は、従来通り1回の呼び出しでビルドする(コードパス・挙動とも変更なし)。
  # pre_built_domains(呼び出し元がこの関数より前に一部のalias_nameだけ先行生成済みのprefix。例: AEの
  # AESTDTCが参照するMH(registration)ブロック。MHSTDTCが他ドメインに依存せず、AEより前に単独で
  # 生成できるため)が指定されている場合、そのalias_nameをspecから除いた上でexisting_data(=
  # pre_built_domains[[px]])に続けて残りのalias_nameを生成する(先行生成済みの値をそのまま使い、
  # 二重生成による値の食い違いを避ける)
  for (px in ordered_prefixes) {
    spec <- cdisc_variable_values %>% filter(prefix == px)
    if (px %in% names(pre_built_domains) && !is.null(pre_built_domains[[px]])) {
      spec <- spec %>% filter(!(alias_name %in% pre_built_alias_names[[px]]))
      built_domains[[px]] <- build_one_prefix(px, spec, existing_data = pre_built_domains[[px]], finalize = TRUE)
    } else {
      built_domains[[px]] <- build_one_prefix(px, spec)
    }
  }

  # 循環に関与するprefix(leftover_prefixes)は、prefix単位ではなくシート(alias_name)単位で
  # 依存関係を解決し、複数wave(alias_nameのまとまり)に分けてビルドする。実際には循環ではなく、
  # 単に同じprefix内の別シートを介した参照チェーンが、prefix単位の粗い依存判定では循環に
  # 見えていただけ、というケースがほとんど(例: SV(hr3fisrt)→RS→LB→FA→SV(prephase))
  if (length(leftover_prefixes) > 0) {
    leftover_values <- cdisc_variable_values %>% filter(prefix %in% leftover_prefixes)
    alias_nodes <- leftover_values %>% distinct(prefix, alias_name)
    alias_edges <- build_alias_level_edges(presence_conditions, field_ref_bounds, cdisc_variable_to_prefix, age_bounds, date_ref_bounds) %>%
      filter(from_prefix %in% leftover_prefixes, to_prefix %in% leftover_prefixes)
    ordered_pairs <- topo_sort_prefix_aliases(alias_nodes, alias_edges)

    if (nrow(ordered_pairs) > 0) {
      run_id <- cumsum(ordered_pairs[["prefix"]] != dplyr::lag(ordered_pairs[["prefix"]], default = ordered_pairs[["prefix"]][1]) | seq_len(nrow(ordered_pairs)) == 1)
      last_run_by_prefix <- tapply(run_id, ordered_pairs[["prefix"]], max)

      for (rid in unique(run_id)) {
        run_rows <- ordered_pairs[run_id == rid, ]
        px <- run_rows[["prefix"]][1]
        wave_alias_names <- unique(run_rows[["alias_name"]])
        spec <- leftover_values %>% filter(prefix == px, alias_name %in% wave_alias_names)
        is_last <- rid == last_run_by_prefix[[px]]
        built_domains[[px]] <- build_one_prefix(px, spec, existing_data = built_domains[[px]], finalize = is_last)
      }
    }
  }

  # built_domainsの中にはalias_name/label(他ドメイン参照の突き合わせキー)が残っている場合があるため、
  # 返り値を作る最後の段階でのみ取り除く
  built_domains[prefixes] %>% map(~ select(.x, -any_of(c("alias_name", "label"))))
}

# ae/sae_reportのように、AE報告と同じフォーム上の他prefixブロック(例: FA)は、
# 既にpopulate_ae_domain側で(AE報告と同じ行として)生成済みのため、
# build_other_domains側では二重生成しないよう該当のprefix/alias_nameをcdisc_variable_valuesから除外する
exclude_ae_linked_prefixes <- function(cdisc_variable_values, ae_linked_domains) {
  ae_linked_prefix_alias <- if (length(ae_linked_domains) > 0) {
    ae_linked_domains %>% imap_dfr(~ tibble(prefix = .y, alias_name = unique(.x[["alias_name"]])))
  } else {
    tibble(prefix = character(0), alias_name = character(0))
  }
  cdisc_variable_values %>% anti_join(ae_linked_prefix_alias, by = c("prefix", "alias_name"))
}

# AE報告と同じ行として生成したリンク先ブロック(例: FA)を、対応するドメインにマージする
merge_linked_domains <- function(other_domains, ae_linked_domains) {
  for (linked_prefix in names(ae_linked_domains)) {
    fragment <- ae_linked_domains[[linked_prefix]] %>% select(-alias_name)
    merged <- if (linked_prefix %in% names(other_domains)) {
      bind_rows(other_domains[[linked_prefix]], fragment)
    } else {
      fragment
    }
    other_domains[[linked_prefix]] <- merged %>%
      add_seq(str_c(linked_prefix, "SEQ")) %>%
      reorder_domain_columns(front_cols = domain_front_cols(linked_prefix))
  }
  other_domains
}

# other_domainsのうち存在するドメインにだけ、対応するORRES整形関数(populate_lb_orres等)を適用する
apply_orres_populators <- function(other_domains, populators) {
  for (domain_name in names(populators)) {
    if (domain_name %in% names(other_domains)) {
      other_domains[[domain_name]] <- populators[[domain_name]](other_domains[[domain_name]])
    }
  }
  other_domains
}

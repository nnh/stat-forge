library(tidyverse)

# field_items$validatorsを(alias_name, field_name, validator_type, validator_key, value)の縦持りtibbleにする。
# validatorsが無いsheet/field、キー構成が不定な場合(presenceのようにnamed list()のみのケースを含む)でもエラーにならない
build_validator_table_raw <- function(sheets) {
  sheets %>%
    map_dfr(function(sheet) {
      field_items <- sheet[["field_items"]]
      if (length(field_items) == 0) {
        return(tibble())
      }

      field_table <- field_items %>%
        map_dfr(function(field) {
          validators <- field[["validators"]]
          if (length(validators) == 0) {
            return(tibble())
          }

          validators %>%
            imap_dfr(function(rules, validator_type) {
              if (length(rules) == 0) {
                tibble(validator_type = validator_type, validator_key = NA_character_, value = NA_character_)
              } else {
                rules %>%
                  imap_dfr(~ tibble(
                    validator_type = validator_type,
                    validator_key = .y,
                    value = paste(unlist(.x), collapse = ", ")
                  ))
              }
            }) %>%
            mutate(field_name = field[["name"]], .before = 1)
        })

      if (nrow(field_table) == 0) {
        return(tibble())
      }
      field_table %>% mutate(alias_name = sheet[["alias_name"]], .before = 1)
    })
}

# validator_keyから、下限か上限かを判定する。date型は日付の以降/以前、numericality型は数値の以上/以下
classify_bound_type <- function(validator_type, validator_key) {
  case_when(
    validator_type == "date" & validator_key == "validate_date_after_or_equal_to" ~ "min_date",
    validator_type == "date" & validator_key == "validate_date_before_or_equal_to" ~ "max_date",
    validator_type == "numericality" & validator_key == "validate_numericality_greater_than_or_equal_to" ~ "min_value",
    validator_type == "numericality" & validator_key == "validate_numericality_less_than_or_equal_to" ~ "max_value",
    TRUE ~ NA_character_
  )
}

# validator_type=="numericality"の場合、valueが数値ならその数値を取り出す(数値でなければNA)
extract_numeric_value <- function(validator_type, value) {
  if_else(validator_type == "numericality", suppressWarnings(as.numeric(value)), NA_real_)
}

# valueに含まれる特殊な参照値を実際の値に解決する。今のところ date型の"Date.current"(今日の日付)のみ対応
resolve_validator_value <- function(validator_type, value) {
  if_else(validator_type == "date" & value == "Date.current", as.character(Sys.Date()), value)
}

# valueが"field3"のような同一シート内の別フィールド参照、"f3"のような省略形(fieldプレフィックス無し、
# EDC仕様上26箇所で使用)、または"f84 +1.day"のような日数オフセット付きの同一シート内参照
# (EDC仕様上25箇所で使用、いずれも"+1.day(s)"のみ)の場合、その参照先フィールド名を取り出す。
# 日数オフセット自体は下限として厳密には反映しない
# (populate_date_fields等はref_field自身の値をそのまま下限にする。+1日分だけ緩い下限になるが、
# 参照が完全に無視されるよりは実態に即しており、この差はcheck_date_after_var_before_today等の
# >=判定には影響しない)。参照先のfield_typeはdateである前提とする
extract_ref_field <- function(validator_type, value) {
  bare_field <- str_detect(value, "^field[0-9]+$")
  short_field <- str_detect(value, "^f[0-9]+$")
  offset_match <- str_match(value, "^f([0-9]+)\\s*\\+\\s*[0-9]+\\.days?$")
  case_when(
    validator_type != "date" ~ NA_character_,
    bare_field ~ value,
    short_field ~ str_c("field", str_sub(value, 2)),
    !is.na(offset_match[, 1]) ~ str_c("field", offset_match[, 2]),
    TRUE ~ NA_character_
  )
}

# valueが"ref('sheet_alias', N)"のような他シート参照の場合(date型バリデータの
# validate_date_after_or_equal_to/validate_date_before_or_equal_toで使われる形。presence/formula側の
# ref('sheet_alias', N)=='値'とは異なり、値の比較を伴わない単独のref()呼び出し)、参照先のシート
# (alias_name)とフィールド名を取り出す
# EDC仕様側の値に"ref('registration',12) "のような末尾スペースが付与されていることがあるため、
# 前後の空白を許容する(付けないと完全一致に失敗し、この参照が無かったものとして扱われてしまう)。
# "ref('maitenance',829)-28.days"のような日数オフセット付きの他シート参照(EDC仕様上使用例あり)にも
# 対応する。オフセット(符号+日数)はextract_date_cross_ref_offset_days()で取り出し、
# build_generation_constraints.Rのdate_ref_boundsに反映する(参照先フィールドの値にオフセットを
# 加減した値を下限/上限として使う)
date_cross_ref_pattern <- "^\\s*ref\\('([^']+)'\\s*,\\s*([0-9]+)\\)\\s*(?:([+-])\\s*([0-9]+)\\.days?)?\\s*$"

extract_date_cross_ref_alias <- function(validator_type, value) {
  m <- str_match(value, date_cross_ref_pattern)
  if_else(validator_type == "date" & !is.na(m[, 1]), m[, 2], NA_character_)
}

extract_date_cross_ref_field <- function(validator_type, value) {
  m <- str_match(value, date_cross_ref_pattern)
  if_else(validator_type == "date" & !is.na(m[, 1]), str_c("field", m[, 3]), NA_character_)
}

# ref('sheet_alias', N)+150.days / ref('sheet_alias', N)-28.daysの符号付き日数オフセットを
# 数値(例: 150, -28)で取り出す。オフセットが無い場合はNA
extract_date_cross_ref_offset_days <- function(validator_type, value) {
  m <- str_match(value, date_cross_ref_pattern)
  sign_val <- if_else(m[, 4] == "-", -1, 1)
  if_else(validator_type == "date" & !is.na(m[, 1]) & !is.na(m[, 5]), sign_val * as.numeric(m[, 5]), NA_real_)
}

# value(例: field2=='ADVERSE EVENT'、f4=='Y' || f4=='N'、field22==2 || field22=='5<='、field6=="Y")を
# "||"で分割し、全断片が同一フィールドに対する fieldN==値(または fN==値) の形であれば、
# フィールド名と値の一覧を返す。値は'X'/"X"のように引用符(シングル・ダブルどちらも)付きの場合と、
# 2 のように引用符無しの数値/文字列の場合の両方に対応する。
# 異なるフィールドが混ざる場合やパースできない断片があればNULL(未対応)
parse_presence_or_conditions <- function(value) {
  fragments <- value %>% str_split("\\|\\|") %>% pluck(1) %>% str_trim()
  m <- str_match(fragments, "^(?:field|f)([0-9]+)\\s*==\\s*(?:'([^']*)'|\"([^\"]*)\"|([^\\s]+))$")
  if (any(is.na(m[, 1]))) {
    return(NULL)
  }
  field_nums <- unique(m[, 2])
  if (length(field_nums) != 1) {
    return(NULL)
  }
  list(field = str_c("field", field_nums), values = coalesce(m[, 3], m[, 4], m[, 5]))
}

# validator_type=="presence" & validator_key=="validate_presence_if"の場合、
# 同一フィールドに対するOR条件から参照フィールド名を取り出す。この条件を満たす時のみ値を設定する
extract_presence_ref_field <- function(validator_type, validator_key, value) {
  is_target <- coalesce(validator_type == "presence" & validator_key == "validate_presence_if", FALSE)
  map2_chr(is_target, value, function(target, v) {
    if (!target) return(NA_character_)
    parsed <- parse_presence_or_conditions(v)
    if (is.null(parsed)) return(NA_character_)
    parsed[["field"]]
  })
}

# 上記と同じ条件式から、条件が真になるための期待値の一覧を","区切りで取り出す(例: 'Y, N')
extract_presence_ref_value <- function(validator_type, validator_key, value) {
  is_target <- coalesce(validator_type == "presence" & validator_key == "validate_presence_if", FALSE)
  map2_chr(is_target, value, function(target, v) {
    if (!target) return(NA_character_)
    parsed <- parse_presence_or_conditions(v)
    if (is.null(parsed)) return(NA_character_)
    str_c(parsed[["values"]], collapse = ", ")
  })
}

# value(例: STAT.blank?、ORRES.present?)が"接尾辞.blank?"/"接尾辞.present?"の形の場合、
# その接尾辞(例: STAT)を取り出す。これは"fieldN=='値'"とは別の書き方で、同じcdisc_sheet_configsブロック内で
# この接尾辞を持つフィールド(=同じprefixのcdisc_variable、例: FASTAT)が空白/非空白のときだけ値を設定する、という意味。
# validate_formula_ifは値の妥当性検証(フィールドに値がある場合にその値が満たすべき条件)であり、
# 提示可否(ゲーティング)の意味を持たないため、ここではvalidate_presence_ifのみを対象にする
# (validate_formula_ifの複雑な式から断片だけを誤って提示条件として抽出してしまうバグがあったため)
presence_predicate_pattern <- "^([A-Za-z_][A-Za-z0-9_]*)\\.(blank|present)\\?$"

extract_presence_predicate_suffix <- function(validator_key, value) {
  is_target <- coalesce(validator_key == "validate_presence_if", FALSE)
  m <- str_match(value, presence_predicate_pattern)
  if_else(is_target, m[, 2], NA_character_)
}

# 上記と同じ条件式から、blank(空白であること)かpresent(非空白であること)かを取り出す
extract_presence_predicate_type <- function(validator_key, value) {
  is_target <- coalesce(validator_key == "validate_presence_if", FALSE)
  m <- str_match(value, presence_predicate_pattern)
  if_else(is_target, m[, 3], NA_character_)
}

# validator_type=="formula" & validator_key=="validate_formula_if"の場合、
# value(例: f18<=3)が単一フィールドに対する条件のときだけ判定する(f18 -> field18)。
# age(f2, f3)>=20のような複数フィールドにまたがる式は対象外(NA)とする
formula_single_field_pattern <- "^f([0-9]+)\\s*(<=|>=|==|<|>)\\s*(-?[0-9]+(?:\\.[0-9]+)?)$"

extract_formula_ref_field <- function(validator_type, validator_key, value) {
  is_target <- validator_type == "formula" & validator_key == "validate_formula_if"
  m <- str_match(value, formula_single_field_pattern)
  ref_field <- if_else(!is.na(m[, 1]), str_c("field", m[, 2]), NA_character_)
  if_else(is_target, ref_field, NA_character_)
}

# 上記と同じ条件式から、演算子に応じて上限(max_value)/下限(min_value)を判定する
classify_formula_bound_type <- function(validator_type, validator_key, value) {
  is_target <- validator_type == "formula" & validator_key == "validate_formula_if"
  operator <- str_match(value, formula_single_field_pattern)[, 3]
  bound_type <- case_when(
    operator %in% c("<=", "<") ~ "max_value",
    operator %in% c(">=", ">") ~ "min_value",
    operator == "==" ~ "exact_value",
    TRUE ~ NA_character_
  )
  if_else(is_target, bound_type, NA_character_)
}

# 上記と同じ条件式から、比較対象の数値を取り出す
extract_formula_bound_value <- function(validator_type, validator_key, value) {
  is_target <- validator_type == "formula" & validator_key == "validate_formula_if"
  bound_value <- suppressWarnings(as.numeric(str_match(value, formula_single_field_pattern)[, 4]))
  if_else(is_target, bound_value, NA_real_)
}

# value(例: f350<=f59)が同一シート内の別フィールドとの比較のときだけ判定する(比較先: f59 -> field59)。
# age(f2, f3)>=20のような関数呼び出しを含む式は対象外(NA)とする
formula_field_ref_pattern <- "^f([0-9]+)\\s*(<=|>=|==|<|>)\\s*f([0-9]+)$"

# value(例: f350<=f59)から比較先フィールド名(field59)を取り出す
extract_formula_field_ref <- function(validator_type, validator_key, value) {
  is_target <- validator_type == "formula" & validator_key == "validate_formula_if"
  m <- str_match(value, formula_field_ref_pattern)
  ref_field <- if_else(!is.na(m[, 1]), str_c("field", m[, 4]), NA_character_)
  if_else(is_target, ref_field, NA_character_)
}

# 上記と同じ条件式から、演算子に応じて比較先フィールドが上限(max_value)/下限(min_value)かを判定する
classify_formula_field_ref_bound_type <- function(validator_type, validator_key, value) {
  is_target <- validator_type == "formula" & validator_key == "validate_formula_if"
  operator <- str_match(value, formula_field_ref_pattern)[, 3]
  bound_type <- case_when(
    operator %in% c("<=", "<") ~ "max_value",
    operator %in% c(">=", ">") ~ "min_value",
    operator == "==" ~ "exact_value",
    TRUE ~ NA_character_
  )
  if_else(is_target, bound_type, NA_character_)
}

# value(例: age(f2, f3)>=20 && age(f2, f3)<=80)を"&&"で分割し、全断片が同じ2フィールドに対する
# age(fN, fM)>=数値 / age(fN, fM)<=数値 の形であれば、2つのフィールド名と下限/上限(どちらか無くてもよい)を返す。
# 断片の解釈に失敗した場合や、2フィールドの組み合わせが断片間で一致しない場合はNULL(未対応)
age_condition_pattern <- "^age\\(\\s*f([0-9]+)\\s*,\\s*f([0-9]+)\\s*\\)\\s*(>=|<=)\\s*([0-9]+(?:\\.[0-9]+)?)$"

parse_age_condition <- function(value) {
  clauses <- value %>% str_split("&&") %>% pluck(1) %>% str_trim()
  m <- str_match(clauses, age_condition_pattern)
  if (any(is.na(m[, 1]))) {
    return(NULL)
  }
  field_pairs <- unique(str_c(m[, 2], "-", m[, 3]))
  if (length(field_pairs) != 1) {
    return(NULL)
  }
  min_age <- suppressWarnings(as.numeric(m[m[, 4] == ">=", 5]))
  max_age <- suppressWarnings(as.numeric(m[m[, 4] == "<=", 5]))
  list(
    field1 = str_c("field", m[1, 2]),
    field2 = str_c("field", m[1, 3]),
    min_age = if (length(min_age) > 0) min_age[1] else NA_real_,
    max_age = if (length(max_age) > 0) max_age[1] else NA_real_
  )
}

# age(ref('sheet1', N1), ref('sheet2', N2)) OP 閾値 のような、別シートの2つの日付フィールドの
# 年齢差でこのフィールド自身の提示可否をゲーティングする条件(validate_presence_if)を解釈する。
# 上記のage(fN, fM)(同一シート内、フィールド自身の値をage_boundsで直接束縛する用途)とは別に、
# ref('sheet', N)形式(別シート参照、フィールド自身とは無関係な2つの日付の年齢差で提示可否を
# ゲーティングする用途。例: FASTATがage(初発診断日, 生年月日)>39のときだけ提示される)を扱う。
# &&で他条件と組み合わさっている場合は非対応(NULL)
age_ref_condition_pattern <- "^\\(?\\s*age\\(\\s*ref\\('([^']+)'\\s*,\\s*([0-9]+)\\)\\s*,\\s*ref\\('([^']+)'\\s*,\\s*([0-9]+)\\)\\)\\s*(>=|<=|>|<)\\s*([0-9]+(?:\\.[0-9]+)?)\\s*\\)?$"

parse_age_ref_condition <- function(value) {
  m <- str_match(value, age_ref_condition_pattern)
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  list(
    ref1_alias_name = m[1, 2], ref1_field = str_c("field", m[1, 3]),
    ref2_alias_name = m[1, 4], ref2_field = str_c("field", m[1, 5]),
    operator = m[1, 6], threshold = as.numeric(m[1, 7])
  )
}

# age(ref('sheet1', N1), ref('sheet2', N2)) OP1 X || age(ref('sheet1', N1), ref('sheet2', N2)) OP2 Y
# のような、同じ2フィールドに対するage()比較を"||"で2つ組み合わせた条件(validate_presence_if。
# 「範囲外のときだけ必須」パターン、例: age(...)<18 || age(...)>=65 = 18歳未満または65歳以上のときだけ必須)を
# 解釈する。2つの断片が同じref1/ref2(alias_name+field番号)を参照しており、演算子が下限側(</<=)と
# 上限側(>/>=)の組み合わせ(順不同)である場合だけ対応する。それ以外(3つ以上への分割、参照先不一致、
# 演算子が同じ側同士等)はNULL(未対応)
parse_age_ref_or_condition <- function(value) {
  clauses <- value %>% str_split("\\|\\|") %>% pluck(1) %>% str_trim()
  if (length(clauses) != 2) {
    return(NULL)
  }
  m <- str_match(clauses, age_ref_condition_pattern)
  if (any(is.na(m[, 1]))) {
    return(NULL)
  }
  ref_pairs <- str_c(m[, 2], "-", m[, 3], "-", m[, 4], "-", m[, 5])
  if (length(unique(ref_pairs)) != 1) {
    return(NULL)
  }
  operators <- m[, 6]
  thresholds <- as.numeric(m[, 7])
  lower_idx <- which(operators %in% c("<", "<="))
  upper_idx <- which(operators %in% c(">", ">="))
  if (length(lower_idx) != 1 || length(upper_idx) != 1) {
    return(NULL)
  }
  list(
    ref1_alias_name = m[1, 2], ref1_field = str_c("field", m[1, 3]),
    ref2_alias_name = m[1, 4], ref2_field = str_c("field", m[1, 5]),
    min_age = thresholds[lower_idx], max_age = thresholds[upper_idx]
  )
}

# validator_type=="formula" & validator_key=="validate_formula_if"の場合、
# 上記のage()条件から、自分自身(field_name)以外のもう一方のフィールド(参照先の日付)と下限/上限年齢を取り出す。
# field_nameがage()の2引数のどちらとも一致しない場合はNULL(未対応)
extract_age_condition <- function(field_name, validator_type, validator_key, value) {
  is_target <- coalesce(validator_key == "validate_formula_if", FALSE)
  pmap_dfr(list(is_target, field_name, value), function(target, fn, v) {
    empty <- tibble(age_ref_field = NA_character_, min_age = NA_real_, max_age = NA_real_)
    if (!target) {
      return(empty)
    }
    parsed <- parse_age_condition(v)
    if (is.null(parsed)) {
      return(empty)
    }
    other_field <- if (parsed[["field1"]] == fn) {
      parsed[["field2"]]
    } else if (parsed[["field2"]] == fn) {
      parsed[["field1"]]
    } else {
      NA_character_
    }
    if (is.na(other_field)) {
      return(empty)
    }
    tibble(age_ref_field = other_field, min_age = parsed[["min_age"]], max_age = parsed[["max_age"]])
  })
}

# value(例: (STAT.blank?) && (ref('registration', 4)=='F'))を"&&"で分割し、各断片の
# 括弧を取り除いた文字列の一覧を返す(&&を含まない場合はNULL)
parse_and_clauses <- function(value) {
  if (!str_detect(value, "&&")) {
    return(NULL)
  }
  value %>%
    str_split("&&") %>%
    pluck(1) %>%
    str_trim() %>%
    str_remove("^\\(") %>%
    str_remove("\\)$") %>%
    str_trim()
}

# ref('sheet_alias', N)=='値' のような、field番号を明示して別シートを参照する条件式のパターン。
# 値側は 'X'/"X" のようにシングル・ダブルどちらの引用符付きにも対応し、引用符無しの場合は
# ||や&&・カッコを含まない単純なリテラルのみを対象とする
# (f6=='Y'&&(f7=='Y'||f8=='Y'||...)のような複合式の断片を誤って値として飲み込まないようにするため)
cross_ref_pattern <- "^ref\\('([^']+)'\\s*,\\s*([0-9]+)\\)\\s*==\\s*(?:'([^']*)'|\"([^\"]*)\"|([^\\s|&()]+))$"
# fieldN==値(または fN==値) の形の条件式のパターン(同一シート内の別フィールド参照)。値側の制約は上記と同様
and_field_ref_pattern <- "^(?:field|f)([0-9]+)\\s*==\\s*(?:'([^']*)'|\"([^\"]*)\"|([^\\s|&()]+))$"
# fieldN==fieldM(または fN==fM)のように、値側もフィールド参照の形。and_field_ref_patternは
# 値側を「引用符無しの単純リテラル」として扱うため、これを先に判定しておかないと
# "fN"という文字列そのものと一致するかのリテラル条件として誤解釈されてしまう
# (この形は「別フィールドの値をそのままコピーする」という意味で、build_generation_constraints.Rの
# extract_field_equality_ref()による別のcopy機構で扱われるため、ここでは何もしない扱いにする)
and_field_equality_pattern <- "^(?:field|f)([0-9]+)\\s*==\\s*(?:field|f)([0-9]+)$"
# fieldN>=数値(または fN>=数値)のように、同一シート内の別フィールドの値を数値として不等号比較する形。
# 例: "f16>=2&&STAT.blank?"(骨壊死のGrade(field16)が2以上のときだけ、かつSTATが空欄のときだけ提示)
and_field_numeric_cmp_pattern <- "^(?:field|f)([0-9]+)\\s*(>=|<=|>|<)\\s*(-?[0-9]+(?:\\.[0-9]+)?)$"

# parse_and_clauses()で分割した1断片を種類ごとに分類する。対応する断片:
#   - "STAT.blank?"/"STAT.present?"のような接尾辞述語 -> kind="predicate"
#   - "ref('sheet_alias', N)=='値'"のような別シート参照 -> kind="cross_ref"
#   - "fieldN==fieldM"のような、値側もフィールド参照のコピー条件 -> kind="field_equality_skip"
#     (別のcopy機構(extract_field_equality_ref)で扱われるため、ここではpresence_conditions行を作らない)
#   - "fieldN=='値'"のような同一シート内の別フィールド参照 -> kind="field_ref"
#   - "fieldN>=数値"のような、同一シート内の別フィールドの値との数値不等号比較 -> kind="field_numeric_cmp"
#   - "fieldN==2 || fieldN==3 || ..."のような、断片自体が同一フィールドに対するOR条件
#     (例: (field22==2||field22==3||...) && (field348=='CR'||field348=='PR'))
#     -> kind="field_ref_or"(parse_presence_or_conditions()を再利用し、複数のexpected_valueを持つ)
# どれにも一致しなければNULL(未対応)
classify_and_clause <- function(clause) {
  m_pred <- str_match(clause, presence_predicate_pattern)
  if (!is.na(m_pred[1, 1])) {
    return(list(kind = "predicate", suffix = m_pred[1, 2], predicate_type = m_pred[1, 3]))
  }
  m_ref <- str_match(clause, cross_ref_pattern)
  if (!is.na(m_ref[1, 1])) {
    return(list(
      kind = "cross_ref",
      ref_alias_name = m_ref[1, 2],
      ref_field = str_c("field", m_ref[1, 3]),
      value = coalesce(m_ref[1, 4], m_ref[1, 5], m_ref[1, 6])
    ))
  }
  m_eq <- str_match(clause, and_field_equality_pattern)
  if (!is.na(m_eq[1, 1])) {
    return(list(kind = "field_equality_skip"))
  }
  m_field <- str_match(clause, and_field_ref_pattern)
  if (!is.na(m_field[1, 1])) {
    return(list(kind = "field_ref", ref_field = str_c("field", m_field[1, 2]), value = coalesce(m_field[1, 3], m_field[1, 4], m_field[1, 5])))
  }
  m_num <- str_match(clause, and_field_numeric_cmp_pattern)
  if (!is.na(m_num[1, 1])) {
    return(list(kind = "field_numeric_cmp", ref_field = str_c("field", m_num[1, 2]), operator = m_num[1, 3], threshold = as.numeric(m_num[1, 4])))
  }
  or_parsed <- parse_presence_or_conditions(clause)
  if (!is.null(or_parsed)) {
    return(list(kind = "field_ref_or", ref_field = or_parsed[["field"]], values = or_parsed[["values"]]))
  }
  NULL
}

# value(例: (STAT.blank?) && (ref('registration', 4)=='F'))を"&&"で分割し、全断片が解釈できた場合、
# 断片ごとの分類結果(classify_and_clauseの返り値)のリストを返す。
# 1つでも解釈できない断片があればNULL(未対応)
parse_and_conditions <- function(value) {
  clauses <- parse_and_clauses(value)
  if (is.null(clauses)) {
    return(NULL)
  }
  parsed <- map(clauses, classify_and_clause)
  if (any(map_lgl(parsed, is.null))) {
    return(NULL)
  }
  parsed
}

# valueのどこかにref('sheet_alias', N)=='値'という断片が含まれていれば、その最初の1箇所を抽出する。
# STAT.blank? && ((ORRES.blank? && ...) || (...))のような、ref()以外の部分がどれだけ複雑(入れ子のOR/AND)でも
# 構造は解析せず、ref()部分の条件だけを取り出す簡易フォールバック用。見つからなければNULL
cross_ref_pattern_loose <- "ref\\('([^']+)'\\s*,\\s*([0-9]+)\\)\\s*==\\s*(?:'([^']*)'|\"([^\"]*)\"|([^\\s|&()]+))"

extract_cross_ref_clause <- function(value) {
  m <- str_match(value, cross_ref_pattern_loose)
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  list(ref_alias_name = m[1, 2], ref_field = str_c("field", m[1, 3]), value = coalesce(m[1, 4], m[1, 5], m[1, 6]))
}

# valueのどこかにfieldN==fieldM(または fN==fM)という、リテラル値を伴わない
# フィールド同士の等号比較が含まれていれば、その2つのフィールド名を取り出す(例: f2==f19 -> field2, field19)。
# 周囲がどれだけ複雑な式(OR/ANDの入れ子など)でも、この断片だけを取り出す簡易検出用。
# この関係は「片方がもう片方の値をそのまま使う(コピーする)べき」という意味で使われることが多い
field_equality_pattern_loose <- "(?:field|f)([0-9]+)\\s*==\\s*(?:field|f)([0-9]+)"

extract_field_equality_clause <- function(value) {
  m <- str_match(value, field_equality_pattern_loose)
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  list(field1 = str_c("field", m[1, 2]), field2 = str_c("field", m[1, 3]))
}

# 上記の断片から、field_name自身(このバリデーターが定義されているフィールド)ではない
# もう一方のフィールド名を取り出す。field_nameがどちらとも一致しない場合はNA
extract_field_equality_ref <- function(field_name, value) {
  clause <- extract_field_equality_clause(value)
  if (is.null(clause)) {
    return(NA_character_)
  }
  if (clause[["field1"]] == field_name) {
    return(clause[["field2"]])
  }
  if (clause[["field2"]] == field_name) {
    return(clause[["field1"]])
  }
  NA_character_
}

# sheetsからvalidator_tableを組み立て、resolved_value/bound_type/ref_field/numeric_value/
# presence_ref_field/presence_ref_valueまで付与した最終形を返す
build_validator_table <- function(sheets) {
  build_validator_table_raw(sheets) %>%
    mutate(
      resolved_value = resolve_validator_value(validator_type, value),
      bound_type = coalesce(
        classify_bound_type(validator_type, validator_key),
        classify_formula_bound_type(validator_type, validator_key, value),
        classify_formula_field_ref_bound_type(validator_type, validator_key, value)
      ),
      ref_field = coalesce(
        extract_ref_field(validator_type, value),
        extract_date_cross_ref_field(validator_type, value),
        extract_formula_ref_field(validator_type, validator_key, value),
        extract_formula_field_ref(validator_type, validator_key, value)
      ),
      date_ref_alias_name = extract_date_cross_ref_alias(validator_type, value),
      date_ref_offset_days = extract_date_cross_ref_offset_days(validator_type, value),
      numeric_value = coalesce(
        extract_numeric_value(validator_type, value),
        extract_formula_bound_value(validator_type, validator_key, value)
      ),
      presence_ref_field = extract_presence_ref_field(validator_type, validator_key, value),
      presence_ref_value = extract_presence_ref_value(validator_type, validator_key, value),
      presence_predicate_suffix = extract_presence_predicate_suffix(validator_key, value),
      presence_predicate_type = extract_presence_predicate_type(validator_key, value)
    ) %>%
    bind_cols(extract_age_condition(.[["field_name"]], .[["validator_type"]], .[["validator_key"]], .[["value"]]))
}

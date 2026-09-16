library(tidyverse)

# validator_table + df_cdiscから、ダミーデータ生成で使う制約テーブル一式を組み立てて返す:
# presence_conditions, required_vars, numeric_bounds, field_ref_bounds
build_generation_constraints <- function(validator_table, df_cdisc, field_reference_table = NULL) {
  field_to_cdisc_variable <- df_cdisc %>% distinct(alias_name, field, cdisc_variable)
  field_to_label <- df_cdisc %>% distinct(alias_name, field, label)
  field_to_prefix <- df_cdisc %>% distinct(alias_name, field, prefix)
  field_to_field_type <- df_cdisc %>% distinct(alias_name, field, field_type)

  # validate_presence_if(例: field22==2 || field22=='5<=')を、field名からcdisc_variable名に変換したうえで
  # (cdisc_variable, label, ref_cdisc_variable, ref_alias_name, ref_label, expected_value)のテーブルにする。
  # labelは、この条件が対象とするcdisc_variable自身が属するブロック(例: CMTRTが5つのlabelに
  # 繰り返し定義されている場合、そのうちどのlabelの条件か)を表す。同じcdisc_variable名を持つ
  # 複数のインスタンス(label違い)がそれぞれ別々の条件を持つ場合に、後段でインスタンスを取り違えないため
  # (例: CM/baselineのCMTRTが5つのlabelにあり、それぞれ別の閾値でゲーティングされているケース)。
  # ref_alias_name/ref_labelは参照先フィールド(例: field22)自身が属するブロックを指す。
  # RS(繰り返し項目)がSC(別labelの繰り返し項目)を参照するような場合、参照元自身のlabelではなく、
  # この固定されたref_labelのレコードを見る必要があるため。
  # 参照先(presence_ref_field)がfield_type=="meddra"の場合、値(例: 10052464)はLLT名ではなくLLTコードとの
  # 比較を意図しているため、ref_cdisc_variableをそのcdisc_variable(例: AETERM)ではなく、
  # MedDRAコーディングブロックのコード列(prefixLLTCD、例: AELLTCD)に差し替える
  presence_conditions <- validator_table %>%
    filter(!is.na(presence_ref_field)) %>%
    distinct(alias_name, field_name, presence_ref_field, presence_ref_value) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
    left_join(
      field_to_cdisc_variable %>% rename(ref_cdisc_variable = cdisc_variable),
      by = c("alias_name", "presence_ref_field" = "field")
    ) %>%
    left_join(
      field_to_label %>% rename(ref_label = label),
      by = c("alias_name", "presence_ref_field" = "field")
    ) %>%
    left_join(
      field_to_field_type %>% rename(ref_field_type = field_type),
      by = c("alias_name", "presence_ref_field" = "field")
    ) %>%
    left_join(
      field_to_prefix %>% rename(ref_prefix = prefix),
      by = c("alias_name", "presence_ref_field" = "field")
    ) %>%
    mutate(
      ref_cdisc_variable = if_else(
        coalesce(ref_field_type == "meddra", FALSE),
        str_c(ref_prefix, "LLTCD"),
        ref_cdisc_variable
      )
    ) %>%
    transmute(cdisc_variable, label, alias_name, ref_cdisc_variable, ref_alias_name = alias_name, ref_label, expected_value = presence_ref_value, condition_type = "equals") %>%
    filter(!is.na(cdisc_variable), !is.na(ref_cdisc_variable)) %>%
    separate_rows(expected_value, sep = ",\\s*")

  # validate_presence_if/validate_formula_if(例: STAT.blank?、ORRES.present?)を
  # "接尾辞.blank?"/"接尾辞.present?"形式として解釈する。同じcdisc_sheet_configsブロック(=同じlabel)内で
  # その接尾辞を持つcdisc_variable(例: FASTAT)が空白/非空白のときだけ値を設定する、という意味。
  # ref_labelはNAのままにし、参照元自身のlabel(同じブロック)で突き合わせる
  # blank -> ref_cdisc_variableが""と一致する場合のみ設定(condition_type="equals")
  # present -> ref_cdisc_variableが空白でない場合のみ設定(condition_type="not_blank")
  presence_predicate_conditions <- validator_table %>%
    filter(!is.na(presence_predicate_suffix)) %>%
    distinct(alias_name, field_name, presence_predicate_suffix, presence_predicate_type) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
    left_join(field_to_prefix, by = c("alias_name", "field_name" = "field")) %>%
    mutate(
      ref_cdisc_variable = str_c(prefix, presence_predicate_suffix),
      ref_alias_name = alias_name,
      ref_label = NA_character_,
      condition_type = if_else(presence_predicate_type == "blank", "equals", "not_blank"),
      expected_value = if_else(presence_predicate_type == "blank", "", NA_character_)
    ) %>%
    transmute(cdisc_variable, label, alias_name, ref_cdisc_variable, ref_alias_name, ref_label, expected_value, condition_type) %>%
    filter(!is.na(cdisc_variable), !is.na(ref_cdisc_variable))

  presence_conditions <- bind_rows(presence_conditions, presence_predicate_conditions)

  # validate_presence_ifで、"&&"により種類の異なる複数条件
  # (STAT.blank?のような述語、ref('sheet', N)=='値'のような別シート参照、fieldN=='値')が
  # 組み合わさっている場合、断片ごとに独立したpresence_conditions行に分解する。
  # AND条件は「いずれかの行が条件を満たさなければ値をNAにする」という既存の仕組みで表現できるため、
  # 断片数だけ行を作ればよい。age(fN,fM)>=X && age(fN,fM)<=Yはage_ref_fieldで別途処理済みのため除外する。
  # &&での分解に失敗する場合(ref()以外の部分がOR/ANDの入れ子など複雑な式)や、&&を伴わない
  # ref('sheet', N)=='値'単独の行は、ref()部分の条件だけを抽出する(それ以外の条件は無視される)。
  # validate_formula_ifは値の妥当性検証であり提示可否のゲーティングには使わない(実際に発生したバグ:
  # reinduction2のECADJで、値の妥当性を表す複雑なOR/AND式の中の一部の断片だけをゲーティング条件として
  # 誤抽出し、本来常に提示されるべき値が誤って空欄化されていた)
  ref_condition_rows <- validator_table %>%
    filter(
      validator_key == "validate_presence_if",
      is.na(age_ref_field),
      str_detect(value, "&&") | str_detect(value, "ref\\(")
    ) %>%
    distinct(alias_name, field_name, value) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
    filter(!is.na(cdisc_variable))

  # ref_alias_name/ref_fieldからcdisc_variableを引く。参照先がfield_type=="meddra"の場合、
  # 値(例: 10052464)はLLT名ではなくLLTコードとの比較を意図しているため、その参照先自身のprefixの
  # LLTCD列(例: AELLTCD)に差し替える(冒頭の平坦なOR専用presence_conditionsと同じルール)。
  # 見つからなければcharacter(0)を返す
  resolve_ref_cdisc_variable <- function(ref_alias_name, ref_field) {
    var <- field_to_cdisc_variable %>% filter(alias_name == ref_alias_name, field == ref_field) %>% pull(cdisc_variable) %>% unname()
    if (length(var) == 0) return(character(0))
    ref_type <- field_to_field_type %>% filter(alias_name == ref_alias_name, field == ref_field) %>% pull(field_type) %>% unname()
    ref_prefix <- field_to_prefix %>% filter(alias_name == ref_alias_name, field == ref_field) %>% pull(prefix) %>% unname()
    if (length(ref_type) > 0 && coalesce(ref_type[1] == "meddra", FALSE) && length(ref_prefix) > 0) {
      return(str_c(ref_prefix[1], "LLTCD"))
    }
    var[1]
  }

  and_presence_conditions <- ref_condition_rows %>%
    pmap_dfr(function(alias_name, field_name, value, cdisc_variable, label) {
      parsed <- parse_and_conditions(value)
      if (is.null(parsed)) {
        clause <- extract_cross_ref_clause(value)
        if (is.null(clause)) {
          return(tibble())
        }
        parsed <- list(list(kind = "cross_ref", ref_alias_name = clause[["ref_alias_name"]], ref_field = clause[["ref_field"]], value = clause[["value"]]))
      }

      parsed %>%
        map_dfr(function(clause) {
          if (clause[["kind"]] == "field_equality_skip") {
            # fieldN==fieldMは別のcopy機構(field_equality_copy_conditions、下記)で扱われるため、
            # ここではpresence_conditions行を作らない(&&の他の断片(OR条件等)の解析は妨げない)
            return(tibble())
          } else if (clause[["kind"]] == "predicate") {
            own_prefix <- field_to_prefix %>% filter(alias_name == .env$alias_name, field == .env$field_name) %>% pull(prefix) %>% unname()
            if (length(own_prefix) == 0) return(tibble())
            tibble(
              cdisc_variable = cdisc_variable,
              label = label,
              alias_name = alias_name,
              ref_cdisc_variable = str_c(own_prefix[1], clause[["suffix"]]),
              ref_alias_name = alias_name,
              ref_label = NA_character_,
              expected_value = if_else(clause[["predicate_type"]] == "blank", "", NA_character_),
              condition_type = if_else(clause[["predicate_type"]] == "blank", "equals", "not_blank")
            )
          } else if (clause[["kind"]] == "cross_ref") {
            ref_var <- resolve_ref_cdisc_variable(clause[["ref_alias_name"]], clause[["ref_field"]])
            if (length(ref_var) == 0) return(tibble())
            ref_lbl <- field_to_label %>% filter(alias_name == clause[["ref_alias_name"]], field == clause[["ref_field"]]) %>% pull(label) %>% unname()
            tibble(
              cdisc_variable = cdisc_variable,
              label = label,
              alias_name = alias_name,
              ref_cdisc_variable = ref_var[1],
              ref_alias_name = clause[["ref_alias_name"]],
              ref_label = if (length(ref_lbl) > 0) ref_lbl[1] else NA_character_,
              expected_value = clause[["value"]],
              condition_type = "equals"
            )
          } else if (clause[["kind"]] == "field_ref") {
            ref_var <- resolve_ref_cdisc_variable(alias_name, clause[["ref_field"]])
            if (length(ref_var) == 0 || ref_var[1] == cdisc_variable) return(tibble())
            ref_lbl <- field_to_label %>% filter(alias_name == .env$alias_name, field == clause[["ref_field"]]) %>% pull(label) %>% unname()
            tibble(
              cdisc_variable = cdisc_variable,
              label = label,
              alias_name = alias_name,
              ref_cdisc_variable = ref_var[1],
              ref_alias_name = alias_name,
              ref_label = if (length(ref_lbl) > 0) ref_lbl[1] else NA_character_,
              expected_value = clause[["value"]],
              condition_type = "equals"
            )
          } else if (clause[["kind"]] == "field_ref_or") {
            # 断片自体がOR条件(例: field22==2||field22==3||...)の場合、同じref_cdisc_variableに対する
            # 複数のexpected_value行を作る(apply_presence_conditions側でref_cdisc_variableごとに
            # グルーピングされ、値の集合に対するOR判定になる。異なるref_cdisc_variable同士はAND)
            ref_var <- resolve_ref_cdisc_variable(alias_name, clause[["ref_field"]])
            if (length(ref_var) == 0 || ref_var[1] == cdisc_variable) return(tibble())
            ref_lbl <- field_to_label %>% filter(alias_name == .env$alias_name, field == clause[["ref_field"]]) %>% pull(label) %>% unname()
            tibble(
              cdisc_variable = cdisc_variable,
              label = label,
              alias_name = alias_name,
              ref_cdisc_variable = ref_var[1],
              ref_alias_name = alias_name,
              ref_label = if (length(ref_lbl) > 0) ref_lbl[1] else NA_character_,
              expected_value = clause[["values"]],
              condition_type = "equals"
            )
          } else if (clause[["kind"]] == "field_numeric_cmp") {
            # fieldN>=数値のような、同一シート内の別フィールドの値との数値不等号比較(例:
            # "f16>=2"(骨壊死のGradeが2以上))。equals/not_blankと異なりref側の値を数値として
            # 閾値と比較する必要があるため、専用のcondition_type(numeric_ge/le/gt/lt)にする
            ref_var <- resolve_ref_cdisc_variable(alias_name, clause[["ref_field"]])
            if (length(ref_var) == 0 || ref_var[1] == cdisc_variable) return(tibble())
            ref_lbl <- field_to_label %>% filter(alias_name == .env$alias_name, field == clause[["ref_field"]]) %>% pull(label) %>% unname()
            numeric_condition_type <- case_when(
              clause[["operator"]] == ">=" ~ "numeric_ge",
              clause[["operator"]] == "<=" ~ "numeric_le",
              clause[["operator"]] == ">" ~ "numeric_gt",
              clause[["operator"]] == "<" ~ "numeric_lt",
              TRUE ~ NA_character_
            )
            if (is.na(numeric_condition_type)) return(tibble())
            tibble(
              cdisc_variable = cdisc_variable,
              label = label,
              alias_name = alias_name,
              ref_cdisc_variable = ref_var[1],
              ref_alias_name = alias_name,
              ref_label = if (length(ref_lbl) > 0) ref_lbl[1] else NA_character_,
              expected_value = as.character(clause[["threshold"]]),
              condition_type = numeric_condition_type
            )
          } else {
            tibble()
          }
        })
    })

  presence_conditions <- bind_rows(presence_conditions, and_presence_conditions)

  # age(ref('sheet1', N1), ref('sheet2', N2)) OP 閾値 の単独条件(validate_presence_if。例: FASTATが
  # age(初発診断日, 生年月日)>39のときだけ提示される)を、presence_conditions行
  # (condition_type="age_gt"/"age_ge"/"age_lt"/"age_le")として追加する。通常のequals/not_blankと
  # 異なり参照先が2つ(ref_cdisc_variable/ref2_cdisc_variable)あるため、apply_presence_conditions側で
  # 専用の年齢比較処理を行う
  age_ref_condition_rows <- validator_table %>%
    filter(validator_key == "validate_presence_if", is.na(age_ref_field), !is.na(value)) %>%
    distinct(alias_name, field_name, value) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
    filter(!is.na(cdisc_variable))

  age_ref_presence_conditions <- age_ref_condition_rows %>%
    pmap_dfr(function(alias_name, field_name, value, cdisc_variable, label) {
      parsed <- parse_age_ref_condition(value)
      if (is.null(parsed)) {
        return(tibble())
      }
      ref1_var <- resolve_ref_cdisc_variable(parsed[["ref1_alias_name"]], parsed[["ref1_field"]])
      ref2_var <- resolve_ref_cdisc_variable(parsed[["ref2_alias_name"]], parsed[["ref2_field"]])
      if (length(ref1_var) == 0 || length(ref2_var) == 0) {
        return(tibble())
      }
      condition_type <- case_when(
        parsed[["operator"]] == ">" ~ "age_gt",
        parsed[["operator"]] == ">=" ~ "age_ge",
        parsed[["operator"]] == "<" ~ "age_lt",
        parsed[["operator"]] == "<=" ~ "age_le",
        TRUE ~ NA_character_
      )
      if (is.na(condition_type)) {
        return(tibble())
      }
      ref1_lbl <- field_to_label %>% filter(alias_name == parsed[["ref1_alias_name"]], field == parsed[["ref1_field"]]) %>% pull(label) %>% unname()
      ref2_lbl <- field_to_label %>% filter(alias_name == parsed[["ref2_alias_name"]], field == parsed[["ref2_field"]]) %>% pull(label) %>% unname()
      tibble(
        cdisc_variable = cdisc_variable,
        label = label,
        alias_name = alias_name,
        ref_cdisc_variable = ref1_var[1],
        ref_alias_name = parsed[["ref1_alias_name"]],
        ref_label = if (length(ref1_lbl) > 0) ref1_lbl[1] else NA_character_,
        ref2_cdisc_variable = ref2_var[1],
        ref2_alias_name = parsed[["ref2_alias_name"]],
        ref2_label = if (length(ref2_lbl) > 0) ref2_lbl[1] else NA_character_,
        expected_value = as.character(parsed[["threshold"]]),
        condition_type = condition_type
      )
    })

  presence_conditions <- bind_rows(presence_conditions, age_ref_presence_conditions)

  # validator_type=="formula"の式(例: (f2==10052464||...)&&(f2==f19))に、リテラル値を伴わない
  # フィールド同士の等号比較(fN==fM)が含まれる場合、周囲がOR/ANDの入れ子で複雑でも、その部分だけを
  # 「このフィールドはもう一方のフィールドの値をそのままコピーする」という意味の
  # condition_type="copy"行として追加する(例: FAOBJがAETERMをコピーする)
  field_equality_copy_conditions <- validator_table %>%
    filter(validator_type == "formula") %>%
    distinct(alias_name, field_name, value) %>%
    mutate(copy_ref_field = map2_chr(field_name, value, extract_field_equality_ref)) %>%
    filter(!is.na(copy_ref_field)) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
    left_join(
      field_to_cdisc_variable %>% rename(ref_cdisc_variable = cdisc_variable),
      by = c("alias_name", "copy_ref_field" = "field")
    ) %>%
    left_join(
      field_to_label %>% rename(ref_label = label),
      by = c("alias_name", "copy_ref_field" = "field")
    ) %>%
    transmute(cdisc_variable, label, alias_name, ref_cdisc_variable, ref_alias_name = alias_name, ref_label, expected_value = NA_character_, condition_type = "copy") %>%
    filter(!is.na(cdisc_variable), !is.na(ref_cdisc_variable))

  presence_conditions <- bind_rows(presence_conditions, field_equality_copy_conditions)

  # FieldItem::Reference(同じシート内の別フィールドの値をそのまま使うフィールド)を、
  # condition_type="copy"のpresence_conditions行として追加する。
  # reference_type=="sheet"(同じシート内参照)のみ対応。それ以外は未対応のためスキップする
  if (!is.null(field_reference_table) && nrow(field_reference_table) > 0) {
    field_copy_conditions <- field_reference_table %>%
      filter(reference_type == "sheet") %>%
      left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
      left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
      left_join(
        field_to_cdisc_variable %>% rename(ref_cdisc_variable = cdisc_variable),
        by = c("alias_name", "reference_field" = "field")
      ) %>%
      left_join(
        field_to_label %>% rename(ref_label = label),
        by = c("alias_name", "reference_field" = "field")
      ) %>%
      transmute(cdisc_variable, label, alias_name, ref_cdisc_variable, ref_alias_name = alias_name, ref_label, expected_value = NA_character_, condition_type = "copy") %>%
      filter(!is.na(cdisc_variable), !is.na(ref_cdisc_variable))

    presence_conditions <- bind_rows(presence_conditions, field_copy_conditions)
  }

  # validator_type=="presence"のレコードを持つ(alias_name, label, cdisc_variable)の一覧。
  # 同じcdisc_variable名が複数のalias_name/labelに定義されている場合(例: MHTERMが
  # "主診断"(必須)と"再発診断"(非必須、複数label)の両方に使われる)があるため、
  # cdisc_variable名だけでなくalias_name・label単位で必須かどうかを判定できるようにする。
  # ここに含まれないradio_button項目のインスタンスは空白も選択肢として許容する
  required_var_instances <- validator_table %>%
    filter(validator_type == "presence") %>%
    distinct(alias_name, field_name) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
    filter(!is.na(cdisc_variable)) %>%
    distinct(alias_name, label, cdisc_variable)

  # 後方互換用: cdisc_variable名だけでunique化したフラット版(段階的に置き換え中)
  required_vars <- required_var_instances %>%
    pull(cdisc_variable) %>%
    unique() %>%
    na.omit()

  # bound_type/numeric_valueが入っている行(date/numericality/formulaの数値上限・下限)を
  # field名からcdisc_variable名に変換し、(cdisc_variable, min_value, max_value)のワイド形式にする。
  # 同じ変数に複数の制約がある場合は、より厳しい方(min_valueは最大、max_valueは最小)を採用する
  numeric_bounds <- validator_table %>%
    filter(!is.na(bound_type), !is.na(numeric_value), bound_type %in% c("min_value", "max_value")) %>%
    distinct(alias_name, field_name, bound_type, numeric_value) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    filter(!is.na(cdisc_variable)) %>%
    group_by(cdisc_variable) %>%
    summarise(
      min_value = suppressWarnings(max(numeric_value[bound_type == "min_value"], na.rm = TRUE)),
      max_value = suppressWarnings(min(numeric_value[bound_type == "max_value"], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      min_value = if_else(is.infinite(min_value), NA_real_, min_value),
      max_value = if_else(is.infinite(max_value), NA_real_, max_value)
    )

  # numeric_boundsはcdisc_variable単位に集約されるため、LBORRESのように同じcdisc_variable名を
  # 多数のTESTCD別フィールドが共有するケースでは使えない(全フィールドのmin/maxが「厳しい方」で
  # 一律にまとまってしまう)。date_ref_boundsと同じく(alias_name, label, cdisc_variable)単位で
  # 集約せず個別に保持したバージョンを別途用意する(LB/TR/VSのORRES生成で使う)
  field_numeric_bounds <- validator_table %>%
    filter(!is.na(bound_type), !is.na(numeric_value), bound_type %in% c("min_value", "max_value")) %>%
    distinct(alias_name, field_name, bound_type, numeric_value) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
    filter(!is.na(cdisc_variable)) %>%
    group_by(alias_name, label, cdisc_variable) %>%
    summarise(
      min_value = suppressWarnings(max(numeric_value[bound_type == "min_value"], na.rm = TRUE)),
      max_value = suppressWarnings(min(numeric_value[bound_type == "max_value"], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      min_value = if_else(is.infinite(min_value), NA_real_, min_value),
      max_value = if_else(is.infinite(max_value), NA_real_, max_value)
    ) %>%
    filter(!is.na(min_value) | !is.na(max_value))

  # formulaでフィールド同士を比較している行(例: f350<=f59)を、field名からcdisc_variable名に変換し、
  # (cdisc_variable, ref_cdisc_variable, bound_type)のテーブルにする。
  # ref_field != field_nameで絞ることで、f18<=3のような自己参照(数値リテラル)行を除外する
  field_ref_bounds <- validator_table %>%
    filter(validator_type == "formula", !is.na(bound_type), !is.na(ref_field), ref_field != field_name) %>%
    distinct(alias_name, field_name, ref_field, bound_type) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(
      field_to_cdisc_variable %>% rename(ref_cdisc_variable = cdisc_variable),
      by = c("alias_name", "ref_field" = "field")
    ) %>%
    # alias_name(自分自身の所属シート。formula参照は必ず同一シート内なのでref_alias_nameも同じ)は、
    # build_alias_level_edges()がprefixだけでなくシート単位で依存関係を見られるようにするために保持する
    transmute(alias_name, cdisc_variable, ref_cdisc_variable, bound_type) %>%
    filter(!is.na(cdisc_variable), !is.na(ref_cdisc_variable))

  # validate_date_after_or_equal_to/validate_date_before_or_equal_toが他フィールド参照
  # (例: "field5")の場合の下限/上限(bound_type="min_date"/"max_date")を、field_ref_boundsと
  # 同様にcdisc_variable名に変換したテーブルにする。populate_date_fields()・build_repeated_domain()で、
  # 対象フィールドの生成範囲を一律のregistration_start_date/今日ではなく、参照先フィールドの値を
  # 下限/上限として使うために参照する。
  # alias_name/label/ref_labelも保持しておく。EC等の繰り返しブロックでは、同じcdisc_variable名
  # (例: ECSTDTC/ECENDTC)が1つのalias内で複数回(投与1回目・2回目...)登場し、
  # 「同じlabel内の開始日<=終了日」と「次のlabelの開始日>=前のlabelの終了日」のように、
  # label(行)を跨いだ参照とlabel内の参照が混在する。cdisc_variable単位まで潰してしまうと
  # (ECSTDTC min_date ECENDTC / ECENDTC min_date ECSTDTC のように)矛盾した規則に見えてしまうため、
  # build_repeated_domain側でlabel/ref_labelを見てlabelを跨ぐ参照かどうかを判定できるようにする
  # ref_field != field_nameの自己参照除外は、date_ref_alias_name(ref('sheet_alias', N)形式の
  # 他シート参照)が無い場合(=同一シート内の参照)にだけ適用する。他シート参照の場合、
  # 参照先の(そのシート内での)フィールド番号が自分のフィールド番号とたまたま同じことがあり
  # (例: earlyintensifiのfield820がinductionのfield820を参照)、これを自己参照として誤除外
  # してしまうため
  date_ref_bounds <- validator_table %>%
    filter(
      validator_type == "date", !is.na(bound_type), !is.na(ref_field),
      !is.na(date_ref_alias_name) | ref_field != field_name
    ) %>%
    distinct(alias_name, field_name, ref_field, bound_type, date_ref_alias_name, date_ref_offset_days) %>%
    # ref('sheet_alias', N)形式の他シート参照(date_ref_alias_name)があればそちらを、無ければ
    # 従来通り自分自身と同じalias_nameを参照先のlookupに使う
    mutate(ref_lookup_alias_name = coalesce(date_ref_alias_name, alias_name)) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(
      field_to_cdisc_variable %>% rename(ref_cdisc_variable = cdisc_variable),
      by = c("ref_lookup_alias_name" = "alias_name", "ref_field" = "field")
    ) %>%
    left_join(field_to_label, by = c("alias_name", "field_name" = "field")) %>%
    left_join(
      field_to_label %>% rename(ref_label = label),
      by = c("ref_lookup_alias_name" = "alias_name", "ref_field" = "field")
    ) %>%
    transmute(alias_name, label, cdisc_variable, ref_alias_name = ref_lookup_alias_name, ref_label, ref_cdisc_variable, bound_type, offset_days = date_ref_offset_days) %>%
    filter(!is.na(cdisc_variable), !is.na(ref_cdisc_variable)) %>%
    distinct()

  # age(fN, fM)>=X && age(fN, fM)<=Y のような年齢条件を、field名からcdisc_variable名に変換し、
  # (cdisc_variable, ref_cdisc_variable, ref_alias_name, ref_label, min_age, max_age)のテーブルにする。
  # cdisc_variableは年齢制約を受ける側の日付(例: RFICDTC)、ref_cdisc_variableはもう一方の日付(例: BRTHDTC)
  age_bounds <- validator_table %>%
    filter(!is.na(age_ref_field)) %>%
    distinct(alias_name, field_name, age_ref_field, min_age, max_age) %>%
    left_join(field_to_cdisc_variable, by = c("alias_name", "field_name" = "field")) %>%
    left_join(
      field_to_cdisc_variable %>% rename(ref_cdisc_variable = cdisc_variable),
      by = c("alias_name", "age_ref_field" = "field")
    ) %>%
    left_join(
      field_to_label %>% rename(ref_label = label),
      by = c("alias_name", "age_ref_field" = "field")
    ) %>%
    # alias_name(自分自身の所属シート)は、build_alias_level_edges()がprefixだけでなくシート単位で
    # 依存関係を見られるようにするために保持する(age()参照は必ず同一シート内なのでref_alias_nameも同じ)
    transmute(alias_name, cdisc_variable, ref_cdisc_variable, ref_alias_name = alias_name, ref_label, min_age, max_age) %>%
    filter(!is.na(cdisc_variable), !is.na(ref_cdisc_variable))

  list(
    presence_conditions = presence_conditions,
    required_vars = required_vars,
    required_var_instances = required_var_instances,
    numeric_bounds = numeric_bounds,
    field_numeric_bounds = field_numeric_bounds,
    field_ref_bounds = field_ref_bounds,
    date_ref_bounds = date_ref_bounds,
    age_bounds = age_bounds
  )
}

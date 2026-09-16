library(tidyverse)

build_cdisc_sheet_config_table <- function(sheet) {
  field_items <- if (length(sheet$field_items) == 0) {
    tibble(field = character(), default_value = character(), is_invisible = logical(), field_type = character())
  } else {
    sheet$field_items %>%
      # field_type=="select"(EDC上のプルダウン)は、以降の選択肢処理(populate_radio_button_fields()等)で
      # radio_buttonと同じ「単一選択のコードリスト」として扱う。ここで正規化しておくことで、
      # 個々の判定箇所(field_type %in% c("radio_button", "check_box"))を毎回書き換えずに済む。
      # field_type=="datetime"も同様、時刻要素は無視してdateと同じ日付生成パイプラインで扱う
      # (個々の日付判定箇所を書き換えずに済む)
      map_dfr(~ tibble(
        field = .$name, default_value = .$default_value, is_invisible = .$is_invisible,
        field_type = if (identical(.$field_type, "select")) "radio_button" else if (identical(.$field_type, "datetime")) "date" else .$field_type
      ))
  }

  prefix_field_table <- if (length(sheet$cdisc_sheet_configs) == 0) {
    tibble(prefix = character(), field = character(), value = character(), label = character())
  } else {
    sheet$cdisc_sheet_configs %>%
      map_dfr(~ tibble(
        prefix = .x[["prefix"]],
        field = names(.x[["table"]]),
        value = map_chr(.x[["table"]], ~ if (is.null(.x)) NA_character_ else .x),
        label = .x[["label"]]
      ))
  }

  result <- prefix_field_table %>%
    inner_join(field_items, by = "field") %>%
    filter(!is.na(value), !str_starts(value, "_"))
  result[["alias_name"]] <- sheet$alias_name
  return(result)
}

# edc_spec/sheetsから、df_cdisc(field単位の生データ)とcdisc_variable_values(選択肢展開済み、
# sheet_seq付き)を組み立てて返す
build_cdisc_variable_values <- function(edc_spec, sheets) {
  df_cdisc <- map_dfr(sheets, build_cdisc_sheet_config_table, .id = "sheet_index") %>%
    mutate(cdisc_variable = case_when(
      prefix == "DM" ~ value,
      value == "VISITNUM" ~ value,
      value == "SPDEVID" ~ value,
      TRUE ~ str_c(prefix, value)
    ))

  options <- edc_spec$options %>% map_dfr(function(opt) {
    opt$values %>%
      map_dfr(~ tibble(
        value_name = .$name,
        seq = .$seq,
        code = .$code,
        is_usable = .$is_usable
      )) %>%
      mutate(option_name = opt$name, .before = 1)
  }) %>% filter(is_usable) %>% select(-is_usable)
  options[["is_invisible"]] <- FALSE

  field_option <- sheets %>% map_dfr( ~ {
    alias_name <- .$alias_name
    field_items <- .$field_items %>% keep( ~ "option_name" %in% names(.x))
    if (length(field_items) == 0) {
      return()
    }
    result <- field_items %>% map_dfr( ~ tibble(field=.$name, option_name=.$option_name))
    result[["alias_name"]] <- alias_name
    return(result)
  })

  cdisc_variable_spec <- df_cdisc %>%
    left_join(field_option, by=c("alias_name", "field")) %>%
    select(prefix, cdisc_variable, default_value, option_name, is_invisible, field_type, alias_name, label)
  cdisc_variable_values <- cdisc_variable_spec %>%
    left_join(options, by=c("option_name", "is_invisible"), relationship = "many-to-many") %>%
    select(-option_name) %>%
    distinct() %>%
    arrange(prefix, cdisc_variable)

  # sheet_orders(シートの表示順)をalias_name(=sheet)で結合
  sheet_order <- edc_spec$sheet_orders %>% map_dfr(~ tibble(alias_name = .$sheet, sheet_seq = .$seq))
  cdisc_variable_values <- cdisc_variable_values %>% left_join(sheet_order, by = "alias_name")

  list(df_cdisc = df_cdisc, cdisc_variable_values = cdisc_variable_values)
}

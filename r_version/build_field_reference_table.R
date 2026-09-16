library(tidyverse)

# field_items内のtype=="FieldItem::Reference"な要素を(alias_name, field_name, reference_type, reference_field)の
# tibbleにする。このフィールドは自分の値を持たず、reference_field(同じシート内の別フィールド)の値をそのまま使う
build_field_reference_table <- function(sheets) {
  sheets %>%
    map_dfr(function(sheet) {
      field_items <- sheet[["field_items"]]
      if (length(field_items) == 0) {
        return(tibble())
      }
      reference_table <- field_items %>%
        keep(~ identical(.x[["type"]], "FieldItem::Reference")) %>%
        map_dfr(~ tibble(
          field_name = .x[["name"]],
          reference_type = .x[["reference_type"]],
          reference_field = .x[["reference_field"]]
        ))

      if (nrow(reference_table) == 0) {
        return(reference_table)
      }
      # reference_fieldは"field48"のような素の名前の場合と、"baseline1.field48"のように
      # 自分自身のalias_name付きの場合がある(EDC仕様側の出力形式の違いによる)。reference_type=="sheet"
      # (同じシート内参照)は必ず自分自身のalias_name内のフィールドを指すため、プレフィックスが
      # 付いていれば取り除いて常に素のフィールド名に正規化する(付いていないと、この後の
      # build_generation_constraints.R側の突き合わせ(alias_name+素のfield名)が常に失敗し、
      # このフィールドの値コピーが機能しなくなる)
      own_prefix <- str_c(sheet[["alias_name"]], ".")
      reference_table %>%
        mutate(
          alias_name = sheet[["alias_name"]],
          reference_field = if_else(
            str_starts(reference_field, fixed(own_prefix)),
            str_sub(reference_field, str_length(own_prefix) + 1),
            reference_field
          ),
          .before = 1
        )
    })
}

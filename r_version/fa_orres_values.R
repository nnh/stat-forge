library(tidyverse)

# FATESTCD/FAORRESが両方ある場合のみ、EDC仕様の数値バリデーション(min/max)に基づいてFAORRESを
# それらしい数値に置き換える。バリデーションが定義されていないFATESTCD(未知のTESTCD含む)は
# generate_orres_value()側で0〜100のランダムな整数になる。FAORRESが既にNA(NOT DONEなどの
# presence_conditionsで空白化された)の行、およびradio_button/check_boxで定義された
# (選択式の)TESTCDの行は上書きしない
# (build_testcd_numeric_bounds()/build_testcd_categorical_set()/generate_orres_value()はbuild_domain_common.R参照)
populate_fa_orres <- function(fa, cdisc_variable_values, field_numeric_bounds) {
  if (!all(c("FATESTCD", "FAORRES") %in% colnames(fa))) {
    return(fa)
  }
  testcd_bounds <- build_testcd_numeric_bounds(cdisc_variable_values, field_numeric_bounds, "FATESTCD", "FAORRES")
  categorical_testcds <- build_testcd_categorical_set(cdisc_variable_values, "FATESTCD", "FAORRES")
  has_value <- !is.na(fa[["FAORRES"]]) & !(fa[["FATESTCD"]] %in% categorical_testcds)
  fa[["FAORRES"]][has_value] <- as.character(generate_orres_value(fa[["FATESTCD"]][has_value], testcd_bounds))
  fa
}

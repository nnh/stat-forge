library(tidyverse)

# VSTESTCD/VSORRESが両方ある場合のみ、EDC仕様の数値バリデーション(min/max)に基づいてVSORRESを
# それらしい数値に置き換える。バリデーションが定義されていないVSTESTCD(未知のTESTCD含む)は
# generate_orres_value()側で0〜100のランダムな整数になる。VSORRESが既にNA(presence_conditionsで
# 空白化された)の行、およびradio_button/check_boxで定義された(選択式の)TESTCDの行は上書きしない
# (build_testcd_numeric_bounds()/build_testcd_categorical_set()/generate_orres_value()はbuild_domain_common.R参照)
populate_vs_orres <- function(vs, cdisc_variable_values, field_numeric_bounds) {
  if (!all(c("VSTESTCD", "VSORRES") %in% colnames(vs))) {
    return(vs)
  }
  testcd_bounds <- build_testcd_numeric_bounds(cdisc_variable_values, field_numeric_bounds, "VSTESTCD", "VSORRES")
  categorical_testcds <- build_testcd_categorical_set(cdisc_variable_values, "VSTESTCD", "VSORRES")
  has_value <- !is.na(vs[["VSORRES"]]) & !(vs[["VSTESTCD"]] %in% categorical_testcds)
  vs[["VSORRES"]][has_value] <- as.character(generate_orres_value(vs[["VSTESTCD"]][has_value], testcd_bounds))
  vs
}

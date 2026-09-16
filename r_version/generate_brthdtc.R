library(tidyverse)

# age_bounds(cdisc_variable, ref_cdisc_variable, min_age, max_age)のうち、ref_cdisc_variable=="BRTHDTC"な
# 行(=BRTHDTCからの年齢で条件付けられている項目、例: RFICDTC)を全て満たす年齢範囲(交差範囲)を求める。
# 該当行が無い(=年齢に関する制約が試験仕様に無い)場合は、小児(0歳)〜高齢者(89歳)まで幅広く対象にする
compute_birth_age_range <- function(age_bounds) {
  if (is.null(age_bounds) || nrow(age_bounds) == 0) {
    return(list(min_age = 0, max_age = 89))
  }
  relevant <- age_bounds %>% filter(ref_cdisc_variable == "BRTHDTC")
  if (nrow(relevant) == 0) {
    return(list(min_age = 0, max_age = 89))
  }
  min_age <- suppressWarnings(max(relevant[["min_age"]], na.rm = TRUE))
  max_age <- suppressWarnings(min(relevant[["max_age"]], na.rm = TRUE))
  list(
    min_age = if (is.infinite(min_age)) 0 else min_age,
    max_age = if (is.infinite(max_age)) 89 else max_age
  )
}

# min_age〜max_age歳(誕生日基準、ref_dateとの経過年数)の範囲で一様ランダムにBRTHDTCを生成する。
# min_age/max_ageは通常compute_birth_age_range()で試験のage_bounds(RFICDTC等がBRTHDTCから
# 何歳以上/以下かという制約)から求めた値を渡す(制約が無ければ0〜89歳の幅広い範囲になる)
generate_brthdtc <- function(data, ref_date = Sys.Date(), var_name = "BRTHDTC", min_age = 0, max_age = 89) {
  ref_date <- as.Date(ref_date)
  n <- nrow(data)

  min_days <- min_age * 365.25
  max_days <- (max_age + 1) * 365.25 - 1
  age_days <- round(runif(n, min_days, max_days))

  data[[var_name]] <- format(ref_date - age_days, "%Y-%m-%d")
  data
}

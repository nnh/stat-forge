library(tidyverse)
library(lubridate)

resolve_date_bound <- function(data, x) {
  if (is.character(x) && length(x) == 1 && x %in% names(data)) {
    ymd(data[[x]])
  } else {
    rep(ymd(x), nrow(data))
  }
}

generate_random_date <- function(data, start_date, end_date, var_name) {
  start_date <- resolve_date_bound(data, start_date)
  end_date <- resolve_date_bound(data, end_date)

  n <- nrow(data)
  # runif()は連続値を返すため、floor()せずas.Date()に渡すと、日付として表示・文字列化すれば
  # 同じに見えても内部の数値表現に小数(日未満)が残り、他所で生成した「きれいな」Date値との
  # 等価比較が一致しなくなる。floor()で整数日に丸めてからDate化する
  random_dates <- as.Date(
    floor(runif(n, as.numeric(start_date), as.numeric(end_date))),
    origin = "1970-01-01"
  )

  data[[var_name]] <- random_dates
  data
}

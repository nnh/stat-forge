library(tidyverse)

external_dict_dir <- "/Users/mariko/Library/CloudStorage/Box-Box/Stat/Tools/test20260817"
meddra_dir <- file.path(external_dict_dir, "MedDRA")

who_drug_idf_parent_dir <- file.path(external_dict_dir, "WHO-DD_IDF")

# AEドメインに必ず1件以上含めたい病名のLLTコード(複数指定可、空でもよい)
required_ae_llt_codes <- c("10052464", "10062314", "10057913", "10055032", "10024855", "10039906", "10042772", "10047281", "10047294", "10065341", "10047302")

dummy_site <- tibble(
  SITEID = as.character(sample(100000000:900000000, 10)),
  SITENAME = str_c("ダミー", str_pad(1:10, width = 2, pad = "0"), "病院")
)

# validate_datasets_test3_web.RのFA(Findings About)関連チェックを切り出したファイル。
# 呼び出し元スクリプトが fa/dm/fixed_value_checks_csv_path を用意した上でこのファイルをsourceすること

# FA: FATESTCDごとの個別チェック(test3用)。指定visitnum・faobj・falocのレコードに絞り込み、FATEST
# (+has_blflならFABLFL)をsuffix付き列名にリネームしたうえで固定値と一致することを確認する
# (FAOBJ/FALOCはfilter条件として使うため、チェック対象には含めない)。
# has_not_done_split=TRUEの場合、FASTAT=="NOT DONE"で分岐し、FAORRESの要否(NOT DONEなら空欄、
# それ以外なら必須)を確認する。has_orres_in_targetならFAORRESもsuffix付き列名にリネームして
# 固定値チェック対象に含める(NOT DONE行を含む全行が対象のときのみ使える)。
# has_not_done_split=TRUEでFAORRESにも固定値があるときは、check_orres_value_when_done=TRUEを
# 指定するとDONE行に絞ったうえでFAORRESの固定値チェックも行う(LBのcheck_lb_testcd_at_visit()に対応)
check_fa_testcd_at_visit <- function(fa, fatestcd, faobj, faloc, visitnum, suffix, fixed_value_checks_csv_path,
                                      has_blfl = TRUE, has_not_done_split = FALSE,
                                      has_orres_in_target = FALSE, check_orres_value_when_done = FALSE) {
  fa_target_cols <- c("FATEST", "VISITNUM")
  if (has_blfl) fa_target_cols <- c(fa_target_cols, "FABLFL")
  if (has_orres_in_target) fa_target_cols <- c(fa_target_cols, "FAORRES")

  tmp_fa <- fa %>% filter(FATESTCD == fatestcd & FAOBJ == faobj & FALOC == faloc & VISITNUM == visitnum)
  tmp_fa <- tmp_fa %>% rename_with(~ str_c(.x, suffix), all_of(fa_target_cols))
  str_c(fa_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))

  orres_col <- if (has_orres_in_target) str_c("FAORRES", suffix) else "FAORRES"

  if (has_not_done_split) {
    tmp_fa_done <- tmp_fa %>% filter(FASTAT != "NOT DONE")
    tmp_fa_not_done <- tmp_fa %>% filter(FASTAT == "NOT DONE")
    orres_col %>% check_required_vars(tmp_fa_done, ., domain_name = "FA")
    orres_col %>% check_blank_vars(tmp_fa_not_done, ., domain_name = "FA")
    if (check_orres_value_when_done) {
      tmp_fa_done <- tmp_fa_done %>% rename(!!str_c("FAORRES", suffix) := FAORRES)
      str_c("FAORRES", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_done, "FA", .x, fixed_value_checks_csv_path))
    }
  } else {
    orres_col %>% check_required_vars(tmp_fa, ., domain_name = "FA")
  }
}

# FA: check_fa_testcd_at_visit()と同内容だが、FALOC(部位)を持たないFATESTCD向けにfaloc引数・
# filter条件を除き、さらにVISITNUM引数・filter条件・チェックも除いたバージョン
check_fa_testcd_no_loc <- function(fa, fatestcd, faobj, suffix, fixed_value_checks_csv_path,
                                             has_blfl = TRUE, has_not_done_split = FALSE,
                                             has_orres_in_target = FALSE, check_orres_value_when_done = FALSE) {
  fa_target_cols <- c("FATEST", "FACAT")
  if (has_blfl) fa_target_cols <- c(fa_target_cols, "FABLFL")
  if (has_orres_in_target) fa_target_cols <- c(fa_target_cols, "FAORRES")

  tmp_fa <- fa %>% filter(FATESTCD == fatestcd & FAOBJ == faobj)
  tmp_fa <- tmp_fa %>% rename_with(~ str_c(.x, suffix), all_of(fa_target_cols))
  str_c(fa_target_cols, suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa, "FA", .x, fixed_value_checks_csv_path))

  orres_col <- if (has_orres_in_target) str_c("FAORRES", suffix) else "FAORRES"

  if (has_not_done_split) {
    tmp_fa_done <- tmp_fa %>% filter(FASTAT != "NOT DONE")
    tmp_fa_not_done <- tmp_fa %>% filter(FASTAT == "NOT DONE")
    orres_col %>% check_required_vars(tmp_fa_done, ., domain_name = "FA")
    orres_col %>% check_blank_vars(tmp_fa_not_done, ., domain_name = "FA")
    if (check_orres_value_when_done) {
      tmp_fa_done <- tmp_fa_done %>% rename(!!str_c("FAORRES", suffix) := FAORRES)
      str_c("FAORRES", suffix) %>% walk(~ run_value_equals_checks_from_csv(tmp_fa_done, "FA", .x, fixed_value_checks_csv_path))
    }
  } else {
    orres_col %>% check_required_vars(tmp_fa, ., domain_name = "FA")
  }
}

fa <- fa %>% inner_join(dm %>% select(USUBJID, BRTHDTC, SEX), by="USUBJID")
fa %>% check_date_after_var_before_today("FADTC", "BRTHDTC", domain_name = "FA")

check_fa_testcd_at_visit(fa,
                         "STATUS", "Tumor Involvement", "CENTRAL NERVOUS SYSTEM",
                         100, "_1",
                         fixed_value_checks_csv_path,
                         has_not_done_split = TRUE, check_orres_value_when_done = TRUE)

# 女性被験者はTESTIS(精巣)自体が無いため、FASTATは全て"NOT DONE"・FAORRESは全て空欄であることを確認する
# (USUBJID列はcheck_value_equals/check_blank_varsのエラー表示に必要なので、selectで落とさない)
tmp_fa_f <- fa %>% filter(SEX == "F" & FALOC=="TESTIS")
tmp_fa_f %>% check_value_equals("FASTAT", "NOT DONE", domain_name = "FA")
tmp_fa_f %>% check_blank_vars("FAORRES", domain_name = "FA")

check_fa_testcd_at_visit(fa,
                         "OCCUR", "Tumor Involvement", "TESTIS",
                         100, "_2",
                         fixed_value_checks_csv_path,
                         has_not_done_split = TRUE, check_orres_value_when_done = TRUE)

check_fa_testcd_at_visit(fa,
                         "OCCUR", "Enlargement", "MEDIASTINUM",
                         100, "_3",
                         fixed_value_checks_csv_path,
                         has_not_done_split = FALSE, check_orres_value_when_done = TRUE)

check_fa_testcd_at_visit(fa,
                         "OCCUR", "Tumor Involvement", "SKIN",
                         100, "_4",
                         fixed_value_checks_csv_path,
                         has_not_done_split = FALSE, check_orres_value_when_done = TRUE)

check_fa_testcd_at_visit(fa,
                         "OCCUR", "Tumor Involvement", "BONE",
                         100, "_4",
                         fixed_value_checks_csv_path,
                         has_not_done_split = FALSE, check_orres_value_when_done = TRUE)

check_fa_testcd_at_visit(fa,
                         "OCCUR", "Tumor Involvement", "LIVER",
                         100, "_4",
                         fixed_value_checks_csv_path,
                         has_not_done_split = FALSE, check_orres_value_when_done = TRUE)

check_fa_testcd_at_visit(fa,
                         "OCCUR", "Tumor Involvement", "SPLEEN",
                         100, "_4",
                         fixed_value_checks_csv_path,
                         has_not_done_split = FALSE, check_orres_value_when_done = TRUE)

check_fa_testcd_at_visit(fa,
                         "OCCUR", "Tumor Involvement", "LYMPH NODE",
                         100, "_4",
                         fixed_value_checks_csv_path,
                         has_not_done_split = FALSE, check_orres_value_when_done = TRUE)

check_fa_testcd_at_visit(fa,
                         "OCCUR", "Tumor Involvement", "KIDNEY",
                         100, "_4",
                         fixed_value_checks_csv_path,
                         has_not_done_split = FALSE, check_orres_value_when_done = TRUE)

# GRADE(重症度)評価のFAOBJ一覧。faobj/suffix以外は全て共通のためtribble+pwalkでまとめて実行する
grade_no_loc_checks <- tribble(
  ~faobj, ~suffix,
  "Anemia", "_5",
  "Disseminated intravascular coagulation", "_6",
  "Febrile Neutropenia", "_7",
  "Heart failure", "_5",
  "Myocardial infarction", "_6",
  "Tachycardia", "_5",
  "Bradycardia", "_5",
  "Glaucoma", "_8",
  "Ascites", "_5",
  "Constipation", "_5",
  "Diarrhea", "_5",
  "Ileus", "_5",
  "Mucositis oral", "_5",
  "Nausea", "_9",
  "Pancreatitis", "_6",
  "Vomiting", "_5",
  "Fever", "_5",
  "Multi-organ failure", "_7",
  "Portal vein thrombosis", "_6",
  "Allergic reaction", "_5",
  "Anaphylaxis", "_7",
  "Cytokine release syndrome", "_5",
  "Catheter related infection", "_6",
  "Fungemia", "_10",
  "Lung infection", "_6",
  "Meningitis", "_7",
  "Sepsis", "_7",
  "Thrush", "_9",
  "Urinary tract infection", "_6",
  "Infusion related reaction", "_5",
  "Alanine aminotransferase increased", "_8",
  "Aspartate aminotransferase increased", "_8",
  "Blood bilirubin increased", "_8",
  "Cholesterol high", "_8",
  "Creatinine increased", "_8",
  "Fibrinogen decreased", "_8",
  "Lymphocyte count decreased", "_8",
  "Neutrophil count decreased", "_8",
  "Platelet count decreased", "_8",
  "Serum amylase increased", "_8",
  "White blood cell decreased", "_8",
  "Hyperglycemia", "_5",
  "Hypertriglyceridemia", "_5",
  "Tumor lysis syndrome", "_7",
  "Osteonecrosis", "_5",
  "Depressed level of consciousness", "_5",
  "Dizziness", "_9",
  "Dysphasia", "_9",
  "Headache", "_9",
  "Intracranial hemorrhage", "_5",
  "Leukoencephalopathy", "_5",
  "Paresthesia", "_9",
  "Peripheral motor neuropathy", "_5",
  "Peripheral sensory neuropathy", "_8",
  "Reversible posterior leukoencephalopathy syndrome", "_6",
  "Seizure", "_5",
  "Stroke", "_5",
  "Tremor", "_9",
  "Agitation", "_8",
  "Confusion", "_8",
  "Delirium", "_5",
  "Depression", "_5",
  "Hallucinations", "_5",
  "Insomnia", "_9",
  "Restlessness", "_9",
  "Suicidal ideation", "_8",
  "Acute kidney injury", "_7",
  "Adult respiratory distress syndrome", "_7",
  "Dyspnea", "_5",
  "Hypoxia", "_6",
  "Bullous dermatitis", "_5",
  "Erythroderma", "_6",
  "Rash maculo-papular", "_9",
  "Arterial thromboembolism", "_7",
  "Capillary leak syndrome", "_5",
  "Hypertension", "_5",
  "Hypotension", "_5",
  "Thromboembolic event", "_5"
)
# GRADE評価パネルを1シート分だけ実行する。faspid/visitnumを指定して呼び出す
check_fa_grade_panel <- function(faspid, visitnum) {
  tmp_fa <- fa %>% filter(FASPID == faspid & VISITNUM == visitnum)
  pwalk(grade_no_loc_checks, function(faobj, suffix) {
    check_fa_testcd_no_loc(tmp_fa, "GRADE", faobj, suffix, fixed_value_checks_csv_path, has_blfl = FALSE, has_orres_in_target = TRUE)
  })
}
# OCCUR/Tumor InvolvementのうちFALOCが固定サイトでないブロック(label 020/026/027/028/029、
# field212等)は、FALOCが800件以上の選択肢から自由に選ばれる「その他部位」枠が5つ繰り返されたもの。
# 生成後のCSVではalias_name/labelが残らずこの5ブロックを個別に区別できないため、まとめて集約検証する。
# FALOCの値そのもの(選択肢通りであること)はpopulate_radio_button_fields()側の仕組みで構造的に
# 保証されるため確認せず、FAORRES=='Y'のときだけFALOCが埋まっている(presence)ことだけを確認する
fa_fixed_locs <- c("CENTRAL NERVOUS SYSTEM", "TESTIS", "SKIN", "LIVER", "SPLEEN", "LYMPH NODE", "KIDNEY", "BONE")
tmp_fa_other <- fa %>% filter(FATESTCD == "OCCUR" & FAOBJ == "Tumor Involvement" & VISITNUM == "100" & !(FALOC %in% fa_fixed_locs))
tmp_fa_other %>% filter(FAORRES == "Y") %>% check_required_vars("FALOC", domain_name = "FA")
tmp_fa_other %>% filter(FAORRES != "Y") %>% check_blank_vars("FALOC", domain_name = "FA")

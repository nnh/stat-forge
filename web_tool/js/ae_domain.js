// AEドメインを生成する。R版のbuild_ae_domain.R/populate_ae_domain()に対応するが、
// 現時点ではUSUBJIDの採番(DMからの重複ありサンプリング)、alias_name割り当て、
// radio_button/check_box型項目(required_vars/numeric_bounds含む)、date型項目、
// meddra型項目+コーディングブロックの埋め込み、inject_required_llt_codes、
// FA等のリンクブロック生成(populate_linked_blocks/split_linked_domains/exclude_ae_linked_prefixes/
// merge_linked_domains)、DUMMYフォールバック、presence_conditionsによるゲーティング、field_ref_bounds、
// AETOXGR=5(死亡)関連の並べ替え・矛盾レコード除外、AESPID/AESEQ・列順整理まで対応する

// dmのUSUBJIDから重複ありでn件サンプリングし、STUDYID/DOMAINを付与する(Rのbuild_ae_domain()に対応)
function buildAeDomain(dm, n) {
  const rows = [];
  for (let i = 0; i < n; i += 1) {
    const dmRow = sampleOne(dm);
    // RFSTDTC(症例登録日)も結合しておく。AEの日付項目は明示的なref()参照を持たず、従来は
    // registrationStartDate(試験共通の定数)を下限にしていたため、稀に被験者本人の登録日より
    // 前の日付が生成され得た。populateAeDateFields()側でこれを下限として使う
    // (最終的な出力列には含めない。finalizeAeDomain()末尾で取り除く)
    rows.push({
      STUDYID: dmRow.STUDYID,
      DOMAIN: "AE",
      USUBJID: dmRow.USUBJID,
      RFSTDTC: dmRow.RFSTDTC,
    });
  }
  return rows;
}

// AE行ごとにalias_name(どのAE報告シートの行か)を割り当てる。
// activeSheets(USUBJID -> Set(alias_name)。buildDmDomain()が返すもの)が指定されている場合、
// その被験者にとって実際に有効な(=そのシートが表示される)alias_nameだけから選ぶ
// (どのalias_nameも有効でない行は、AE報告自体が存在しないとみなして除外する)。
// 指定が無い場合は全alias_nameから一様ランダムに選ぶ(Rのpopulate_ae_domain()の該当部分に対応)
function assignAeAliasNames(ae, aeSpec, activeSheets) {
  const aliasNames = [...new Set(aeSpec.map((r) => r.alias_name))];

  if (activeSheets) {
    return ae
      .map((row) => {
        const eligible = activeSheets[row.USUBJID];
        const pool = eligible ? aliasNames.filter((a) => eligible.has(a)) : [];
        return pool.length > 0 ? { ...row, alias_name: sampleOne(pool) } : null;
      })
      .filter((row) => row !== null);
  }

  return ae.map((row) => ({ ...row, alias_name: sampleOne(aliasNames) }));
}

// radio_button/check_box型のAE項目に、選択肢(code、無ければdefault_value)からランダムな値を入れる
// (check_boxは複数選択がカンマ区切りで1つの文字列になる)。そのalias_nameのインスタンスがisRequiredでなく、
// かつそのalias_name内で可視(いずれの行もis_invisibleでない)場合は、空欄("")も選択肢に加える。
// isRequiredはcdisc_variable名単位ではなくalias_name/label単位の判定(attachIsRequired()由来)のため、
// 同じcdisc_variable名でもalias_nameによって必須/非必須が異なりうる。numericBoundsに該当エントリが
// あれば、数値として範囲外のcodeを選択肢から除く。
// AEはalias_name(どのAE報告シートの行か)によって同じcdisc_variableでも選択肢が異なりうるため、
// 行のalias_nameと一致するaliasSpecの選択肢だけから選ぶ(Rのpopulate_radio_button_fields()の
// has_alias_name==TRUEの分岐に対応)
function populateAeChoiceFields(ae, aeSpec, numericBounds) {
  const existingColumns = new Set(Object.keys(ae[0] || {}));
  const choiceSpec = aeSpec.filter((r) => r.field_type === "radio_button" || r.field_type === "check_box");
  const targetVars = [...new Set(choiceSpec.map((r) => r.cdisc_variable))].filter((v) => !existingColumns.has(v));

  targetVars.forEach((varName) => {
    // Rの`data[[var_name]] <- NA_character_`に対応: このcdisc_variableを定義していないalias_nameの行にも
    // 列自体は必ず持たせる(値はnullのまま)。持たせないと、後続のDUMMYフォールバックが
    // (最初の行のキー集合だけを見て)列の有無を誤判定してしまう
    ae.forEach((row) => {
      row[varName] = null;
    });
    const varRows = choiceSpec.filter((r) => r.cdisc_variable === varName);
    const aliasNamesForVar = [...new Set(varRows.map((r) => r.alias_name))];
    aliasNamesForVar.forEach((an) => {
      const anRows = varRows.filter((r) => r.alias_name === an);
      let choices = [...new Set(anRows.map((r) => (r.code != null ? r.code : r.default_value)))];
      const isVisible = !anRows.some((r) => r.is_invisible);
      const isRequired = anRows.some((r) => r.is_required);
      if (!isRequired && isVisible) {
        choices = [...new Set([...choices, ""])];
      }
      const bounds = numericBounds && numericBounds[varName];
      if (bounds) {
        choices = choices.filter((c) => {
          const n = Number(c);
          if (Number.isNaN(n)) return true;
          if (bounds.min_value != null && n < bounds.min_value) return false;
          if (bounds.max_value != null && n > bounds.max_value) return false;
          return true;
        });
      }
      const isCheckBox = anRows.some((r) => r.field_type === "check_box");
      const targetRows = ae.filter((row) => row.alias_name === an);
      if (choices.length === 0 || targetRows.length === 0) return;
      if (isCheckBox) {
        const values = sampleCheckBoxValuesWithCoverage(choices, targetRows.length);
        targetRows.forEach((row, i) => {
          row[varName] = values[i];
        });
      } else {
        const values = sampleValuesWithCoverage(choices, targetRows.length);
        targetRows.forEach((row, i) => {
          row[varName] = values[i];
        });
      }
    });
  });
  return ae;
}

// date型のAE項目に、registrationStartDate〜今日の間のランダムな日付を入れる。
// AEENDTC>=AESTDTCの前後関係や、sae_reportのAESTDTC<-MH(registration)のMHSTDTC(診断日)のような
// 他ドメイン参照はdateRefBoundsに定義されている。以前はAEENDTC>=AESTDTCの関係だけをハードコードし、
// 他ドメイン参照は一切考慮していなかった(sae_reportのAESTDTCが診断日より前になり得るバグの原因)。
// 他の汎用ドメイン(buildGenericDomain/buildRepeatedDomain)と同じくinjectCrossDomainRefs()で
// 参照先の値を結合してからpopulateGenericDateFields()に通すことで、dateRefBoundsに定義された
// 全ての制約(AEENDTC>=AESTDTCを含む)を一律に反映する(Rのpopulate_ae_domain()の日付生成部分に対応)
function populateAeDateFields(ae, aeSpec, registrationStartDate, dateRefBoundsAll, builtDomains, cdiscVariableToPrefix) {
  const existingColumns = new Set(Object.keys(ae[0] || {}));
  const dateVars = [...new Set(aeSpec.filter((r) => r.field_type === "date").map((r) => r.cdisc_variable))].filter(
    (v) => !existingColumns.has(v)
  );
  // dateRefBoundsは全ドメイン分を含む共通テーブルのため、AE自身のcdisc_variableに関する行だけに絞る
  const aeVars = new Set(aeSpec.map((r) => r.cdisc_variable));
  const aeDateRefBounds = (dateRefBoundsAll || []).filter((r) => aeVars.has(r.cdisc_variable));

  const injected = injectCrossDomainRefs(ae, null, null, builtDomains || {}, cdiscVariableToPrefix || {}, null, aeDateRefBounds, "AE");
  ae = injected.data;
  ae = populateGenericDateFields(ae, aeSpec, registrationStartDate, aeDateRefBounds, null);

  // AE報告が複数のalias(シート、例: "sae_report"/"ae2")にまたがる場合、シートの本来の並び順
  // (sheet_seq)に沿うようalias単位でまとめて日付をシフトする(同じ行のAESTDTC<=AEENDTCの関係は保つ)。
  // このシフトはae自身の日付列だけをまとめて動かすため、他ドメイン参照(dateRefBounds、例:
  // sae_reportのAESTDTC<-MHSTDTC)の下限が再び崩れる場合がある(同じUSUBJIDが"ae"と"sae_report"の
  // 両方を持つ場合など)。buildGenericDomain等はreorder後にclampDatesToDiscontinuation()を再度
  // 呼んで修復しているが、AEはこの時点でDS/中止日情報をまだ持たないため、その簡易版
  // (reclampAeDatesToRefBounds、discon超過は扱わずmin_date違反のみ対象)で修復する。参照列
  // (MHSTDTC等)が必要なため、injectedColsを取り除くのはこの後にする
  ae = reorderDatesBySheetSeq(ae, dateVars, aeSpec, registrationStartDate, null, aeDateRefBounds);
  ae = reclampAeDatesToRefBounds(ae, dateVars, aeDateRefBounds);
  injected.injectedCols.forEach((col) => {
    ae.forEach((row) => delete row[col]);
  });
  return ae;
}

// reorderDatesBySheetSeq()による同日ブロックの入れ替えでdateRefBounds(他ドメイン参照。例:
// sae_reportのAESTDTC<-MHSTDTC)のmin_date制約が再び崩れた行だけ、参照値以降になるよう最小限で
// 再生成する。other_domains.jsのclampDatesToDiscontinuation()と同じ考え方の簡易版だが、AEは
// この時点でDS/中止日情報をまだ持たないため、discon超過は扱わずmin_date違反のみを対象にする
function reclampAeDatesToRefBounds(ae, dateVars, dateRefBounds) {
  if (!dateRefBounds || dateRefBounds.length === 0 || dateVars.length === 0) return ae;
  const orderedDateVars = sortDateVarsByDependency(dateVars, dateRefBounds);
  const today = new Date().toISOString().slice(0, 10);

  orderedDateVars.forEach((varName) => {
    if (!ae[0] || !(varName in ae[0])) return;
    const minRefVals = resolveDateRefBoundVals(ae, dateRefBounds, varName, "min_date");
    if (minRefVals == null) return;
    ae.forEach((row, i) => {
      const refVal = minRefVals[i];
      const current = row[varName];
      if (refVal == null || current == null) return;
      if (current < refVal) {
        const upper = refVal > today ? refVal : today;
        row[varName] = randomDateBetween(refVal, upper);
      }
    });
  });
  return ae;
}

// LLTコードを少数(poolSize件)に絞り、Zipf的な重み(1/順位)でサンプリングすることで、
// 頻出病名と稀な病名が混在するようにする。1件につきLLT〜SOCの階層をまとめて返すため、
// 各コード値の対応関係が崩れない(Rのsample_meddra_rows()に対応)
function sampleMeddraRows(meddraData, n, poolSize = 20) {
  const seen = new Set();
  const distinctRows = [];
  meddraData.forEach((row) => {
    if (!seen.has(row.llt_code)) {
      seen.add(row.llt_code);
      distinctRows.push(row);
    }
  });
  const shuffled = [...distinctRows].sort(() => rng() - 0.5);
  const pool = shuffled.slice(0, Math.min(poolSize, distinctRows.length));

  const weights = pool.map((_, i) => 1 / (i + 1));
  const totalWeight = weights.reduce((a, b) => a + b, 0);
  const result = [];
  for (let i = 0; i < n; i += 1) {
    let r = rng() * totalWeight;
    let idx = 0;
    while (idx < weights.length - 1 && r > weights[idx]) {
      r -= weights[idx];
      idx += 1;
    }
    result.push(pool[idx]);
  }
  return result;
}

// presence_conditionsのうち、ref_cdisc_variableが"<prefix>LLTCD"(meddra参照)かつcondition_type=="equals"な
// 行のexpected_valueを集め、必ずサンプルに混ぜ込むべきLLTコード一覧を求める。
// R版はconstant.Rに手動定数(required_ae_llt_codes)として持っていたが、Web版はpresence_conditionsを
// 既に構築しているため、そこから自動導出する(試験ごとの手動設定は不要にする)。
// 該当する行が無ければ空配列を返す(=注入しない。指定なしでも正常動作する)
function deriveRequiredLltCodes(presenceConditions) {
  const codes = new Set();
  (presenceConditions || []).forEach((pc) => {
    if (pc.condition_type !== "equals") return;
    if (!pc.ref_cdisc_variable || !pc.ref_cdisc_variable.endsWith("LLTCD")) return;
    if (pc.expected_value != null && pc.expected_value !== "") codes.add(pc.expected_value);
  });
  return [...codes];
}

// meddraSampleの一部の行を、requiredLltCodes(必ずデータに含めたいLLTコード)の値で上書きする。
// コードごとに1行を選び、そのLLTコードに対応する階層一式に丸ごと差し替える。
// requiredLltCodesが空、meddraSampleが0件、または該当コードがmeddraDataに存在しない場合は
// そのコードを無視する(=何もしない。指定なしでも正常動作する。Rのinject_required_llt_codes()に対応)
function injectRequiredLltCodes(meddraSample, meddraData, requiredLltCodes) {
  const codes = (requiredLltCodes || []).filter((c) => c != null && c !== "");
  if (codes.length === 0 || meddraSample.length === 0) return meddraSample;

  const n = meddraSample.length;
  const withReplacement = codes.length > n;
  const targetRows = [];
  if (withReplacement) {
    for (let i = 0; i < codes.length; i += 1) targetRows.push(Math.floor(rng() * n));
  } else {
    const shuffled = [...Array(n).keys()].sort(() => rng() - 0.5);
    targetRows.push(...shuffled.slice(0, codes.length));
  }

  codes.forEach((code, i) => {
    const hierarchyRow = meddraData.find((r) => r.llt_code === code);
    if (!hierarchyRow) return;
    meddraSample[targetRows[i]] = hierarchyRow;
  });
  return meddraSample;
}

// meddra型のAE項目にLLT名を格納する。default_valueが8桁数字の場合はllt_codeとみなし、
// 対応するllt_nameを固定値として使う。それ以外はmeddraSample(行ごとに対応する階層)のllt_nameを使う
// (Rのpopulate_meddra_fields()に対応。alias_nameによる絞り込みは行わない点もRと同じ)
function populateAeMeddraFields(ae, aeSpec, meddraData, meddraSample) {
  const existingColumns = new Set(Object.keys(ae[0] || {}));
  const meddraVars = [...new Set(aeSpec.filter((r) => r.field_type === "meddra").map((r) => r.cdisc_variable))].filter(
    (v) => !existingColumns.has(v)
  );

  meddraVars.forEach((varName) => {
    const fixedCodes = [
      ...new Set(
        aeSpec
          .filter((r) => r.field_type === "meddra" && r.cdisc_variable === varName && /^[0-9]{8}$/.test(r.default_value || ""))
          .map((r) => r.default_value)
      ),
    ];
    if (fixedCodes.length === 1) {
      const fixedRow = meddraData.find((r) => r.llt_code === fixedCodes[0]);
      const lltName = fixedRow ? fixedRow.llt_name : undefined;
      ae.forEach((row) => {
        row[varName] = lltName;
      });
    } else {
      ae.forEach((row, i) => {
        row[varName] = meddraSample[i].llt_name;
      });
    }
  });
  return ae;
}

// MedDRAコーディングブロック(LLT〜SOC)の列名(例: prefix="AE" -> AELLT, AELLTCD, ...)
const MEDDRA_CODING_COLS = ["LLT", "LLTCD", "DECOD", "PTCD", "HLT", "HLTCD", "HLGT", "HLGTCD", "BODSYS", "BDSYCD", "SOC", "SOCCD"];
const MEDDRA_CODING_FIELD_MAP = {
  LLT: "llt_name",
  LLTCD: "llt_code",
  DECOD: "pt_name",
  PTCD: "pt_code",
  HLT: "hlt_name",
  HLTCD: "hlt_code",
  HLGT: "hlgt_name",
  HLGTCD: "hlgt_code",
  BODSYS: "soc_name",
  BDSYCD: "soc_code",
  SOC: "soc_name",
  SOCCD: "soc_code",
};

// MedDRAコーディングブロック(LLT〜SOC)を追加する。meddraSampleと同じ階層を使い、
// コード間の対応関係を保つ(Rのadd_meddra_coding_block()に対応)
function addAeMeddraCodingBlock(ae, meddraSample, prefix) {
  MEDDRA_CODING_COLS.forEach((col) => {
    const field = MEDDRA_CODING_FIELD_MAP[col];
    ae.forEach((row, i) => {
      row[prefix + col] = meddraSample[i][field];
    });
  });
  return ae;
}

// dataの各行が持つalias_name(例: AEドメインの"ae"/"sae_report")について、excludePrefix以外に
// 同じalias_nameでフィールドを定義しているprefix(例: FA)がある場合、そのフィールドを同じ行に
// 直接追加する(FieldItem的な意味で「同じフォーム上の別ブロック」を表す)。
// これにより、"ae"シートのようにAE報告と同一フォーム上にあるFA項目が同じ行(=同じ報告インスタンス)として
// 扱われ、ブロックをまたぐpresence_conditions(例: FAOBJがAELLTCDを参照)がドメインをまたぐ結合なしに
// 正しく判定できるようになる。戻り値のlinkedSpecは、実際に追加したprefix/alias_nameの一覧
// (呼び出し側で、二重生成を避けるための除外や、後でsplitLinkedDomains()に分離する際に使う)
// (Rのpopulate_linked_blocks()に対応)
function populateLinkedBlocks(data, cdiscVariableValues, excludePrefix, registrationStartDate, meddraData, whoDrugIdf, dateRefBoundsAll) {
  const ownAliasNames = new Set(data.map((r) => r.alias_name));
  const linkedSpec = cdiscVariableValues.filter((r) => r.prefix !== excludePrefix && ownAliasNames.has(r.alias_name));

  if (linkedSpec.length === 0) {
    return { data, linkedSpec };
  }

  const drugNames = whoDrugIdf ? [...new Set(whoDrugIdf.map((r) => r.full_name_en).filter((v) => v != null))] : [];
  let linkedVars = [...new Set(linkedSpec.map((r) => r.cdisc_variable))];
  const doseChoices = ["50", "100", "150", "200", "250", "300", "400", "500"];
  const today = new Date().toISOString().slice(0, 10);

  // date型の変数同士が他フィールド参照で数珠つなぎに依存し合う場合(buildRepeatedDomain()と同じ理由)、
  // 参照先が先に生成されるよう並べ替える
  const linkedVarSet = new Set(linkedVars);
  const dateRefBounds = (dateRefBoundsAll || []).filter((r) => linkedVarSet.has(r.cdisc_variable));
  {
    const linkedDateVars = [...new Set(linkedSpec.filter((r) => r.field_type === "date").map((r) => r.cdisc_variable))].filter((v) =>
      linkedVars.includes(v)
    );
    const sortedDateVars = sortDateVarsByDependency(linkedDateVars, dateRefBounds);
    const sortedDateVarSet = new Set(sortedDateVars);
    linkedVars = [...linkedVars.filter((v) => !sortedDateVarSet.has(v)), ...sortedDateVars];
  }

  linkedVars.forEach((varName) => {
    const dateMinRow = dateRefBounds.find((r) => r.cdisc_variable === varName && r.bound_type === "min_date" && r.ref_cdisc_variable in data[0]);
    const dateMaxRow = dateRefBounds.find((r) => r.cdisc_variable === varName && r.bound_type === "max_date" && r.ref_cdisc_variable in data[0]);
    const varSpec = linkedSpec.filter((r) => r.cdisc_variable === varName);
    const specByAlias = new Map();
    varSpec.forEach((r) => {
      if (!specByAlias.has(r.alias_name)) {
        specByAlias.set(r.alias_name, { fieldType: r.field_type, defaultValue: r.default_value, codes: new Set(), isInvisibleAny: false, isRequiredAny: false });
      }
      const g = specByAlias.get(r.alias_name);
      g.codes.add(r.code != null ? r.code : r.default_value);
      if (r.is_invisible) g.isInvisibleAny = true;
      if (r.is_required) g.isRequiredAny = true;
    });

    data.forEach((row) => {
      row[varName] = null;
    });

    specByAlias.forEach((g, aliasName) => {
      const rows = data.filter((row) => row.alias_name === aliasName);
      if (rows.length === 0) return;

      let codes = [...g.codes];
      if (!g.isRequiredAny && !g.isInvisibleAny) {
        codes = [...new Set([...codes, ""])];
      }

      if (g.fieldType === "radio_button" || g.fieldType === "check_box") {
        if (codes.length === 0) return;
        if (g.fieldType === "check_box") {
          const values = sampleCheckBoxValuesWithCoverage(codes, rows.length);
          rows.forEach((row, i) => {
            row[varName] = values[i];
          });
        } else {
          const values = sampleValuesWithCoverage(codes, rows.length);
          rows.forEach((row, i) => {
            row[varName] = values[i];
          });
        }
      } else if (g.fieldType === "date") {
        rows.forEach((row) => {
          let lower = registrationStartDate;
          // 明示的なmin_date参照(dateMinRow)があっても、labelを跨ぐ連鎖等で行によっては参照先の値が
          // まだ無いことがある。そのような行にだけRFSTDTCをデフォルト下限として補う(参照値がある行では、
          // その変数本来の意味を尊重してRFSTDTCは加えない)。ref('sheet_alias', N)+N.days/-N.daysの
          // 符号付き日数オフセット(dateMinRow.offset_days。無指定ならnull=0として扱う)を参照先の値に加味する
          const rawMinRefVal = dateMinRow != null ? row[dateMinRow.ref_cdisc_variable] : null;
          const refVal = rawMinRefVal != null && dateMinRow.offset_days ? addDaysToDateString(rawMinRefVal, dateMinRow.offset_days) : rawMinRefVal;
          if (refVal == null && row.RFSTDTC != null && row.RFSTDTC > lower) lower = row.RFSTDTC;
          if (refVal != null && refVal > lower) {
            lower = refVal;
          }
          let upper = today;
          const rawMaxRefVal = dateMaxRow != null ? row[dateMaxRow.ref_cdisc_variable] : null;
          const maxRefVal = rawMaxRefVal != null && dateMaxRow.offset_days ? addDaysToDateString(rawMaxRefVal, dateMaxRow.offset_days) : rawMaxRefVal;
          if (maxRefVal != null && maxRefVal < upper) {
            upper = maxRefVal;
          }
          if (upper < lower) upper = lower;
          row[varName] = randomDateBetween(lower, upper);
        });
      } else if (g.fieldType === "meddra") {
        const dv = g.defaultValue;
        if (dv != null && /^[0-9]{8}$/.test(dv)) {
          const hit = meddraData.find((r) => r.llt_code === dv);
          const lltName = hit ? hit.llt_name : null;
          rows.forEach((row) => {
            row[varName] = lltName;
          });
        } else {
          const sample = sampleMeddraRows(meddraData, rows.length);
          rows.forEach((row, i) => {
            row[varName] = sample[i].llt_name;
          });
        }
      } else if (g.fieldType === "drug") {
        const dv = g.defaultValue;
        let fixedName = null;
        if (dv != null && /^[0-9]+$/.test(dv) && whoDrugIdf) {
          const hit = whoDrugIdf.find((r) => r.drug_code === dv && r.full_name_en != null);
          if (hit) fixedName = hit.full_name_en;
        }
        if (fixedName != null) {
          rows.forEach((row) => {
            row[varName] = fixedName;
          });
        } else if (drugNames.length > 0) {
          rows.forEach((row) => {
            row[varName] = sampleOne(drugNames);
          });
        }
      } else if (/DOSE$/.test(varName)) {
        rows.forEach((row) => {
          row[varName] = sampleOne(doseChoices);
        });
      } else {
        rows.forEach((row) => {
          row[varName] = "DUMMY";
        });
      }
    });
  });

  return { data, linkedSpec };
}

// populateLinkedBlocks()で同じ行に追加した列を、prefixごとの別テーブルに分離する。
// sourceSpidCol(例: AESPID)の値をそのままprefixSPID(例: FASPID)として引き継ぐことで、
// どのAE報告インスタンスに対応するリンク行かが分かるようにする(Rのsplit_linked_domains()に対応)
function splitLinkedDomains(data, linkedSpec, sourceSpidCol) {
  if (!linkedSpec || linkedSpec.length === 0) return {};

  const prefixes = [...new Set(linkedSpec.map((r) => r.prefix))];
  const result = {};
  prefixes.forEach((px) => {
    const pxAliasNames = new Set(linkedSpec.filter((r) => r.prefix === px).map((r) => r.alias_name));
    const pxVars = [...new Set(linkedSpec.filter((r) => r.prefix === px).map((r) => r.cdisc_variable))];
    const spidVar = `${px}SPID`;
    result[px] = data
      .filter((row) => pxAliasNames.has(row.alias_name))
      .map((row) => {
        const newRow = { STUDYID: row.STUDYID, DOMAIN: px, USUBJID: row.USUBJID, alias_name: row.alias_name };
        newRow[spidVar] = row[sourceSpidCol];
        pxVars.forEach((v) => {
          if (v in row) newRow[v] = row[v];
        });
        return newRow;
      });
  });
  return result;
}

// radio_button/check_box/date/meddra型のいずれでも埋まらなかった対象変数(specに定義はあるが
// まだ値の無い列)に、とりあえずDUMMY値を格納する(Rのpopulate_dummy_fields()に対応)。
// presence_conditionsが後段でこれらの列を参照する場合があるため、ゲーティング適用前に列自体は
// 必ず埋めておく必要がある
function populateAeDummyFields(ae, aeSpec) {
  const existingColumns = new Set(Object.keys(ae[0] || {}));
  const remainingVars = [...new Set(aeSpec.map((r) => r.cdisc_variable))].filter((v) => !existingColumns.has(v));
  remainingVars.forEach((varName) => {
    ae.forEach((row) => {
      row[varName] = "DUMMY";
    });
  });
  return ae;
}

// AETOXGR=="5"(死亡)のAEENDTC(被験者ごとの最も早い日)より後にAESTDTCが始まる他のAEレコードは、
// 死亡後に新たな有害事象が発生したことになり矛盾するため除外する(Rのpopulate_ae_domain()の
// 該当部分に対応)
function filterAeDeathDateConsistency(ae) {
  if (!ae[0] || !("AETOXGR" in ae[0]) || !("AESTDTC" in ae[0]) || !("AEENDTC" in ae[0])) return ae;
  const deathDateByUsubjid = {};
  ae.forEach((row) => {
    if (row.AETOXGR !== "5") return;
    const current = deathDateByUsubjid[row.USUBJID];
    if (current == null || row.AEENDTC < current) {
      deathDateByUsubjid[row.USUBJID] = row.AEENDTC;
    }
  });
  return ae.filter((row) => {
    const deathDate = deathDateByUsubjid[row.USUBJID];
    return deathDate == null || row.AESTDTC <= deathDate;
  });
}

// USUBJIDごとの死亡日テーブル(AETOXGR=="5"のレコードのうち、最も早いAEENDTC)を作る。
// DSドメインのDEATH確定(finalizeDsDisposition)で使う(Rのbuild_death_date_table()に対応)
function buildDeathDateTable(ae) {
  const byUsubjid = {};
  ae.forEach((row) => {
    if (row.AETOXGR !== "5") return;
    if (!byUsubjid[row.USUBJID] || row.AEENDTC < byUsubjid[row.USUBJID]) {
      byUsubjid[row.USUBJID] = row.AEENDTC;
    }
  });
  return Object.keys(byUsubjid).map((usubjid) => ({ USUBJID: usubjid, DTHDTC: byUsubjid[usubjid] }));
}

// AESPID(USUBJID内の連番、例: sae_report1, sae_report2)・AESEQ(全体通番)を付与し、
// populateLinkedBlocks()で同じ行に追加した他prefix(例: FA)の列をsplitLinkedDomains()で
// 断片テーブルに分離してから、alias_name・リンク先prefixの列を除去したうえで、
// 列順を STUDYID/DOMAIN/USUBJID/AESEQ/AESPID -> meddra項目 -> MedDRAコーディングブロック ->
// その他 -> AETOXGR/AESTDTC/AEENDTC に整理する。
// 戻り値は{ ae, linked }(linkedはsplitLinkedDomains()の結果。prefixをキーにしたオブジェクト)
// (Rのpopulate_ae_domain()末尾のAESPID/AESEQ付与・split_linked_domains()・reorder_domain_columns()に対応)
function finalizeAeDomain(ae, aeSpec, linkedSpec) {
  const usubjidCounters = {};
  ae.forEach((row) => {
    usubjidCounters[row.USUBJID] = (usubjidCounters[row.USUBJID] || 0) + 1;
    row.AESPID = `${row.alias_name}${usubjidCounters[row.USUBJID]}`;
  });

  // AESEQはUSUBJID・AESTDTC・AESPIDの昇順で振る(同日にAETOXGR=="5"(死亡)と他のAEがある場合の
  // 前後関係は問わない)
  ae = [...ae].sort((a, b) => {
    if (a.USUBJID !== b.USUBJID) return a.USUBJID < b.USUBJID ? -1 : 1;
    const aDtc = a.AESTDTC || "";
    const bDtc = b.AESTDTC || "";
    if (aDtc !== bDtc) return aDtc < bDtc ? -1 : 1;
    if (a.AESPID !== b.AESPID) return a.AESPID < b.AESPID ? -1 : 1;
    return 0;
  });
  ae.forEach((row, i) => {
    row.AESEQ = i + 1;
  });

  const linked = splitLinkedDomains(ae, linkedSpec || [], "AESPID");
  const linkedVars = new Set((linkedSpec || []).map((r) => r.cdisc_variable));
  ae.forEach((row) => {
    delete row.alias_name;
    delete row.RFSTDTC;
    linkedVars.forEach((v) => delete row[v]);
  });

  const meddraVars = [...new Set(aeSpec.filter((r) => r.field_type === "meddra").map((r) => r.cdisc_variable))];
  const frontCols = ["STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AESPID", ...meddraVars, ...MEDDRA_CODING_COLS.map((c) => "AE" + c)];
  const endCols = ["AETOXGR", "AESTDTC", "AEENDTC"];
  const allCols = Object.keys(ae[0] || {});
  const middleCols = allCols.filter((c) => !frontCols.includes(c) && !endCols.includes(c));
  const orderedCols = [
    ...frontCols.filter((c) => allCols.includes(c)),
    ...middleCols,
    ...endCols.filter((c) => allCols.includes(c)),
  ];

  const finalAe = ae.map((row) => {
    const newRow = {};
    orderedCols.forEach((c) => {
      newRow[c] = row[c];
    });
    return newRow;
  });

  return { ae: finalAe, linked };
}

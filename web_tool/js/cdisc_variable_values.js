// edc_spec(パース済みJSON)から、cdisc_variable_values(選択肢展開済みの配列)を組み立てる。
// R版のbuild_cdisc_variable_values.Rに対応する

// 1シート分のfield_itemsとcdisc_sheet_configsを結合し、
// (prefix, cdisc_variable, alias_name, label, field_type, default_value, is_invisible, option_name)の配列にする
function buildCdiscSheetConfigTable(sheet) {
  const fieldItems = {};
  (sheet.field_items || []).forEach((item) => {
    fieldItems[item.name] = item;
  });

  const rows = [];
  (sheet.cdisc_sheet_configs || []).forEach((config) => {
    const prefix = config.prefix || "";
    const table = config.table || {};
    Object.keys(table).forEach((fieldName) => {
      const value = table[fieldName];
      if (value == null || String(value).startsWith("_")) return;
      const item = fieldItems[fieldName];
      if (!item) return;
      // prefixがDMの場合、またはvalueがVISITNUM/SPDEVIDの場合は、prefixを付けずそのままcdisc_variable名にする
      // (build_cdisc_variable_values.Rのcase_whenに対応)
      const cdiscVariable = prefix === "DM" || value === "VISITNUM" || value === "SPDEVID" ? value : prefix + value;
      rows.push({
        prefix,
        cdisc_variable: cdiscVariable,
        alias_name: sheet.alias_name,
        field: fieldName,
        label: config.label,
        // field_type=="select"(EDC上のプルダウン)は、以降の選択肢処理(populateGenericChoiceFields()等)で
        // radioButtonと同じ「単一選択のコードリスト」として扱う。ここで正規化しておくことで、
        // 個々の判定箇所(fieldType === "radio_button" || fieldType === "check_box")を毎回書き換えずに済む
        // (Rのbuild_cdisc_variable_values.Rに対応)。field_type=="datetime"も同様、時刻要素は無視して
        // dateと同じ日付生成パイプラインで扱う(個々の日付判定箇所を書き換えずに済む)
        field_type: item.field_type === "select" ? "radio_button" : item.field_type === "datetime" ? "date" : item.field_type,
        default_value: item.default_value,
        is_invisible: !!item.is_invisible,
        option_name: item.option_name || null,
      });
    });
  });
  return rows;
}

// edc_specの全シートを対象に、buildCdiscSheetConfigTable()を連結したもの(選択肢展開前のfield単位の生データ)。
// Rのdf_cdisc(build_cdisc_variable_values.R)に対応し、presence_conditions等の制約テーブル構築時に
// (alias_name, field)からcdisc_variable/label/prefix/field_typeを引くのに使う
function buildDfCdisc(edcSpec) {
  const sheets = edcSpec.sheets || [];
  const rows = [];
  sheets.forEach((sheet) => {
    buildCdiscSheetConfigTable(sheet).forEach((row) => rows.push(row));
  });
  return rows;
}

// edc_spec.optionsを(option_name, code, value_name)の配列に展開する(is_usable=falseは除く)
function buildOptionValues(edcSpec) {
  const rows = [];
  (edcSpec.options || []).forEach((opt) => {
    (opt.values || []).forEach((v) => {
      if (v.is_usable === false) return;
      rows.push({
        option_name: opt.name,
        code: v.code,
        value_name: v.name,
      });
    });
  });
  return rows;
}

// edc_spec.sheet_orders(alias_name -> シート表示順のseq)から、alias_name -> sheet_seqの索引を作る
function buildSheetSeqLookup(edcSpec) {
  const lookup = {};
  (edcSpec.sheet_orders || []).forEach((row) => {
    lookup[row.sheet] = row.seq;
  });
  return lookup;
}

// cdisc_variable_values本体を組み立てる。option_nameを持つ行(radio_button/check_box等)は、
// 対応する選択肢の数だけ複製し、それぞれにcodeを持たせる(Rのleft_join(options, by=c("option_name","is_invisible"))に相当)。
// R側のoptionsテーブルは常にis_invisible=falseとして扱われるため、フィールド自身がis_invisible=trueの場合は
// (is_invisibleが一致せず結合できないため)選択肢展開されず、code=nullの1行のままになる。
// この非表示フィールドの挙動もRと合わせて再現する。
// sheet_seq(そのalias_nameのシート表示順)も付与する(DSドメインのEPOCH展開順などで使う)
function buildCdiscVariableValues(edcSpec) {
  const sheets = edcSpec.sheets || [];
  const optionValues = buildOptionValues(edcSpec);
  const optionsByName = {};
  optionValues.forEach((row) => {
    if (!optionsByName[row.option_name]) optionsByName[row.option_name] = [];
    optionsByName[row.option_name].push(row);
  });
  const sheetSeqLookup = buildSheetSeqLookup(edcSpec);

  const cdiscVariableValues = [];
  sheets.forEach((sheet) => {
    buildCdiscSheetConfigTable(sheet).forEach((row) => {
      const withSeq = { ...row, sheet_seq: sheetSeqLookup[row.alias_name] };
      const opts = withSeq.option_name && !withSeq.is_invisible ? optionsByName[withSeq.option_name] : null;
      if (opts && opts.length > 0) {
        opts.forEach((opt) => {
          cdiscVariableValues.push({ ...withSeq, code: opt.code });
        });
      } else {
        cdiscVariableValues.push({ ...withSeq, code: null });
      }
    });
  });
  return cdiscVariableValues;
}

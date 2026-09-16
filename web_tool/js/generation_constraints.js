// EDC仕様のvalidate_presence_if等(field_itemsのvalidators)から、presence_conditions等の
// 生成制約テーブル一式を組み立て、ドメインデータに適用する。
// R版のbuild_validator_table.R/build_generation_constraints.R/build_domain_common.Rの
// apply_presence_conditions()・apply_age_date_bounds()に対応する

// --- build_validator_table.R相当 ---

// sheetsのfield_items.validatorsを(alias_name, field_name, validator_type, validator_key, value)の
// 縦持り配列にする(Rのbuild_validator_table_raw()に対応)
function buildValidatorTableRaw(sheets) {
  const rows = [];
  (sheets || []).forEach((sheet) => {
    (sheet.field_items || []).forEach((field) => {
      const validators = field.validators || {};
      Object.keys(validators).forEach((validatorType) => {
        const rules = validators[validatorType] || {};
        const keys = Object.keys(rules);
        if (keys.length === 0) {
          rows.push({ alias_name: sheet.alias_name, field_name: field.name, validator_type: validatorType, validator_key: null, value: null });
        } else {
          keys.forEach((validatorKey) => {
            const raw = rules[validatorKey];
            const value = Array.isArray(raw) ? raw.map(String).join(", ") : raw == null ? null : String(raw);
            rows.push({ alias_name: sheet.alias_name, field_name: field.name, validator_type: validatorType, validator_key: validatorKey, value });
          });
        }
      });
    });
  });
  return rows;
}

function classifyBoundType(validatorType, validatorKey) {
  if (validatorType === "date" && validatorKey === "validate_date_after_or_equal_to") return "min_date";
  if (validatorType === "date" && validatorKey === "validate_date_before_or_equal_to") return "max_date";
  if (validatorType === "numericality" && validatorKey === "validate_numericality_greater_than_or_equal_to") return "min_value";
  if (validatorType === "numericality" && validatorKey === "validate_numericality_less_than_or_equal_to") return "max_value";
  return null;
}

function extractNumericValue(validatorType, value) {
  // Number("")は0を返してしまう(RのNAとは異なりJSは空文字列を数値として扱えるため)。
  // 値未設定のバリデータ行を上限/下限0として誤って扱わないよう、空文字列もnullとして除外する
  if (validatorType !== "numericality" || value == null || value === "") return null;
  const n = Number(value);
  return Number.isNaN(n) ? null : n;
}

// "field3"のような同一シート内の別フィールド参照、または"f84 +1.day"のような日数オフセット付きの
// 同一シート内参照(EDC仕様上25箇所で使用、いずれも"+1.day(s)"のみ)の場合、参照先フィールド名を
// 取り出す。日数オフセット自体は下限として厳密には反映しない(populateDateFields等はref_field自身の
// 値をそのまま下限にする。+1日分だけ緩い下限になるが、参照が完全に無視されるよりは実態に即しており、
// この差はcheckDateAfterVarBeforeToday等の>=判定には影響しない)
function extractRefField(validatorType, value) {
  if (validatorType !== "date" || value == null) return null;
  if (/^field[0-9]+$/.test(value)) return value;
  if (/^f[0-9]+$/.test(value)) return `field${value.slice(1)}`;
  const m = value.match(/^f([0-9]+)\s*\+\s*[0-9]+\.days?$/);
  if (m) return `field${m[1]}`;
  return null;
}

// valueが"ref('sheet_alias', N)"のような他シート参照の場合(date型バリデータの
// validate_date_after_or_equal_to/validate_date_before_or_equal_toで使われる形。presence/formula側の
// ref('sheet_alias', N)=='値'とは異なり、値の比較を伴わない単独のref()呼び出し)、参照先のシート
// (alias_name)とフィールド名を取り出す(Rのextract_date_cross_ref_alias/extract_date_cross_ref_fieldに対応)。
// "ref('maitenance',829)-28.days"のような日数オフセット付きの他シート参照(EDC仕様上使用例あり)にも
// 対応する。オフセット(符号+日数)はextractDateCrossRefOffsetDays()で取り出し、
// buildGenerationConstraints()のdateRefBoundsに反映する(参照先フィールドの値にオフセットを
// 加減した値を下限/上限として使う)
const DATE_CROSS_REF_PATTERN = /^\s*ref\('([^']+)'\s*,\s*([0-9]+)\)\s*(?:([+-])\s*([0-9]+)\.days?)?\s*$/;

function extractDateCrossRefAlias(validatorType, value) {
  if (validatorType !== "date" || value == null) return null;
  const m = value.match(DATE_CROSS_REF_PATTERN);
  return m ? m[1] : null;
}

function extractDateCrossRefField(validatorType, value) {
  if (validatorType !== "date" || value == null) return null;
  const m = value.match(DATE_CROSS_REF_PATTERN);
  return m ? `field${m[2]}` : null;
}

// ref('sheet_alias', N)+150.days / ref('sheet_alias', N)-28.daysの符号付き日数オフセットを
// 数値(例: 150, -28)で取り出す。オフセットが無い場合はnull
function extractDateCrossRefOffsetDays(validatorType, value) {
  if (validatorType !== "date" || value == null) return null;
  const m = value.match(DATE_CROSS_REF_PATTERN);
  if (!m || m[4] == null) return null;
  return (m[3] === "-" ? -1 : 1) * Number(m[4]);
}

// value(例: field2=='ADVERSE EVENT'、f4=='Y' || f4=='N'、field6=="Y")を"||"で分割し、
// 全断片が同一フィールドに対するfieldN==値(またはfN==値)の形であれば、フィールド名と値の一覧を返す。
// 異なるフィールドが混ざる、またはパースできない断片があればnull(Rのparse_presence_or_conditions()に対応)
const PRESENCE_OR_FRAGMENT_RE = /^(?:field|f)([0-9]+)\s*==\s*(?:'([^']*)'|"([^"]*)"|(\S+))$/;
function parsePresenceOrConditions(value) {
  const fragments = value.split("||").map((s) => s.trim());
  const matches = fragments.map((f) => f.match(PRESENCE_OR_FRAGMENT_RE));
  if (matches.some((m) => m === null)) return null;
  const fieldNums = [...new Set(matches.map((m) => m[1]))];
  if (fieldNums.length !== 1) return null;
  const values = matches.map((m) => (m[2] !== undefined ? m[2] : m[3] !== undefined ? m[3] : m[4]));
  return { field: `field${fieldNums[0]}`, values };
}

function computePresenceRefFieldAndValue(validatorType, validatorKey, value) {
  if (validatorType !== "presence" || validatorKey !== "validate_presence_if" || value == null) {
    return { field: null, value: null };
  }
  const parsed = parsePresenceOrConditions(value);
  if (!parsed) return { field: null, value: null };
  return { field: parsed.field, value: parsed.values.join(", ") };
}

// value(例: STAT.blank?、ORRES.present?)が"接尾辞.blank?"/"接尾辞.present?"の形かどうかを判定する。
// validateFormulaIfは値の妥当性検証(フィールドに値がある場合にその値が満たすべき条件)であり、
// 提示可否(ゲーティング)の意味を持たないため、validatePresenceIfのみを対象にする(validateFormulaIfの
// 複雑な式から断片だけを誤って提示条件として抽出してしまうバグがあったため)
const PRESENCE_PREDICATE_RE = /^([A-Za-z_][A-Za-z0-9_]*)\.(blank|present)\?$/;
function extractPresencePredicate(validatorKey, value) {
  if (validatorKey !== "validate_presence_if" || value == null) {
    return { suffix: null, type: null };
  }
  const m = value.match(PRESENCE_PREDICATE_RE);
  if (!m) return { suffix: null, type: null };
  return { suffix: m[1], type: m[2] };
}

// f18<=3のような、自分自身のフィールドに対する単一数値比較(formula)を解釈する
const FORMULA_SINGLE_FIELD_RE = /^f([0-9]+)\s*(<=|>=|==|<|>)\s*(-?[0-9]+(?:\.[0-9]+)?)$/;
function boundTypeFromOperator(operator) {
  if (operator === "<=" || operator === "<") return "max_value";
  if (operator === ">=" || operator === ">") return "min_value";
  if (operator === "==") return "exact_value";
  return null;
}
function computeFormulaSingleField(validatorType, validatorKey, value) {
  if (validatorType !== "formula" || validatorKey !== "validate_formula_if" || value == null) {
    return { refField: null, boundType: null, boundValue: null };
  }
  const m = value.match(FORMULA_SINGLE_FIELD_RE);
  if (!m) return { refField: null, boundType: null, boundValue: null };
  return { refField: `field${m[1]}`, boundType: boundTypeFromOperator(m[2]), boundValue: Number(m[3]) };
}

// f350<=f59のような、同一シート内の別フィールドとの比較(formula)を解釈する
const FORMULA_FIELD_REF_RE = /^f([0-9]+)\s*(<=|>=|==|<|>)\s*f([0-9]+)$/;
function computeFormulaFieldRef(validatorType, validatorKey, value) {
  if (validatorType !== "formula" || validatorKey !== "validate_formula_if" || value == null) {
    return { refField: null, boundType: null };
  }
  const m = value.match(FORMULA_FIELD_REF_RE);
  if (!m) return { refField: null, boundType: null };
  return { refField: `field${m[3]}`, boundType: boundTypeFromOperator(m[2]) };
}

// age(f2, f3)>=20 && age(f2, f3)<=80のような年齢条件を解釈する
const AGE_CONDITION_RE = /^age\(\s*f([0-9]+)\s*,\s*f([0-9]+)\s*\)\s*(>=|<=)\s*([0-9]+(?:\.[0-9]+)?)$/;
function parseAgeCondition(value) {
  const clauses = value.split("&&").map((s) => s.trim());
  const matches = clauses.map((c) => c.match(AGE_CONDITION_RE));
  if (matches.some((m) => m === null)) return null;
  const fieldPairs = [...new Set(matches.map((m) => `${m[1]}-${m[2]}`))];
  if (fieldPairs.length !== 1) return null;
  const geClause = matches.find((m) => m[3] === ">=");
  const leClause = matches.find((m) => m[3] === "<=");
  return {
    field1: `field${matches[0][1]}`,
    field2: `field${matches[0][2]}`,
    minAge: geClause ? Number(geClause[4]) : null,
    maxAge: leClause ? Number(leClause[4]) : null,
  };
}
function extractAgeCondition(fieldName, validatorKey, value) {
  if (validatorKey !== "validate_formula_if" || value == null) return { ageRefField: null, minAge: null, maxAge: null };
  const parsed = parseAgeCondition(value);
  if (!parsed) return { ageRefField: null, minAge: null, maxAge: null };
  let otherField = null;
  if (parsed.field1 === fieldName) otherField = parsed.field2;
  else if (parsed.field2 === fieldName) otherField = parsed.field1;
  if (!otherField) return { ageRefField: null, minAge: null, maxAge: null };
  return { ageRefField: otherField, minAge: parsed.minAge, maxAge: parsed.maxAge };
}

// age(ref('sheet1', N1), ref('sheet2', N2)) OP 閾値 のような、別シートの2つの日付フィールドの
// 年齢差でこのフィールド自身の提示可否をゲーティングする条件(validate_presence_if)を解釈する。
// 上記のage(fN, fM)(同一シート内、フィールド自身の値をageBoundsで直接束縛する用途)とは別に、
// ref('sheet', N)形式(別シート参照、フィールド自身とは無関係な2つの日付の年齢差で提示可否を
// ゲーティングする用途。例: FASTATがage(初発診断日, 生年月日)>39のときだけ提示される)を扱う。
// &&で他条件と組み合わさっている場合は非対応(null)(Rのparse_age_ref_condition()に対応)
const AGE_REF_CONDITION_RE = /^\(?\s*age\(\s*ref\('([^']+)'\s*,\s*([0-9]+)\)\s*,\s*ref\('([^']+)'\s*,\s*([0-9]+)\)\)\s*(>=|<=|>|<)\s*([0-9]+(?:\.[0-9]+)?)\s*\)?$/;
function parseAgeRefCondition(value) {
  const m = value.match(AGE_REF_CONDITION_RE);
  if (!m) return null;
  return {
    ref1AliasName: m[1],
    ref1Field: `field${m[2]}`,
    ref2AliasName: m[3],
    ref2Field: `field${m[4]}`,
    operator: m[5],
    threshold: Number(m[6]),
  };
}

// value(例: (STAT.blank?) && (ref('registration', 4)=='F'))を"&&"で分割し、各断片の括弧を除いた文字列にする
function parseAndClauses(value) {
  if (!value.includes("&&")) return null;
  return value.split("&&").map((s) => s.trim().replace(/^\(/, "").replace(/\)$/, "").trim());
}

const CROSS_REF_RE = /^ref\('([^']+)'\s*,\s*([0-9]+)\)\s*==\s*(?:'([^']*)'|"([^"]*)"|([^\s|&()]+))$/;
const AND_FIELD_REF_RE = /^(?:field|f)([0-9]+)\s*==\s*(?:'([^']*)'|"([^"]*)"|([^\s|&()]+))$/;
// fieldN==fieldM(または fN==fM)のように、値側もフィールド参照の形。AND_FIELD_REF_REは値側を
// 「引用符無しの単純リテラル」として扱うため、これを先に判定しておかないと"fN"という文字列そのものと
// 一致するかのリテラル条件として誤解釈されてしまう(この形は「別フィールドの値をそのままコピーする」
// という意味で、extractFieldEqualityRef()による別のcopy機構で扱われるため、ここでは何もしない扱いにする)
const AND_FIELD_EQUALITY_RE = /^(?:field|f)([0-9]+)\s*==\s*(?:field|f)([0-9]+)$/;
// fieldN>=数値(または fN>=数値)のように、同一シート内の別フィールドの値を数値として不等号比較する形。
// 例: "f16>=2&&STAT.blank?"(骨壊死のGrade(field16)が2以上のときだけ、かつSTATが空欄のときだけ提示)
const AND_FIELD_NUMERIC_CMP_RE = /^(?:field|f)([0-9]+)\s*(>=|<=|>|<)\s*(-?[0-9]+(?:\.[0-9]+)?)$/;

// parseAndClauses()で分割した1断片を種類ごとに分類する(Rのclassify_and_clause()に対応)
//   - "fieldN==fieldM"のような、値側もフィールド参照のコピー条件
//     -> kind="field_equality_skip"(別のcopy機構(extractFieldEqualityRef)で扱われるため、
//        ここではpresenceConditions行を作らない)
//   - "fieldN>=数値"のような、同一シート内の別フィールドの値との数値不等号比較 -> kind="field_numeric_cmp"
//   - "fieldN==2 || fieldN==3 || ..."のような、断片自体が同一フィールドに対するOR条件
//     (例: (field22==2||field22==3||...) && (field348=='CR'||field348=='PR'))
//     -> kind="field_ref_or"(parsePresenceOrConditions()を再利用し、複数のexpected_valueを持つ)
function classifyAndClause(clause) {
  const mPred = clause.match(PRESENCE_PREDICATE_RE);
  if (mPred) return { kind: "predicate", suffix: mPred[1], predicateType: mPred[2] };
  const mRef = clause.match(CROSS_REF_RE);
  if (mRef) return { kind: "cross_ref", refAliasName: mRef[1], refField: `field${mRef[2]}`, value: mRef[3] ?? mRef[4] ?? mRef[5] };
  const mEq = clause.match(AND_FIELD_EQUALITY_RE);
  if (mEq) return { kind: "field_equality_skip" };
  const mField = clause.match(AND_FIELD_REF_RE);
  if (mField) return { kind: "field_ref", refField: `field${mField[1]}`, value: mField[2] ?? mField[3] ?? mField[4] };
  const mNum = clause.match(AND_FIELD_NUMERIC_CMP_RE);
  if (mNum) return { kind: "field_numeric_cmp", refField: `field${mNum[1]}`, operator: mNum[2], threshold: Number(mNum[3]) };
  const orParsed = parsePresenceOrConditions(clause);
  if (orParsed) return { kind: "field_ref_or", refField: orParsed.field, values: orParsed.values };
  return null;
}

function parseAndConditions(value) {
  const clauses = parseAndClauses(value);
  if (!clauses) return null;
  const parsed = clauses.map(classifyAndClause);
  if (parsed.some((p) => p === null)) return null;
  return parsed;
}

// valueのどこかにref('sheet_alias', N)=='値'という断片が含まれていれば、その最初の1箇所を抽出する
// (Rのextract_cross_ref_clause()に対応、&&を伴わない単独ref()や、他が複雑な式のフォールバック用)
const CROSS_REF_LOOSE_RE = /ref\('([^']+)'\s*,\s*([0-9]+)\)\s*==\s*(?:'([^']*)'|"([^"]*)"|([^\s|&()]+))/;
function extractCrossRefClause(value) {
  const m = value.match(CROSS_REF_LOOSE_RE);
  if (!m) return null;
  return { refAliasName: m[1], refField: `field${m[2]}`, value: m[3] ?? m[4] ?? m[5] };
}

// valueのどこかにfieldN==fieldM(またはfN==fM)というフィールド同士の等号比較が含まれていれば、
// field_name自身ではないもう一方のフィールド名を返す(Rのextract_field_equality_ref()に対応)
const FIELD_EQUALITY_LOOSE_RE = /(?:field|f)([0-9]+)\s*==\s*(?:field|f)([0-9]+)/;
function extractFieldEqualityRef(fieldName, value) {
  const m = value.match(FIELD_EQUALITY_LOOSE_RE);
  if (!m) return null;
  const field1 = `field${m[1]}`;
  const field2 = `field${m[2]}`;
  if (field1 === fieldName) return field2;
  if (field2 === fieldName) return field1;
  return null;
}

// sheetsから、resolved_value/bound_type/ref_field/numeric_value/presence_ref_field/presence_ref_value/
// presence_predicate_suffix/presence_predicate_type/age_ref_field/min_age/max_ageまで付与した
// validator_tableを組み立てる(Rのbuild_validator_table()に対応)
function buildValidatorTable(sheets) {
  const raw = buildValidatorTableRaw(sheets);
  return raw.map((row) => {
    const { validator_type: validatorType, validator_key: validatorKey, value, field_name: fieldName } = row;

    const formulaSingle = computeFormulaSingleField(validatorType, validatorKey, value);
    const formulaFieldRef = computeFormulaFieldRef(validatorType, validatorKey, value);
    const boundType = classifyBoundType(validatorType, validatorKey) ?? formulaSingle.boundType ?? formulaFieldRef.boundType;
    const refField = extractRefField(validatorType, value) ?? extractDateCrossRefField(validatorType, value) ?? formulaSingle.refField ?? formulaFieldRef.refField;
    const dateRefAliasName = extractDateCrossRefAlias(validatorType, value);
    const dateRefOffsetDays = extractDateCrossRefOffsetDays(validatorType, value);
    const numericValue = extractNumericValue(validatorType, value) ?? formulaSingle.boundValue ?? null;

    const presence = computePresenceRefFieldAndValue(validatorType, validatorKey, value);
    const predicate = extractPresencePredicate(validatorKey, value);
    const age = extractAgeCondition(fieldName, validatorKey, value);

    return {
      ...row,
      bound_type: boundType,
      ref_field: refField,
      date_ref_alias_name: dateRefAliasName,
      date_ref_offset_days: dateRefOffsetDays,
      numeric_value: numericValue,
      presence_ref_field: presence.field,
      presence_ref_value: presence.value,
      presence_predicate_suffix: predicate.suffix,
      presence_predicate_type: predicate.type,
      age_ref_field: age.ageRefField,
      min_age: age.minAge,
      max_age: age.maxAge,
    };
  });
}

// field_items内のtype=="FieldItem::Reference"な要素を(alias_name, field_name, reference_type, reference_field)の
// 配列にする(Rのbuild_field_reference_table()に対応)
function buildFieldReferenceTable(sheets) {
  const rows = [];
  (sheets || []).forEach((sheet) => {
    (sheet.field_items || []).forEach((item) => {
      if (item.type === "FieldItem::Reference") {
        // reference_fieldは"field48"のような素の名前の場合と、"baseline1.field48"のように
        // 自分自身のalias_name付きの場合がある(EDC仕様側の出力形式の違いによる)。reference_type=="sheet"
        // (同じシート内参照)は必ず自分自身のalias_name内のフィールドを指すため、プレフィックスが
        // 付いていれば取り除いて常に素のフィールド名に正規化する(付いていないと、このあとの
        // buildFieldReferenceCopyConditions()側の突き合わせ(alias_name+素のfield名)が常に失敗し、
        // このフィールドの値コピーが機能しなくなる)
        const ownPrefix = `${sheet.alias_name}.`;
        const referenceField =
          item.reference_field != null && item.reference_field.startsWith(ownPrefix)
            ? item.reference_field.slice(ownPrefix.length)
            : item.reference_field;
        rows.push({
          alias_name: sheet.alias_name,
          field_name: item.name,
          reference_type: item.reference_type,
          reference_field: referenceField,
        });
      }
    });
  });
  return rows;
}

// --- build_generation_constraints.R相当 ---

// dfCdisc((alias_name, field, cdisc_variable, label, prefix, field_type)を含む行の配列。
// cdisc_variable_values.jsのbuildDfCdisc()が返す形)から、(alias_name, field) -> 候補一覧の索引を作る。
// Rは(alias_name,field,cdisc_variable)等を個別にdistinct/left_joinするが、実データでは1つの(alias_name,field)は
// 通常1つの(cdisc_variable,label,prefix,field_type)の組にしか対応しないため、df_cdisc本来の行の組をそのまま
// 候補として保持する(行ごとに完結した組を使うことで、個別joinの組み合わせ爆発を避ける)
function buildFieldLookup(dfCdisc) {
  const map = {};
  dfCdisc.forEach((row) => {
    const key = `${row.alias_name} ${row.field}`;
    if (!map[key]) map[key] = [];
    const exists = map[key].some(
      (t) => t.cdisc_variable === row.cdisc_variable && t.label === row.label && t.prefix === row.prefix && t.field_type === row.field_type
    );
    if (!exists) {
      map[key].push({ cdisc_variable: row.cdisc_variable, label: row.label, prefix: row.prefix, field_type: row.field_type });
    }
  });
  return map;
}
function lookupField(fieldLookup, aliasName, field) {
  if (field == null) return [];
  return fieldLookup[`${aliasName} ${field}`] || [];
}

// validate_presence_ifの単純なfieldN==値(OR可)から、condition_type="equals"のpresence_conditions行を作る。
// 参照先(presence_ref_field)がmeddra型の場合、値はLLT名ではなくLLTコードとの比較を意図しているため、
// ref_cdisc_variableをMedDRAコーディングブロックのコード列(prefixLLTCD)に差し替える
function buildEqualsPresenceConditions(validatorTable, fieldLookup) {
  const seen = new Set();
  const rows = [];
  validatorTable.forEach((vr) => {
    if (vr.presence_ref_field == null) return;
    const key = `${vr.alias_name}|${vr.field_name}|${vr.presence_ref_field}|${vr.presence_ref_value}`;
    if (seen.has(key)) return;
    seen.add(key);

    const ownMatches = lookupField(fieldLookup, vr.alias_name, vr.field_name);
    const refMatches = lookupField(fieldLookup, vr.alias_name, vr.presence_ref_field);
    ownMatches.forEach((own) => {
      refMatches.forEach((ref) => {
        const refCdiscVariable = ref.field_type === "meddra" ? `${ref.prefix}LLTCD` : ref.cdisc_variable;
        if (own.cdisc_variable == null || refCdiscVariable == null) return;
        (vr.presence_ref_value || "")
          .split(",")
          .map((s) => s.trim())
          .forEach((expectedValue) => {
            rows.push({
              cdisc_variable: own.cdisc_variable,
              label: own.label,
              alias_name: vr.alias_name,
              ref_cdisc_variable: refCdiscVariable,
              ref_alias_name: vr.alias_name,
              ref_label: ref.label != null ? ref.label : null,
              expected_value: expectedValue,
              condition_type: "equals",
            });
          });
      });
    });
  });
  return rows;
}

// "接尾辞.blank?"/"接尾辞.present?"形式のvalidate_presence_if/validate_formula_ifから、
// condition_type="equals"(blank)または"not_blank"(present)のpresence_conditions行を作る
function buildPredicatePresenceConditions(validatorTable, fieldLookup) {
  const seen = new Set();
  const rows = [];
  validatorTable.forEach((vr) => {
    if (vr.presence_predicate_suffix == null) return;
    const key = `${vr.alias_name}|${vr.field_name}|${vr.presence_predicate_suffix}|${vr.presence_predicate_type}`;
    if (seen.has(key)) return;
    seen.add(key);
    lookupField(fieldLookup, vr.alias_name, vr.field_name).forEach((own) => {
      if (own.cdisc_variable == null || own.prefix == null) return;
      rows.push({
        cdisc_variable: own.cdisc_variable,
        label: own.label,
        alias_name: vr.alias_name,
        ref_cdisc_variable: `${own.prefix}${vr.presence_predicate_suffix}`,
        ref_alias_name: vr.alias_name,
        ref_label: null,
        expected_value: vr.presence_predicate_type === "blank" ? "" : null,
        condition_type: vr.presence_predicate_type === "blank" ? "equals" : "not_blank",
      });
    });
  });
  return rows;
}

// age(ref('sheet1', N1), ref('sheet2', N2)) OP 閾値 の単独条件(validate_presence_if。例: FASTATが
// age(初発診断日, 生年月日)>39のときだけ提示される)を、presenceConditions行
// (condition_type="age_gt"/"age_ge"/"age_lt"/"age_le")として追加する。通常のequals/not_blankと
// 異なり参照先が2つ(ref_cdisc_variable/ref2_cdisc_variable)あるため、applyPresenceConditions側で
// 専用の年齢比較処理を行う(Rのage_ref_presence_conditionsに対応)
const AGE_OPERATOR_TO_CONDITION_TYPE = { ">": "age_gt", ">=": "age_ge", "<": "age_lt", "<=": "age_le" };
function buildAgeRefPresenceConditions(validatorTable, fieldLookup) {
  const rows = [];
  const seen = new Set();
  validatorTable.forEach((vr) => {
    if (vr.validator_key !== "validate_presence_if" || vr.age_ref_field != null || vr.value == null) return;
    const key = `${vr.alias_name}|${vr.field_name}|${vr.value}`;
    if (seen.has(key)) return;
    seen.add(key);
    const parsed = parseAgeRefCondition(vr.value);
    if (!parsed) return;
    const conditionType = AGE_OPERATOR_TO_CONDITION_TYPE[parsed.operator];
    if (!conditionType) return;
    const ownMatches = lookupField(fieldLookup, vr.alias_name, vr.field_name);
    const ref1Matches = lookupField(fieldLookup, parsed.ref1AliasName, parsed.ref1Field);
    const ref2Matches = lookupField(fieldLookup, parsed.ref2AliasName, parsed.ref2Field);
    if (ref1Matches.length === 0 || ref2Matches.length === 0) return;
    const ref1 = ref1Matches[0];
    const ref2 = ref2Matches[0];
    if (ref1.cdisc_variable == null || ref2.cdisc_variable == null) return;
    ownMatches.forEach((own) => {
      if (own.cdisc_variable == null) return;
      rows.push({
        cdisc_variable: own.cdisc_variable,
        label: own.label,
        alias_name: vr.alias_name,
        ref_cdisc_variable: ref1.cdisc_variable,
        ref_alias_name: parsed.ref1AliasName,
        ref_label: ref1.label != null ? ref1.label : null,
        ref2_cdisc_variable: ref2.cdisc_variable,
        ref2_alias_name: parsed.ref2AliasName,
        ref2_label: ref2.label != null ? ref2.label : null,
        expected_value: String(parsed.threshold),
        condition_type: conditionType,
      });
    });
  });
  return rows;
}

// "&&"で複数条件が組み合わさったvalidate_presence_ifを断片ごとに分解し、断片の種類
// (predicate/cross_ref/field_ref)ごとにpresence_conditions行を作る。validate_formula_ifは値の
// 妥当性検証であり提示可否のゲーティングには使わない(実際に発生したバグ: reinduction2のECADJで、
// 値の妥当性を表す複雑なOR/AND式の中の一部の断片だけをゲーティング条件として誤抽出し、本来常に
// 提示されるべき値が誤って空欄化されていた)
function buildAndPresenceConditions(validatorTable, fieldLookup) {
  const rows = [];
  const seen = new Set();
  validatorTable.forEach((vr) => {
    if (vr.validator_key !== "validate_presence_if") return;
    if (vr.age_ref_field != null) return;
    const value = vr.value;
    if (value == null || !(value.includes("&&") || value.includes("ref("))) return;
    const dedupKey = `${vr.alias_name}|${vr.field_name}|${value}`;
    if (seen.has(dedupKey)) return;
    seen.add(dedupKey);

    const ownMatches = lookupField(fieldLookup, vr.alias_name, vr.field_name);
    if (ownMatches.length === 0) return;

    let parsed = parseAndConditions(value);
    if (!parsed) {
      const clause = extractCrossRefClause(value);
      if (!clause) return;
      parsed = [{ kind: "cross_ref", refAliasName: clause.refAliasName, refField: clause.refField, value: clause.value }];
    }

    ownMatches.forEach((own) => {
      parsed.forEach((clause) => {
        if (clause.kind === "predicate") {
          if (own.prefix == null) return;
          rows.push({
            cdisc_variable: own.cdisc_variable,
            label: own.label,
            alias_name: vr.alias_name,
            ref_cdisc_variable: `${own.prefix}${clause.suffix}`,
            ref_alias_name: vr.alias_name,
            ref_label: null,
            expected_value: clause.predicateType === "blank" ? "" : null,
            condition_type: clause.predicateType === "blank" ? "equals" : "not_blank",
          });
        } else if (clause.kind === "field_equality_skip") {
          // fieldN==fieldMは別のcopy機構(extractFieldEqualityRef、下記)で扱われるため、
          // ここではpresenceConditions行を作らない(&&の他の断片(OR条件等)の解析は妨げない)
        } else if (clause.kind === "cross_ref") {
          const refMatches = lookupField(fieldLookup, clause.refAliasName, clause.refField);
          refMatches.forEach((ref) => {
            const refCdiscVariable = ref.field_type === "meddra" ? `${ref.prefix}LLTCD` : ref.cdisc_variable;
            if (refCdiscVariable == null) return;
            rows.push({
              cdisc_variable: own.cdisc_variable,
              label: own.label,
              alias_name: vr.alias_name,
              ref_cdisc_variable: refCdiscVariable,
              ref_alias_name: clause.refAliasName,
              ref_label: ref.label != null ? ref.label : null,
              expected_value: clause.value,
              condition_type: "equals",
            });
          });
        } else if (clause.kind === "field_ref") {
          const refMatches = lookupField(fieldLookup, vr.alias_name, clause.refField);
          refMatches.forEach((ref) => {
            const refCdiscVariable = ref.field_type === "meddra" ? `${ref.prefix}LLTCD` : ref.cdisc_variable;
            if (refCdiscVariable == null || refCdiscVariable === own.cdisc_variable) return;
            rows.push({
              cdisc_variable: own.cdisc_variable,
              label: own.label,
              alias_name: vr.alias_name,
              ref_cdisc_variable: refCdiscVariable,
              ref_alias_name: vr.alias_name,
              ref_label: ref.label != null ? ref.label : null,
              expected_value: clause.value,
              condition_type: "equals",
            });
          });
        } else if (clause.kind === "field_ref_or") {
          // 断片自体がOR条件(例: field22==2||field22==3||...)の場合、同じref_cdisc_variableに対する
          // 複数のexpected_value行を作る(applyPresenceConditions側でref_cdisc_variableごとに
          // グルーピングされ、値の集合に対するOR判定になる。異なるref_cdisc_variable同士はAND)
          const refMatches = lookupField(fieldLookup, vr.alias_name, clause.refField);
          refMatches.forEach((ref) => {
            const refCdiscVariable = ref.field_type === "meddra" ? `${ref.prefix}LLTCD` : ref.cdisc_variable;
            if (refCdiscVariable == null || refCdiscVariable === own.cdisc_variable) return;
            clause.values.forEach((expectedValue) => {
              rows.push({
                cdisc_variable: own.cdisc_variable,
                label: own.label,
                alias_name: vr.alias_name,
                ref_cdisc_variable: refCdiscVariable,
                ref_alias_name: vr.alias_name,
                ref_label: ref.label != null ? ref.label : null,
                expected_value: expectedValue,
                condition_type: "equals",
              });
            });
          });
        } else if (clause.kind === "field_numeric_cmp") {
          // fieldN>=数値のような、同一シート内の別フィールドの値との数値不等号比較(例:
          // "f16>=2"(骨壊死のGradeが2以上))。equals/not_blankと異なりref側の値を数値として
          // 閾値と比較する必要があるため、専用のcondition_type(numeric_ge/le/gt/lt)にする
          const refMatches = lookupField(fieldLookup, vr.alias_name, clause.refField);
          const numericOpToType = { ">=": "numeric_ge", "<=": "numeric_le", ">": "numeric_gt", "<": "numeric_lt" };
          const numericConditionType = numericOpToType[clause.operator];
          if (!numericConditionType) return;
          refMatches.forEach((ref) => {
            const refCdiscVariable = ref.field_type === "meddra" ? `${ref.prefix}LLTCD` : ref.cdisc_variable;
            if (refCdiscVariable == null || refCdiscVariable === own.cdisc_variable) return;
            rows.push({
              cdisc_variable: own.cdisc_variable,
              label: own.label,
              alias_name: vr.alias_name,
              ref_cdisc_variable: refCdiscVariable,
              ref_alias_name: vr.alias_name,
              ref_label: ref.label != null ? ref.label : null,
              expected_value: String(clause.threshold),
              condition_type: numericConditionType,
            });
          });
        }
      });
    });
  });
  return rows;
}

// validator_type=="formula"の式にリテラルを伴わないfieldN==fieldMが含まれる場合、
// condition_type="copy"のpresence_conditions行を作る(例: FAOBJがAETERMをコピーする)
function buildFieldEqualityCopyConditions(validatorTable, fieldLookup) {
  const rows = [];
  const seen = new Set();
  validatorTable.forEach((vr) => {
    if (vr.validator_type !== "formula" || vr.value == null) return;
    const dedupKey = `${vr.alias_name}|${vr.field_name}|${vr.value}`;
    if (seen.has(dedupKey)) return;
    seen.add(dedupKey);
    const copyRefField = extractFieldEqualityRef(vr.field_name, vr.value);
    if (!copyRefField) return;
    const ownMatches = lookupField(fieldLookup, vr.alias_name, vr.field_name);
    const refMatches = lookupField(fieldLookup, vr.alias_name, copyRefField);
    ownMatches.forEach((own) => {
      refMatches.forEach((ref) => {
        if (own.cdisc_variable == null || ref.cdisc_variable == null) return;
        rows.push({
          cdisc_variable: own.cdisc_variable,
          label: own.label,
          alias_name: vr.alias_name,
          ref_cdisc_variable: ref.cdisc_variable,
          ref_alias_name: vr.alias_name,
          ref_label: ref.label != null ? ref.label : null,
          expected_value: null,
          condition_type: "copy",
        });
      });
    });
  });
  return rows;
}

// FieldItem::Reference(reference_type=="sheet")から、condition_type="copy"のpresence_conditions行を作る
function buildFieldReferenceCopyConditions(fieldReferenceTable, fieldLookup) {
  const rows = [];
  fieldReferenceTable.forEach((fr) => {
    if (fr.reference_type !== "sheet") return;
    const ownMatches = lookupField(fieldLookup, fr.alias_name, fr.field_name);
    const refMatches = lookupField(fieldLookup, fr.alias_name, fr.reference_field);
    ownMatches.forEach((own) => {
      refMatches.forEach((ref) => {
        if (own.cdisc_variable == null || ref.cdisc_variable == null) return;
        rows.push({
          cdisc_variable: own.cdisc_variable,
          label: own.label,
          alias_name: fr.alias_name,
          ref_cdisc_variable: ref.cdisc_variable,
          ref_alias_name: fr.alias_name,
          ref_label: ref.label != null ? ref.label : null,
          expected_value: null,
          condition_type: "copy",
        });
      });
    });
  });
  return rows;
}

// validator_type=="presence"を持つ(alias_name, label, cdisc_variable)の一覧を抽出する。
// 同じcdisc_variable名が複数のalias_name/labelに定義されている場合(例: MHTERMが"主診断"(必須)と
// "再発診断"(非必須、複数label)の両方に使われる)があるため、cdisc_variable名だけでなく
// alias_name・label単位で必須かどうかを判定できるようにする
// (Rのbuild_generation_constraints.Rのrequired_var_instancesに対応)
function buildRequiredVarInstances(validatorTable, fieldLookup) {
  const seenField = new Set();
  const seenInstance = new Set();
  const instances = [];
  validatorTable.forEach((vr) => {
    if (vr.validator_type !== "presence") return;
    const fieldKey = `${vr.alias_name}|${vr.field_name}`;
    if (seenField.has(fieldKey)) return;
    seenField.add(fieldKey);
    lookupField(fieldLookup, vr.alias_name, vr.field_name).forEach((own) => {
      if (own.cdisc_variable == null) return;
      const instanceKey = `${vr.alias_name}|${own.label}|${own.cdisc_variable}`;
      if (seenInstance.has(instanceKey)) return;
      seenInstance.add(instanceKey);
      instances.push({ alias_name: vr.alias_name, label: own.label, cdisc_variable: own.cdisc_variable });
    });
  });
  return instances;
}

// 後方互換用: cdisc_variable名だけでunique化したフラット版(段階的に置き換え中)
function buildRequiredVars(requiredVarInstances) {
  return [...new Set(requiredVarInstances.map((r) => r.cdisc_variable))];
}

// bound_type(min_value/max_value)を持つ行から、cdisc_variableごとの数値範囲(より厳しい方を採用)を作る
function buildNumericBounds(validatorTable, fieldLookup) {
  const perVar = {};
  const seen = new Set();
  validatorTable.forEach((vr) => {
    if (vr.bound_type == null || vr.numeric_value == null) return;
    if (vr.bound_type !== "min_value" && vr.bound_type !== "max_value") return;
    const key = `${vr.alias_name}|${vr.field_name}|${vr.bound_type}|${vr.numeric_value}`;
    if (seen.has(key)) return;
    seen.add(key);
    lookupField(fieldLookup, vr.alias_name, vr.field_name).forEach((own) => {
      if (own.cdisc_variable == null) return;
      if (!perVar[own.cdisc_variable]) perVar[own.cdisc_variable] = { mins: [], maxs: [] };
      if (vr.bound_type === "min_value") perVar[own.cdisc_variable].mins.push(vr.numeric_value);
      else perVar[own.cdisc_variable].maxs.push(vr.numeric_value);
    });
  });
  const result = {};
  Object.keys(perVar).forEach((v) => {
    const { mins, maxs } = perVar[v];
    result[v] = {
      min_value: mins.length > 0 ? Math.max(...mins) : null,
      max_value: maxs.length > 0 ? Math.min(...maxs) : null,
    };
  });
  return result;
}

// numericBoundsはcdisc_variable単位に集約されるため、LBORRESのように同じcdisc_variable名を
// 多数のTESTCD別フィールドが共有するケースでは使えない(全フィールドのmin/maxが「厳しい方」で
// 一律にまとまってしまう)。buildDateRefBoundsと同じく(alias_name, label, cdisc_variable)単位で
// 集約せず個別に保持したバージョンを別途用意する(LB/TR/VSのORRES生成で使う)(Rのfield_numeric_boundsに対応)
function buildFieldNumericBounds(validatorTable, fieldLookup) {
  const perGroup = {};
  const seen = new Set();
  validatorTable.forEach((vr) => {
    if (vr.bound_type == null || vr.numeric_value == null) return;
    if (vr.bound_type !== "min_value" && vr.bound_type !== "max_value") return;
    const key = `${vr.alias_name}|${vr.field_name}|${vr.bound_type}|${vr.numeric_value}`;
    if (seen.has(key)) return;
    seen.add(key);
    lookupField(fieldLookup, vr.alias_name, vr.field_name).forEach((own) => {
      if (own.cdisc_variable == null) return;
      const groupKey = `${vr.alias_name}|${own.label}|${own.cdisc_variable}`;
      if (!perGroup[groupKey]) {
        perGroup[groupKey] = { alias_name: vr.alias_name, label: own.label != null ? own.label : null, cdisc_variable: own.cdisc_variable, mins: [], maxs: [] };
      }
      if (vr.bound_type === "min_value") perGroup[groupKey].mins.push(vr.numeric_value);
      else perGroup[groupKey].maxs.push(vr.numeric_value);
    });
  });
  return Object.values(perGroup)
    .map((g) => ({
      alias_name: g.alias_name,
      label: g.label,
      cdisc_variable: g.cdisc_variable,
      min_value: g.mins.length > 0 ? Math.max(...g.mins) : null,
      max_value: g.maxs.length > 0 ? Math.min(...g.maxs) : null,
    }))
    .filter((r) => r.min_value != null || r.max_value != null);
}

// formulaでフィールド同士を比較している行(例: f350<=f59)から、(cdisc_variable, ref_cdisc_variable, bound_type)を作る
function buildFieldRefBounds(validatorTable, fieldLookup) {
  const rows = [];
  const seen = new Set();
  validatorTable.forEach((vr) => {
    if (vr.validator_type !== "formula") return;
    if (vr.bound_type == null || vr.ref_field == null) return;
    if (vr.ref_field === vr.field_name) return;
    const key = `${vr.alias_name}|${vr.field_name}|${vr.ref_field}|${vr.bound_type}`;
    if (seen.has(key)) return;
    seen.add(key);
    const ownMatches = lookupField(fieldLookup, vr.alias_name, vr.field_name);
    const refMatches = lookupField(fieldLookup, vr.alias_name, vr.ref_field);
    ownMatches.forEach((own) => {
      refMatches.forEach((ref) => {
        if (own.cdisc_variable == null || ref.cdisc_variable == null) return;
        // alias_name(自分自身の所属シート。formula参照は必ず同一シート内なのでref_alias_nameも同じ)は、
        // buildAliasLevelEdges()がprefixだけでなくシート単位で依存関係を見られるようにするために保持する
        rows.push({ alias_name: vr.alias_name, cdisc_variable: own.cdisc_variable, ref_cdisc_variable: ref.cdisc_variable, bound_type: vr.bound_type });
      });
    });
  });
  return rows;
}

// validate_date_after_or_equal_to/validate_date_before_or_equal_toが他フィールド参照(例: "field5")の場合の
// 下限/上限(bound_type="min_date"/"max_date")を、buildFieldRefBoundsと同様にcdisc_variable名に変換した
// テーブルにする。alias_name/label/ref_labelも保持する。ECのような繰り返しブロックでは、同じcdisc_variable名
// (例: ECSTDTC/ECENDTC)が1つのalias内で複数回(投与1回目・2回目...)登場し、「同じlabel内の開始日<=終了日」と
// 「次のlabelの開始日>=前のlabelの終了日」のようにlabelを跨ぐ参照と跨がない参照が混在する。cdisc_variable単位
// まで潰してしまうと(ECSTDTC min_date ECENDTC / ECENDTC min_date ECSTDTCのように)矛盾した規則に見えてしまう
// ため、buildRepeatedDomain側でlabel/ref_labelを見てlabelを跨ぐ参照かどうかを判定できるようにする
// (R版build_generation_constraints.Rのdate_ref_boundsと同じ理由)
function buildDateRefBounds(validatorTable, fieldLookup) {
  const rows = [];
  const seen = new Set();
  validatorTable.forEach((vr) => {
    if (vr.validator_type !== "date") return;
    if (vr.bound_type == null || vr.ref_field == null) return;
    // 自己参照除外(ref_field===field_name)は、dateRefAliasName(ref('sheet_alias', N)形式の
    // 他シート参照)が無い場合(=同一シート内の参照)にだけ適用する。他シート参照の場合、
    // 参照先の(そのシート内での)フィールド番号が自分のフィールド番号とたまたま同じことがあり
    // (例: earlyintensifiのfield820がinductionのfield820を参照)、これを自己参照として
    // 誤除外してしまうため
    if (vr.date_ref_alias_name == null && vr.ref_field === vr.field_name) return;
    const key = `${vr.alias_name}|${vr.field_name}|${vr.ref_field}|${vr.bound_type}`;
    if (seen.has(key)) return;
    seen.add(key);
    // ref('sheet_alias', N)形式の他シート参照(dateRefAliasName)があればそちらを、無ければ
    // 従来通り自分自身と同じalias_nameを参照先のlookupに使う(Rのref_lookup_alias_nameに対応)
    const refLookupAliasName = vr.date_ref_alias_name != null ? vr.date_ref_alias_name : vr.alias_name;
    const ownMatches = lookupField(fieldLookup, vr.alias_name, vr.field_name);
    const refMatches = lookupField(fieldLookup, refLookupAliasName, vr.ref_field);
    ownMatches.forEach((own) => {
      refMatches.forEach((ref) => {
        if (own.cdisc_variable == null || ref.cdisc_variable == null) return;
        rows.push({
          alias_name: vr.alias_name,
          label: own.label != null ? own.label : null,
          cdisc_variable: own.cdisc_variable,
          ref_alias_name: refLookupAliasName,
          ref_label: ref.label != null ? ref.label : null,
          ref_cdisc_variable: ref.cdisc_variable,
          bound_type: vr.bound_type,
          offset_days: vr.date_ref_offset_days,
        });
      });
    });
  });
  return rows;
}

// age(fN,fM)>=X && age(fN,fM)<=Yのような年齢条件から、(cdisc_variable, ref_cdisc_variable, min_age, max_age)を作る
function buildAgeBounds(validatorTable, fieldLookup) {
  const rows = [];
  const seen = new Set();
  validatorTable.forEach((vr) => {
    if (vr.age_ref_field == null) return;
    const key = `${vr.alias_name}|${vr.field_name}|${vr.age_ref_field}|${vr.min_age}|${vr.max_age}`;
    if (seen.has(key)) return;
    seen.add(key);
    const ownMatches = lookupField(fieldLookup, vr.alias_name, vr.field_name);
    const refMatches = lookupField(fieldLookup, vr.alias_name, vr.age_ref_field);
    ownMatches.forEach((own) => {
      refMatches.forEach((ref) => {
        if (own.cdisc_variable == null || ref.cdisc_variable == null) return;
        // alias_name(自分自身の所属シート)は、buildAliasLevelEdges()がprefixだけでなくシート単位で
        // 依存関係を見られるようにするために保持する(age()参照は必ず同一シート内なのでref_alias_nameも同じ)
        rows.push({
          alias_name: vr.alias_name,
          cdisc_variable: own.cdisc_variable,
          ref_cdisc_variable: ref.cdisc_variable,
          ref_alias_name: vr.alias_name,
          ref_label: ref.label != null ? ref.label : null,
          min_age: vr.min_age,
          max_age: vr.max_age,
        });
      });
    });
  });
  return rows;
}

// cdiscVariableValues(各生成関数にspecとして渡される配列)の各行に、そのalias_name/label/
// cdisc_variableのインスタンスが実際にpresenceバリデータを持つかどうか(isRequired)を付与する。
// requiredVars(cdisc_variable名だけでunique化したフラット版)と違い、同じcdisc_variable名が
// 複数のalias_name/labelに定義されていても、インスタンスごとに正確に必須/非必須を判定できる。
// labelがnull(そのfieldにlabelが無い)の行同士も一致させるため、joinキーは空文字列に揃える
// (Rのload_edc_spec.Rのcdisc_variable_values %>% left_join(...)に対応)
function attachIsRequired(cdiscVariableValues, requiredVarInstances) {
  const requiredSet = new Set(requiredVarInstances.map((r) => `${r.alias_name}|${r.label != null ? r.label : ""}|${r.cdisc_variable}`));
  cdiscVariableValues.forEach((row) => {
    row.is_required = requiredSet.has(`${row.alias_name}|${row.label != null ? row.label : ""}|${row.cdisc_variable}`);
  });
  return cdiscVariableValues;
}

// validatorTable + dfCdisc(+fieldReferenceTable)から、presence_conditions/required_vars/numeric_bounds/
// field_ref_bounds/age_boundsを組み立てて返す(Rのbuild_generation_constraints()に対応)
function buildGenerationConstraints(validatorTable, dfCdisc, fieldReferenceTable) {
  const fieldLookup = buildFieldLookup(dfCdisc);
  let presenceConditions = [
    ...buildEqualsPresenceConditions(validatorTable, fieldLookup),
    ...buildPredicatePresenceConditions(validatorTable, fieldLookup),
    ...buildAndPresenceConditions(validatorTable, fieldLookup),
    ...buildAgeRefPresenceConditions(validatorTable, fieldLookup),
    ...buildFieldEqualityCopyConditions(validatorTable, fieldLookup),
  ];
  if (fieldReferenceTable && fieldReferenceTable.length > 0) {
    presenceConditions = presenceConditions.concat(buildFieldReferenceCopyConditions(fieldReferenceTable, fieldLookup));
  }
  const requiredVarInstances = buildRequiredVarInstances(validatorTable, fieldLookup);
  return {
    presenceConditions,
    requiredVars: buildRequiredVars(requiredVarInstances),
    requiredVarInstances,
    numericBounds: buildNumericBounds(validatorTable, fieldLookup),
    fieldNumericBounds: buildFieldNumericBounds(validatorTable, fieldLookup),
    fieldRefBounds: buildFieldRefBounds(validatorTable, fieldLookup),
    dateRefBounds: buildDateRefBounds(validatorTable, fieldLookup),
    ageBounds: buildAgeBounds(validatorTable, fieldLookup),
  };
}

// --- apply_presence_conditions相当 ---

// presence_conditionsに基づき、条件を満たさない行のcdisc_variableをnullにする(Rのapply_presence_conditions()に対応)。
// dataがalias_name列を持つ場合、ref_alias_nameがdata自身のalias_nameのいずれかと一致する行だけに絞り込む
// (一致しなければ真に外部の固定参照とみなし全行を対象にする)。dataがlabel列も持つ場合は、同様にlabelでも絞り込む
// (現状DM/AEドメインはlabel列を持たないため、この絞り込みは実質alias_nameのみで機能する)
function applyPresenceConditions(data, presenceConditions, cdiscVariableToPrefix) {
  if (!data || data.length === 0) return data;
  const columns = new Set(Object.keys(data[0]));
  const applicable = presenceConditions.filter((pc) => columns.has(pc.cdisc_variable) && columns.has(pc.ref_cdisc_variable));

  const hasAliasName = columns.has("alias_name");
  const hasLabel = hasAliasName && columns.has("label");
  const dataAliasNames = hasAliasName ? new Set(data.map((r) => r.alias_name)) : new Set();

  // ゲーティング対象(row[cdisc_variable]をnull化するかもしれない行)は、あくまで自分自身の
  // (alias_name, label)で決める。参照先(ref_alias_name, ref_label)がown側と異なるlabelを指す場合、
  // 両方を同時に満たす行は存在しない(labelは1行につき1つの値しか持たない)ため、参照先条件を
  // ゲーティング対象の絞り込みに混ぜてはいけない(実際に発生したバグ: 同一alias内で別labelを参照する
  // equals条件(例: GRADEがOCCURのFAORRES=="Y"を参照)が、参照先labelと自分自身labelの両方を
  // 満たす行を探そうとして常に0件になり、条件が一切適用されないまま素通りしていた)
  function ownTargetRows(ownAliasName, ownLabel) {
    return data.map((row) => {
      let match = true;
      if (hasAliasName && ownAliasName != null) match = match && row.alias_name === ownAliasName;
      if (hasLabel && ownLabel != null) match = match && row.label === ownLabel;
      return match;
    });
  }

  // refAliasName/refLabelがdata自身の実際の行の組み合わせ(refAliasName内に実在するlabel)と
  // 一致するかどうか。一致しない場合、真に外部(別prefix)の固定参照であり、injectCrossDomainRefs()が
  // 既にUSUBJID単位で正しい値をrefVar列としてその行(ownAlias/ownLabelの行)に結合済みなので、
  // そのままrow[refVar]を読めばよい。ここでさらに同じdata内をUSUBJIDベースで検索し直すと、
  // refLabelが一致する行が1件も無い場合にrefLookupが空のままになり全行nullになる、あるいは
  // 同じUSUBJIDの他の行(inject時のpin対象外だった行)を誤って拾ってしまう(実際に発生したバグ:
  // CM(baseline)のCMTRT(SPDEVID2〜5)がSC(baseline)のSCORRES(label=003)を参照する際、CM自身には
  // label="003"の行が存在しないため、refLookupが空になりCMTRTが常にnull化されていた)
  function refRowExists(refAliasName, refLabel) {
    if (!hasAliasName || refAliasName == null || !dataAliasNames.has(refAliasName)) return false;
    if (refLabel == null) return true;
    if (!hasLabel) return false;
    return data.some((r) => r.alias_name === refAliasName && r.label === refLabel);
  }

  // 参照先の値を行ごとに解決する。参照先が自分自身と同じ(alias_name, label)(=同じ行)であれば
  // row[refVar]をそのまま読めばよいが、同一alias内で別labelを参照する場合や他ドメイン(既に
  // injectCrossDomainRefsで結合済みの列)の場合は、参照先の値をUSUBJIDで対応付けて引く必要がある
  function resolveRefVals(refVar, refAliasName, refLabel, ownAliasName, ownLabel) {
    const refIsOwnRow =
      (refAliasName == null || refAliasName === ownAliasName) && (refLabel == null || refLabel === ownLabel);
    if (refIsOwnRow || !hasUsubjid || !refRowExists(refAliasName, refLabel)) {
      return data.map((row) => row[refVar]);
    }
    const refLookup = new Map();
    data.forEach((r) => {
      let match = r.alias_name === refAliasName;
      if (match && refLabel != null) match = r.label === refLabel;
      if (match) refLookup.set(r.USUBJID, r[refVar]);
    });
    return data.map((row) => (refLookup.has(row.USUBJID) ? refLookup.get(row.USUBJID) : null));
  }

  // copy: 先に適用する(同じcdisc_variableに他のゲーティング条件も併せて存在する場合、
  // 先にコピーしてから後段でNA化できるようにするため)
  const hasUsubjid = columns.has("USUBJID");
  const copySeen = new Set();
  applicable
    .filter((pc) => pc.condition_type === "copy")
    .forEach((pc) => {
      const key = `${pc.cdisc_variable}|${pc.ref_cdisc_variable}|${pc.alias_name}|${pc.label}|${pc.ref_alias_name}|${pc.ref_label}`;
      if (copySeen.has(key)) return;
      copySeen.add(key);
      const targetRows = data.map((r) => {
        let match = true;
        if (hasAliasName && pc.alias_name != null) match = match && r.alias_name === pc.alias_name;
        if (hasLabel && pc.label != null) match = match && r.label === pc.label;
        return match;
      });

      // 参照元がref_cdisc_variable(別prefixの変数、例: TU側のTUDTC)である場合、この関数が呼ばれる前の
      // injectCrossDomainRefs()が既にUSUBJID単位で正しい値をref_cdisc_variable列としてdataに結合済みのため、
      // targetRowsの位置でそのまま読めばよい(ここでさらにalias_name/labelで突き合わせようとすると、
      // ref_label/ref_alias_nameは参照先(別prefix)自身のラベル空間の値であり、data(このprefix自身の行)の
      // alias_name/labelとは無関係な値のため、誤って一致してしまう/一致せず空になるおそれがある)。
      // 一方、参照元が自分自身と同じprefixの場合、同じcdisc_variable列を複数labelブロックが共有しているため、
      // (alias_name, label)が自分自身と一致する場合(例: FAOBJがAETERMをコピーする、同じ行の別フィールドを
      // 参照する)はtargetRowsの値をそのまま読めばよいが、別の(alias_name, label)ブロックを参照する場合
      // (例: SAXISのTRDTCがLDIAMのTRDTCをコピーする)は、コピー元・コピー先が別々の行になるため、
      // 同じ行のインデックスをそのまま使うと自分自身(まだ値が入っていない)を読んでしまう。USUBJIDで
      // 対応付けてから値を引く
      const ownPrefix = cdiscVariableToPrefix ? cdiscVariableToPrefix[pc.cdisc_variable] : null;
      const refPrefix = cdiscVariableToPrefix ? cdiscVariableToPrefix[pc.ref_cdisc_variable] : null;
      const isCrossPrefix = ownPrefix != null && refPrefix != null && ownPrefix !== refPrefix;

      const sameAlias = pc.ref_alias_name == null || (pc.alias_name != null && pc.ref_alias_name === pc.alias_name);
      const sameLabel = pc.ref_label == null || (pc.label != null && pc.ref_label === pc.label);
      const isSameBlock = isCrossPrefix || (sameAlias && sameLabel);

      if (isSameBlock || !hasUsubjid) {
        data.forEach((row, i) => {
          if (targetRows[i]) row[pc.cdisc_variable] = row[pc.ref_cdisc_variable];
        });
      } else {
        const refLookup = new Map();
        data.forEach((r) => {
          let match = true;
          if (hasAliasName && pc.ref_alias_name != null) match = match && r.alias_name === pc.ref_alias_name;
          if (hasLabel && pc.ref_label != null) match = match && r.label === pc.ref_label;
          if (match) refLookup.set(r.USUBJID, r[pc.ref_cdisc_variable]);
        });
        data.forEach((row, i) => {
          if (targetRows[i]) row[pc.cdisc_variable] = refLookup.has(row.USUBJID) ? refLookup.get(row.USUBJID) : null;
        });
      }
    });

  // age_gt/age_ge/age_lt/age_le: age(ref1, ref2)(2つの日付の経過年数)がoperator/expected_value(閾値)を
  // 満たさない行をnullにする(validate_presence_ifのage(ref('sheet',N), ref('sheet',M)) OP 閾値に対応)。
  // copyと同様、他のequals/not_blank条件がこの変数自身を参照している場合がある(例: FASTATのage_gt条件で
  // null化された後の値をFAORRESのequals条件("FASTATが空欄のときだけ値を持つ")が読む)ため、
  // equals/not_blankより先に適用する。ref_cdisc_variable/ref2_cdisc_variableという2つの参照先を持つ点が
  // 通常のequals/not_blankと異なるため、専用の処理にする
  applicable
    .filter((pc) => ["age_gt", "age_ge", "age_lt", "age_le"].includes(pc.condition_type) && columns.has(pc.ref2_cdisc_variable))
    .forEach((pc) => {
      const targetRows = ownTargetRows(pc.alias_name, pc.label);
      const date1Vals = resolveRefVals(pc.ref_cdisc_variable, pc.ref_alias_name, pc.ref_label, pc.alias_name, pc.label);
      const date2Vals = resolveRefVals(pc.ref2_cdisc_variable, pc.ref2_alias_name, pc.ref2_label, pc.alias_name, pc.label);
      const threshold = Number(pc.expected_value);
      data.forEach((row, i) => {
        if (!targetRows[i]) return;
        const d1 = date1Vals[i] != null ? new Date(date1Vals[i]) : null;
        const d2 = date2Vals[i] != null ? new Date(date2Vals[i]) : null;
        let satisfied = false;
        if (d1 != null && d2 != null && !Number.isNaN(d1.getTime()) && !Number.isNaN(d2.getTime())) {
          const ageYears = (d1.getTime() - d2.getTime()) / (365.25 * 86400000);
          if (pc.condition_type === "age_gt") satisfied = ageYears > threshold;
          else if (pc.condition_type === "age_ge") satisfied = ageYears >= threshold;
          else if (pc.condition_type === "age_lt") satisfied = ageYears < threshold;
          else if (pc.condition_type === "age_le") satisfied = ageYears <= threshold;
        }
        if (!satisfied) {
          row[pc.cdisc_variable] = null;
        }
      });
    });

  // numeric_ge/numeric_le/numeric_gt/numeric_lt: refCdiscVariableの値を数値としてexpectedValue(閾値)と
  // 比較し、満たさない行をnullにする(validate_presence_ifの"fieldN>=数値"のような同一シート内の別
  // フィールドとの数値不等号比較に対応。例: QSORRESの"f16>=2&&STAT.blank?"のうちf16>=2の部分)。
  // age_gt等と同様、他のequals/not_blank条件がこの変数自身を参照している場合があるため先に適用する
  applicable
    .filter((pc) => ["numeric_ge", "numeric_le", "numeric_gt", "numeric_lt"].includes(pc.condition_type))
    .forEach((pc) => {
      const targetRows = ownTargetRows(pc.alias_name, pc.label);
      const refVals = resolveRefVals(pc.ref_cdisc_variable, pc.ref_alias_name, pc.ref_label, pc.alias_name, pc.label);
      const threshold = Number(pc.expected_value);
      data.forEach((row, i) => {
        if (!targetRows[i]) return;
        const refNum = refVals[i] != null ? Number(refVals[i]) : NaN;
        let satisfied = false;
        if (!Number.isNaN(refNum)) {
          if (pc.condition_type === "numeric_ge") satisfied = refNum >= threshold;
          else if (pc.condition_type === "numeric_le") satisfied = refNum <= threshold;
          else if (pc.condition_type === "numeric_gt") satisfied = refNum > threshold;
          else if (pc.condition_type === "numeric_lt") satisfied = refNum < threshold;
        }
        if (!satisfied) {
          row[pc.cdisc_variable] = null;
        }
      });
    });

  // equals: (cdisc_variable, ref_cdisc_variable, ref_alias_name, ref_label, alias_name, label)でグループ化し、
  // 期待値集合のいずれにも一致しない行をnullにする
  const equalsGroups = new Map();
  applicable
    .filter((pc) => pc.condition_type === "equals")
    .forEach((pc) => {
      const key = `${pc.cdisc_variable}|${pc.ref_cdisc_variable}|${pc.ref_alias_name}|${pc.ref_label}|${pc.alias_name}|${pc.label}`;
      if (!equalsGroups.has(key)) {
        equalsGroups.set(key, {
          cdisc_variable: pc.cdisc_variable,
          ref_cdisc_variable: pc.ref_cdisc_variable,
          ref_alias_name: pc.ref_alias_name,
          ref_label: pc.ref_label,
          alias_name: pc.alias_name,
          label: pc.label,
          expectedValues: new Set(),
        });
      }
      equalsGroups.get(key).expectedValues.add(pc.expected_value);
    });
  // equalsGroups同士に依存関係がある場合(例: GRADEのFAORRESがOCCURのFAORRES(別label)を参照し、
  // OCCUR自身のFAORRESも別のequals条件(FASTATベース)でnull化される)、参照先を先にnull化して
  // からでないと、参照元がまだ確定していない(生成直後の)値を読んでしまう。Map挿入順(=
  // presence_conditions配列の順序、シート定義の並び順に依存する不定な順序)のまま処理すると、
  // 参照先が後から処理されるケースで誤判定が起きる(実際に発生したバグ: GRADEがOCCURより先に
  // 処理されると、OCCURのFAORRESがまだnull化される前のランダムな初期値のままGRADE側の判定に
  // 使われてしまい、本来nullにすべきGRADEの値が残ってしまっていた)。そのため、あるgroupの
  // 参照先(ref_alias_name, ref_label, ref_cdisc_variable)が別のgroupの対象
  // (alias_name, label, cdisc_variable)と一致する場合は、そちらを先に処理するようトポロジカル順に
  // 並べ替える
  const groupNodeKey = (alias, label, v) => `${alias}::${label}::${v}`;
  const groupList = [...equalsGroups.values()];
  const targetKeyToIndex = new Map();
  groupList.forEach((g, i) => {
    targetKeyToIndex.set(groupNodeKey(g.alias_name, g.label, g.cdisc_variable), i);
  });
  const dependsOn = groupList.map((g, i) => {
    const refKey = groupNodeKey(g.ref_alias_name, g.ref_label, g.ref_cdisc_variable);
    const depIndex = targetKeyToIndex.get(refKey);
    return depIndex != null && depIndex !== i ? depIndex : null;
  });
  const orderedGroups = [];
  const visited = new Array(groupList.length).fill(false);
  const visiting = new Array(groupList.length).fill(false);
  const visit = (i) => {
    if (visited[i] || visiting[i]) return;
    visiting[i] = true;
    if (dependsOn[i] != null) visit(dependsOn[i]);
    visiting[i] = false;
    visited[i] = true;
    orderedGroups.push(groupList[i]);
  };
  groupList.forEach((_, i) => visit(i));

  orderedGroups.forEach((g) => {
    const targetRows = ownTargetRows(g.alias_name, g.label);
    const refVals = resolveRefVals(g.ref_cdisc_variable, g.ref_alias_name, g.ref_label, g.alias_name, g.label);
    // expectedValuesが""(空欄)を含む場合(例: "field.blank?"由来の条件)、参照先列は他の
    // presence_conditionsで既にnull化されていることがあり、その場合refValは""ではなくnull/undefinedに
    // なっている。厳密なSet.hasだけで判定すると「空欄のはずが空欄と認識されない」まま誤ってNG扱いに
    // なってしまうため、""が期待値に含まれる場合はnull/undefinedも空欄として一致させる
    const blankOk = g.expectedValues.has("");
    data.forEach((row, i) => {
      if (!targetRows[i]) return;
      const refVal = refVals[i];
      const isMatch = g.expectedValues.has(refVal) || (blankOk && (refVal == null || refVal === ""));
      if (!isMatch) {
        row[g.cdisc_variable] = null;
      }
    });
  });

  // not_blank: ref_cdisc_variableが空白/nullの行をnullにする
  const notBlankSeen = new Set();
  applicable
    .filter((pc) => pc.condition_type === "not_blank")
    .forEach((pc) => {
      const key = `${pc.cdisc_variable}|${pc.ref_cdisc_variable}|${pc.ref_alias_name}|${pc.ref_label}|${pc.alias_name}|${pc.label}`;
      if (notBlankSeen.has(key)) return;
      notBlankSeen.add(key);
      const targetRows = ownTargetRows(pc.alias_name, pc.label);
      const refVals = resolveRefVals(pc.ref_cdisc_variable, pc.ref_alias_name, pc.ref_label, pc.alias_name, pc.label);
      data.forEach((row, i) => {
        const refVal = refVals[i];
        const isBlank = refVal == null || refVal === "";
        if (targetRows[i] && isBlank) {
          row[pc.cdisc_variable] = null;
        }
      });
    });

  return data;
}

// --- apply_age_date_bounds相当 ---

function daysFromEpoch(dateStr) {
  return Math.floor(new Date(dateStr).getTime() / 86400000);
}
function dateFromDays(days) {
  return new Date(days * 86400000).toISOString().slice(0, 10);
}

// ageBounds(cdisc_variable, ref_cdisc_variable, min_age, max_age)に基づき、cdisc_variable(日付)を
// ref_cdisc_variable(日付、例: BRTHDTC)からの経過年数がmin_age〜max_ageに収まるよう生成し直す
// (片方だけ、あるいは両方無い場合もある)。生成範囲はregistrationStartDate〜今日にも収める。
// ref_cdisc_variableが無効な日付、またはcdisc_variableが既にnull(presence_conditions等で
// ゲーティングされ空欄になった場合を含む)の行は変更しない(Rのapply_age_date_bounds()に対応)
function applyAgeDateBounds(data, ageBounds, registrationStartDate) {
  if (!ageBounds || ageBounds.length === 0 || !data || data.length === 0) return data;
  const columns = new Set(Object.keys(data[0]));
  const applicable = ageBounds.filter((ab) => columns.has(ab.cdisc_variable) && columns.has(ab.ref_cdisc_variable));
  if (applicable.length === 0) return data;

  const regStart = daysFromEpoch(registrationStartDate);
  const today = daysFromEpoch(new Date().toISOString().slice(0, 10));

  applicable.forEach((ab) => {
    const varName = ab.cdisc_variable;
    const refVar = ab.ref_cdisc_variable;
    const minAge = ab.min_age;
    const maxAge = ab.max_age;

    data.forEach((row) => {
      const current = row[varName];
      const refValRaw = row[refVar];
      if (current == null || refValRaw == null) return;
      const refDays = daysFromEpoch(refValRaw);
      if (Number.isNaN(refDays)) return;

      const rawLower = minAge != null ? refDays + Math.round(minAge * 365.25) : regStart;
      const rawUpper = maxAge != null ? refDays + Math.round(maxAge * 365.25) : today;

      let lower = Math.max(rawLower, regStart);
      let upper = Math.min(rawUpper, today);
      // registrationStartDate〜今日でクランプすると逆転してしまう場合(高齢のため年齢条件と
      // 登録期間が両立しない等)は、年齢条件を優先してクランプせずそのまま使う
      if (lower > upper) {
        lower = rawLower;
        upper = rawUpper;
      }
      upper = Math.max(upper, lower);

      const randomDay = Math.floor(lower + rng() * (upper - lower + 1));
      row[varName] = dateFromDays(randomDay);
    });
  });
  return data;
}

// --- apply_field_ref_bounds相当 ---

// fieldRefBounds(cdisc_variable, ref_cdisc_variable, bound_type)に基づき、cdisc_variableの値が
// ref_cdisc_variableの値との大小関係(max_value/min_value/exact_value)を満たさない場合、
// 条件を満たすradio_button選択肢から選び直す。空白("")や、ref_cdisc_variableが数値でない場合は
// 対象外(そのまま)とする(Rのapply_field_ref_bounds()に対応)
function applyFieldRefBounds(data, spec, fieldRefBounds) {
  if (!fieldRefBounds || fieldRefBounds.length === 0 || !data || data.length === 0) return data;
  const columns = new Set(Object.keys(data[0]));
  const applicable = fieldRefBounds.filter((fb) => columns.has(fb.cdisc_variable) && columns.has(fb.ref_cdisc_variable));

  applicable.forEach((fb) => {
    const varName = fb.cdisc_variable;
    const refVar = fb.ref_cdisc_variable;
    const boundType = fb.bound_type;

    const choiceRows = spec.filter((r) => r.cdisc_variable === varName && r.field_type === "radio_button");
    const choices = [...new Set(choiceRows.map((r) => (r.code != null ? r.code : r.default_value)))];

    data.forEach((row) => {
      const current = row[varName];
      if (current == null || current === "") return;
      const refValue = Number(row[refVar]);
      if (Number.isNaN(refValue)) return;

      let valid;
      if (boundType === "max_value") {
        valid = choices.filter((c) => {
          const n = Number(c);
          return !Number.isNaN(n) && n <= refValue;
        });
      } else if (boundType === "min_value") {
        valid = choices.filter((c) => {
          const n = Number(c);
          return !Number.isNaN(n) && n >= refValue;
        });
      } else if (boundType === "exact_value") {
        valid = choices.filter((c) => {
          const n = Number(c);
          return !Number.isNaN(n) && n === refValue;
        });
      } else {
        valid = choices;
      }
      if (valid.length === 0 || valid.includes(current)) return;
      row[varName] = sampleOne(valid);
    });
  });
  return data;
}

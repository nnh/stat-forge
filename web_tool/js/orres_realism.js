// *DOSE項目を数値バリデーションに沿った値に置き換える(populateDoseRealism)。
// LB/TR/VS/FAのORRES(検査結果値)は、other_domains.jsのgenerateNumericTextValue()で
// 項目単位の範囲から直接生成するため、ここでは扱わない

function randomUniform(min, max) {
  return min + rng() * (max - min);
}

// DOSE(用量)項目を、EDC仕様の数値バリデーション(min/max)に基づいたそれらしい数値に置き換える。
// populateDoseFields()/buildRepeatedDomain()側のDOSE生成は、変数名が"DOSE"で終わることだけを
// 条件にハードコードされた候補値(50〜500)から選ぶため、その項目本来の範囲を超えることがある
// (例: PRDOSE=照射線量は仕様上1〜30Gyだが、CM向けの候補値がそのまま使われてしまう)。
// TRT(例: CMTRT/PRTRT)をキーにして範囲を引く

// fieldNumericBoundsをtrtVarの値をキーにした対応表に変換する。trtVarは(alias_name, label)ごとに
// 1つの固定値(defaultValue)を持つ項目であるため、これをブリッジにしてalias_name/label情報が失われた
// 後のデータからでもtrt値だけで範囲を引けるようにする。trtVarがfield_type=="drug"の場合、
// cdisc_variable_valuesのdefault_valueはWHO Drugコードであり実際のデータ列にはその薬剤名(full_name_en、無ければgeneric_name_enへの
// フォールバック名)が入るため、マップの鍵として使う前にwhoDrugIdfで解決する
function buildTrtNumericBounds(cdiscVariableValues, fieldNumericBounds, trtVar, doseVar, whoDrugIdf) {
  const trtMap = new Map();
  cdiscVariableValues.forEach((r) => {
    if (r.cdisc_variable !== trtVar || r.default_value == null || r.default_value === "") return;
    let trtValue = r.default_value;
    if (r.field_type === "drug" && /^[0-9]+$/.test(trtValue) && whoDrugIdf) {
      const hit = whoDrugIdf.find((w) => w.drug_code === trtValue && w.full_name_en != null);
      if (hit) {
        trtValue = hit.full_name_en;
      } else {
        const genericHit = whoDrugIdf.find((w) => w.drug_code === trtValue && w.generic_name_en != null);
        if (genericHit) trtValue = genericHit.generic_name_en;
      }
    }
    trtMap.set(`${r.alias_name}|${r.label}`, trtValue);
  });
  const result = new Map();
  (fieldNumericBounds || []).forEach((r) => {
    if (r.cdisc_variable !== doseVar) return;
    // 範囲なし(数値であることのみ)の行は対象外(元の値のまま)にする
    if (r.min_value == null && r.max_value == null) return;
    const trt = trtMap.get(`${r.alias_name}|${r.label}`);
    if (trt == null) return;
    if (!result.has(trt)) result.set(trt, { min_value: r.min_value, max_value: r.max_value });
  });
  return result;
}

// dataの中から"*DOSE"列を持ち、対応する"*TRT"列(DOSEをTRTに置き換えた変数名。例:
// CMDOSE→CMTRT)も持つものだけを対象に、行のTRT値に対応する範囲を外れているDOSE値だけを
// 生成し直す。TRTに対応する範囲が定義されていない場合、およびtrtVar列自体が無いdoseVarは
// 対象外(元の値のまま)とする
function populateDoseRealism(data, cdiscVariableValues, fieldNumericBounds, whoDrugIdf) {
  if (!data || data.length === 0) return data;
  const doseVars = Object.keys(data[0]).filter((v) => /DOSE$/.test(v));
  doseVars.forEach((doseVar) => {
    const trtVar = doseVar.replace(/DOSE$/, "TRT");
    if (!(trtVar in data[0])) return;
    const trtBounds = buildTrtNumericBounds(cdiscVariableValues, fieldNumericBounds, trtVar, doseVar, whoDrugIdf);
    if (trtBounds.size === 0) return;
    data.forEach((row) => {
      if (row[doseVar] == null || row[doseVar] === "") return;
      const bound = trtBounds.get(row[trtVar]);
      if (bound == null) return;
      const current = Number(row[doseVar]);
      const min = bound.min_value != null ? bound.min_value : -Infinity;
      const max = bound.max_value != null ? bound.max_value : Infinity;
      if (!Number.isNaN(current) && current >= min && current <= max) return;
      const lo = bound.min_value != null ? bound.min_value : 0;
      const hi = bound.max_value != null ? bound.max_value : lo + 100;
      row[doseVar] = String(Math.round(randomUniform(lo, hi)));
    });
  });
  return data;
}

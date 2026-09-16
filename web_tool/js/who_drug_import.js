// WHO Drug/IDFバージョンフォルダ(WHODD・IDFサブフォルダ)をパースし、
// r_version/read_who_drug_idf.RのbuildWhoDrugIdf()と同じ結合結果
// (drug_code, full_name_en, generic_name_en)を持つ{columns, rows}形式のオブジェクトを作る。
// IDF/full_ja.txtは最終出力列に使われないため読み込まない。
// IDF/data.txtはCP932(Shift-JIS)エンコードだが、使う列(drug_code・SEQ・FLG)はASCIIのみのため、
// 1バイト=1文字として読める"iso-8859-1"でデコードする(comma/quoteの構造解析には支障が無く、
// TextDecoder("shift_jis")のブラウザ対応可否にも依存しないため)。

const WHO_DRUG_COLUMNS = ["drug_code", "full_name_en", "generic_name_en"];

// カンマ区切り・ダブルクォート囲みの1行をパースする(引用符内のカンマ・二重引用符エスケープに対応)
function parseCsvLine(line) {
  const fields = [];
  let cur = "";
  let inQuotes = false;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (inQuotes) {
      if (c === '"') {
        if (line[i + 1] === '"') {
          cur += '"';
          i++;
        } else {
          inQuotes = false;
        }
      } else {
        cur += c;
      }
    } else if (c === '"') {
      inQuotes = true;
    } else if (c === ",") {
      fields.push(cur);
      cur = "";
    } else {
      cur += c;
    }
  }
  fields.push(cur);
  return fields;
}

function splitLines(text) {
  return text.split(/\r\n|\r|\n/).filter((line) => line.length > 0);
}

// タブ区切り・ヘッダー無しファイルをパースする(IDMapping.csv・WHODDsGenericNames.csv用)
function parseTsv(text) {
  return splitLines(text).map((line) => line.split("\t"));
}

// カンマ区切り(クォート囲み)・ヘッダー無しファイルをパースする(full_en.txt・data.txt用)
function parseQuotedCsv(text) {
  return splitLines(text).map(parseCsvLine);
}

// dirHandle配下のrelativePath(例: "WHODD/IDMapping.csv")をdecoderでデコードして読む
async function readTextFile(dirHandle, relativePath, decoder) {
  const parts = relativePath.split("/");
  let handle = dirHandle;
  for (let i = 0; i < parts.length - 1; i++) {
    handle = await handle.getDirectoryHandle(parts[i]);
  }
  const fileHandle = await handle.getFileHandle(parts[parts.length - 1]);
  const file = await fileHandle.getFile();
  const buffer = await file.arrayBuffer();
  return decoder.decode(buffer);
}

// WHO Drug/IDFバージョンフォルダのディレクトリハンドルから、build_who_drug_idf()と同じ結合結果を作る
async function buildWhoDrugHierarchy(dirHandle) {
  const utf8Decoder = new TextDecoder("utf-8");
  const latin1Decoder = new TextDecoder("iso-8859-1");

  const idMappingRows = parseTsv(await readTextFile(dirHandle, "WHODD/IDMapping.csv", utf8Decoder));
  const genericNameRows = parseTsv(await readTextFile(dirHandle, "WHODD/WHODDsGenericNames.csv", utf8Decoder));
  const fullEnRows = parseQuotedCsv(await readTextFile(dirHandle, "IDF/full_en.txt", utf8Decoder));
  const dataRows = parseQuotedCsv(await readTextFile(dirHandle, "IDF/data.txt", latin1Decoder));

  // IDF: SEQ -> full_name_en(R側のreadrは空文字列をデフォルトでNA扱いするため、それに合わせる)
  const fullNameEnMap = new Map();
  fullEnRows.forEach((row) => fullNameEnMap.set(row[0], row[1] === "" ? undefined : row[1]));

  // WHODD: ddd_code -> generic_name_en(同上)
  const genericNameEnMap = new Map();
  genericNameRows.forEach((row) => genericNameEnMap.set(row[0], row[1] === "" ? undefined : row[1]));

  // WHODD: idf_code -> [{ddd_code}, ...] (IDMapping.csv: ddd_label, ddd_code, idf_label, idf_code, note)
  const idMappingByIdfCode = new Map();
  idMappingRows.forEach((row) => {
    const idfCode = row[3];
    if (!idMappingByIdfCode.has(idfCode)) idMappingByIdfCode.set(idfCode, []);
    idMappingByIdfCode.get(idfCode).push({ ddd_code: row[1] });
  });

  // idf_combined: data.txt(X1=drug_code, X14=SEQ, X15=FLG)からFLG!="C"の行だけ、
  // full_en.txtのfull_name_enを結合する
  // R側(readr)は空文字列のフィールドをデフォルトでNAとして扱い、
  // filter(FLG != "C")はFLGがNAの行も(NA != "C"がNAになるため)除外する。
  // それに合わせ、FLGが空文字列の行も除外する
  const idfCombined = [];
  dataRows.forEach((row) => {
    const drugCode = row[0];
    const seq = row[13];
    const flg = row[14];
    if (flg === "C" || flg === "" || flg === undefined) return;
    idfCombined.push({ drug_code: drugCode, full_name_en: fullNameEnMap.get(seq) });
  });

  // full_join(idf_combined, id_mapping, by = drug_code == idf_code): 両側の不一致行も残す
  const matchedIdfCodes = new Set();
  const joined = [];
  idfCombined.forEach(({ drug_code, full_name_en }) => {
    const matches = idMappingByIdfCode.get(drug_code);
    if (matches && matches.length > 0) {
      matchedIdfCodes.add(drug_code);
      matches.forEach((m) => joined.push({ drug_code, full_name_en, ddd_code: m.ddd_code }));
    } else {
      joined.push({ drug_code, full_name_en, ddd_code: undefined });
    }
  });
  idMappingByIdfCode.forEach((matches, idfCode) => {
    if (matchedIdfCodes.has(idfCode)) return;
    matches.forEach((m) => joined.push({ drug_code: idfCode, full_name_en: undefined, ddd_code: m.ddd_code }));
  });

  // left_join whodd_generic_names by ddd_code
  const seen = new Set();
  const rows = [];
  joined.forEach(({ drug_code, full_name_en, ddd_code }) => {
    const generic_name_en = ddd_code !== undefined ? genericNameEnMap.get(ddd_code) : undefined;
    const row = [drug_code, full_name_en ?? null, generic_name_en ?? null];
    const key = row.join("");
    if (seen.has(key)) return;
    seen.add(key);
    rows.push(row);
  });

  return { columns: WHO_DRUG_COLUMNS, rows };
}

// WHO Drug/IDFバージョンフォルダのディレクトリハンドルから、data/who_drug/<version>.jsと同じ内容の
// JSテキストを作る(versionはフォルダ名をそのまま使う)
async function buildWhoDrugVersionJsContent(dirHandle) {
  const version = dirHandle.name;
  const hierarchy = await buildWhoDrugHierarchy(dirHandle);
  const content =
    "window.__whoDrugVersions = window.__whoDrugVersions || {};\n" +
    "window.__whoDrugVersions[" + JSON.stringify(version) + "] = " + JSON.stringify(hierarchy) + ";\n";
  return { version, content, rowCount: hierarchy.rows.length };
}

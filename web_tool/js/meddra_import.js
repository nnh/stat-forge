// MedDRAバージョンフォルダ(.ascファイル群、$区切り・ヘッダー無し)をパースし、
// r_version/build_meddra_soc_pt_llt.RのbuildMeddraHierarchy()と同じ結合結果
// (llt_code, llt_name, pt_code, pt_name, hlt_code, hlt_name, hlgt_code, hlgt_name, soc_code, soc_name)
// を持つ{columns, rows}形式のオブジェクトを作る。
// 日本語名(_j.ascファイル)は最終出力列に使わないため読み込まない。

const MEDDRA_COLUMNS = [
  "llt_code", "llt_name", "pt_code", "pt_name",
  "hlt_code", "hlt_name", "hlgt_code", "hlgt_name", "soc_code", "soc_name",
];

const MEDDRA_REQUIRED_FILES = [
  "soc.asc", "soc_hlgt.asc", "hlgt.asc", "hlgt_hlt.asc",
  "hlt.asc", "hlt_pt.asc", "pt.asc", "llt.asc",
];

// $区切り・ヘッダー無しの.ascファイルをパースし、行ごとの列配列の配列を返す
function parseAscText(text) {
  return text
    .split(/\r\n|\r|\n/)
    .filter((line) => line.length > 0)
    .map((line) => line.split("$"));
}

// dirHandle(バージョンフォルダのディレクトリハンドル)から必要な.ascファイルをすべて読み込む
async function readMeddraAscFiles(dirHandle) {
  const data = {};
  for (const filename of MEDDRA_REQUIRED_FILES) {
    const fileHandle = await dirHandle.getFileHandle(filename);
    const file = await fileHandle.getFile();
    data[filename] = parseAscText(await file.text());
  }
  return data;
}

// 1列目をキー、2列目を値とするMapを作る(コード->名称の対応表用)
function buildCodeNameMap(rows) {
  const map = new Map();
  rows.forEach((row) => map.set(row[0], row[1]));
  return map;
}

// 1列目をキーに、2列目を配列として集約するMapを作る(1対多のブリッジテーブル用)
function buildBridgeMap(rows) {
  const map = new Map();
  rows.forEach((row) => {
    if (!map.has(row[0])) map.set(row[0], []);
    map.get(row[0]).push(row[1]);
  });
  return map;
}

// readMeddraAscFiles()の結果から、SOC->HLGT->HLT->PT->LLTの階層をたどって
// build_meddra_hierarchy()と同じ結合結果({columns, rows}形式)を作る
function buildMeddraHierarchyFromAscData(ascData) {
  const socMap = buildCodeNameMap(ascData["soc.asc"]);
  const hlgtMap = buildCodeNameMap(ascData["hlgt.asc"]);
  const hltMap = buildCodeNameMap(ascData["hlt.asc"]);
  const ptMap = buildCodeNameMap(ascData["pt.asc"]);
  const hlgtHltMap = buildBridgeMap(ascData["hlgt_hlt.asc"]);
  const hltPtMap = buildBridgeMap(ascData["hlt_pt.asc"]);

  const lltByPt = new Map();
  ascData["llt.asc"].forEach((row) => {
    const ptCode = row[2];
    if (!lltByPt.has(ptCode)) lltByPt.set(ptCode, []);
    lltByPt.get(ptCode).push([row[0], row[1]]);
  });

  const seen = new Set();
  const rows = [];
  ascData["soc_hlgt.asc"].forEach(([socCode, hlgtCode]) => {
    const socName = socMap.get(socCode);
    const hlgtName = hlgtMap.get(hlgtCode);
    if (socName === undefined || hlgtName === undefined) return;
    (hlgtHltMap.get(hlgtCode) || []).forEach((hltCode) => {
      const hltName = hltMap.get(hltCode);
      if (hltName === undefined) return;
      (hltPtMap.get(hltCode) || []).forEach((ptCode) => {
        const ptName = ptMap.get(ptCode);
        if (ptName === undefined) return;
        (lltByPt.get(ptCode) || []).forEach(([lltCode, lltName]) => {
          const row = [lltCode, lltName, ptCode, ptName, hltCode, hltName, hlgtCode, hlgtName, socCode, socName];
          const key = row.join("");
          if (seen.has(key)) return;
          seen.add(key);
          rows.push(row);
        });
      });
    });
  });

  return { columns: MEDDRA_COLUMNS, rows };
}

// MedDRAバージョンフォルダのディレクトリハンドルから、data/meddra/<version>.jsと同じ内容の
// JSテキストを作る(versionはフォルダ名をそのまま使う)
async function buildMeddraVersionJsContent(dirHandle) {
  const version = dirHandle.name;
  const ascData = await readMeddraAscFiles(dirHandle);
  const hierarchy = buildMeddraHierarchyFromAscData(ascData);
  const content =
    "window.__meddraVersions = window.__meddraVersions || {};\n" +
    "window.__meddraVersions[" + JSON.stringify(version) + "] = " + JSON.stringify(hierarchy) + ";\n";
  return { version, content, rowCount: hierarchy.rows.length };
}

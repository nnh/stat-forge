// MedDRA/WHO Drugのデータフォルダ(web_tool/data)へのFile System Access APIアクセスを管理する。
// 一度許可を得ると、IndexedDBにディレクトリハンドルを保存し、次回以降はqueryPermission()で
// 権限が残っているか確認するだけで済む(ブラウザ再起動後は失効するため、その場合は
// 再許可ボタンでrequestPermission()を呼ぶ。フォルダ選択ダイアログは再度出ない)。
// 許可を得たフォルダは、バージョン一覧の動的取得(data/meddra・data/who_drug配下のファイル名から
// プルダウンを作る)と、新しいバージョンファイルの書き込み(Stage 2以降)に使う。
// 未許可の場合は、従来通りdata/versions.jsの一覧を使う(dictionaries.js側にフォールバックあり)。

const DICTIONARY_DATA_DIR_DB_NAME = "dictionaryDataDirDb";
const DICTIONARY_DATA_DIR_STORE = "handles";
const DICTIONARY_DATA_DIR_KEY = "dataDir";

let currentDataDirHandle = null;
let dataDirAccessGranted = false;

function openDictionaryDataDirDb() {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open(DICTIONARY_DATA_DIR_DB_NAME, 1);
    request.onupgradeneeded = () => {
      request.result.createObjectStore(DICTIONARY_DATA_DIR_STORE);
    };
    request.onsuccess = () => resolve(request.result);
    request.onerror = () => reject(request.error);
  });
}

async function getStoredDataDirHandle() {
  const db = await openDictionaryDataDirDb();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(DICTIONARY_DATA_DIR_STORE, "readonly");
    const req = tx.objectStore(DICTIONARY_DATA_DIR_STORE).get(DICTIONARY_DATA_DIR_KEY);
    req.onsuccess = () => resolve(req.result || null);
    req.onerror = () => reject(req.error);
  });
}

async function saveDataDirHandle(handle) {
  const db = await openDictionaryDataDirDb();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(DICTIONARY_DATA_DIR_STORE, "readwrite");
    tx.objectStore(DICTIONARY_DATA_DIR_STORE).put(handle, DICTIONARY_DATA_DIR_KEY);
    tx.oncomplete = () => resolve();
    tx.onerror = () => reject(tx.error);
  });
}

// 選んだフォルダがdata直下(meddra・who_drugサブフォルダを持つ)かどうかの簡易チェック
async function looksLikeDataDir(handle) {
  try {
    await handle.getDirectoryHandle("meddra");
    await handle.getDirectoryHandle("who_drug");
    return true;
  } catch (e) {
    return false;
  }
}

// 初回、フォルダ選択ダイアログを開いてアクセス許可を得る
async function requestDataDirAccess() {
  const handle = await window.showDirectoryPicker({ mode: "readwrite" });
  if (!(await looksLikeDataDir(handle))) {
    throw new Error("選択したフォルダはweb_tool/dataではないようです(meddra・who_drugフォルダが見つかりません)。");
  }
  await saveDataDirHandle(handle);
  currentDataDirHandle = handle;
  dataDirAccessGranted = true;
  return handle;
}

// ページ読み込み時、保存済みハンドルの権限を確認する。
// 戻り値: "granted"(そのまま使える) / "needs-reauth"(ハンドルはあるが再許可が必要) / "not-set"(未設定)
async function restoreDataDirAccess() {
  const handle = await getStoredDataDirHandle();
  if (!handle) return "not-set";
  currentDataDirHandle = handle;
  const permission = await handle.queryPermission({ mode: "readwrite" });
  dataDirAccessGranted = permission === "granted";
  return dataDirAccessGranted ? "granted" : "needs-reauth";
}

// 「再許可」ボタン用。保存済みハンドルに対してrequestPermission()を呼ぶ
// (showDirectoryPicker()は呼ばないため、フォルダ選択ダイアログは出ない)
async function reauthorizeDataDirAccess() {
  if (!currentDataDirHandle) return false;
  const permission = await currentDataDirHandle.requestPermission({ mode: "readwrite" });
  dataDirAccessGranted = permission === "granted";
  return dataDirAccessGranted;
}

function hasDataDirAccess() {
  return dataDirAccessGranted;
}

// kind("meddra"または"who_drug")配下の.jsファイル一覧から、バージョンラベルを返す。
// バージョン名にスペース等の記号が含まれる場合、ファイル名は記号が_に置換されて保存されているため
// (例: "2025 Sep 1" -> "2025_Sep_1.js")、ファイル名をそのままラベルにすると実際のデータキー
// (window.__whoDrugVersions等のキー。元のバージョン名そのまま)と一致しなくなる。
// versions.js(D&D登録時に自動更新され、file->正しいlabelの対応を保持している)と突き合わせて、
// 対応するエントリがあれば正しいlabelに復元する(無ければファイル名をそのまま使う)
async function listVersionsFromDataDir(kind) {
  const subDir = await currentDataDirHandle.getDirectoryHandle(kind);
  const files = [];
  for await (const [name, entryHandle] of subDir.entries()) {
    if (entryHandle.kind === "file" && name.endsWith(".js")) {
      files.push(name.replace(/\.js$/, ""));
    }
  }

  const versionsData = await readVersionsJsData();
  const fileToLabel = new Map();
  (versionsData[kind] || []).forEach((v) => fileToLabel.set(v.file, v.label));

  const labels = files.map((file) => fileToLabel.get(file) ?? file);
  labels.sort((a, b) => a.localeCompare(b, undefined, { numeric: true }));
  return labels;
}

// kind配下にversion.js(ファイル名はr_version側の変換スクリプトと同じ規則で
// バージョン名の記号類を_に置換したもの)が既に存在するかを調べる
async function dictionaryVersionFileExists(kind, version) {
  const subDir = await currentDataDirHandle.getDirectoryHandle(kind, { create: true });
  const safeFilename = version.replace(/[^A-Za-z0-9._-]/g, "_") + ".js";
  try {
    await subDir.getFileHandle(safeFilename);
    return true;
  } catch (e) {
    return false;
  }
}

// kind配下のversion.jsを、versions.jsに載っていないバージョン(D&Dでdataフォルダに直接登録した
// もの)用に、ディレクトリハンドル経由で直接読み込んで実行する(<script src>と同じ内容を
// window.__meddraVersions/__whoDrugVersionsに設定する)
async function loadDictionaryVersionFromDataDir(kind, label) {
  const subDir = await currentDataDirHandle.getDirectoryHandle(kind);
  const safeFilename = label.replace(/[^A-Za-z0-9._-]/g, "_") + ".js";
  const fileHandle = await subDir.getFileHandle(safeFilename);
  const file = await fileHandle.getFile();
  const text = await file.text();
  new Function(text)();
}

// versions.jsを読み込み、window.__dictionaryVersionsの中身(なければmeddra/who_drugとも空配列)を返す。
// windowという名前の引数にダミーのオブジェクトを渡すことで、実際のグローバルwindowには影響を与えずに
// versions.js中の「window.__dictionaryVersions = ...」を安全に実行し、その値だけを取り出す
async function readVersionsJsData() {
  let text = "";
  try {
    const fileHandle = await currentDataDirHandle.getFileHandle("versions.js");
    text = await (await fileHandle.getFile()).text();
  } catch (e) {
    // versions.jsが無ければ空扱い
  }
  const sandbox = {};
  if (text.trim().length > 0) {
    new Function("window", text)(sandbox);
  }
  const data = sandbox.__dictionaryVersions || {};
  if (!Array.isArray(data.meddra)) data.meddra = [];
  if (!Array.isArray(data.who_drug)) data.who_drug = [];
  return data;
}

// versions.jsのkind配下の一覧に{label, file}を追加(同じlabelがあれば上書き)し、ファイルに書き戻す
async function addVersionToVersionsJs(kind, label, file) {
  const data = await readVersionsJsData();
  const list = data[kind];
  const idx = list.findIndex((v) => v.label === label);
  const entry = { label, file };
  if (idx >= 0) {
    list[idx] = entry;
  } else {
    list.push(entry);
  }

  const content =
    "// MedDRA/WHO Drugの利用可能なバージョン一覧(プルダウンの選択肢に使う)。\n" +
    "// 「辞書バージョンの登録・管理」からD&Dでバージョンを登録すると、この一覧にも自動で追記される。\n" +
    "// r_version/tools/convert_meddra_to_js.R・convert_who_drug_to_js.Rで変換を追加した場合は、\n" +
    "// ここに手動で追記すること。\n" +
    "window.__dictionaryVersions = " + JSON.stringify(data, null, 2) + ";\n";

  const fileHandle = await currentDataDirHandle.getFileHandle("versions.js", { create: true });
  const writable = await fileHandle.createWritable();
  await writable.write(content);
  await writable.close();
}

// kind配下にversion.jsとしてcontentを書き込み(既存があれば上書き)、versions.jsにも登録する。
// 書き込んだファイル名を返す
async function writeDictionaryVersionFile(kind, version, content) {
  const subDir = await currentDataDirHandle.getDirectoryHandle(kind, { create: true });
  const safeFilename = version.replace(/[^A-Za-z0-9._-]/g, "_") + ".js";
  const fileHandle = await subDir.getFileHandle(safeFilename, { create: true });
  const writable = await fileHandle.createWritable();
  await writable.write(content);
  await writable.close();
  await addVersionToVersionsJs(kind, version, safeFilename.replace(/\.js$/, ""));
  return safeFilename;
}

// ドロップされたフォルダがMedDRA(soc.ascがある)かWHO Drug/IDF(WHODD・IDFサブフォルダがある)かを判定する。
// どちらでもなければnullを返す
async function detectDictionaryFolderKind(dirHandle) {
  try {
    await dirHandle.getFileHandle("soc.asc");
    return "meddra";
  } catch (e) {
    // MedDRAではない
  }
  try {
    await dirHandle.getDirectoryHandle("WHODD");
    await dirHandle.getDirectoryHandle("IDF");
    return "who_drug";
  } catch (e) {
    // WHO Drug/IDFでもない
  }
  return null;
}

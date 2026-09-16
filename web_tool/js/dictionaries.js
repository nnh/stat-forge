// MedDRA/WHO Drug辞書のバージョンを選択式で読み込む仕組み。
// 辞書データ自体はr_version/tools/convert_meddra_to_js.R・convert_who_drug_to_js.Rで事前に
// data/meddra/<version>.js・data/who_drug/<version>.jsとして変換済みのものを、
// 選択されたバージョンの1ファイルだけその場で<script>タグを動的に追加して読み込む
// (file://でもfetch()を使わずに済むようにするため)。
// どのバージョンが選べるかはdata/versions.jsのwindow.__dictionaryVersionsを見る

// kind("meddra"または"who_drug")のバージョンlabel一覧を返す(プルダウンの選択肢用)
function listDictionaryVersions(kind) {
  const list = (window.__dictionaryVersions && window.__dictionaryVersions[kind]) || [];
  return list.map((v) => v.label);
}

// versions.jsの一覧から、labelに対応するファイル名(拡張子なし)を探す
function findDictionaryFile(kind, label) {
  const list = (window.__dictionaryVersions && window.__dictionaryVersions[kind]) || [];
  const entry = list.find((v) => v.label === label);
  return entry ? entry.file : null;
}

// kindのlabelバージョンのデータファイルを<script>タグで動的に読み込む。
// 既に読み込み済み(window.__meddraVersions[label]等が存在)なら何もしない。
// versions.jsに載っていない(=D&Dでdataフォルダに直接登録した)バージョンの場合は、
// データフォルダへのアクセスが許可されていれば、ディレクトリハンドル経由で直接読み込む。
// 戻り値はPromise(読み込み完了時にresolve、失敗時にreject)
async function loadDictionaryVersion(kind, label) {
  const store = kind === "meddra" ? "__meddraVersions" : "__whoDrugVersions";
  if (window[store] && window[store][label]) {
    return;
  }
  const file = findDictionaryFile(kind, label);
  if (file) {
    return new Promise((resolve, reject) => {
      const script = document.createElement("script");
      script.src = `data/${kind}/${file}.js`;
      script.onload = () => resolve();
      script.onerror = () => reject(new Error(`${kind}データの読み込みに失敗しました: ${script.src}`));
      document.head.appendChild(script);
    });
  }
  if (hasDataDirAccess()) {
    return loadDictionaryVersionFromDataDir(kind, label);
  }
  throw new Error(`${kind}のバージョン「${label}」が見つかりません`);
}

// 読み込み済みの辞書データ(array-of-arrays形式)を、扱いやすいオブジェクトの配列に変換して返す。
// 未読み込みの場合はnull
function getDictionaryData(kind, label) {
  const store = kind === "meddra" ? "__meddraVersions" : "__whoDrugVersions";
  const raw = window[store] && window[store][label];
  if (!raw) return null;
  return raw.rows.map((row) => {
    const obj = {};
    raw.columns.forEach((col, i) => {
      obj[col] = row[i];
    });
    return obj;
  });
}

// MedDRA/WHO Drugの利用可能なバージョン一覧(プルダウンの選択肢に使う)。
// 「辞書バージョンの登録・管理」からD&Dでバージョンを登録すると、この一覧にも自動で追記される。
// r_version/tools/convert_meddra_to_js.R・convert_who_drug_to_js.Rで変換を追加した場合は、
// ここに手動で追記すること。
window.__dictionaryVersions = {
  "meddra": [
    {
      "label": "29.0",
      "file": "29.0"
    }
  ],
  "who_drug": [
    {
      "label": "2025 Sep 1",
      "file": "2025_Sep_1"
    }
  ]
};

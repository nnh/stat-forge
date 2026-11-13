# 検証スクリプト共通の設定(実行環境に依存するパス・辞書バージョン)。
# testN.R・lib/validate_common.Rの先頭でsource()して使う

# 実行環境(Mac/Windows)に応じて、あらかじめ用意した固定のパスのどちらかを返す
resolve_os_path <- function(mac_path, windows_path) {
  if (.Platform$OS.type == "windows") windows_path else mac_path
}

# Box同期フォルダのルート。ユーザーのホームフォルダはfs::path_home()で取得し、
# ホームより下の場所はMac/Windowsで異なるため切り替える
box_root <- resolve_os_path(
  fs::path_home("Library", "CloudStorage", "Box-Box"),
  fs::path_home("Box")
)

# WHO Drug/IDF辞書(test2のCMのチェックで使う)
external_dict_dir <- resolve_os_path(
  file.path(box_root, "Stat/Tools/WHO-DD_IDF_MedDRA"),
  file.path(box_root, "Stat/Tools/FORGE/dictionary_data")
)
who_drug_idf_parent_dir <- file.path(external_dict_dir, "WHO-DD_IDF")
# Web版が使うWHO Drug/IDFの変換済み.js置き場。
# who_drug_idf_parent_dir配下に生データ(WHODD/IDF)が無い場合のフォールバックとして使う
who_drug_js_dir <- file.path(external_dict_dir, "who_drug")
# WHO Drug/IDFのバージョン(who_drug_idf_parent_dir直下のフォルダ名。例: "2025 Mar 1")
who_drug_idf_version_folder <- "2025 Mar 1"

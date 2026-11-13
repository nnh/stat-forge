# 実行環境(Mac/Windows)に応じて、あらかじめ用意した固定のパスのどちらかを返す。
# ローカル環境依存のパスを定義する各ファイルの先頭でsource()して使う
resolve_os_path <- function(mac_path, windows_path) {
  if (.Platform$OS.type == "windows") windows_path else mac_path
}

# Box同期フォルダのルート。ユーザーのホームフォルダはfs::path_home()で取得し、
# ホームより下の場所はMac/Windowsで異なるため切り替える
box_root <- resolve_os_path(
  fs::path_home("Library", "CloudStorage", "Box-Box"),
  fs::path_home("Box")
)

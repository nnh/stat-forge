// コンソール出力と同じ内容を logs/<name>_<日時>.log にも書き出す共通ロガー。
// logsフォルダ自体はリポジトリ管理下(.gitkeep)、個々のログファイルは.gitignore対象
import fs from "node:fs";
import path from "node:path";

export function createLogger(repoRoot, name) {
  const logsDir = path.join(repoRoot, "logs");
  fs.mkdirSync(logsDir, { recursive: true });
  const timestamp = new Date().toISOString().replace(/[:.]/g, "-");
  const logPath = path.join(logsDir, `${name}_${timestamp}.log`);
  const lines = [];

  function log(line = "") {
    console.log(line);
    lines.push(line);
  }
  function logError(line) {
    console.error(line);
    lines.push(line);
  }
  // 実行の最後に呼び出し、蓄積した内容をログファイルへ書き出す
  function flush() {
    fs.writeFileSync(logPath, lines.join("\n") + "\n");
    console.log(`\nログ出力先: ${logPath}`);
  }

  return { log, logError, flush, logPath };
}

// FORGE(Web版)の「JSON読み込み→生成→ZIPで一括ダウンロード」を自動化するスクリプト。
// 辞書(MedDRA/WHO Drug)は data/versions.js に登録済みのバージョンをプルダウンの
// デフォルト値(末尾=最新)のまま使う。辞書バージョンの新規登録(showDirectoryPicker()を使う
// D&D操作)はネイティブのフォルダ選択ダイアログが絡むため自動化の対象外(手動での事前登録が前提)。
//
// 使い方: node scripts/generate_dummy_data.js <jsonPath> <outputDir>
// 例:     node scripts/generate_dummy_data.js ./fortest1.json ~/Downloads/dummy_data
//
// 実行内容(各ステップの確認結果・展開したファイル一覧と行数)は logs/generate_dummy_data_<日時>.log
// にも書き出す(logsフォルダ自体はリポジトリ管理下、個々のログファイルは.gitignore対象)

import { chromium } from "playwright";
import AdmZip from "adm-zip";
import path from "node:path";
import fs from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import os from "node:os";
import { createLogger } from "./lib/log_file.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, "..");

const { log, logError, flush } = createLogger(repoRoot, "generate_dummy_data");

const [jsonPath, outputDir] = process.argv.slice(2);
if (!jsonPath || !outputDir) {
  logError("使い方: node scripts/generate_dummy_data.js <jsonPath> <outputDir>");
  flush();
  process.exit(1);
}
if (!fs.existsSync(jsonPath)) {
  logError(`JSONファイルが見つかりません: ${jsonPath}`);
  flush();
  process.exit(1);
}

const htmlPath = path.join(repoRoot, "FORGE ver1.0.html");
log(`起動ファイル: ${htmlPath}`);
log(`EDC仕様JSON: ${path.resolve(jsonPath)}`);

const browser = await chromium.launch();
let failed = false;
try {
  const page = await browser.newPage();
  await page.goto(pathToFileURL(htmlPath).href);

  await page.setInputFiles("#file-input", path.resolve(jsonPath));
  await page.locator("#config-section").waitFor({ state: "visible" });
  log("JSON読み込み・設定画面表示: OK");

  await page.click("#generate-btn");
  await page.locator("#result-section").waitFor({ state: "visible" });
  log("生成: OK");

  const [download] = await Promise.all([
    page.waitForEvent("download"),
    page.click("#download-all-btn"),
  ]);

  const tmpZipPath = path.join(os.tmpdir(), `forge_dummy_data_${Date.now()}.zip`);
  await download.saveAs(tmpZipPath);
  log("ZIPダウンロード: OK");

  fs.mkdirSync(outputDir, { recursive: true });
  new AdmZip(tmpZipPath).extractAllTo(outputDir, true);
  fs.rmSync(tmpZipPath);

  log(`生成完了: ${outputDir} に展開しました`);
  log("展開したファイル:");
  fs.readdirSync(outputDir)
    .sort()
    .forEach((file) => {
      const filePath = path.join(outputDir, file);
      if (file.endsWith(".csv")) {
        const rowCount = fs.readFileSync(filePath, "utf-8").trimEnd().split("\n").length - 1;
        log(`  ${file}(${rowCount}件)`);
      } else {
        log(`  ${file}`);
      }
    });
} catch (e) {
  logError(`エラー: ${e.message}`);
  failed = true;
} finally {
  await browser.close();
}

flush();
if (failed) {
  process.exit(1);
}

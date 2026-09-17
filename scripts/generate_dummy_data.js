// FORGE(Web版)の「JSON読み込み→生成→ZIPで一括ダウンロード」を自動化するスクリプト。
// 辞書(MedDRA/WHO Drug)は data/versions.js に登録済みのバージョンをプルダウンの
// デフォルト値(末尾=最新)のまま使う。辞書バージョンの新規登録(showDirectoryPicker()を使う
// D&D操作)はネイティブのフォルダ選択ダイアログが絡むため自動化の対象外(手動での事前登録が前提)。
//
// 使い方: node scripts/generate_dummy_data.js <jsonPath> <outputDir>
// 例:     node scripts/generate_dummy_data.js ./fortest1.json ~/Downloads/dummy_data

import { chromium } from "playwright";
import AdmZip from "adm-zip";
import path from "node:path";
import fs from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import os from "node:os";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, "..");

const [jsonPath, outputDir] = process.argv.slice(2);
if (!jsonPath || !outputDir) {
  console.error("使い方: node scripts/generate_dummy_data.js <jsonPath> <outputDir>");
  process.exit(1);
}
if (!fs.existsSync(jsonPath)) {
  console.error(`JSONファイルが見つかりません: ${jsonPath}`);
  process.exit(1);
}

const htmlPath = path.join(repoRoot, "FORGE ver1.0.html");

const browser = await chromium.launch();
try {
  const page = await browser.newPage();
  await page.goto(pathToFileURL(htmlPath).href);

  await page.setInputFiles("#file-input", path.resolve(jsonPath));
  await page.locator("#config-section").waitFor({ state: "visible" });

  await page.click("#generate-btn");
  await page.locator("#result-section").waitFor({ state: "visible" });

  const [download] = await Promise.all([
    page.waitForEvent("download"),
    page.click("#download-all-btn"),
  ]);

  const tmpZipPath = path.join(os.tmpdir(), `forge_dummy_data_${Date.now()}.zip`);
  await download.saveAs(tmpZipPath);

  fs.mkdirSync(outputDir, { recursive: true });
  new AdmZip(tmpZipPath).extractAllTo(outputDir, true);
  fs.rmSync(tmpZipPath);

  console.log(`生成完了: ${outputDir} に展開しました`);
} finally {
  await browser.close();
}

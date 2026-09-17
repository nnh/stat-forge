// 被験者数・施設数・AEレコード数(.integer-input、main.jsのsanitizeIntegerInputValue()対象)が、
// 全角数字・小数点・文字等ASCII数字以外を実際に受け付けないことを確認する回帰テスト。
// generate_dummy_data.jsとは別スクリプトにしているのは、EDC仕様JSONの読み込み・生成という
// 本番の生成フローに、このテスト専用の入力(全角文字等)を混ぜたくないため
// (誤って生成に使われる・値を元に戻し忘れる、といった事故を避ける)。
// EDC仕様JSONが無くても検証できるよう、#config-sectionはpage.evaluate()で強制的に表示する。
// 実行結果はコンソール表示に加えて logs/test_integer_input_sanitization_<日時>.log にも書き出す
// (logsフォルダ自体はリポジトリ管理下、中身の各ログファイルは.gitignore対象)。
//
// 使い方: node scripts/test_integer_input_sanitization.js

import { chromium } from "playwright";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { createLogger } from "./lib/log_file.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, "..");
const htmlPath = path.join(repoRoot, "FORGE ver1.0.html");

const { log, logError, flush } = createLogger(repoRoot, "test_integer_input_sanitization");

// [入力するテキスト, 期待される結果]。全角数字・小数点・英字・マイナス記号を含む
const INPUT_CASES = [
  ["1２3.5abc", "135"],
  ["１２３４", ""],
  ["-42", "42"],
];
const INTEGER_INPUT_IDS = ["registration-n", "site-n", "ae-n"];

let failed = 0;

function check(label, actual, expected) {
  if (actual === expected) {
    log(`OK: ${label}(実際: "${actual}")`);
  } else {
    failed += 1;
    logError(`NG: ${label}(期待値: "${expected}" / 実際: "${actual}")`);
  }
}

const browser = await chromium.launch();
try {
  const page = await browser.newPage();
  await page.goto(pathToFileURL(htmlPath).href);
  await page.evaluate(() => {
    document.getElementById("config-section").style.display = "block";
  });

  for (const id of INTEGER_INPUT_IDS) {
    for (const [input, expected] of INPUT_CASES) {
      // 通常の入力(inputイベント)経由
      await page.locator(`#${id}`).fill("");
      await page.locator(`#${id}`).pressSequentially(input);
      const inputResult = await page.locator(`#${id}`).inputValue();
      check(`#${id} input「${input}」`, inputResult, expected);

      // IME確定(compositionendイベント)経由。pressSequentially()は実際のIME変換を
      // 再現できないため、value代入+compositionendディスパッチで確定直後の状態を模擬する
      await page.locator(`#${id}`).evaluate((el, value) => {
        el.value = value;
        el.dispatchEvent(new Event("compositionend", { bubbles: true }));
      }, input);
      const compositionResult = await page.locator(`#${id}`).inputValue();
      check(`#${id} compositionend「${input}」`, compositionResult, expected);
    }
  }

  // 対照: 乱数シード(.integer-input対象外)は全角文字等がそのまま残ることを確認する
  const seedInput = "全角シード１２３";
  await page.locator("#random-seed").fill("");
  await page.locator("#random-seed").pressSequentially(seedInput);
  const seedResult = await page.locator("#random-seed").inputValue();
  check("#random-seed(対象外)は素通り", seedResult, seedInput);
} finally {
  await browser.close();
}

if (failed > 0) {
  logError(`\n${failed}件NGです`);
} else {
  log("\n全て OK");
}
flush();

if (failed > 0) {
  process.exit(1);
}

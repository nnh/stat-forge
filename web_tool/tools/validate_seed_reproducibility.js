// 乱数シード機能(setRandomSeed()/rng()、web_tool/js/util.js)のバリデーションプログラム。
// main.jsの生成ボタンハンドラ・ZIPダウンロードハンドラと同じロジックでCSVファイル一式を
// 実際にディスクへ書き出し、以下3点を確認する:
//   1. 同じシードで3回出力 -> 全てのファイルが完全一致する
//   2. 異なるシードで3回出力 -> ファイルの内容が異なる
//   3. シード値42で出力した結果とシード値空欄で出力した結果 -> 全てのファイルが完全一致する
//      (未入力時はDEFAULT_RANDOM_SEED="42"にフォールバックする仕様のため)
//
// 使い方: node web_tool/tools/validate_seed_reproducibility.js <edc仕様JSONのパス>

const fs = require("fs");
const path = require("path");
const os = require("os");

const jsonPath = process.argv[2];
if (!jsonPath) {
  console.error("usage: node validate_seed_reproducibility.js <json_path>");
  process.exit(1);
}

const webToolDir = path.join(__dirname, "..");
global.window = global;
const jsFiles = [
  "js/util.js",
  "js/cdisc_variable_values.js",
  "js/generation_constraints.js",
  "js/dm_domain.js",
  "js/ae_domain.js",
  "js/ds_domain.js",
  "js/other_domains.js",
  "js/orres_realism.js",
  "data/versions.js",
  "data/meddra/29.0.js",
  "data/who_drug/2025_Sep_1.js",
];
eval(jsFiles.map((f) => fs.readFileSync(path.join(webToolDir, f), "utf8")).join("\n;\n"));

function listDictionaryVersions(kind) {
  return ((window.__dictionaryVersions || {})[kind] || []).map((v) => v.label);
}
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

// main.jsの生成ボタンハンドラ(DM->AE->DS->otherDomains->ORRES整形->sortBySeq)と
// ZIPダウンロードハンドラ(CSVファイル一覧の組み立て)を再現し、実際にoutDirへCSVを書き出す
function generateAndWriteCsvs(edcSpec, seedValue, outDir) {
  setRandomSeed(seedValue);

  const cdiscVariableValues = buildCdiscVariableValues(edcSpec);
  const dfCdisc = buildDfCdisc(edcSpec);
  const validatorTable = buildValidatorTable(edcSpec.sheets);
  const fieldReferenceTable = buildFieldReferenceTable(edcSpec.sheets);
  const constraints = buildGenerationConstraints(validatorTable, dfCdisc, fieldReferenceTable);
  const { presenceConditions, ageBounds, requiredVarInstances, numericBounds, fieldNumericBounds, fieldRefBounds, dateRefBounds } = constraints;
  attachIsRequired(cdiscVariableValues, requiredVarInstances);

  const n = 100;
  const siteN = 10;
  const aeN = 100;
  const registrationStartDate = "2024-04-01";
  const meddraVersion = listDictionaryVersions("meddra")[0];
  const whoDrugVersion = listDictionaryVersions("who_drug")[0];
  const meddraData = getDictionaryData("meddra", meddraVersion);
  const whoDrugIdf = getDictionaryData("who_drug", whoDrugVersion);

  const studyid = `${edcSpec.name}_dummy`;
  const dmResult = buildDmDomain(n, edcSpec.sheets, edcSpec.sheet_groups, ageBounds, studyid, siteN);
  const dummySites = dmResult.dummySites;
  let dm = populateDmDomain(dmResult.dm, cdiscVariableValues, registrationStartDate, meddraData, presenceConditions, numericBounds, fieldRefBounds, ageBounds, dateRefBounds);
  dm = sortBySeq(dm, "DM");

  const aeSpec = cdiscVariableValues.filter((r) => r.prefix === "AE");
  let ae = buildAeDomain(dm, aeN);
  ae = assignAeAliasNames(ae, aeSpec, dmResult.activeSheets);
  ae = populateAeChoiceFields(ae, aeSpec, numericBounds);
  ae = populateAeDateFields(ae, aeSpec, registrationStartDate);
  const meddraSample = sampleMeddraRows(meddraData, ae.length);
  const requiredLltCodes = deriveRequiredLltCodes(presenceConditions);
  injectRequiredLltCodes(meddraSample, meddraData, requiredLltCodes);
  ae = populateAeMeddraFields(ae, aeSpec, meddraData, meddraSample);
  ae = addAeMeddraCodingBlock(ae, meddraSample, "AE");
  const linkedResult = populateLinkedBlocks(ae, cdiscVariableValues, "AE", registrationStartDate, meddraData, whoDrugIdf, dateRefBounds);
  ae = linkedResult.data;
  const linkedSpec = linkedResult.linkedSpec;
  ae = populateAeDummyFields(ae, aeSpec);
  ae = applyPresenceConditions(ae, presenceConditions);
  ae = applyFieldRefBounds(ae, aeSpec, fieldRefBounds);
  ae = filterAeDeathDateConsistency(ae);
  const aeResult = finalizeAeDomain(ae, aeSpec, linkedSpec);
  ae = aeResult.ae;
  const aeLinkedDomains = aeResult.linked;
  ae = sortBySeq(ae, "AE");

  let ds = buildDsDomain(dm, cdiscVariableValues);
  ds = populateDsDomain(ds, cdiscVariableValues, registrationStartDate, meddraData, presenceConditions, numericBounds, fieldRefBounds, dateRefBounds);
  const deathDate = buildDeathDateTable(ae);
  ds = finalizeDsDisposition(ds, deathDate, cdiscVariableValues);
  ds = addRandomizationDsRows(ds, dm, registrationStartDate);
  const discontinuationDate = buildDiscontinuationDateTable(ds);

  const activeSheetTable = buildActiveSheetTable(dmResult.activeSheets);
  const multiRecordAliasNames = (edcSpec.sheets || [])
    .filter((s) => s.category === "ae_report" || s.category === "multiple")
    .map((s) => s.alias_name);
  const visitLookup = buildVisitLookup(edcSpec.sheets, edcSpec.visits);
  const cdiscVariableValuesForOthers = excludeAeLinkedPrefixes(cdiscVariableValues, aeLinkedDomains);
  const otherDomains = buildOtherDomains(dm, cdiscVariableValuesForOthers, registrationStartDate, meddraData, presenceConditions, requiredVarInstances, numericBounds, fieldRefBounds, {
    builtDomains: { DM: dm, AE: ae, DS: ds },
    ageBounds,
    multiRecordAliasNames,
    activeSheetTable,
    whoDrugIdf,
    visitLookup,
    discontinuationDate,
    dateRefBounds,
  });
  if (otherDomains.DD) {
    const ddGatedVars = [...new Set(presenceConditions.filter((pc) => pc.cdisc_variable in (otherDomains.DD[0] || {})).map((pc) => pc.cdisc_variable))];
    otherDomains.DD = dropEmptyDomainRows(otherDomains.DD, ddGatedVars);
  }
  mergeLinkedDomains(otherDomains, aeLinkedDomains);
  applyOrresPopulators(otherDomains, {
    LB: (d) => populateLbOrres(d, cdiscVariableValues, fieldNumericBounds),
    TR: (d) => populateTrOrres(d, cdiscVariableValues, fieldNumericBounds),
    VS: (d) => populateVsOrres(d, cdiscVariableValues, fieldNumericBounds),
  });
  Object.keys(otherDomains).forEach((prefix) => {
    otherDomains[prefix] = sortBySeq(otherDomains[prefix], prefix);
  });

  ds = ds.map((row) => {
    const { alias_name, label, sheet_seq, ...rest } = row;
    return rest;
  });
  ds = sortBySeq(ds, "DS");

  const files = [
    { name: "DM_dummy.csv", content: toCsv(dm, Object.keys(dm[0])) },
    { name: "AE_dummy.csv", content: toCsv(ae, Object.keys(ae[0])) },
    { name: "DS_dummy.csv", content: toCsv(ds, Object.keys(ds[0])) },
  ];
  if (dummySites && dummySites.length > 0) {
    files.push({ name: "facilities_dummy.csv", content: toCsv(dummySites, ["code", "ja", "en"]) });
  }
  Object.keys(otherDomains)
    .sort()
    .forEach((prefix) => {
      const data = otherDomains[prefix];
      if (!data || data.length === 0) return;
      files.push({ name: `${prefix}_dummy.csv`, content: toCsv(data, Object.keys(data[0])) });
    });

  fs.mkdirSync(outDir, { recursive: true });
  files.forEach((f) => fs.writeFileSync(path.join(outDir, f.name), f.content));
  return files.map((f) => f.name).sort();
}

// 2つのディレクトリが、同じファイル名の集合を持ち、全ファイルの内容が完全一致するかを確認する
function dirsIdentical(dirA, dirB) {
  const namesA = fs.readdirSync(dirA).sort();
  const namesB = fs.readdirSync(dirB).sort();
  if (JSON.stringify(namesA) !== JSON.stringify(namesB)) {
    return { identical: false, reason: `ファイル一覧が異なる: ${dirA}=[${namesA}] / ${dirB}=[${namesB}]` };
  }
  for (const name of namesA) {
    const a = fs.readFileSync(path.join(dirA, name));
    const b = fs.readFileSync(path.join(dirB, name));
    if (!a.equals(b)) {
      return { identical: false, reason: `${name} の内容が異なる` };
    }
  }
  return { identical: true, reason: "" };
}

function main() {
  const edcSpec = JSON.parse(fs.readFileSync(jsonPath, "utf8"));
  const baseDir = fs.mkdtempSync(path.join(os.tmpdir(), "seed-validate-"));
  console.log(`作業ディレクトリ: ${baseDir}`);
  let allPass = true;

  // 1. 同じシードで3回出力 -> 全て完全一致
  console.log("\n=== 検証1: 同じシード(seed=777)で3回出力 -> 全て完全一致するか ===");
  const sameSeedDirs = [1, 2, 3].map((i) => {
    const d = path.join(baseDir, `same_seed_run${i}`);
    generateAndWriteCsvs(edcSpec, "777", d);
    return d;
  });
  const cmp12 = dirsIdentical(sameSeedDirs[0], sameSeedDirs[1]);
  const cmp13 = dirsIdentical(sameSeedDirs[0], sameSeedDirs[2]);
  const test1Pass = cmp12.identical && cmp13.identical;
  console.log(`run1 vs run2: ${cmp12.identical ? "一致" : "不一致(" + cmp12.reason + ")"}`);
  console.log(`run1 vs run3: ${cmp13.identical ? "一致" : "不一致(" + cmp13.reason + ")"}`);
  console.log(test1Pass ? "検証1: PASS" : "検証1: FAIL");
  allPass = allPass && test1Pass;

  // 2. 異なるシードで3回出力 -> 内容が異なる
  console.log("\n=== 検証2: 異なるシード(111/222/333)で3回出力 -> 内容が異なるか ===");
  const diffSeeds = ["111", "222", "333"];
  const diffSeedDirs = diffSeeds.map((seed, i) => {
    const d = path.join(baseDir, `diff_seed_run${i + 1}`);
    generateAndWriteCsvs(edcSpec, seed, d);
    return d;
  });
  const dcmp12 = dirsIdentical(diffSeedDirs[0], diffSeedDirs[1]);
  const dcmp13 = dirsIdentical(diffSeedDirs[0], diffSeedDirs[2]);
  const dcmp23 = dirsIdentical(diffSeedDirs[1], diffSeedDirs[2]);
  const test2Pass = !dcmp12.identical && !dcmp13.identical && !dcmp23.identical;
  console.log(`seed=111 vs seed=222: ${dcmp12.identical ? "一致(想定外)" : "不一致(想定通り)"}`);
  console.log(`seed=111 vs seed=333: ${dcmp13.identical ? "一致(想定外)" : "不一致(想定通り)"}`);
  console.log(`seed=222 vs seed=333: ${dcmp23.identical ? "一致(想定外)" : "不一致(想定通り)"}`);
  console.log(test2Pass ? "検証2: PASS" : "検証2: FAIL");
  allPass = allPass && test2Pass;

  // 3. シード値42とシード値空欄 -> 全て完全一致
  console.log("\n=== 検証3: シード値42とシード値空欄で出力 -> 完全一致するか ===");
  const dir42 = path.join(baseDir, "seed_42");
  const dirBlank = path.join(baseDir, "seed_blank");
  generateAndWriteCsvs(edcSpec, "42", dir42);
  generateAndWriteCsvs(edcSpec, "", dirBlank);
  const cmp3 = dirsIdentical(dir42, dirBlank);
  console.log(`seed=42 vs seed=(空欄): ${cmp3.identical ? "一致(想定通り)" : "不一致(" + cmp3.reason + ")"}`);
  console.log(cmp3.identical ? "検証3: PASS" : "検証3: FAIL");
  allPass = allPass && cmp3.identical;

  console.log(`\n=== 総合結果: ${allPass ? "全件PASS" : "FAILあり"} ===`);
  process.exit(allPass ? 0 : 1);
}

main();

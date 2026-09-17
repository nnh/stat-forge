# stat-forge

EDC仕様JSON(.json)から、統計解析プログラムの開発・検証用にダミーの生データセット(SDTM形式)を生成するツール群。実データが揃う前の開発・バリデーションを支援する。

## 構成

- `FORGE ver1.0.html` — ダミーデータ生成ツールの起動ファイル(これをブラウザで開く)
- `web_tool/` — 上記ツールの実装本体(js/data)
- `r_version/` — 生成ロジックのR版実装 + 生成データの検証(バリデーション)スクリプト

## 使い方

- リポジトリ直下の `FORGE ver1.0.html` をブラウザで開く
- EDC仕様JSON(.json)をドラッグ&ドロップ、または選択して読み込む
- 「生成する」→ドメインごと、またはZIPで一括ダウンロード

### MedDRA / WHO Drug辞書の登録(必要時、半年に一度程度)

- 辞書のバージョン更新時など、必要になったときだけ行う作業(頻度は半年に1回程度)
- `web_tool/data/meddra/`・`web_tool/data/who_drug/` にバージョンごとの変換済みJSファイルを置く
- ライセンス上リポジトリに再配布できないため、フォルダ自体はgit管理下だが中身は`.gitignore`対象
- 登録方法: 「辞書バージョンの登録・管理」からフォルダをD&D

### 制限事項

- フォルダのドラッグ&ドロップはChromium系ブラウザ(Chrome, Edge)のみ対応(Directory Entries APIの制約でSafari/Firefoxは非対応/一部対応)
- 複雑な割り付け(多段階の割り付け、層別化を伴う割り付け等)には対応していない
- 条件(presence_conditions等)があまりにも厳しい場合、選択肢の一部が生成されないことがある
- 症例数(被験者数)が少ないと、乱数によるサンプリングの都合上、選択肢の一部が生成されないことがある

## Rでのテスト

現在、生成データの検証は以下の3ファイルのみを実行する運用になっている:

- `r_version/tools/validate_datasets_test1_web.R`
- `r_version/tools/validate_datasets_test2_web.R`
- `r_version/tools/validate_datasets_test3_web.R`

### セットアップ

1. `r_version/test_config.R.sample` を `r_version/test_config.R` としてコピー(gitignore対象、各自のローカルパスを書く)
2. `json_path`(EDC仕様JSON)・`other_domains_web_csv_dir`(Webツールの「ZIPで一括ダウンロード」展開先)を自分の環境のパスに書き換える
3. 上記3ファイルそれぞれの冒頭にある `json_path` / `fixed_value_checks_csv_path` も、テストごとに固定値なので必要に応じて書き換える
4. 対象ファイルを先頭から実行する

## ディレクトリ構成

```
FORGE ver1.0.html                          # 画面本体(起動ファイル)

web_tool/
├── js/
│   ├── main.js                     # 画面操作の配線(アップロード→設定→生成→ダウンロード)
│   ├── cdisc_variable_values.js    # EDC仕様JSONからcdisc_variable_values(選択肢展開済み)を組み立て
│   ├── generation_constraints.js   # presence_conditions等の生成制約テーブルを組み立て・適用
│   ├── dm_domain.js                # DMドメイン生成
│   ├── ae_domain.js                # AEドメイン生成
│   ├── ds_domain.js                # DSドメイン生成
│   ├── other_domains.js            # DM/AE/DS以外の各ドメイン生成(CM/MH/EG等)
│   ├── orres_realism.js            # LB/TR/VSのORRESを数値バリデーションに沿った値に置き換え
│   ├── dictionaries.js             # MedDRA/WHO Drug辞書バージョンの選択式読み込み
│   ├── dictionary_data_dir.js      # data/フォルダへのFile System Access API管理・D&D登録
│   ├── meddra_import.js            # MedDRAバージョンフォルダ(.asc)のパース
│   ├── who_drug_import.js          # WHO Drug/IDFバージョンフォルダのパース
│   └── util.js                     # 共通ユーティリティ(CSV変換・ダウンロード・乱数シード)
├── tools/
│   └── validate_seed_reproducibility.js  # 乱数シード機能の再現性検証
└── data/
    ├── versions.js                 # 辞書バージョン一覧のマニフェスト
    ├── meddra/                     # 変換済みMedDRAデータ(gitignore対象、フォルダのみ管理)
    └── who_drug/                   # 変換済みWHO Drug/IDFデータ(gitignore対象、フォルダのみ管理)

r_version/
├── r_version.Rproj                 # Rプロジェクト
├── constant.R                      # 定数(外部辞書のパス等)
├── user_input.R                    # 登録予定被験者数・登録開始日
├── load_edc_spec.R                 # EDC仕様JSONの読み込み〜各ドメイン生成までを一括実行
├── generate_random_date.R          # 日付項目の乱数生成
├── generate_brthdtc.R              # 年齢制約からBRTHDTC(生年月日)の許容範囲を算出
├── build_domain_common.R           # ドメイン生成の共通処理
├── build_dm_domain.R / build_ae_domain.R / build_ds_domain.R  # DM/AE/DSドメイン生成
├── build_cdisc_variable_values.R   # cdisc_variable_values組み立て
├── build_validator_table.R         # field_itemsのvalidatorsを縦持りtibble化
├── build_generation_constraints.R  # 生成制約テーブル一式の組み立て
├── build_field_reference_table.R   # Reference型フィールドの参照関係テーブル
├── build_meddra_soc_pt_llt.R       # MedDRA階層(SOC〜LLT)の結合
├── read_who_drug_idf.R             # WHO Drug/IDFテーブルの読み込み・結合
├── lb_reference_ranges.R / tr_orres_values.R / vs_orres_values.R / fa_orres_values.R
│                                   # LB/TR/VS/FAのORRESを数値バリデーションに沿った値に置き換え
├── test_config.R.sample            # test_config.Rのひな形(要コピー、詳細は上記「Rでのテスト」参照)
└── tools/
    ├── convert_meddra_to_js.R      # MedDRA辞書をweb_tool用JSファイルに変換するCLI
    ├── convert_who_drug_to_js.R    # WHO Drug/IDF辞書をweb_tool用JSファイルに変換するCLI
    ├── validate_common.R           # バリデーション共通関数・run_full_validation
    ├── validate_dm.R / validate_ae.R / validate_ds.R / validate_other_domains.R
    │                               # ドメインごとの検証関数
    ├── validate_test1_shared.R / validate_test2_shared.R
    │                               # test1/test2で共通の検証処理
    ├── validate_datasets_test3_fa.R  # test3のFA特別チェック(test3_webからsource)
    └── validate_datasets_test1_web.R / test2_web.R / test3_web.R
                                    # Webツール生成CSVを検証するテスト本体(現在運用中の3ファイル)
```

## Web版生成の自動化(Playwright)

`scripts/generate_dummy_data.js` で、Web版の「JSON読み込み→生成→ZIPで一括ダウンロード→展開」を自動化できる。
辞書(MedDRA/WHO Drug)は `web_tool/data/versions.js` に登録済みのバージョン(プルダウンのデフォルト=末尾/最新)を使う。
辞書バージョンの新規登録(D&D、ネイティブのフォルダ選択ダイアログを使う操作)は自動化の対象外なので、事前に手動で登録しておくこと。

### 必要な環境

- Node.js(npmが使えること)

### セットアップ・実行

```bash
npm install
npx playwright install chromium
node scripts/generate_dummy_data.js <jsonPath> <outputDir>
```

`outputDir` に `r_version/test_config.R` の `other_domains_web_csv_dir` と同じフォルダを指定すれば、
そのままRの検証スクリプト(前述「Rでのテスト」)から読み込める。

## ライセンス

MIT License(`LICENSE`参照)

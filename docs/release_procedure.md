# リリーステスト手順

masterにマージした後、本番フォルダへデプロイするまでの手順。
以下の `X.X` / `YYYY-MM-DD` はリリースするバージョン・日付に読み替える(例: `1.4` / `2026-11-11`)。

## 1. 準備

1. masterを最新化する

   ```bash
   git checkout master && git pull
   ```

2. 作業ツリーがクリーンであることを確認する

   ```bash
   git status
   ```

   以下が表示されればOK:
   - `On branch master`
   - `Your branch is up to date with 'origin/master'.`
   - `nothing to commit, working tree clean`

3. バージョンの整合を確認する

   ```bash
   grep -n -E "APP_VERSION =|APP_BUILD_DATE =" web_tool/js/main.js
   ```

   ```bash
   ls FORGE*.html
   ```

   ```bash
   git grep -n "FORGE ver[0-9]" -- ':!*.html'
   ```

   - `APP_VERSION` / `APP_BUILD_DATE` が想定どおりであること
   - `FORGE verX.X.html` が1ファイルだけであること
   - README・スクリプト内のファイル名参照が同じバージョンであること

## 2. UI回帰テスト

```bash
npm run test:integer-input
```

## 3. 生成とR検証

test1〜test6を順に、1テストずつ以下を行う。

1. ダウンロードフォルダに `dummy_data` フォルダ・`dummy_data.zip` があれば削除する
   (`dummy_data.zip` が残っていると、新しいZIPが `dummy_data (1).zip` 等の別名で保存されるため)
2. `FORGE verX.X.html` をブラウザで開き、画面に `vX.X (YYYY-MM-DD)` と表示されていることを確認する
3. testNのJSONを読み込み、下表の設定で「生成」を押す(表にない項目は初期値のまま)

   | テスト | 被験者数 | AEレコード数 | 乱数シード |
   |---|---|---|---|
   | test2 | 初期値 | 初期値 | 初期値 |
   | test3 | 5000 | 1000 | 100 |
   | test1・test4・test5・test6 | 1000 | 1000 | 初期値 |
4. 「ZIPで一括ダウンロード」を押し、ダウンロードフォルダの `dummy_data.zip` を展開する
5. `r_version/tools/validate_datasets_testN_web.R` を先頭から実行する
6. 結果を確認する
   - エラーで停止せず最後まで通ること
   - 出たワーニングの内容に問題がないこと

- 日付は「今日」基準で生成されるため、6テストとも同じ日に行う
- `r_version/test_config.R` の `other_domains_web_csv_dir` がダウンロードフォルダ内の `dummy_data` を指していること

## 4. タグ付け

1. テストしたmasterの最新コミットにタグを付ける

   ```bash
   git tag -a vX.X -m "vX.X (YYYY-MM-DD)"
   ```

2. タグをリモートに送る

   ```bash
   git push origin vX.X
   ```

3. タグを確認する

   ```bash
   git show vX.X --stat
   ```

- 手順1〜3の間にmasterへコミットを追加しない(テストした内容とタグがずれるため)

## 5. デプロイ(Windowsで実施)

1. Windows側のリポジトリでmasterを最新化する

   ```powershell
   git checkout master; git pull
   ```

2. 作業ツリーがクリーンであることを確認する

   ```powershell
   git status
   ```

   以下が表示されればOK:
   - `On branch master`
   - `Your branch is up to date with 'origin/master'.`
   - `nothing to commit, working tree clean`

3. デプロイスクリプトを実行する

   ```powershell
   powershell -ExecutionPolicy Bypass -File .\scripts\deploy_to_production.ps1
   ```

   - 「リポジトリ側にはもう無いファイル」の一覧が出たら控えておく(手順6で使う)
   - 確認プロンプトで `y` を入力する

## 6. 本番確認

1. 本番フォルダの `FORGE verX.X.html` を開き、画面に `vX.X (YYYY-MM-DD)` と表示されていることを確認する
2. 1回生成し、ZIPがダウンロードできることを確認する
3. 本番フォルダに古い `FORGE ver*.html`(例: `FORGE ver1.3.html`)が残っていないか確認し、残っていれば削除する
4. 手順5で控えた一覧に `web_tool\` 配下のファイルがあれば、削除するか判断する

<#
.SYNOPSIS
  リポジトリ直下の FORGE verX.X.html・README.md と web_tool フォルダを、本番フォルダ(Box同期)にコピーする。

.DESCRIPTION
  コピー対象は以下の3つだけ:
    - リポジトリ直下の "FORGE ver*.html"(現在のバージョンファイル。複数あればエラーにする)
    - リポジトリ直下の README.md
    - web_tool フォルダの中身(既存ファイルは上書き、本番側にしか無いファイルは削除しない)
  dictionary_data フォルダ(本番の実データ)は対象外。一切触らない。

.PARAMETER DestinationRoot
  本番フォルダのパス。省略時は既定の本番パスを使う。

.PARAMETER NoConfirm
  確認プロンプトを出さずに実行する(自動化用)。

.EXAMPLE
  .\scripts\deploy_to_production.ps1
#>
param(
  [string]$DestinationRoot = "C:\Users\c0002691\Box\Stat\Tools\FORGE",
  [switch]$NoConfirm
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot

$htmlFiles = Get-ChildItem -Path $repoRoot -Filter "FORGE ver*.html" -File
if ($htmlFiles.Count -eq 0) {
  throw "リポジトリ直下に FORGE ver*.html が見つかりません: $repoRoot"
}
if ($htmlFiles.Count -gt 1) {
  throw "FORGE ver*.html が複数見つかりました。どれが現在のバージョンか判断できません: $($htmlFiles.Name -join ', ')"
}
$htmlFile = $htmlFiles[0]

$readmeFile = Join-Path $repoRoot "README.md"
if (-not (Test-Path $readmeFile)) {
  throw "リポジトリ直下に README.md が見つかりません: $repoRoot"
}

if (-not (Test-Path $DestinationRoot)) {
  throw "本番フォルダが見つかりません: $DestinationRoot"
}

Write-Output "コピー元: $repoRoot"
Write-Output "コピー先: $DestinationRoot"
Write-Output ""
Write-Output "コピーする内容:"
Write-Output "  - $($htmlFile.Name)"
Write-Output "  - README.md"
Write-Output "  - web_tool\ 配下一式(上書き・追加のみ。本番側のみに存在するファイルは削除しない)"
Write-Output ""
Write-Output "対象外(一切触らない): dictionary_data\"
Write-Output ""

# 本番側にあるがリポジトリ側にはもう無いファイルの一覧を表示する(削除はしない。把握用)
$staleHtmlFiles = Get-ChildItem -Path $DestinationRoot -Filter "FORGE ver*.html" -File -ErrorAction SilentlyContinue |
  Where-Object { $_.Name -ne $htmlFile.Name }

$destWebToolForDiff = Join-Path $DestinationRoot "web_tool"
$staleWebToolFiles = @()
if (Test-Path $destWebToolForDiff) {
  $sourceRelativePaths = Get-ChildItem -Path (Join-Path $repoRoot "web_tool") -File -Recurse |
    ForEach-Object { $_.FullName.Substring((Join-Path $repoRoot "web_tool").Length + 1) }
  $destRelativePaths = Get-ChildItem -Path $destWebToolForDiff -File -Recurse |
    ForEach-Object { $_.FullName.Substring($destWebToolForDiff.Length + 1) }
  $staleWebToolFiles = $destRelativePaths | Where-Object { $sourceRelativePaths -notcontains $_ }
}

if ($staleHtmlFiles.Count -gt 0 -or $staleWebToolFiles.Count -gt 0) {
  Write-Output "リポジトリ側にはもう無いファイル(本番側に残る。削除はしません):"
  $staleHtmlFiles | ForEach-Object { Write-Output "  - $($_.Name)" }
  $staleWebToolFiles | ForEach-Object { Write-Output "  - web_tool\$_" }
  Write-Output ""
}

if (-not $NoConfirm) {
  $answer = Read-Host "本番フォルダに書き込みます。よろしいですか? (y/N)"
  if ($answer -ne "y" -and $answer -ne "Y") {
    Write-Output "中止しました"
    exit 0
  }
}

Copy-Item -Path $htmlFile.FullName -Destination $DestinationRoot -Force
Copy-Item -Path $readmeFile -Destination $DestinationRoot -Force

$destWebTool = Join-Path $DestinationRoot "web_tool"
New-Item -ItemType Directory -Path $destWebTool -Force | Out-Null
Copy-Item -Path (Join-Path $repoRoot "web_tool\*") -Destination $destWebTool -Recurse -Force

Write-Output ""
Write-Output "完了: $($htmlFile.Name)・README.md と web_tool\ を $DestinationRoot にコピーしました"

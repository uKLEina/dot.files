<#
.SYNOPSIS
  Windowsネイティブ環境のEmacsセットアップを一括で行う。

.DESCRIPTION
  dot.filesリポジトリを任意の場所にcloneした後、リポジトリ内から実行する想定。
  以下を冪等に実行する (済んでいる項目はスキップ):
    1. HOME環境変数をユーザープロファイル (C:\Users\<name>) に設定
    2. ~/.emacs.d, ~/.skk.d のジャンクション作成 (+旧%APPDATA%側の掃除)
    3. cmigemo (win64バイナリ+utf-8辞書) を .emacs.d\cmigemo に配置
    4. SKK辞書 (SKK-JISYO.L ほか) を .emacs.d\skk-get-jisyo にダウンロード
    5. HackGen / Symbols Nerd Font をユーザーフォントとしてインストール
    6. MSYS2のlibgccjitが導入済みならユーザーPATHに追加 (native-comp用)
  管理者権限は不要。

.EXAMPLE
  git clone -c core.autocrlf=false https://github.com/uKLEina/dot.files.git
  powershell -ExecutionPolicy Bypass -File dot.files\.emacs.d\setup-windows.ps1
#>
[CmdletBinding()]
param(
    [switch]$SkipCmigemo,
    [switch]$SkipSkkDict,
    [switch]$SkipFonts
)

$ErrorActionPreference = 'Stop'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

function Step([string]$msg) { Write-Host "==> $msg" -ForegroundColor Cyan }
function Ok([string]$msg)   { Write-Host "    $msg" }
function Warn([string]$msg) { Write-Host "  ! $msg" -ForegroundColor Yellow }

# --- 場所の解決 (スクリプト自身がリポジトリの.emacs.d内にある前提) ---------
$EmacsDir = $PSScriptRoot
$RepoRoot = Split-Path $EmacsDir

# --- 前提チェック ---------------------------------------------------------
Step '改行コード設定の確認'
try {
    $autocrlf = git -C $RepoRoot config --get core.autocrlf 2>$null
    if ($autocrlf -eq 'true') {
        Warn 'このcloneは core.autocrlf=true。elispやshがCRLF化してる可能性がある。'
        Warn "  git -C $RepoRoot config core.autocrlf false して checkout し直し推奨"
    } else {
        Ok 'OK'
    }
} catch {
    Warn 'gitが見つからないため確認をスキップ (magit等を使うなら Git for Windows を入れて)'
}

# --- 1. HOME環境変数 ------------------------------------------------------
Step "HOME を $env:USERPROFILE に設定"
if ([Environment]::GetEnvironmentVariable('HOME', 'User') -eq $env:USERPROFILE) {
    Ok '設定済み'
} else {
    [Environment]::SetEnvironmentVariable('HOME', $env:USERPROFILE, 'User')
    Ok '設定した (既存の端末/アプリには反映されないので起動し直すこと)'
}
$env:HOME = $env:USERPROFILE

# --- 2. ジャンクション ----------------------------------------------------
Step 'ジャンクション作成'
function Ensure-Junction([string]$link, [string]$target) {
    if (Test-Path $link) {
        $item = Get-Item $link -Force
        if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint) {
            Ok "$link は作成済み"
        } else {
            Warn "$link が実体ディレクトリとして存在する。中身 (elpa等) を $target へ移して削除後、再実行して"
        }
        return
    }
    New-Item -ItemType Junction -Path $link -Target $target | Out-Null
    Ok "$link -> $target"
}
Ensure-Junction (Join-Path $env:USERPROFILE '.emacs.d') $EmacsDir
Ensure-Junction (Join-Path $env:USERPROFILE '.skk.d')  (Join-Path $RepoRoot '.skk.d')

# 旧HOME (%APPDATA%) 側に残ったジャンクションの掃除
foreach ($name in '.emacs.d', '.skk.d') {
    $old = Join-Path $env:APPDATA $name
    if (-not (Test-Path $old)) { continue }
    if ((Get-Item $old -Force).Attributes -band [IO.FileAttributes]::ReparsePoint) {
        cmd /c rmdir $old
        Ok "旧ジャンクション $old を削除"
    } else {
        Warn "$old が実体ディレクトリとして残っている。必要な中身を移して手動で削除して"
    }
}

# --- 3. cmigemo -----------------------------------------------------------
if (-not $SkipCmigemo) {
    Step 'cmigemo'
    $cmigemoDir = Join-Path $EmacsDir 'cmigemo'
    if (Test-Path (Join-Path $cmigemoDir 'cmigemo.exe')) {
        Ok '配置済み'
    } else {
        try {
            $rel = Invoke-RestMethod 'https://api.github.com/repos/koron/cmigemo/releases/latest'
            $asset = $rel.assets | Where-Object { $_.name -match 'win64.*\.zip$' } | Select-Object -First 1
            if (-not $asset) { throw 'リリースにwin64のzipが見つからない' }
            $tmp = Join-Path $env:TEMP 'cmigemo-setup'
            if (Test-Path $tmp) { Remove-Item $tmp -Recurse -Force }
            New-Item -ItemType Directory -Force -Path $tmp | Out-Null
            $zip = Join-Path $tmp $asset.name
            Invoke-WebRequest $asset.browser_download_url -OutFile $zip
            Expand-Archive $zip -DestinationPath $tmp
            $exe = Get-ChildItem $tmp -Recurse -Filter 'cmigemo.exe' | Select-Object -First 1
            if (-not $exe) { throw 'zip内にcmigemo.exeが見つからない' }
            New-Item -ItemType Directory -Force -Path $cmigemoDir | Out-Null
            Copy-Item (Join-Path $exe.DirectoryName '*') $cmigemoDir -Recurse -Force
            # zipの構成によってはdictがexeと別階層にあるので拾い直す
            if (-not (Test-Path (Join-Path $cmigemoDir 'dict\utf-8\migemo-dict'))) {
                $dict = Get-ChildItem $tmp -Recurse -Filter 'migemo-dict' |
                        Where-Object { $_.FullName -match 'utf-8' } | Select-Object -First 1
                if ($dict) { Copy-Item $dict.Directory.Parent.FullName -Destination $cmigemoDir -Recurse -Force }
            }
            if (Test-Path (Join-Path $cmigemoDir 'dict\utf-8\migemo-dict')) {
                Ok "cmigemo を $cmigemoDir に配置"
            } else {
                Warn 'dict\utf-8\migemo-dict を配置できなかった。zipの構成を確認して手で置いて'
            }
            Remove-Item $tmp -Recurse -Force
        } catch {
            Warn "cmigemoの自動配置に失敗: $($_.Exception.Message)"
            Warn "手動で https://github.com/koron/cmigemo/releases のwin64 zipを $cmigemoDir に展開して"
        }
    }
}

# --- 4. SKK辞書 -----------------------------------------------------------
if (-not $SkipSkkDict) {
    Step 'SKK辞書'
    $jisyoDir = Join-Path $EmacsDir 'skk-get-jisyo'
    New-Item -ItemType Directory -Force -Path $jisyoDir | Out-Null
    function Expand-Gz([string]$src, [string]$dst) {
        $in = [IO.File]::OpenRead($src)
        try {
            $gz = New-Object IO.Compression.GZipStream($in, [IO.Compression.CompressionMode]::Decompress)
            $out = [IO.File]::Create($dst)
            try { $gz.CopyTo($out) } finally { $out.Dispose(); $gz.Dispose() }
        } finally { $in.Dispose() }
    }
    $names = @('SKK-JISYO.L', 'SKK-JISYO.jinmei', 'SKK-JISYO.fullname', 'SKK-JISYO.geo',
               'SKK-JISYO.propernoun', 'SKK-JISYO.station', 'SKK-JISYO.law', 'SKK-JISYO.okinawa')
    foreach ($n in $names) {
        $dst = Join-Path $jisyoDir $n
        if (Test-Path $dst) { Ok "$n 済み"; continue }
        try {
            $gz = "$dst.gz"
            Invoke-WebRequest "https://skk-dev.github.io/dict/$n.gz" -OutFile $gz
            Expand-Gz $gz $dst
            Remove-Item $gz
            Ok "$n を取得"
        } catch {
            Warn "$n の取得に失敗: $($_.Exception.Message)"
        }
    }
}

# --- 5. フォント (ユーザーインストール、管理者権限不要) -------------------
if (-not $SkipFonts) {
    Step 'フォント'
    $fontDir = Join-Path $env:LOCALAPPDATA 'Microsoft\Windows\Fonts'
    New-Item -ItemType Directory -Force -Path $fontDir | Out-Null
    $fontReg = 'HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts'
    if (-not (Test-Path $fontReg)) { New-Item -Path $fontReg -Force | Out-Null }
    function Install-UserFont([string]$ttf) {
        $file = [IO.Path]::GetFileName($ttf)
        $dest = Join-Path $fontDir $file
        if (Test-Path $dest) { return $false }
        Copy-Item $ttf $dest
        New-ItemProperty -Path $fontReg -Value $dest -PropertyType String -Force `
                         -Name ('{0} (TrueType)' -f [IO.Path]::GetFileNameWithoutExtension($file)) | Out-Null
        return $true
    }
    function Install-FontZip([string]$url, [string]$label) {
        try {
            $tmp = Join-Path $env:TEMP ('font-' + [IO.Path]::GetRandomFileName())
            New-Item -ItemType Directory -Force -Path $tmp | Out-Null
            $zip = Join-Path $tmp 'font.zip'
            Invoke-WebRequest $url -OutFile $zip
            Expand-Archive $zip -DestinationPath $tmp
            $n = 0
            Get-ChildItem $tmp -Recurse -Filter '*.ttf' | ForEach-Object {
                if (Install-UserFont $_.FullName) { $n++ }
            }
            Ok ("{0}: {1}ファイルをインストール" -f $label, $n)
            Remove-Item $tmp -Recurse -Force
        } catch {
            Warn ("{0} のインストールに失敗: {1}" -f $label, $_.Exception.Message)
        }
    }
    # nerd-icons / doom-modeline のアイコン用
    Install-FontZip 'https://github.com/ryanoasis/nerd-fonts/releases/latest/download/NerdFontsSymbolsOnly.zip' 'Symbols Nerd Font'
    # エディタ本文用 (init.elのWindowsフォント候補の第一候補)
    try {
        $rel = Invoke-RestMethod 'https://api.github.com/repos/yuru7/HackGen/releases/latest'
        $asset = $rel.assets | Where-Object { $_.name -match '^HackGen_v.*\.zip$' } | Select-Object -First 1
        if ($asset) { Install-FontZip $asset.browser_download_url 'HackGen' }
        else { Warn 'HackGenリリースにzipが見つからない' }
    } catch {
        Warn "HackGenの取得に失敗: $($_.Exception.Message)"
    }
}

# --- 6. native-comp用ツールチェーン (MSYS2導入済みの場合のみ) --------------
# Emacsのlibgccjit検出はプロセス初期化時のPATHを見るため、elisp側からの追加では
# 間に合わない。ユーザー環境変数のPathに恒久追加する必要がある。
Step 'native-comp (libgccjit)'
$jitDir = @('C:\msys64\mingw64\bin', 'C:\msys64\ucrt64\bin') |
          Where-Object { Test-Path (Join-Path $_ 'libgccjit-0.dll') } |
          Select-Object -First 1
if ($jitDir) {
    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    if (-not $userPath) { $userPath = '' }
    if (($userPath -split ';') -contains $jitDir) {
        Ok 'ユーザーPATHに設定済み'
    } else {
        $newPath = ($userPath.TrimEnd(';') + ';' + $jitDir).TrimStart(';')
        [Environment]::SetEnvironmentVariable('Path', $newPath, 'User')
        Ok "$jitDir をユーザーPATHの末尾に追加"
    }
} else {
    Warn 'libgccjitが見つからないためスキップ。native-compを使うならMSYS2で以下を入れて再実行:'
    Warn '  pacman -S mingw-w64-x86_64-libgccjit mingw-w64-x86_64-gcc mingw-w64-x86_64-binutils'
}

# --- 完了 -----------------------------------------------------------------
Step '完了'
Write-Host @'

残りの手作業:
  1. 端末とEmacsを起動し直す (HOME環境変数は新しいプロセスからしか効かない)
  2. 初回のEmacs起動時に my/windows-packages が自動インストールされる (少し待つ)
  3. gptelを使うなら OPENAI_API_KEY 環境変数を設定
  4. forgeを使うなら ~/.authinfo を配置
'@

#!/bin/bash
set -e

# tree-sitter言語文法をビルドして~/.emacs.d/tree-sitter/にインストールするスクリプト
#
# 事前準備
# libtree-sitter-devをDebian公式のaptリポジトリからインストールしておく
# tree-sitter-cliはcargoで最新版をインストールしておく
#
# 使い方:
#   build-treesit-grammar.sh <dir>
#
# 単一リポジトリ:
#   build-treesit-grammar.sh /path/to/tree-sitter-python
#
# 複数リポジトリを一括:
#   build-treesit-grammar.sh /path/to/tree-sitter/
#   → 配下のgrammar.jsを全て探索してビルド
#
# モノレポ（typescript等）も自動検出:
#   build-treesit-grammar.sh /path/to/tree-sitter-typescript
#   → typescript/, tsx/ 両方ビルド

if [ $# -lt 1 ]; then
    echo "使い方: $0 <dir>"
    echo ""
    echo "例:"
    echo "  $0 /path/to/tree-sitter-python          # 単一言語"
    echo "  $0 /path/to/tree-sitter/                 # 配下を一括ビルド"
    exit 1
fi

TARGET_DIR="$1"
INSTALL_DIR="$HOME/.emacs.d/tree-sitter"

if [ ! -d "$TARGET_DIR" ]; then
    echo "エラー: ディレクトリが見つかりません: $TARGET_DIR"
    exit 1
fi

# tree-sitter CLIの存在確認
if ! command -v tree-sitter &> /dev/null; then
    echo "エラー: tree-sitter CLIが見つかりません"
    echo "インストールしてください: cargo install tree-sitter-cli"
    exit 1
fi

# EmacsからABIバージョンを取得
if ! command -v emacs &> /dev/null; then
    echo "エラー: Emacsが見つかりません（ABIバージョンの自動取得に必要です）"
    exit 1
fi
ABI_VERSION=$(emacs --batch --eval "(progn (require 'treesit) (princ (treesit-library-abi-version nil)))" 2>/dev/null)
if [ -z "$ABI_VERSION" ]; then
    echo "エラー: EmacsからABIバージョンを取得できませんでした"
    exit 1
fi

mkdir -p "$INSTALL_DIR"

# 言語名をgrammar.jsのあるディレクトリから推測
detect_language() {
    local grammar_dir="$1"
    local dir_name
    dir_name=$(basename "$grammar_dir")

    # ディレクトリ名がtree-sitter-xxxならxxxを使う
    if [[ "$dir_name" == tree-sitter-* ]]; then
        echo "${dir_name#tree-sitter-}"
    else
        # モノレポのサブディレクトリ（typescript, tsx等）はそのまま使う
        echo "$dir_name"
    fi
}

# 単一の文法をビルド・インストール
build_one() {
    local grammar_dir="$1"
    local language
    language=$(detect_language "$grammar_dir")

    echo "--- $language ($grammar_dir)"

    # npm依存関係のインストール（package.jsonがある場合）
    # モノレポの場合、親ディレクトリのpackage.jsonも確認
    local repo_root
    repo_root=$(cd "$grammar_dir" && git rev-parse --show-toplevel 2>/dev/null || echo "$grammar_dir")
    if [ -f "$repo_root/package.json" ] && [ -d "$repo_root/node_modules" ] 2>/dev/null; then
        : # 既にインストール済み
    elif [ -f "$repo_root/package.json" ]; then
        echo "  npm install..."
        (cd "$repo_root" && npm install --ignore-scripts > /dev/null 2>&1) || true
    fi

    # パーサー生成
    (cd "$grammar_dir" && tree-sitter generate --abi "$ABI_VERSION") || { echo "  失敗: generate"; return 1; }

    # ソースファイル収集
    local src_files=("$grammar_dir/src/parser.c")
    [ -f "$grammar_dir/src/scanner.c" ] && src_files+=("$grammar_dir/src/scanner.c")
    [ -f "$grammar_dir/src/scanner.cc" ] && src_files+=("$grammar_dir/src/scanner.cc")

    # コンパイル
    local so_file="$grammar_dir/libtree-sitter-${language}.so"
    if [ -f "$grammar_dir/src/scanner.cc" ]; then
        g++ -shared -fPIC -fno-exceptions -I "$grammar_dir/src" -o "$so_file" "${src_files[@]}" -std=c++14
    else
        cc -shared -fPIC -I "$grammar_dir/src" -o "$so_file" "${src_files[@]}" -std=c11
    fi || { echo "  失敗: compile"; return 1; }

    # インストール
    /bin/cp -f "$so_file" "$INSTALL_DIR/"

    # ロードテスト
    if emacs --batch --eval "(progn (require 'treesit) (unless (treesit-language-available-p '$language) (kill-emacs 1)))" 2>/dev/null; then
        echo "  OK"
    else
        echo "  警告: Emacsでのロードに失敗しました"
        return 1
    fi
}

# grammar.jsを探索してビルド対象を収集
# node_modules, test, examples等は除外
find_grammars() {
    local dir="$1"
    find "$dir" -name grammar.js \
        -not -path "*/node_modules/*" \
        -not -path "*/test/*" \
        -not -path "*/examples/*" \
        -not -path "*/templates/*" \
        | while read -r f; do dirname "$f"; done \
        | sort
}

GRAMMAR_DIRS=()
if [ -f "$TARGET_DIR/grammar.js" ]; then
    # 直下にgrammar.jsがある → 単一ビルド
    GRAMMAR_DIRS=("$(cd "$TARGET_DIR" && pwd)")
else
    # 配下を探索
    while IFS= read -r d; do
        GRAMMAR_DIRS+=("$d")
    done < <(find_grammars "$(cd "$TARGET_DIR" && pwd)")
fi

if [ ${#GRAMMAR_DIRS[@]} -eq 0 ]; then
    echo "エラー: grammar.jsが見つかりません: $TARGET_DIR"
    exit 1
fi

echo "=========================================="
echo "tree-sitter文法のビルド (ABI $ABI_VERSION)"
echo "対象: ${#GRAMMAR_DIRS[@]} 言語"
echo "=========================================="
echo ""

SUCCESS=0
FAILED=0
FAILED_NAMES=()

for grammar_dir in "${GRAMMAR_DIRS[@]}"; do
    if build_one "$grammar_dir"; then
        SUCCESS=$((SUCCESS + 1))
    else
        FAILED=$((FAILED + 1))
        FAILED_NAMES+=("$(detect_language "$grammar_dir")")
    fi
done

echo ""
echo "=========================================="
echo "結果: ${SUCCESS}/${#GRAMMAR_DIRS[@]} 成功"
if [ $FAILED -gt 0 ]; then
    echo "失敗: ${FAILED_NAMES[*]}"
fi
echo "=========================================="

#!/bin/bash
set -e

# tree-sitter言語文法をビルドして~/.emacs.d/tree-sitter/にインストールするスクリプト
#
# 使い方:
#   ./build-treesit-grammar.sh <tree-sitter-repo-dir> <language-name> [abi-version] [grammar-subdir]
#
# 例:
#   ./build-treesit-grammar.sh /path/to/tree-sitter-python python
#   ./build-treesit-grammar.sh /path/to/tree-sitter-python python 14
#   ./build-treesit-grammar.sh . python 14
#   ./build-treesit-grammar.sh /path/to/tree-sitter-markdown markdown 14 tree-sitter-markdown

# 引数チェック
if [ $# -lt 2 ]; then
    echo "エラー: 引数が不足しています"
    echo ""
    echo "使い方: $0 <tree-sitter-repo-dir> <language-name> [abi-version] [grammar-subdir]"
    echo ""
    echo "例:"
    echo "  $0 /path/to/tree-sitter-python python"
    echo "  $0 /path/to/tree-sitter-python python 14"
    echo "  $0 . python 14"
    echo "  $0 /path/to/tree-sitter-markdown markdown 14 tree-sitter-markdown"
    exit 1
fi

REPO_DIR="$1"
LANGUAGE="$2"
ABI_VERSION="${3:-14}"  # デフォルトはABIバージョン14
GRAMMAR_SUBDIR="${4:-}"  # オプション: grammar.jsがあるサブディレクトリ
INSTALL_DIR="$HOME/.emacs.d/tree-sitter"

# ディレクトリの存在確認
if [ ! -d "$REPO_DIR" ]; then
    echo "エラー: ディレクトリが見つかりません: $REPO_DIR"
    exit 1
fi

# grammar.jsの場所を決定
if [ -n "$GRAMMAR_SUBDIR" ]; then
    # サブディレクトリが指定されている場合
    GRAMMAR_DIR="$REPO_DIR/$GRAMMAR_SUBDIR"
    if [ ! -d "$GRAMMAR_DIR" ]; then
        echo "エラー: サブディレクトリが見つかりません: $GRAMMAR_DIR"
        exit 1
    fi
else
    # サブディレクトリが指定されていない場合は、リポジトリルートを使用
    GRAMMAR_DIR="$REPO_DIR"
fi

# grammar.jsの存在確認
if [ ! -f "$GRAMMAR_DIR/grammar.js" ]; then
    echo "エラー: grammar.jsが見つかりません: $GRAMMAR_DIR/grammar.js"
    echo "指定されたディレクトリはtree-sitterリポジトリではないようです"
    echo ""
    echo "モノレポ構造の場合は、第4引数でサブディレクトリを指定してください:"
    echo "  $0 $REPO_DIR $LANGUAGE $ABI_VERSION <grammar-subdir>"
    exit 1
fi

# tree-sitter CLIの存在確認
if ! command -v tree-sitter &> /dev/null; then
    echo "エラー: tree-sitter CLIが見つかりません"
    echo "インストールしてください: sudo apt install tree-sitter-cli"
    exit 1
fi

# インストールディレクトリの作成
mkdir -p "$INSTALL_DIR"

echo "=========================================="
echo "tree-sitter文法のビルドとインストール"
echo "=========================================="
echo "リポジトリ: $REPO_DIR"
echo "文法ディレクトリ: $GRAMMAR_DIR"
echo "言語: $LANGUAGE"
echo "ABIバージョン: $ABI_VERSION"
echo "インストール先: $INSTALL_DIR"
echo "=========================================="
echo ""

# 文法ディレクトリに移動
cd "$GRAMMAR_DIR"

# パーサーの生成
echo "[1/4] パーサーを生成中 (tree-sitter generate --abi $ABI_VERSION)..."
tree-sitter generate --abi "$ABI_VERSION"

# ABIバージョンの確認
if [ -f "src/parser.c" ]; then
    GENERATED_ABI=$(grep "#define LANGUAGE_VERSION" src/parser.c | awk '{print $3}')
    echo "✓ 生成されたABIバージョン: $GENERATED_ABI"
    if [ "$GENERATED_ABI" != "$ABI_VERSION" ]; then
        echo "警告: 指定されたABIバージョン($ABI_VERSION)と生成されたバージョン($GENERATED_ABI)が一致しません"
    fi
else
    echo "警告: src/parser.cが見つかりません"
fi
echo ""

# クリーンビルド
echo "[2/4] クリーンビルド中 (make clean && make)..."
make clean > /dev/null 2>&1 || true
make

# 生成されたsoファイルを確認
SO_FILE="libtree-sitter-${LANGUAGE}.so"
if [ ! -f "$SO_FILE" ]; then
    echo "エラー: $SO_FILE が生成されませんでした"
    exit 1
fi
echo "✓ ビルド完了: $SO_FILE"
echo ""

# インストール
echo "[3/4] インストール中..."
/bin/cp -f "$SO_FILE" "$INSTALL_DIR/"
echo "✓ インストール完了: $INSTALL_DIR/$SO_FILE"
echo ""

# Emacsでの確認
echo "[4/4] Emacsでの確認..."
if command -v emacs &> /dev/null; then
    EMACS_ABI=$(emacs --batch --eval "(progn (require 'treesit) (princ (treesit-library-abi-version nil)))" 2>/dev/null)
    echo "✓ Emacsが期待するABIバージョン: $EMACS_ABI"

    # 言語のロードテスト
    if emacs --batch --eval "(progn (require 'treesit) (treesit-language-available-p '$LANGUAGE t))" 2>/dev/null; then
        LANG_ABI=$(emacs --batch --eval "(progn (require 'treesit) (treesit-language-available-p '$LANGUAGE t) (princ (treesit-library-abi-version '$LANGUAGE)))" 2>/dev/null)
        echo "✓ ${LANGUAGE}ライブラリのABIバージョン: $LANG_ABI"

        if [ "$EMACS_ABI" = "$LANG_ABI" ]; then
            echo "✓ ABIバージョンが一致しています！"
        else
            echo "⚠ 警告: ABIバージョンが一致しません (Emacs: $EMACS_ABI, Library: $LANG_ABI)"
        fi
    else
        echo "⚠ 警告: ライブラリのロードに失敗しました"
    fi
else
    echo "⚠ Emacsが見つからないため、確認をスキップします"
fi
echo ""

echo "=========================================="
echo "✓ すべての処理が完了しました！"
echo "=========================================="
echo ""
echo "Emacsで以下のコマンドを実行して${LANGUAGE}-ts-modeが使えることを確認してください:"
echo "  M-x ${LANGUAGE}-ts-mode"
echo ""

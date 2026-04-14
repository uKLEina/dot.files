#!/bin/bash
set -e

# 複数のtree-sitter言語文法を一括でクローン・ビルド・インストールするスクリプト
#
# 使い方:
#   ./install-all-treesit-grammars.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_SCRIPT="$SCRIPT_DIR/build-treesit-grammar.sh"

if [ ! -f "$BUILD_SCRIPT" ]; then
    echo "エラー: build-treesit-grammar.shが見つかりません: $BUILD_SCRIPT"
    exit 1
fi

if ! command -v ghq &> /dev/null; then
    echo "エラー: ghqコマンドが見つかりません"
    echo "インストールしてください: go install github.com/x-motemen/ghq@latest"
    exit 1
fi

# インストール対象のリポジトリ一覧
REPOS=(
    https://github.com/tree-sitter/tree-sitter-bash.git
    https://github.com/tree-sitter/tree-sitter-c.git
    https://github.com/tree-sitter/tree-sitter-cpp.git
    https://github.com/tree-sitter/tree-sitter-css.git
    https://github.com/tree-sitter/tree-sitter-go.git
    https://github.com/tree-sitter/tree-sitter-html.git
    https://github.com/tree-sitter/tree-sitter-java.git
    https://github.com/tree-sitter/tree-sitter-javascript.git
    https://github.com/tree-sitter/tree-sitter-jsdoc.git
    https://github.com/tree-sitter/tree-sitter-json.git
    https://github.com/tree-sitter/tree-sitter-php.git
    https://github.com/tree-sitter/tree-sitter-python.git
    https://github.com/tree-sitter/tree-sitter-rust.git
    https://github.com/tree-sitter/tree-sitter-typescript.git
    https://github.com/tree-sitter-grammars/tree-sitter-toml.git
    https://github.com/tree-sitter-grammars/tree-sitter-yaml.git
    https://github.com/claytonrcarter/tree-sitter-phpdoc.git
)

GHQ_ROOT=$(ghq root)

echo "=========================================="
echo "ステップ 1/2: リポジトリのクローン/更新"
echo "=========================================="
echo ""

for repo in "${REPOS[@]}"; do
    echo "  $repo"
    # ビルド時に変更されたsrc/parser.c等をリセットしてからpull
    repo_path="$GHQ_ROOT/$(echo "$repo" | sed 's|https://||; s|\.git$||')"
    if [ -d "$repo_path" ]; then
        git -C "$repo_path" checkout -- . 2>/dev/null || true
    fi
    ghq get -u "$repo" 2>&1 | sed 's/^/    /' || true
done

echo ""
echo "=========================================="
echo "ステップ 2/2: ビルドとインストール"
echo "=========================================="
echo ""

SUCCESS=0
FAILED=0
FAILED_NAMES=()

for repo in "${REPOS[@]}"; do
    # URLからローカルパスを構築
    repo_path="$GHQ_ROOT/$(echo "$repo" | sed 's|https://||; s|\.git$||')"

    if [ ! -d "$repo_path" ]; then
        echo "--- スキップ (クローン失敗): $repo_path"
        FAILED=$((FAILED + 1))
        FAILED_NAMES+=("$(basename "$repo_path")")
        continue
    fi

    # build-treesit-grammar.shに委譲（モノレポも自動検出される）
    if "$BUILD_SCRIPT" "$repo_path" 2>&1; then
        SUCCESS=$((SUCCESS + 1))
    else
        FAILED=$((FAILED + 1))
        FAILED_NAMES+=("$(basename "$repo_path")")
    fi
done

echo ""
echo "=========================================="
echo "結果: ${SUCCESS}/${#REPOS[@]} リポジトリ成功"
if [ $FAILED -gt 0 ]; then
    echo "失敗: ${FAILED_NAMES[*]}"
fi
echo "=========================================="

#!/bin/bash
set -e

# 複数のtree-sitter言語文法を一括でクローン・ビルド・インストールするスクリプト
#
# 使い方:
#   ./install-all-treesit-grammars.sh [abi-version]
#
# 例:
#   ./install-all-treesit-grammars.sh      # デフォルトABI 14
#   ./install-all-treesit-grammars.sh 14   # ABI 14を明示指定

# ABIバージョン（デフォルト: 14）
ABI_VERSION="${1:-14}"

# このスクリプトのディレクトリを取得
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_SCRIPT="$SCRIPT_DIR/build-treesit-grammar.sh"

# build-treesit-grammar.shの存在確認
if [ ! -f "$BUILD_SCRIPT" ]; then
    echo "エラー: build-treesit-grammar.shが見つかりません: $BUILD_SCRIPT"
    exit 1
fi

# ghqコマンドの存在確認
if ! command -v ghq &> /dev/null; then
    echo "エラー: ghqコマンドが見つかりません"
    echo "インストールしてください: go install github.com/x-motemen/ghq@latest"
    exit 1
fi

# ghqのルートディレクトリを取得
GHQ_ROOT=$(ghq root)
echo "=========================================="
echo "tree-sitter文法の一括インストール"
echo "=========================================="
echo "ABIバージョン: $ABI_VERSION"
echo "GHQルート: $GHQ_ROOT"
echo "=========================================="
echo ""

# 言語定義の配列
# フォーマット: "言語名|リポジトリURL|サブディレクトリ|npm_install要否"
declare -a LANGUAGES=(
    "bash|https://github.com/tree-sitter/tree-sitter-bash.git||"
    "c|https://github.com/tree-sitter/tree-sitter-c.git||"
    "cpp|https://github.com/tree-sitter/tree-sitter-cpp.git||yes"
    "css|https://github.com/tree-sitter/tree-sitter-css.git||"
    "go|https://github.com/tree-sitter/tree-sitter-go.git||"
    "html|https://github.com/tree-sitter/tree-sitter-html.git||"
    "java|https://github.com/tree-sitter/tree-sitter-java.git||"
    "javascript|https://github.com/tree-sitter/tree-sitter-javascript.git||"
    "jsdoc|https://github.com/tree-sitter/tree-sitter-jsdoc.git||"
    "json|https://github.com/tree-sitter/tree-sitter-json.git||"
    "php|https://github.com/tree-sitter/tree-sitter-php.git|php|"
    "phpdoc|https://github.com/claytonrcarter/tree-sitter-phpdoc.git||"
    "python|https://github.com/tree-sitter/tree-sitter-python.git||"
    "rust|https://github.com/tree-sitter/tree-sitter-rust.git||"
    "toml|https://github.com/tree-sitter-grammars/tree-sitter-toml.git||"
    "typescript|https://github.com/tree-sitter/tree-sitter-typescript.git|typescript|yes"
    "tsx|https://github.com/tree-sitter/tree-sitter-typescript.git|tsx|yes"
    "yaml|https://github.com/tree-sitter-grammars/tree-sitter-yaml.git||"
)

# カウンター
TOTAL=${#LANGUAGES[@]}
SUCCESS=0
FAILED=0
declare -a FAILED_LANGUAGES

echo "📦 ステップ 1/3: リポジトリのクローン"
echo "=========================================="

# ユニークなリポジトリURLのリストを作成
declare -A UNIQUE_REPOS
for lang_def in "${LANGUAGES[@]}"; do
    IFS='|' read -r name repo subdir npm <<< "$lang_def"
    UNIQUE_REPOS["$repo"]=1
done

# クローン処理
for repo in "${!UNIQUE_REPOS[@]}"; do
    echo "📥 クローン中: $repo"
    if ghq get -u "$repo"; then
        echo "✓ クローン完了: $repo"
    else
        echo "⚠ クローンをスキップ（既に存在する可能性）: $repo"
    fi
    echo ""
done

echo ""
echo "📦 ステップ 2/3: npm依存関係のインストール"
echo "=========================================="

# npm installが必要なリポジトリを処理
declare -A NPM_INSTALLED
for lang_def in "${LANGUAGES[@]}"; do
    IFS='|' read -r name repo subdir npm <<< "$lang_def"

    if [ "$npm" = "yes" ]; then
        # リポジトリのローカルパスを構築
        repo_path="$GHQ_ROOT/$(echo "$repo" | sed 's|https://||' | sed 's|\.git$||')"

        # 既にnpm installを実行済みかチェック
        if [ -z "${NPM_INSTALLED[$repo_path]}" ]; then
            echo "📦 npm install: $repo_path"
            if [ -d "$repo_path" ]; then
                (cd "$repo_path" && npm install > /dev/null 2>&1) && echo "✓ npm install完了" || echo "⚠ npm installに失敗"
                NPM_INSTALLED[$repo_path]=1
            else
                echo "⚠ ディレクトリが見つかりません: $repo_path"
            fi
            echo ""
        fi
    fi
done

echo ""
echo "🔨 ステップ 3/3: パーサーのビルドとインストール"
echo "=========================================="
echo ""

# 各言語をビルド
for lang_def in "${LANGUAGES[@]}"; do
    IFS='|' read -r name repo subdir npm <<< "$lang_def"

    # リポジトリのローカルパスを構築
    repo_path="$GHQ_ROOT/$(echo "$repo" | sed 's|https://||' | sed 's|\.git$||')"

    echo "----------------------------------------"
    echo "🔨 ビルド中: $name"
    echo "----------------------------------------"

    # ビルドコマンドを構築
    if [ -n "$subdir" ]; then
        build_cmd="$BUILD_SCRIPT $repo_path $name $ABI_VERSION $subdir"
    else
        build_cmd="$BUILD_SCRIPT $repo_path $name $ABI_VERSION"
    fi

    # ビルド実行
    if $build_cmd 2>&1 | grep -E "(✓|⚠|エラー|完了)"; then
        SUCCESS=$((SUCCESS + 1))
        echo "✓ $name のビルドが完了しました"
    else
        FAILED=$((FAILED + 1))
        FAILED_LANGUAGES+=("$name")
        echo "✗ $name のビルドに失敗しました"
    fi
    echo ""
done

# 最終結果
echo "=========================================="
echo "📊 インストール結果"
echo "=========================================="
echo "合計: $TOTAL 言語"
echo "成功: $SUCCESS 言語"
echo "失敗: $FAILED 言語"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo "失敗した言語:"
    for lang in "${FAILED_LANGUAGES[@]}"; do
        echo "  - $lang"
    done
fi

echo ""
echo "=========================================="
if [ $FAILED -eq 0 ]; then
    echo "✓ すべての言語のインストールが完了しました！"
else
    echo "⚠ 一部の言語のインストールに失敗しました"
fi
echo "=========================================="
echo ""
echo "インストールされたライブラリの場所: ~/.emacs.d/tree-sitter/"
echo ""
echo "Emacsで各言語の*-ts-modeを試してください。例:"
echo "  M-x python-ts-mode"
echo "  M-x rust-ts-mode"
echo "  M-x typescript-ts-mode"
echo ""

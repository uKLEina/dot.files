#!/usr/bin/env bash
set -euo pipefail
# build-treesit-grammar.sh
# Usage: ./build-treesit-grammar.sh [-s SRC_DIR] [-l LANG] [-e EMACS_BIN] [-o OUT_DIR] [-k]
# -s SRC_DIR: tree-sitter language repo root (default: current dir)
# -l LANG: language symbol/name used in output filename (default: 'language')
# -e EMACS_BIN: path to emacs binary to detect tree-sitter runtime (default: which emacs)
# -o OUT_DIR: install dir for resulting .so (default: ~/.local/lib/tree-sitter)
# -k: keep intermediate object files (do not delete parser.o/scanner.o)
#
# This script:
#  - runs `tree-sitter generate` in SRC_DIR
#  - compiles src/parser.c and src/scanner.c (if exists) with pkg-config flags
#  - links shared lib and forces a DT_NEEDED on Emacs's libtree-sitter
#  - installs the .so into OUT_DIR
# 使い方（例）
# 1. スクリプトを保存（例: build-treesit-grammar.sh）
# 2. 実行権を付ける: chmod +x build-treesit-grammar.sh
# 3. tree-sitter-python リポジトリのルートで実行:
#    ./build-treesit-grammar.sh -l python
#    あるいは Emacs 実行ファイルを指定する場合:
#    ./build-treesit-grammar.sh -l python -e /usr/local/bin/emacs

LANG="language"
SRC_DIR="."
EMACS_BIN="$(command -v emacs || true)"
OUT_DIR="${HOME}/.local/lib/tree-sitter"
KEEP=0

show_help() {
  sed -n '1,120p' <<<"$0" >/dev/stderr
}

while getopts "s:l:e:o:kh" opt; do
  case "$opt" in
    s) SRC_DIR="$OPTARG" ;;
    l) LANG="$OPTARG" ;;
    e) EMACS_BIN="$OPTARG" ;;
    o) OUT_DIR="$OPTARG" ;;
    k) KEEP=1 ;;
    h) show_help; exit 0 ;;
    *) show_help; exit 1 ;;
  esac
done

cd "$SRC_DIR"

# Checks
command -v gcc >/dev/null 2>&1 || { echo "gcc not found"; exit 1; }
command -v pkg-config >/dev/null 2>&1 || { echo "pkg-config not found"; exit 1; }

if ! command -v tree-sitter >/dev/null 2>&1; then
  echo "Warning: tree-sitter CLI not found in PATH. If repository needs generation, run 'tree-sitter generate' manually or install tree-sitter CLI."
fi

if [ ! -f src/parser.c ]; then
  echo "Error: src/parser.c not found in $PWD/src"
  exit 1
fi

# run generate (idempotent)
if [ -f package.json ] && command -v npx >/dev/null 2>&1; then
  echo "Running: npx tree-sitter generate"
  npx tree-sitter generate
elif command -v tree-sitter >/dev/null 2>&1; then
  echo "Running: tree-sitter generate"
  tree-sitter generate
else
  echo "Skipping 'tree-sitter generate' (CLI not found)."
fi

# Setup compile flags
PKG_CFLAGS="$(pkg-config --cflags tree-sitter 2>/dev/null || true)"
PKG_LIBS="$(pkg-config --libs tree-sitter 2>/dev/null || true)"

echo "pkg-config cflags: $PKG_CFLAGS"
echo "pkg-config libs: $PKG_LIBS"

# compile parser and scanner
CFLAGS="$PKG_CFLAGS -I${SRC_DIR}/src"
echo "Compiling parser.c..."
gcc -O2 -fPIC $CFLAGS -c src/parser.c -o parser.o

SCANNER_OBJ=""
if [ -f src/scanner.c ]; then
  echo "Compiling scanner.c..."
  gcc -O2 -fPIC $CFLAGS -c src/scanner.c -o scanner.o
  SCANNER_OBJ="scanner.o"
fi

# Find Emacs's libtree-sitter runtime path
EMACS_TS_LIB=""
if [ -n "$EMACS_BIN" ] && [ -x "$EMACS_BIN" ]; then
  EMACS_TS_LIB="$(ldd "$EMACS_BIN" 2>/dev/null | awk '/libtree-sitter/ {print $3; exit}')"
fi

# fallback: try ldconfig
if [ -z "$EMACS_TS_LIB" ]; then
  EMACS_TS_LIB="$(ldconfig -p | awk '/libtree-sitter/ {print $4; exit}')"
fi

# fallback: try pkg-config libs to get -L path
if [ -z "$EMACS_TS_LIB" ] && [ -n "$PKG_LIBS" ]; then
  # try to extract -L path from pkg-config --libs
  EMACS_TS_DIR="$(echo "$PKG_LIBS" | sed -n 's/.*-L\([^ ]*\).*/\1/p')"
  if [ -n "$EMACS_TS_DIR" ]; then
    EMACS_TS_LIB="$(ls "$EMACS_TS_DIR"/libtree-sitter*.so* 2>/dev/null | head -n1 || true)"
  fi
fi

if [ -z "$EMACS_TS_LIB" ]; then
  echo "Warning: Could not determine Emacs's libtree-sitter runtime path automatically."
  echo "You can pass Emacs path with -e /path/to/emacs or ensure libtree-sitter is installed on system."
  # continue, but link may not include libtree-sitter DT_NEEDED
fi

EMACS_TS_DIR=""
if [ -n "$EMACS_TS_LIB" ]; then
  EMACS_TS_DIR="$(dirname "$EMACS_TS_LIB")"
  echo "Detected Emacs tree-sitter lib: $EMACS_TS_LIB (dir: $EMACS_TS_DIR)"
fi

SO_NAME="libtree-sitter-${LANG}.so"
echo "Linking shared library: $SO_NAME"

# Link: if EMACS_TS_DIR known, force DT_NEEDED on that lib; otherwise link normally using pkg-config libs.
if [ -n "$EMACS_TS_DIR" ]; then
  # ensure using no-as-needed around -ltree-sitter so DT_NEEDED is recorded
  gcc -shared -o "$SO_NAME" parser.o $SCANNER_OBJ \
    -Wl,--no-as-needed -L"$EMACS_TS_DIR" -ltree-sitter -Wl,--as-needed \
    -Wl,-rpath,"$EMACS_TS_DIR"
else
  gcc -shared -o "$SO_NAME" parser.o $SCANNER_OBJ $PKG_LIBS
fi

echo "Resulting dependencies (ldd):"
ldd "$SO_NAME" || true

echo "Check for ABI marker (yNNV):"
strings "$SO_NAME" | grep -E 'y[0-9]+V' -m1 || true

# install
mkdir -p "$OUT_DIR"
mv -f "$SO_NAME" "$OUT_DIR/"

if [ "$KEEP" -eq 0 ]; then
  rm -f parser.o scanner.o || true
fi

echo "Installed $SO_NAME -> $OUT_DIR/"
echo "Add the following to your Emacs config (or evaluate in Emacs):"
echo "  (add-to-list 'treesit-extra-load-path \"${OUT_DIR}\")"
echo "Then restart Emacs (from a terminal to inherit PATH) or run (treesit-reload-language 'LANGUAGE) in Emacs."

#!/usr/bin/env bash
# fix-wslg-taskbar-icon.sh
#
# WSLg (WSL2 + Windows) 上でソースからビルドした Emacs のタスクバー/
# スタートメニューのアイコンが、Windows のデフォルトアイコン(ペンギン)
# になってしまう問題を回避するためのスクリプト。
#
# 原因:
#   `make install` は /usr/local (stow経由) にしか .desktop / アイコンを
#   配置しないが、WSLg (WSLDVCPlugin / weston) の静的アイコン解決は
#   /usr/share 配下しか見ていないため、/usr/local だけではアイコンが
#   解決されない。
#
# 使い方:
#   ./fix-wslg-taskbar-icon.sh
#   (make install を実行してEmacsを再インストールした後、毎回これも実行する)
#
# 重要: このスクリプトを実行した後、`wsl --shutdown` では不十分。
#       Windows側のタスクバー/エクスプローラーが古いアイコンを
#       キャッシュしたままになるため、最終的に Windows 自体の再起動
#       (サインアウト/再起動) が必要になる。

set -euo pipefail

EMACS_PREFIX="/usr/local/stow/emacs"
ICON_SIZES=(16x16 32x32 48x48 128x128)

echo "==> /usr/share/icons/hicolor 配下にディレクトリを作成"
for size in "${ICON_SIZES[@]}"; do
  sudo mkdir -p "/usr/share/icons/hicolor/${size}/apps"
done

echo "==> Emacsのアイコンを /usr/share 配下にコピー"
for size in "${ICON_SIZES[@]}"; do
  src="${EMACS_PREFIX}/share/icons/hicolor/${size}/apps/emacs.png"
  dst="/usr/share/icons/hicolor/${size}/apps/emacs.png"
  if [ -f "$src" ]; then
    sudo cp "$src" "$dst"
    echo "    $src -> $dst"
  else
    echo "    警告: $src が見つかりません(スキップ)"
  fi
done

echo "==> emacs.desktop を /usr/share/applications にコピー"
sudo cp "${EMACS_PREFIX}/share/applications/emacs.desktop" /usr/share/applications/emacs.desktop

echo "==> アイコンキャッシュを更新"
sudo gtk-update-icon-cache -f /usr/share/icons/hicolor

echo
echo "==> 完了。ただしこれだけではタスクバーのアイコンはまだ反映されない可能性がある。"
echo "    WSL側は 'wsl --shutdown' では不十分で、Windows自体の再起動が必要。"
echo "    (Windowsのタスクバー/エクスプローラーが古いアイコンをキャッシュしたまま更新しないため)"

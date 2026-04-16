#!/usr/bin/bash

# index.theme をコピー
sudo cp /usr/share/icons/hicolor/index.theme /usr/local/stow/emacs/share/icons/hicolor/ 2>/dev/null

# アイコンキャッシュを更新
sudo gtk-update-icon-cache /usr/local/share/icons/hicolor/

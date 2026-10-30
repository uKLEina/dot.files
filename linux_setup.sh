#!/bin/bash

# remove old files
rm -fr ~/.config/pip
rm -f ~/.config/pep8
rm -f ~/.config/flake8
rm -f ~/.config/pylintrc
rm -fr ~/.emacs.d
rm -fr ~/.skk.d
rm -fr ~/.themes/Numix/gnome-shell
rm -fr ~/.zsh
rm -fr ~/bin
rm -f ~/.aspell.conf
rm -f ~/.gitignore
rm -f ~/.config/git/ignore
rm -f ~/.gitmodules
rm -f ~/.latexmkrc
rm -f ~/.tmux.conf
rm -f ~/.zshrc
rm -f ~/.zshenv
rm -f ~/hhkb.nodoka
rm -f ~/nihongo.nodoka
rm -f ~/.config/systemd/user/keyring-unlock.service
rm -f ~/.config/systemd/user/secret-sync.service
rm -f ~/.config/secret-sync/secrets.tsv
rm -f ~/.local/share/dbus-1/services/org.freedesktop.secrets.service
rm -f ~/.local/share/dbus-1/services/org.gnome.keyring.service

# .config/
# mkdir -p ~/.config
# ln -s ~/dot.files/.config/pip ~/.config/pip
# ln -s ~/dot.files/.config/flake8 ~/.config/flake8
# ln -s ~/dot.files/.config/pep8 ~/.config/pep8
# ln -s ~/dot.files/.config/pylintrc ~/.config/pylintrc
mkdir -p ~/.config/git
ln -s ~/dot.files/.config/git/ignore ~/.config/git/ignore
# ln -s ~/dot.files/nihongo.kanata.lisp ~/.config/nihongo.kanata.lisp

# Emacs
ln -s ~/dot.files/.emacs.d ~/.emacs.d
ln -s ~/dot.files/.skk.d ~/.skk.d

# themes
# mkdir ~/.themes
# ln -s ~/dot.files/.themes/Numix ~/.themes/Numix

# shell
ln -s ~/dot.files/.zsh ~/.zsh
ln -s ~/dot.files/.zshrc ~/.zshrc
ln -s ~/dot.files/.zshenv ~/.zshenv
# touch .bash_profile
# echo "export PATH=\"$HOME/.cargo/bin:\$PATH\"" >> ~/.bash_profile
# echo "export PATH=\"$HOME/.cargo/bin:\$PATH\"" >> ~/.profile
ln -s ~/dot.files/.tmux.conf ~/.tmux.conf

# scripts
ln -s ~/dot.files/bin ~/bin

# keyring（1Password の秘密情報の写しを GNOME Keyring に置く。要 gnome-keyring と libsecret-tools）
mkdir -p ~/.config/systemd/user ~/.config/secret-sync ~/.local/share/dbus-1/services
ln -s ~/dot.files/.config/systemd/user/keyring-unlock.service ~/.config/systemd/user/keyring-unlock.service
ln -s ~/dot.files/.config/systemd/user/secret-sync.service ~/.config/systemd/user/secret-sync.service
ln -s ~/dot.files/.config/secret-sync/secrets.tsv ~/.config/secret-sync/secrets.tsv
ln -s ~/dot.files/.local/share/dbus-1/services/org.freedesktop.secrets.service ~/.local/share/dbus-1/services/org.freedesktop.secrets.service
ln -s ~/dot.files/.local/share/dbus-1/services/org.gnome.keyring.service ~/.local/share/dbus-1/services/org.gnome.keyring.service
# 標準のデーモンは keyring を開けないので止め、keyring-unlock.service に任せる
systemctl --user mask gnome-keyring-daemon.socket gnome-keyring-daemon.service
systemctl --user daemon-reload
systemctl --user enable keyring-unlock.service secret-sync.service

# git
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.st status

# others
# ln -s ~/dot.files/.aspell.conf ~/.aspell.conf
# ln -s ~/dot.files/.latexmkrc ~/.latexmkrc

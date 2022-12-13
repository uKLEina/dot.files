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
rm -f ~/.gitmodules
rm -f ~/.latexmkrc
rm -f ~/.tmux.conf
rm -f ~/.zshrc
rm -f ~/.zshenv
rm -f ~/hhkb.nodoka
rm -f ~/nihongo.nodoka

# .config/
mkdir -p ~/.config
ln -s ~/dot.files/.config/pip ~/.config/pip
ln -s ~/dot.files/.config/flake8 ~/.config/flake8
ln -s ~/dot.files/.config/pep8 ~/.config/pep8
ln -s ~/dot.files/.config/pylintrc ~/.config/pylintrc
ln -s ~/dot.files/.gitignore_global ~/.gitignore_global

# Emacs
ln -s ~/dot.files/.emacs.d ~/.emacs.d
ln -s ~/dot.files/.skk.d ~/.skk.d

# themes
mkdir ~/.themes
ln -s ~/dot.files/.themes/Numix ~/.themes/Numix

# shell
ln -s ~/dot.files/.zsh ~/.zsh
ln -s ~/dot.files/.zshrc ~/.zshrc
ln -s ~/dot.files/.zshenv ~/.zshenv
touch .bash_profile
echo "export PATH=\"$HOME/.cargo/bin:\$PATH\"" >> ~/.bash_profile
echo "export PATH=\"$HOME/.cargo/bin:\$PATH\"" >> ~/.profile
ln -s ~/dot.files/.tmux.conf ~/.tmux.conf

# scripts
ln -s ~/dot.files/bin ~/bin

# git
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.st status

# others
ln -s ~/dot.files/.aspell.conf ~/.aspell.conf
ln -s ~/dot.files/.latexmkrc ~/.latexmkrc

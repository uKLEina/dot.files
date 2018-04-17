#!/bin/bash

# .config/
ln -s ~/dot.files/.config/pip ~/.config/pip
ln -s ~/dot.files/.config/flake8 ~/.config/flake8
ln -s ~/dot.files/.config/pep8 ~/.config/pep8
ln -s ~/dot.files/.config/pylintrc ~/.config/pylintrc

# Emacs
ln -s ~/dot.files/.emacs.d ~/.emacs.d
ln -s ~/dot.files/.skk.d ~/.skk.d

# themes
mkdir ~/.themes
ln -s ~/dot.files/.themes/Numix ~/.themes/Numix

# shell
ln -s ~/dot.files/.zsh ~/.zsh
ln -s ~/dot.files/.zshrc ~/.zshrc
touch .bash_profile
echo "export PATH=\"$HOME/.cargo/bin:$PATH\"" >> ~/.bash_profile
echo "export PATH=\"$HOME/.cargo/bin:$PATH\"" >> ~/.profile
ln -s ~/dot.files/.tmux.conf ~/.tmux.conf

# scripts
ln -s ~/dot.files/bin ~/bin

# others
ln -s ~/dot.files/.aspell.conf ~/.aspell.conf
ln -s ~/dot.files/.latexmkrc ~/.latexmkrc

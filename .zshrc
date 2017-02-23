# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt autocd extendedglob
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/nkob/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# The following lines added by ukleina(user)
## --prefix=~/localというように「=」の後でも
## 「~」や「=コマンド」などのファイル名展開を行う。
setopt magic_equal_subst

## 全てのユーザのログイン・ログアウトを監視する。
watch="all"
## ログイン時にはすぐに表示する。
log

# history search
bindkey '^P' history-beginning-search-backward
bindkey '^N' history-beginning-search-forward

## 完全に削除。
alias rr="command rm -rf"
## ファイル操作を確認する。
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

# environments
export EDITOR='emacsclient'
export VISUAL='emacsclient'
export PATH="/home/nkob/.cargo/bin:/home/nkob/opt/anaconda3/bin":$PATH
export PYTHONPATH="/home/nkob/Documents/Python_Scripts/mypackage"

# aliases
setopt complete_aliases

alias sudo='sudo '

alias ls='ls -hFC --color=auto --show-control-chars'
alias la='ls -hAFC --color=auto --show-control-chars'
alias ll='ls -hFC --color=auto --show-control-chars -l'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias cp="cp -i"

alias du='du -h'
alias df='df -h'

alias isomnt='mount -t iso9660 -o loop'

alias apti='apt install'
alias apts='aptitude search'
alias aptr='apt remove'
alias aptp='apt purge'
alias aptsh='aptitude show'
alias aptu='apt update'
alias aptug='apt upgrade'
alias aptfu='apt full-upgrade'

alias e='emacsclient'
alias ps='ps auxfww'
setopt hist_ignore_dups
setopt share_history

# change directory
setopt auto_cd
setopt auto_pushd

setopt correct
setopt list_packed
setopt nolistbeep
setopt noautoremoveslash

export LANG=ja_JP.UTF-8
case ${UID} in
0)
    LANG=C
    ;;
esac

# prompt
PROMPT="%B%F{blue}%m:%f%F{green}%n%f %% %b"
RPROMPT="%F{yellow}[%~]%f"
SPROMPT="correct: %R -> %r ? [n,y,a,e]: "

# vcs
autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () { vcs_info }
RPROMPT=$RPROMPT'${vcs_info_msg_0_}'

# zsh autosuggestion
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

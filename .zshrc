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

# environments
export EDITOR='emacsclient'
export VISUAL='emacsclient'

# aliases
setopt completealiases

alias sudo='sudo '

## 完全に削除。
alias rr="command rm -rf"
## ファイル操作を確認する。
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

alias ls='ls -hF --color=always --show-control-chars --group-directories-first'
alias la='ls -hFA --color=always --show-control-chars --group-directories-first'
alias ll='ls -hFl --color=always --show-control-chars --group-directories-first'
alias lal='ls -hFAl --color=always --show-control-chars --group-directories-first'
alias lla='ls -hFlA --color=always --show-control-chars --group-directories-first'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias cp="cp -i"

alias du='du -h'
alias df='df -h'
alias free='free -h'

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
alias ke='emacsclient -e "(kill-emacs)"'
alias ps='ps auxfww'
alias top='top -d 1'
eval $(thefuck --alias)
alias ha='fuck'
export THEFUCK_REQUIRE_CONFIRMATION=true
export THEFUCK_ALTER_HISTORY=true

alias dcip="docker inspect --format '{{ .NetworkSettings.IPAddress }}' $1"
alias dcls="docker container ls"

alias tx='tar -xf'

# whichで見付けたexecutableのあるディレクトリにcdする
function cdwhich () {
    cd $(which $1 | xargs -0 dirname)
}

# install python dev tools
alias pydev="pip install jedi rope autopep8 yapf black flake8 flake8-docstrings flake8-import-order pep8-naming pylint python-lsp-server pyls-flake8"

alias cdaws='cd "$(ghq root)"/git-codecommit.ap-northeast-1.amazonaws.com/v1/repos/'

function showfont (){ fc-match "$@" -f "%{file}" | xargs display }

setopt hist_ignore_dups
setopt share_history

# change directory
setopt auto_cd
setopt auto_pushd

setopt list_packed
setopt nolistbeep
setopt noautoremoveslash

export LANG=ja_JP.UTF-8

# prompt
PROMPT="%B%F{blue}%m:%f%F{green}%n%f %F{white}%%%f %b"
SPROMPT="correct: %R -> %r ? [n,y,a,e]: "
# RPROMPT
# shorten if too long
shorten_path() {
    local current_path="$1"
    local git_root=""
    local shortened_path=""

    # git コマンドが存在するか確認
    if command -v git >/dev/null 2>&1; then
        # カレントディレクトリがGitリポジトリ内かを確認
        if git_root=$(git rev-parse --show-toplevel 2>/dev/null); then
            # Gitのルートディレクトリ名を取得
            local root_dir="$(basename "$git_root")"

            # 現在のパスがGitのルートディレクトリの場合はそのまま表示
            if [ "$current_path" = "$git_root" ]; then
                shortened_path="$root_dir"
            else
                # ルートディレクトリ以下の相対パスを取得し、ルートディレクトリ名を保持
                local relative_path="${current_path#$git_root}"
                shortened_path="$root_dir${relative_path}"
            fi
        else
            # Git管理外の場合、通常のパス短縮を適用
            shortened_path=$(echo "$current_path" | sed "s|^/home/$(whoami)|~|g")
        fi
    else
        # git コマンドが存在しない場合、通常のパス短縮を適用
        shortened_path=$(echo "$current_path" | sed "s|^/home/$(whoami)|~|g")
    fi

    # パスの長さを計算
    local len=$(echo -n "$shortened_path" | wc -m)

    # パスの長さがターミナル幅の40%以上なら短縮
    if [ "$len" -ge $(($COLUMNS * 40 / 100)) ]; then
        # Gitのルートディレクトリ部分は省略せず、相対パス部分のみ短縮
        if [ -n "$git_root" ] && [ "$current_path" != "$git_root" ]; then
            local relative_shortened_path="${shortened_path#$root_dir}"
            echo "$root_dir$(echo "$relative_shortened_path" | sed -E "s|(\w)[^/]+/|\1/|g")"
        else
            echo "$shortened_path" | sed -E "s|(\w)[^/]+/|\1/|g"
        fi
    else
        echo "$shortened_path"
    fi
}

# RPROMPT の設定を更新する関数
update_rprompt() {
    # vcs_info を先に更新
    vcs_info
    # shorten_path の結果と vcs_info の結果を組み合わせて RPROMPT を構築
    RPROMPT="%F{yellow}[$(shorten_path "$PWD")]%f${vcs_info_msg_0_}"
}

# コマンド実行後に update_rprompt を呼び出すフックを設定
autoload -Uz add-zsh-hook
add-zsh-hook precmd update_rprompt

# vcs_info 設定
autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'

# シェル起動時に一度 update_rprompt を呼び出しておく
update_rprompt

# zsh autosuggestion
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/zsh-completions/zsh-completions.plugin.zsh

# 環境依存の内容は別ファイルに置いておく
[ -f ~/.zshrc.local ] && source ~/.zshrc.local

# fzf
[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ] && source /usr/share/doc/fzf/examples/key-bindings.zsh
[ -f /usr/share/doc/fzf/examples/completion.zsh ] && source /usr/share/doc/fzf/examples/completion.zsh

if [ $SHLVL = 1 ];then
  tmux
fi

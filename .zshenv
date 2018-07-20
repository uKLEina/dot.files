# colored less
export LESS=-R
export LESS_TERMCAP_mb=$'\E[1;32m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;33m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;36m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

# local settings
[ -f ~/.zshenv.local ] && source ~/.zshenv.local

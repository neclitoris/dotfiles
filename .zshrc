# environment
source "$HOME/.profile"
source "$HOME/.env"
source "$HOME/.path"
source "$HOME/.aliases"

# zsh init
ZSH="$UTIL/oh-my-zsh"
[[ $UID = 0 ]] && ZSH_DISABLE_COMPFIX=true

ZSH_THEME="agnoster"
DEFAULT_USER="neclitoris"
VI_MODE_SET_CURSOR=true
MODE_INDICATOR="%F{yellow}+%f"

plugins=(autoenv
    virtualenv
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-dircolors-solarized
    vi-mode)

source $ZSH/oh-my-zsh.sh
unalias gm

# other things init
# source "$UTIL/torch/install/bin/torch-activate"

# completions
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
source "$HOME/.completions/tldr"
source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh

setopt nosharehistory

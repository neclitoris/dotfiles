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

plugins=(autoenv
    virtualenv
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-dircolors-solarized)

source $ZSH/oh-my-zsh.sh
unalias gm

# other things init
# source "$UTIL/torch/install/bin/torch-activate"

# completions
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
source "$HOME/.completions/tldr"

setopt nosharehistory

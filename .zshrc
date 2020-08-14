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

eval `dircolors ~/.dir_colors/dircolors`

plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh
unalias gm

# other things init
source "$UTIL/torch/install/bin/torch-activate"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/neclitoris/util/anaconda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/neclitoris/util/anaconda/etc/profile.d/conda.sh" ]; then
        . "/home/neclitoris/util/anaconda/etc/profile.d/conda.sh"
    else
        export PATH="/home/neclitoris/util/anaconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# completions
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
source "$HOME/.completions/tldr"

setopt nosharehistory


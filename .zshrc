# environment
source "$HOME/.profile"
source "$HOME/.environment"
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

compdef _git config

# other things init
# source "$UTIL/torch/install/bin/torch-activate"

# completions
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
source "$HOME/.completions/tldr"
source "$HOME/.cargo/env"
source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh

setopt nosharehistory
unsetopt complete_aliases

if [ -e /home/neclitoris/.nix-profile/etc/profile.d/nix.sh ]; then . /home/neclitoris/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# The next line updates PATH for Yandex Cloud CLI.
if [ -f '/home/neclitoris/yandex-cloud/path.bash.inc' ]; then source '/home/neclitoris/yandex-cloud/path.bash.inc'; fi

# The next line enables shell command completion for yc.
if [ -f '/home/neclitoris/yandex-cloud/completion.zsh.inc' ]; then source '/home/neclitoris/yandex-cloud/completion.zsh.inc'; fi



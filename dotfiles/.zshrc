# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=10000
unsetopt autocd beep notify
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/belimawr/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt interactivecomments

source ~/.zsh/git.zsh
source ~/.zsh/nice-exit-code/nice-exit-code.plugin.zsh
source ~/.zsh/zsh-git-prompt/zshrc.sh
source ~/.zsh/kubectl.zsh
source ~/.zsh/base16-grayscale-dark.sh

alias ls='ls --color=auto --group-directories-first'
alias la='ls -lAh'
alias ll='ls -lh'
alias grep='grep --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias history='fc -l 1'
alias emacs='emacs -nw'
alias lsusb='lsusb.py -iu'

alias gr='go run .'
alias gt='go test ./...'

alias brkeyboard='setxkbmap -option ctrl:nocaps -layout us -variant altgr-intl'
alias normalkeyboard='setxkbmap -option ctrl:nocaps -layout us'

alias cpuFreqMonitor='watch -n.1 "cat /proc/cpuinfo | grep \"^[c]pu MHz\""'

export EDITOR=emacs
export PATH=$PATH:/usr/local/go/bin
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/linux-laptop/bin/:$PATH
export PATH=$HOME/go/bin:$PATH

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Set primpt

PROMPT='$(nice_exit_code) %B%n@%m%~%b$(git_super_status) %# '
PS1=$PROMPT
setopt promptsubst
unset RPROMPT

## ELASTIC

## TODO: organise this
# export GOPATH=$HOME"/go"
ulimit -n 200000
alias docker_rm_all='docker ps --all -q | xargs -p docker rm -fv'
alias beats-clean-repo='git clean -f -X ~/devel/beats && sudo git clean -f ~/devel/beats'
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"


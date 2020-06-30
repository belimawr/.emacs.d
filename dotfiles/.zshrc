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

#alias docker-clean-images='docker images -f "dangling=true" -q | xargs docker rmi'
#alias history='fc -l 1'
alias ls='ls --color=auto --group-directories-first'
alias grep='grep --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias history='fc -l 1'
alias la='ls -lAh'
alias ll='ls -lh'
alias emacs='emacs -nw'

source ~/.zsh/git.zsh
source ~/.zsh/kubectl.zsh
source ~/.zsh/base16-grayscale-dark.sh

export PS1='%n@%m %~ %# '
export RPROMPT='$(git_prompt_info) %?'
setopt promptsubst
export PS1=$'${(r:$COLUMNS::-:)}'$PS1
export EDITOR=emacs
export PATH=$HOME/go/bin:$PATH

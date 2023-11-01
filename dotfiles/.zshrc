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

# source ~/.zsh/git.zsh
source ~/.zsh/nice-exit-code/nice-exit-code.plugin.zsh
# source ~/.zsh/zsh-git-prompt/zshrc.sh
source ~/.zsh/kubectl.zsh
source ~/.zsh/base16-grayscale-dark.sh

alias ls='ls --color=auto --group-directories-first'
alias la='ls -lAh'
alias ll='ls -lh'
alias grep='grep --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias history='fc -l 1'
alias emacs='emacsclient -r -n'
alias lsusb='lsusb.py -iu'
alias mpv='mpv -hwdec=auto --hwdec-codecs=all'

alias gr='go run .'
alias gt='go test ./...'

alias brkeyboard='setxkbmap -option ctrl:nocaps -layout us -variant altgr-intl'
alias normalkeyboard='setxkbmap -option ctrl:nocaps -layout us'

alias cpuFreqMonitor='watch -n.1 "cat /proc/cpuinfo | grep \"^[c]pu MHz\""'
alias governor='cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor'
alias performance='echo "performance"|sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor'
alias powersave='echo "powersave"|sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor'

alias grep='grep -v grep| grep'
alias aunpack='aunpack -q'
alias beats_grep='grep -v eslegclient|grep -v docker|grep -v kubernetes|grep -v publisher|grep -v acker |grep -v elasticsearch |grep -v registrar'
alias merge_logs='cat *.ndjson |jq -c > clean.ndjson'

## Build docs alias
export GIT_HOME=$HOME/devel
# Fleet and Elastic Agent guide
alias build-docs-ingest-management='$GIT_HOME/docs/build_docs --doc $GIT_HOME/ingest-docs/docs/en/ingest-management/index.asciidoc --resource=$GIT_HOME/apm-server/docs --resource=$GIT_HOME/observability-docs/docs --chunk 2 --open'
alias build-docs-libbeat='$GIT_HOME/docs/build_docs --respect_edit_url_overrides --doc $GIT_HOME/beats/libbeat/docs/index.asciidoc --chunk 1 --open'
alias build-docs-beats-dev-guide='$GIT_HOME/docs/build_docs --respect_edit_url_overrides --doc $GIT_HOME/beats/docs/devguide/index.asciidoc --chunk 1 --open'
alias build-docs-filebeat='$GIT_HOME/docs/build_docs --respect_edit_url_overrides --doc $GIT_HOME/beats/filebeat/docs/index.asciidoc --resource=$GIT_HOME/beats/x-pack/filebeat/docs --chunk 1 --open'
alias build-docs-metricbeat='$GIT_HOME/docs/build_docs --respect_edit_url_overrides --doc $GIT_HOME/beats/metricbeat/docs/index.asciidoc --chunk 1 --open'

export EDITOR=emacs
export PATH=$PATH:/usr/local/go/bin
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/linux-laptop/bin/:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/bin:$PATH

# export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# Set primpt

# PROMPT='$(nice_exit_code) %B%n@%m%~%b$(git_super_status) %# '
PROMPT='$(nice_exit_code) %B%n@%m%~%b %# '
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

## New Starship prompt
eval "$(starship init zsh)"

export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent
gpg-connect-agent updatestartuptty /bye > /dev/null

# >>>> Vagrant command completion (start)
fpath=(/opt/vagrant/embedded/gems/2.3.4/gems/vagrant-2.3.4/contrib/zsh $fpath)
compinit
# <<<<  Vagrant command completion (end)

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/tiago/bin/google-cloud-sdk/path.zsh.inc' ]; then . '/home/tiago/bin/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/tiago/bin/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/tiago/bin/google-cloud-sdk/completion.zsh.inc'; fi


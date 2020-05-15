# Based on:https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/kubectl/kubectl.plugin.zsh

# if (( $+commands[kubectl] )); then
#     __KUBECTL_COMPLETION_FILE="${ZSH_CACHE_DIR}/kubectl_completion"

#     if [[ ! -f $__KUBECTL_COMPLETION_FILE ]]; then
#         kubectl completion zsh >! $__KUBECTL_COMPLETION_FILE
#     fi

#     [[ -f $__KUBECTL_COMPLETION_FILE ]] && source $__KUBECTL_COMPLETION_FILE

#     unset __KUBECTL_COMPLETION_FILE
# fi

# This command is used a LOT both below and in daily life
alias k=kubectl

# Execute a kubectl command against all namespaces
alias kca='_kca(){ kubectl "$@" --all-namespaces;  unset -f _kca; }; _kca'

# Apply a YML file
alias kaf='kubectl apply -f'

# Drop into an interactive terminal on a container
alias keti='kubectl exec -ti'

# Manage configuration quickly to switch contexts between local, dev ad staging.
alias kcuc='kubectl config use-context'
alias kccc='kubectl config current-context'

# List all contexts
alias kcgc='kubectl config get-contexts'

# General aliases
alias kdel='kubectl delete'
alias kdelf='kubectl delete -f'

# Pod management.
alias kgp='kubectl get pods'
alias kgpa='kubectl get pods --all-namespaces'
alias kgpw='kgp --watch'
alias kgpwide='kgp -o wide'
alias kdp='kubectl describe pods'
alias kdelp='kubectl delete pods'

# Service management.
alias kgs='kubectl get svc'
alias kgsa='kubectl get svc --all-namespaces'
alias kgsw='kgs --watch'
alias kgswide='kgs -o wide'
alias kes='kubectl edit svc'
alias kds='kubectl describe svc'
alias kdels='kubectl delete svc'


# Namespace management
alias kgns='kubectl get namespaces'
alias kens='kubectl edit namespace'
alias kdns='kubectl describe namespace'
alias kdelns='kubectl delete namespace'
alias kcn='kubectl config set-context $(kubectl config current-context) --namespace' # Set 'default' namespace

# Deployment management.
alias kgd='kubectl get deployment'
alias kgda='kubectl get deployment --all-namespaces'
alias kgdw='kgd --watch'
alias kgdwide='kgd -o wide'
alias ked='kubectl edit deployment'
alias kdd='kubectl describe deployment'
alias ksd='kubectl scale deployment'
alias krsd='kubectl rollout status deployment'

# Logs
alias kl='kubectl logs'
alias klf='kubectl logs -f'

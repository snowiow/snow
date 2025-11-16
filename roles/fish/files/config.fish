# Load vi mode
fish_vi_key_bindings 2>/dev/null

# Settings
set fish_greeting

# Aliases
alias ave='aws-vault exec'
alias ase='aws-sso exec -p'
alias g="git"
alias k='kubectl'
alias poweroff="systemctl poweroff"
alias r="ranger"
alias reboot="systemctl reboot"
alias v="nvim"
alias vimdiff="nvim -d"
alias weather="curl wttr.in"

# Keybindings
bind -M insert \cr fzf_cmd_history
bind -M insert \cl accept-autosuggestion
bind \cl accept-autosuggestion

# Set env variables
set -gx PATH /usr/local/go/bin $HOME/go/bin $HOME/.node_modules/bin $HOME/.local/bin $HOME/flutter/bin $HOME/flutter/bin/cache/dart-sdk/bin/ $HOME/.pub-cache/bin $PATH 
set -gx npm_config_prefix $HOME/.node_modules
set -gx FISH_KUBECTL_COMPLETION_TIMEOUT 30s
set -x GPG_TTY (tty)
set -gx EDITOR emacsclient -c -a emacs
set -gx HELIX_RUNTIME $HOME/workspace/helix/runtime

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/marcel_patzwahl/Downloads/google-cloud-sdk/path.fish.inc' ]; . '/home/marcel_patzwahl/Downloads/google-cloud-sdk/path.fish.inc'; end

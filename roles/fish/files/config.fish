# Load vi mode
fish_vi_key_bindings 2>/dev/null

# Settings
set fish_greeting

# Aliases
alias ave='aws-vault exec'
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

# Source additional stuff
source ~/workspace/fzf-marks/fzf-marks.plugin.fish

# Set env variables
set -gx PATH $HOME/go/bin $HOME/.local/bin $HOME/flutter/bin $HOME/flutter/bin/cache/dart-sdk/bin/ $HOME/.pub-cache/bin $PATH
set -gx FISH_KUBECTL_COMPLETION_TIMEOUT 30s

# Load vi mode
fish_vi_key_bindings

# Settings
set fish_greeting

# Aliases
alias g="git"
alias poweroff="systemctl poweroff"
alias r="ranger"
alias reboot="systemctl reboot"
alias v="nvim"
alias vimdiff="nvim -d"
alias weather="curl wttr.in"

# Keybingings
bind -M insert \cr fzf_cmd_history
bind -M insert \cl accept-autosuggestion
bind \cl accept-autosuggestion
# Source additional stuff
source ~/workspace/fzf-marks/fzf-marks.plugin.fish

set fish_prompt_pwd_dir_length 0

set -g __fish_git_prompt_show_informative_status 1
set -g __fish_git_prompt_hide_untrackedfiles 1

set -g __fish_git_prompt_color_branch "#f0b0ff"  # magenta from acid theme
set -g __fish_git_prompt_showupstream "informative"
set -g __fish_git_prompt_char_upstream_ahead "↑"
set -g __fish_git_prompt_char_upstream_behind "↓"
set -g __fish_git_prompt_char_upstream_prefix ""

set -g __fish_git_prompt_char_stagedstate "●"
set -g __fish_git_prompt_char_dirtystate "+"
set -g __fish_git_prompt_char_untrackedfiles "!"
set -g __fish_git_prompt_char_conflictedstate "✖"
set -g __fish_git_prompt_char_cleanstate "✔"

set -g __fish_git_prompt_color_dirtystate "#80d0ff"  # cyan from acid theme
set -g __fish_git_prompt_color_stagedstate "#ffd080"  # yellow from acid theme
set -g __fish_git_prompt_color_invalidstate "#ff7070"  # red from acid theme
set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
set -g __fish_git_prompt_color_cleanstate "#b0ff80"  # green from acid theme

function fish_prompt
    echo -n (prompt_kubernetes)
    echo -n (prompt_aws_vault)
    set_color "#f0b0ff"  # magenta - username
    echo -n (whoami)
    set_color normal
    echo -n "@"
    set_color "#ffa060"  # orange - hostname
    echo -n (hostname)
    set_color normal
    echo -n ": "
    set_color "#80d0ff"  # cyan - path
    echo -n (prompt_pwd)
    set_color "#b0ff80"  # green - git info
    echo (__fish_git_prompt)
    set_color normal
    echo -n ' $ '
end


function fish_mode_prompt
  switch $fish_bind_mode
    case default
      set_color "#ff7070"  # red - normal mode
      echo '[N] '
    case insert
      set_color "#ffd080"  # yellow - insert mode (matches helix statusline)
      echo '[I] '
    case replace_one
      set_color "#ffa060"  # orange - replace mode
      echo '[R] '
    case visual
      set_color "#f0b0ff"  # magenta - visual mode (matches helix statusline)
      echo '[V] '
    case '*'
      set_color "#ff7070"  # red
      echo '[?] '
  end
  set_color normal
end

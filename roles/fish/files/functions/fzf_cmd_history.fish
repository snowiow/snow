function fzf_cmd_history
  history | fzf --no-sort | read -l command
  if test $command
    commandline -rb $command
  end
end

bind \cr fzf_cmd_history

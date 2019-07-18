ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}⚡"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function prompt_char {
	if [ $UID -eq 0 ]; then echo "%{$fg[red]%}#%{$reset_color%}"; else echo $; fi
}

function aws_vault_env {
    if [ "$AWS_VAULT" != "" ]; then
        echo "(%{$fg[red]%}$AWS_VAULT%{$reset_color%})";
    fi
}

PROMPT='%(?, ,%{$fg[red]%}FAIL: $?%{$reset_color%}
)
%{$fg[magenta]%}%n%{$reset_color%}@%{$fg[yellow]%}%m%{$reset_color%} $(kube_ps1): %{$fg_bold[blue]%}%~%{$reset_color%}$(git_prompt_info)
$(aws_vault_env) $(prompt_char) '

RPROMPT='$(vi_mode_prompt_info)'
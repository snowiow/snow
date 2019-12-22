function prompt_aws_vault
    if test -n "$AWS_VAULT"
        set_color cyan
        string join "" "[vault: " $AWS_VAULT "] "
    end
end

function prompt_aws_vault
    if test -n "$AWS_SSO_PROFILE"
        set_color cyan
        string join "" "[aws: " $AWS_SSO_PROFILE "] "
    end
end

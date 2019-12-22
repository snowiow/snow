function prompt_kubernetes
    if test -x (command -v kubectl)
        set_color red;
        string join "" "[" (kubectl config current-context) "] "
    end
end

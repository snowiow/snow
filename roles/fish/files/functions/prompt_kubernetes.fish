function prompt_kubernetes
    if test -x (command -v kubectl)
        set_color red;
        string join "" "[k8s: " (kubectl config current-context) "] "
    end
end

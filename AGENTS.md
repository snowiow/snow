# Agent Instructions

- Make Emacs configuration changes in `roles/emacs/files/init.org`, not directly in `roles/emacs/files/init.el`.
- After changing `init.org`, tangle it to regenerate `roles/emacs/files/init.el`.
- Do not hand-edit generated/tangled Emacs Lisp in `init.el`; update the corresponding Org source block in `init.org` instead.
- Use `emacsclient` for tangling, for example:

  ```sh
  emacsclient --eval '
  (progn
    (require '\''org)
    (org-babel-tangle-file "roles/emacs/files/init.org" "roles/emacs/files/init.el" "emacs-lisp"))'
  ```

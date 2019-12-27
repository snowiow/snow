; =============================> Package Management
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)
; =============================> BuiltIns
; Increase/Decrease Text size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-k") 'previous-line)

(setq backup-directory-alist `(("." . "~/tmp")))

; Hide scrollbar
(scroll-bar-mode -1)
; Hide Toolbar
(tool-bar-mode -1)
; Hide Menu Bar
(menu-bar-mode -1)
;; Activate Paren Mode
(show-paren-mode t)
; Always show this many lines above or below cursor
(setq scroll-margin 5)
; Automatically wrap lines
(global-visual-line-mode t)
; Always create closing bracket
(electric-pair-mode 1)
; Set default font
(set-face-attribute 'default nil
		    :family "Iosevka Term"
                    :height 120
                    :weight 'normal
                    :width 'normal)

; Mac OSX Settings make command meta
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
; Always add a final newline at the end of the file
(setq require-final-newline t)
; Tab width
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

; ============================> BuiltIn Packages
; Auto Fill Mode
(setq-default fill-column 80)

; Dired
(require 'dired)

; ERC
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
(setq erc-prompt-for-password nil)

; EDiff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-Ancestor ((t (:background "#223448" :foreground "#4db5bd"))))
 '(ediff-current-diff-B ((t (:inherit ediff-current-diff-A :background "#223448" :foreground "#50a14f"))))
 '(ediff-current-diff-C ((t (:inherit ediff-current-diff-A :background "#223448" :foreground "dark gray")))))

; Org Mode
(require 'org)
(setq org-image-actual-width nil)
(setq org-directory "~/Seafile/My Library/notes")
(setq org-journal-dir "~/Seafile/My Library/notes/journal")
(setq org-agenda-files
	(file-expand-wildcards (concat org-directory "/*.org")))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))
(setq org-default-notes-file (concat org-directory "/capture.org"))
(defun get-journal-file-this-year ()
  "Return filename for today's journal entry."
  (let ((yearly-name (concat "/" (format-time-string "%Y") ".org")))
    (expand-file-name (concat org-journal-dir yearly-name))))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (lambda () (concat org-directory "/todos.org")) "Allgemein")
         "* TODO %?")
        ("m" "Todo MOIA" entry (file+headline (lambda () (concat org-directory "/moia.org")) "Todos")
         "* TODO %?")
        ("j" "Journal Note"
         entry (file+datetree (lambda () (get-journal-file-this-year)))
         "* %U %?")))
(setq org-todo-keywords
      '((sequence "TODO(t)" "TODAY(y)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELLED(c)")))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ledger . t)))

; Whitspace Column
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
; =============================> Packages

(use-package company
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key evil-insert-state-map (kbd "C-o") 'company-complete)
  :hook (after-init . global-company-mode))

(use-package company-lsp
  :after company
  :init
  (setq lsp-log-io t)
  :config
  (push 'company-lsp company-backends))

(use-package counsel
  :after evil-leader
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style nil)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
  :config
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-S-k") 'ivy-beginning-of-buffer)
  (define-key ivy-minibuffer-map (kbd "C-S-j") 'ivy-end-of-buffer)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-up-command)
  (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-y") 'ivy-immediate-done)
  (define-key ivy-switch-buffer-map (kbd "C-d") 'ivy-switch-buffer-kill)
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)
  (defhydra hydra-info (:color blue)
    "Info Menu"
    ("v" counsel-describe-variable "Describe Variable")
    ("f" counsel-describe-function "Describe Function")
    ("k" describe-key "Describe Key")
    ("a" info-apropos "Info Apropos")
    ("i" info "Info"))
  (evil-leader/set-key "h" 'hydra-info/body)
  (evil-leader/set-key
    "b"   'ivy-switch-buffer
    ":"   'counsel-M-x))

(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 5)
			  (recents  . 5)
			  (bookmarks . 5)
			  (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package dockerfile-mode)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t    
	doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one-light t)
  (load-theme 'doom-tomorrow-night t)
  )

(use-package go-tag)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  (evil-define-key 'visual emacs-lisp-mode-map
    "e" 'eval-region))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init '(calc ediff eshell term)))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  ; Global
  (evil-leader/set-key "e"
    (lambda ()
      (interactive)
      (dired default-directory)))
  (evil-leader/set-key "c"
    (lambda ()
      (interactive)
      (find-file "~/.emacs.d/init.el")))
  (defhydra hydra-org (:color blue)
    ("a" org-agenda "Agenda")
    ("c" org-capture "Capture Templates"))
    (evil-leader/set-key "$" 'hydra-org/body)
  ; Dired
  (evil-define-key 'normal dired-mode-map (kbd "h")
    (lambda ()
      (interactive)
      (find-alternate-file "..")))
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map (kbd "c") 'find-file)
  (evil-define-key 'normal dired-mode-map (kbd "d") 'dired-create-directory)
  (evil-define-key 'normal dired-mode-map (kbd "m") 'dired-mark)
  (evil-define-key 'normal dired-mode-map (kbd "D") 'dired-do-delete)
  (evil-define-key 'normal dired-mode-map (kbd "R") 'dired-do-rename)
  ; Org Mode
  (evil-leader/set-key-for-mode 'org-mode
    "RET" 'org-open-at-point
    "it" 'org-toggle-inline-images
    "li" 'org-insert-link
    "lo" 'org-agenda-open-link
    "SPC" 'org-ctrl-c-ctrl-c)
  ; Frame Management
  (defhydra hydra-frames (:color blue)
    ("o" make-frame-command "Make a new frame")
    ("d" delete-frame "Delete Frame")
    ("n" other-frame "Other Frame"))
    (evil-leader/set-key "w" 'hydra-frames/body))

(use-package evil-magit
  :after (evil-leader magit)
  :config
  (defhydra hydra-magit (:color blue)
    "Language Server Protocol Mode"
    ("g" magit "Status")
    ("b" magit-blame "Blame")
    ("d" magit-diff "Diff"))
  (evil-leader/set-key "g" 'hydra-magit/body))

(use-package evil-org
  :after evil
  :config
  (evil-org-set-key-theme '(
			    navigation
			    insert
			    textobjects
			    additional
			    calendar
			    todo
			    heading))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :hook (org-mode . evil-org-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package flymake-cursor
  :load-path "~/.emacs.d/packages/emacs-flymake-cursor"
  :config
  (flymake-cursor-mode))

(use-package go-mode)

(use-package gotests
  :load-path "~/.emacs.d/packages/GoTests-Emacs")

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package hydra)
(use-package jsonnet-mode)

(use-package kubel)
(load-file "~/.emacs.d/kubel-evil.el")
(require 'kubel-evil)

(use-package ledger-mode
  :config
  (defhydra hydra-ledger (:color blue)
    "ledger mode"
    ("r" ledger-reconcile "reconcile")
    ("a" ledger-add-transaction "add transaction")
    ("c" ledger-occur "occur")
    ("p" ledger-report "reports"))
  (evil-leader/set-key-for-mode 'ledger-mode "l" 'hydra-ledger/body))

(use-package linum-relative
  :config
  (linum-relative-global-mode))

(use-package lsp-mode
  :hook
  (go-mode . lsp)
  (python-mode . lsp)
  :config
  (defhydra hydra-lsp (:color blue)
    "Language Server Protocol Mode"
    ("d" lsp-find-definition "Definition")
    ("f" lsp-format-buffer "Format Buffer")
    ("r" lsp-find-references "References")
    ("i" lsp-organize-imports "Organize Imports")
    ("t" imenu "Tags"))
  (evil-leader/set-key "l" 'hydra-lsp/body))

(use-package magit)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . auto-fill-mode)
  (markdown-mode . whitespace-mode))

(use-package python-mode)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode)
  (setq
   org-super-agenda-groups
   '(
     (:name "Today"
            :time-grid t
            :todo "TODAY")
     (:name "High Priority"
            :priority "A"
            :order 0)
     (:name "Work"
            :category "MOIA")
     (:name "To Read"
            :category "Read"
            :order 100)
     (:name "Waiting"
            :todo "WAITING"
            :order 101)
     )))

(use-package ripgrep)

(use-package projectile
  :after evil-leader
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (defhydra hydra-projectile (:color blue)
    "Projectile"
    ("o" projectile-switch-project "Switch Project")
    ("t" projectile-run-term "Terminal")
    ("s" projectile-run-eshell "EShell"))
  (evil-leader/set-key "p" 'hydra-projectile/body)
  (evil-leader/set-key
    "o" 'projectile-find-file
    "7" 'projectile-ripgrep
    "8" 'ripgrep-regexp))

(use-package pyvenv)
(use-package package-lint)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook (function (lambda ()
					(setq evil-shift-width 2)))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(load "~/.emacs.d/modeline-dark.el")

; Custom Functions
(defun snow/switch-theme ()
    "switches between dark and light theme"
    (interactive)
  (if (eq (car custom-enabled-themes) 'doom-vibrant)
      (progn
	(disable-theme 'doom-vibrant)
	(load "~/.emacs.d/modeline-light.el"))
    (progn
      (enable-theme 'doom-vibrant)
	(load "~/.emacs.d/modeline-dark.el"))))

; ERC
(defun snow/erc ()
    "Join ERC with default settings"
    (interactive)
    (erc :server "irc.freenode.net" :port "6667" :nick "snowiow"))

; Term Mode
(defun snow/term ()
  "Opens term without asking which shell to run"
  (interactive)
  (projectile-run-term "zsh"))

(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
        (format "%s -f TAGS -e -R %s" "ctags" (directory-file-name dir-name)))
)
(defun snow/jsonnet-reformat-buffer ()
  "Reformat entire buffer using the Jsonnet format utility."
  (interactive)
  (call-process-region (point-min) (point-max) "jsonnetfmt" t t nil "-"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Seafile/My Library/notes/2019.org" "~/Seafile/My Library/notes/Rezepte.org" "~/Seafile/My Library/notes/Spanisch.org" "~/Seafile/My Library/notes/aws.org" "~/Seafile/My Library/notes/container_days2019.org" "~/Seafile/My Library/notes/emacs.org" "~/Seafile/My Library/notes/fantasy.org" "~/Seafile/My Library/notes/finanzen.org" "~/Seafile/My Library/notes/linux.org" "~/Seafile/My Library/notes/moia.org" "~/Seafile/My Library/notes/private_projekte.org" "~/Seafile/My Library/notes/raspberry.org" "~/Seafile/My Library/notes/todos.org" "~/Seafile/My Library/notes/unterhaltung.org" "~/Seafile/My Library/notes/vim.org")))
 '(package-selected-packages
   (quote
    (org-super-agenda kubel-evil package-lint pyvenv pyenv evil-numbers quote
                      (use-package)))))

(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

; =============================> Package Management
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package
  :ensure t)


; =============================> BuiltIns
(setq tags-revert-without-query 1)
(setq backup-directory-alist `(("." . "~/tmp")))

; Hide scrollbar
(scroll-bar-mode -1)
; Hide Toolbar
(tool-bar-mode -1)
; Hide Menu Bar
(menu-bar-mode -1)
; Hide tab-bar
(setq tab-bar-show nil)
;; Activate Paren Mode
(show-paren-mode t)
; Always show this many lines above or below cursor
(setq scroll-margin 5)
; turn off annoying bell sound
(setq ring-bell-function 'ignore)
; Automatically wrap lines
(global-visual-line-mode t)
; Always create closing bracket
(electric-pair-mode 1)
; always answer questions with y/n
(defalias 'yes-or-no-p 'y-or-n-p)
; Set fonts
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil
                        :family "Iosevka Term"
                        :height 120)
  (set-face-attribute 'default nil
                      :family "Iosevka Term"
                      :height 140))

; Mac OSX Settings make command meta
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
; Always add a final newline at the end of the file
(setq require-final-newline t)
; Tab width
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
; Custom-file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;number of bytes between consing. Mainly increased for lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;set spelling program
(setq ispell-program-name "aspell")

; ============================> BuiltIn Packages
; Auto Fill Mode
(setq-default fill-column 80)


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

; Whitspace Column
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

; set holidays
(setq solar-n-hemi-seasons
    '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

(setq holiday-general-holidays
    '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

(setq holiday-christian-holidays
    '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        (holiday-easter-etc -3 "Gründonnerstag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        (holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))

(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-bahai-holidays nil)
(setq holiday-oriental-holidays nil)

; =============================> Packages
(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))


(use-package auth-source-pass
  :ensure nil
  :config
  (auth-source-pass-enable)
  (setq auth-sources '(password-store)))

(use-package cider)

(use-package clojure-mode)

(use-package company
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  :config
  :hook
  (after-init . global-company-mode)
  :bind (:map company-active-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-p" . company-select-previous)
   ("C-d" . company-show-doc-buffer)))

(use-package counsel)

(use-package dart-mode
  :hook
  (dart-mode . flutter-test-mode))

(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((agenda . 5)
                          (projects . 5)
                          (recents  . 5)))
  :config
  (dashboard-setup-startup-hook)
  (setq tab-bar-new-tab-choice "*dashboard*"))

(use-package dired-single)
(use-package dockerfile-mode)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one-light t)
  (load-theme 'doom-tomorrow-night t))

(defun snow/eshell-config ()
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  (define-key eshell-mode-map (kbd "<up>") 'eshell-previous-input)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (setq eshell-scroll-to-bottom-on-input t
        eshell-prompt-regexp             "^$ "))

(defun snow/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (aws-vault (getenv "AWS_VAULT"))
        (k8s-context (shell-command-to-string "kubectl config current-context")))
    (concat
     "\n"
     (propertize (user-login-name) 'face `(:foreground "#c196d6"))
     (propertize "@" 'face `(:foreground "white"))
     (propertize (system-name) 'face `(:foreground "#f0c574"))
     (when current-branch
        (propertize (concat "  " current-branch) 'face `(:foreground "#c196d6")))
     (when kubel-context
        (propertize (concat " k8s: " k8s-context) 'face `(:foreground "#c86464")))
     (when aws-vault
        (propertize (concat "  " aws-vault) 'face `(:foreground "#b2b966")))
     "\n"
     (propertize (eshell/pwd) 'face `(:foreground "#819fbb"))
     "\n"
     (propertize "$ " 'face `(:foreground "white"))
     )))

(use-package eshell
  :hook
  (eshell-first-time-mode . snow/eshell-config)
  (eshell-pre-command . eshell-save-some-history)
  :config
  (setq eshell-prompt-function 'snow/eshell-prompt))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :bind (:map esh-autosuggest-active-map
   ("C-l" . 'company-complete-selection))
  :config
  (setq esh-autosuggest-delay 0.5)
  ;; (set-face-foreground 'company-preview-common "#4b5668")
  ;; (set-face-background 'company-preview nil))
  )

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init '(calc
                          calendar
                          dashboard
                          dired
                          ediff
                          eshell
                          helpful
                          info
                          magit
                          mu4e
                          pass
                          proced
                          term)))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-numbers
  :after evil)

(use-package evil-org
  :after org
  :hook
  (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package flutter
  :after dart-mode
  :custom
  (flutter-sdk-path "~/flutter/"))

(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save new-line mode-enabled)))

(use-package forge
  :disabled)

(use-package general
  :config
  (general-evil-setup t)
  (general-define-key
   "C-+" 'text-scale-increase
   "C--" 'text-scale-decrease
   ;; "C-k" 'previous-line
   )

  ;; general normal mappings
  (general-nmap
    "C-c +" 'evil-numbers/inc-at-pt
    "C-c -" 'evil-numbers/dec-at-pt)

  ;; org-mode mappings
  (general-define-key
   :keymaps 'org-mode-map
   :states 'normal
   "RET"  'org-open-at-point)

  ;; org-agenda-mode mappings
  (general-define-key
   :keymaps 'org-agenda-mode-map
   "<"  'org-agenda-earlier
   ">"  'org-agenda-later)

  ;; emacs-lisp-mode mappings
  (general-define-key
   :states 'visual
   :keymaps 'emacs-lisp-mode-map
   "e" 'eval-region)

  ;; evil-insert-state mappings
  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-o" 'company-complete)


  ;; leader key mappings
  (general-create-definer snow/leader-keys
    :states '(normal motion)
    :keymaps 'override
    :prefix "SPC")

  (snow/leader-keys
    ;; general
    "b" 'ivy-switch-buffer
    "c" (lambda ()
          (interactive)
          (find-file "~/.emacs.d/init.el"))
    "e" 'dired-jump

    ;; find
    "f"  '(:ignore t :which-key "find in file")
    "fs" 'swiper
    "fi" 'counsel-imenu

    ;; git
    "g"  '(:ignore t :which-key "Git")
    "gg" 'magit
    "gb" 'magit-blame
    "gd" 'magit-diff
    "gl" 'git-link

    ;; help
    "h" '(:ignore t :which-key "Help")
    "ha" 'info-apropos
    "hf" 'counsel-describe-function
    "hk" 'describe-key
    "hi" 'info
    "hp" 'describe-package
    "hs" 'counsel-describe-symbol
    "hv" 'counsel-describe-variable

    ;; language-server-protocol
    "l" '(:ignore t :which-key "LSP")
    "ld" 'lsp-find-definition
    "lf" 'lsp-format-buffer
    "li" 'lsp-organize-imports
    "ln" 'lsp-rename
    "lr" 'lsp-find-references
    "ls" 'lsp-describe-session
    "lt" 'imenu

    ;; org mode
    "o" '(:ignore t :which-key "Org Mode")
    "oa" 'org-agenda
    "oc" 'org-capture

    ;; projectile
    "p" 'projectile-command-map

    ;;tab-bar-mode
    "t" '(:ignore t :which-key "Tabs")
    "tc" 'tab-close
    "tn" 'tab-new
    "tr" 'tab-bar-rename-tab
    "tt" 'tab-bar-select-tab-by-name

    "w" '(:ignore t :which-key "Window")
    "ww" 'hydra-scale-window/body
    "wf" 'hydra-scale-font/body

    "/"  'rg-menu
    ":"  'counsel-M-x
    )

  ;; local-leader key mappings
  (general-create-definer snow/local-leader-keys
    :prefix ",")

  ;; dart-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'dart-mode-map
    "h" 'flutter-run-or-hot-reload
    "r" 'flutter-hot-restart
    )

  ;; json-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'json-mode-map
    "f" 'json-pretty-print-buffer
  )
  ;; jsonnet-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'jsonnet-mode-map
    "f" 'jsonnet-reformat-buffer
  )
  ;; emacs-lisp-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "b" 'eval-buffer
    "e" 'eval-last-sexp
    )

  ;; ledger-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'ledger-mode-map
    "r" 'ledger-reconcile
    "a" 'ledger-add-transaction
    "c" 'ledger-occur
    "p" 'ledger-report
    )

  ;; lisp-interaction-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'lisp-interaction-mode-map
    "e" 'eval-print-last-sexp
    )
  ;; org-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'org-mode-map
    "RET" 'org-open-at-point
    "g" '(:ignore t :which-key "go to")
    "gg" 'counsel-org-goto
    "gp" 'org-previous-visible-heading
    "i" 'org-toggle-inline-images
    "l" 'org-insert-link
    "o" 'org-agenda-open-link
    "p" 'org-plot/gnuplot
    "t" 'org-set-tags-command
    "," 'org-ctrl-c-ctrl-c
    "v" 'snow/org-exec-codeblock-in-vterm
    "0" 'snow/org-start-presentation
    "$" 'org-archive-subtree
    )

  ;; vterm-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'vterm-mode-map
    "p" 'vterm-yank
    :config
    (setq vterm-shell "/opt/homebrew/bin/fish")
    )
  )

(use-package git-link
  :config
  (setq git-link-open-in-browser t))

(use-package go-mode)

(use-package go-tag)

(use-package gotests
  :load-path "~/.emacs.d/packages/GoTests-Emacs")

(use-package gnuplot)

(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package hydra)

(defhydra hydra-scale-window (:timeout 4)
  "scale window"
  ("l" enlarge-window-horizontally "+")
  ("h" shrink-window-horizontally "-")
  ("q" nil "finished" :exit t))

(defhydra hydra-scale-font (:timeout 4)
  "scale text"
  ("j" text-scale-increase "+")
  ("k" text-scale-decrease "-")
  ("q" nil "finished" :exit t))

(use-package ivy
  :init
  (ivy-mode 1)
  :config

  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  :bind
  (:map ivy-minibuffer-map
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   ("C-S-k" . ivy-beginning-of-buffer)
   ("C-S-j" . ivy-end-of-buffer)
   ("C-d" . ivy-scroll-up-command)
   ("C-u" . ivy-scroll-down-command)
   ("C-y" . ivy-immediate-done)
   :map ivy-switch-buffer-map
   ("C-d" . ivy-switch-buffer-kill)
   ("C-k" . ivy-previous-line)))

(use-package json-mode)
(use-package jsonnet-mode)

(use-package kubel
  :config
  (setq kubel-use-namespace-list 'on))

(use-package kubel-evil)

(use-package ledger-mode)

(use-package linum-relative
  :config
  (linum-relative-global-mode))

(use-package lsp-mode
  :hook
  (go-mode . lsp)
  (python-mode . lsp)
  ;; (terraform-mode . lsp) ;; currently not working properly
  (typescript-mode . lsp)
  :init
  (setq lsp-headerline-breadcrumb-enable t)
  :config
  (setq lsp-file-watch-threshold 5000))

(use-package lsp-dart
  :hook
  (dart-mode . lsp))

(use-package magit)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . auto-fill-mode))

(use-package mu4e
  :ensure nil
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"
  :config
  (mu4e t)
  ;; refresh mail every 30 minutes
  (setq mu4e-update-interval (* 30 60))
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-compose-format-flowed t)

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder "/Sent")
  (setq mu4e-refile-folder "/Archiv")
  (setq mu4e-trash-folder "/Trash")
  (setq user-mail-address "marcel.patzwahl@posteo.de")

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; smtp settings
  (setq smtpmail-default-smtp-server "posteo.de")
  (setq smtpmail-smtp-server "posteo.de")
  (setq smtpmail-smtp-user "marcel.patzwahl@posteo.de")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-stream-type 'starttls)
  (setq message-send-mail-function 'smtpmail-send-it))

(use-package ob-async)
(use-package ob-typescript)
(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("pdf"))
                "open"
                '(file))))
  (openwith-mode t))
(use-package python-mode)

(use-package org
  :hook
  (org-after-todo-statistics . org-summary-todo)
  ;; (org-mode . flyspell-mode)
  :config
  (advice-add 'org-open-at-point :before 'evil-set-jump)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
  (setq org-agenda-custom-commands
        '(("w" "Work Todos"
           ((agenda "" ((org-agenda-span 1)))
            (todo ""
                  ((org-agenda-overriding-header "\nUnscheduled TODOs")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
           ((org-agenda-compact-blocks t)
            (org-agenda-files '("~/Sync/notes/work.org"))))))
  (setq org-directory "~/Sync/notes")
  (setq org-agenda-files
        (file-expand-wildcards (concat org-directory "/*.org")))

  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-archive-location "%s_archive::datetree/* Archived Tasks")
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python3")

  (setq org-capture-templates
        '(("a" "Appointments")
          ("ap" "Private" entry (file+headline
                              (lambda ()
                                (concat org-directory "/todos.org"))
                              "Termine")
           "* %?")
          ("aw" "Work" entry (file+headline
                                   (lambda ()
                                     (concat org-directory "/work.org"))
                                   "Appointments")
           "* %?")
          ("t" "Todos")
          ("tt" "Todo" entry (file+headline
                              (lambda ()
                                (concat org-directory "/todos.org"))
                              "Allgemein")
           "* TODO %?")
          ("tw" "Todo Work" entry (file+headline
                                   (lambda ()
                                     (concat org-directory "/work.org"))
                                   "Todos")
           "* TODO %?")
          ("j" "Journal Note"
           entry (file+datetree (lambda () (get-journal-file-this-year)))
           "* %U %?")
          ("f" "Fitness")
          ("fj" "Workout Journal Entry"
           entry (file+datetree (lambda () (concat org-directory "/fitness.org"))
                                "Gym" "Workout Journal")
           "* %U %?")
          ("fw" "Gewicht Eintrag" table-line
           (id "weight-table")
           "| %u | %^{Gewicht} | %^{Körperfettanteil} | %^{Körperwasser} | %^{Muskelmasse} | %^{Knochenmasse} |"  :immediate-finish t)))

  (setq org-default-notes-file (concat org-directory "/capture.org"))
  (setq org-ellipsis " ▾")
  (setq org-image-actual-width nil)
  (setq org-journal-dir "~/Sync/notes/journal")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "TODAY(y)" "WAITING(w)" "|" "DONE(d)")
          (sequence "|" "CANCELLED(c)"))))

(defun get-journal-file-this-year ()
  "Return filename for today's journal entry."
  (let ((yearly-name (concat "/" (format-time-string "%Y") ".org")))
    (expand-file-name (concat org-journal-dir yearly-name))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((eshell . t)
   (gnuplot . t)
   (ledger . t)
   (python . t)
   (shell . t)
   (typescript . t)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun snow/org-start-presentation ()
  (interactive)
  (org-tree-slide-mode 1)
  (setq text-scale-mode-amount 1)
  (text-scale-mode 1))

(defun snow/org-end-presentation ()
  (interactive)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :config
  (evil-define-key 'normal org-tree-slide-mode-map
    (kbd "q") 'snow/org-end-presentation
    (kbd "<right>") 'org-tree-slide-move-next-tree
    (kbd "<left>") 'org-tree-slide-move-previous-tree))

(use-package pass)
(use-package package-lint)

(use-package proced
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update t))))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1))

(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(use-package rainbow-delimiters
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package ripgrep)
(use-package rg)

(use-package terraform-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode))

(use-package typescript-mode
  :init
  (setq typescript-indent-level 2))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package vterm)

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook (function (lambda ()
					(setq evil-shift-width 2)))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(load "~/.emacs.d/modeline-dark.el")

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "L" 'dired-display-file))

; Custom Functions

; dark/light theme switch
(defun snow/switch-theme ()
    "switches between dark and light theme"
    (interactive)
  (if (eq (car custom-enabled-themes) 'doom-tomorrow-night)
      (progn
	(disable-theme 'doom-tomorrow-night)
	(load "~/.emacs.d/modeline-light.el"))
    (progn
      (enable-theme 'doom-tomorrow-night)
	(load "~/.emacs.d/modeline-dark.el"))))

; ERC
(defun snow/erc ()
    "Join ERC with default settings"
    (interactive)
    (erc :server "irc.freenode.net" :port "6667" :nick "snowiow"))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(put 'dired-find-alternate-file 'disabled nil)

; execute org code in project vterm
(defun snow/org-exec-codeblock-in-vterm ()
    "execute current org mode code block in vterm"
    (org-babel-mark-block)
    (interactive)
    (kill-ring-save (region-beginning) (region-end))
    (projectile-run-vterm)
    (vterm-yank)
    (yank-pop))

(use-package aws
  :load-path "~/.emacs.d/packages/awscli"
  :custom
  (aws-vault t))

(use-package aws-evil
  :load-path "~/.emacs.d/packages/awscli")

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

; Whitspace Column
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

; =============================> Packages
(use-package cider)

(use-package clojure-mode)

(use-package company
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.0)
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

(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((agenda . 5)
                          (projects . 5)
                          (recents  . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package dockerfile-mode)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one-light t)
  (load-theme 'doom-tomorrow-night t))

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
                          ediff
                          eshell
                          helpful
                          minibuffer
                          term)))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-magit
  :after (evil magit))

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

(use-package flymake-cursor
  :load-path "~/.emacs.d/packages/emacs-flymake-cursor"
  :config
  (flymake-cursor-mode))

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

  ;; org-agenda-mode mappings
  (general-define-key
   :keymaps 'org-agenda-mode-map
   "<"  'org-agenda-earlier
   ">"  'org-agenda-later)

  ;; evil-insert-state mappings
  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-o" 'company-complete)
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "h" (lambda ()
         (interactive)
         (find-alternate-file ".."))
   "l" 'dired-find-alternate-file
   "c" 'find-file
   "d" 'dired-create-directory
   "m" 'dired-mark
   "D" 'dired-do-delete
   "R" 'dired-do-rename)

  ;; emacs-lisp-mode mappings
  (general-define-key
   :states 'visual
   :keymaps 'emacs-lisp-mode-map
    "e" 'eval-region)

  ;; leader key mappings
  (general-create-definer snow/leader-keys
    :keymaps '(normal emacs)
    :prefix "SPC")

  (snow/leader-keys
    ;; general
    "b" 'ivy-switch-buffer
    "c" (lambda ()
          (interactive)
          (find-file "~/.emacs.d/init.el"))
    "e" (lambda ()
          (interactive)
          (dired default-directory))
    ;; git
    "g"  '(:ignore t :which-key "Git")
    "gg" 'magit
    "gb" 'magit-blame
    "gd" 'magit-diff

    ":" 'counsel-M-x

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
    "lr" 'lsp-find-references
    "ls" 'lsp-describe-session
    "lt" 'imenu

    ;; projectile
    "p" '(:ignore t :which-key "Projectile")
    "p7" 'projectile-ripgrep
    "p8" 'ripgrep-regexp
    "po" 'projectile--find-file
    "pp" 'projectile-switch-project
    "pt" '(:ignore t :which-key "Terminal")
    "pte" 'projectile-run-eshell
    "ptt" 'projectile-run-term
    "ptv" 'projectile-run-vterm

    ;; org mode
    "$" '(:ignore t :which-key "Org Mode")
    "$a" 'org-agenda
    "$c" 'org-capture
    )

  ;; local-leader key mappings
  (general-create-definer snow/local-leader-keys
    ;; :keymaps '(override normal emacs)
    :prefix ",")

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

  ;; org-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'org-mode-map
    "RET" 'org-open-at-point
    "i" 'org-toggle-inline-images
    "l" 'org-insert-link
    "o" 'org-agenda-open-link
    "," 'org-ctrl-c-ctrl-c
    )
  ;; vterm-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'vterm-mode-map
    "p" 'vterm-yank
    )
  )

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

(use-package jsonnet-mode)

(use-package kubel)

(use-package ledger-mode)

(use-package linum-relative
  :config
  (linum-relative-global-mode))

(use-package lsp-mode
  :hook
  (go-mode . lsp)
  (python-mode . lsp)
  (typescript-mode . lsp)
  :init
  (setq lsp-headerline-breadcrumb-enable t))

(use-package magit)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . auto-fill-mode))

(use-package ob-async)
(use-package python-mode)

(use-package org
  :hook
  (org-after-todo-statistics . org-summary-todo)
  :config
  (progn
    (setq org-image-actual-width nil)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-ellipsis " ▾")
    (setq org-directory "~/Seafile/My Library/notes")
    (setq org-journal-dir "~/Seafile/My Library/notes/journal")
    (setq org-agenda-files
        (file-expand-wildcards (concat org-directory "/*.org")))
    (setq org-agenda-window-setup 'current-window)
    (setq org-default-notes-file (concat org-directory "/capture.org"))
    (setq org-babel-python-command "python3")
    (setq org-agenda-custom-commands
        '(("w" "Work Todos"
           ((agenda "" ((org-agenda-span 1)))
            (todo ""
                ((org-agenda-overriding-header "\nUnscheduled TODOs")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
           ((org-agenda-compact-blocks t)
            (org-agenda-files '("~/Seafile/My Library/notes/work.org"))
            ))))
    (setq org-capture-templates
        '(("t" "Todo" entry (file+headline
                            (lambda ()
                                (concat org-directory "/todos.org"))
                            "Allgemein")
            "* TODO %?")
            ("m" "Todo Work" entry (file+headline
                                    (lambda ()
                                    (concat org-directory "/work.org"))
                                    "Todos")
            "* TODO %?")
            ("j" "Journal Note"
            entry (file+datetree (lambda () (get-journal-file-this-year)))
            "* %U %?")
            ("w" "Gewicht Eintrag" table-line
            (id "weight-table")
            "| %u | %^{Gewicht} |" :immediate-finish t)))
    (setq org-todo-keywords
        '((sequence "TODO(t)" "TODAY(y)" "WAITING(w)" "|" "DONE(d)")
            (sequence "|" "CANCELLED(c)")))
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
    (setq holiday-oriental-holidays nil))
  :bind
  (("C-c g" . org-plot/gnuplot)))

(defun get-journal-file-this-year ()
  "Return filename for today's journal entry."
  (let ((yearly-name (concat "/" (format-time-string "%Y") ".org")))
    (expand-file-name (concat org-journal-dir yearly-name))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (ledger . t)
   (python . t)
   (shell . t)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package rainbow-delimiters
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package ripgrep)

(use-package package-lint)

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1))

(use-package pyvenv)

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

; Term Mode
(defun snow/term ()
  "Opens term without asking which shell to run"
  (interactive)
  (projectile-run-term "fish"))

(defun snow/jsonnet-reformat-buffer ()
  "Reformat entire buffer using the Jsonnet format utility."
  (interactive)
  (call-process-region (point-min) (point-max) "jsonnetfmt" t t nil "-"))
(put 'dired-find-alternate-file 'disabled nil)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

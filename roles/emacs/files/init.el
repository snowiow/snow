; =============================> Package Management
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package
  :ensure t)
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
; turn off annoying bell sound
(setq ring-bell-function 'ignore)
; Automatically wrap lines
(global-visual-line-mode t)
; Always create closing bracket
(electric-pair-mode 1)
; always answer questions with y/n
(defalias 'yes-or-no-p 'y-or-n-p)
; Set default font
(set-face-attribute 'default nil
                    :family "Iosevka Term"
                    :height 140
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

; Org Mode
(require 'org)
(bind-key "C-c g" 'org-plot/gnuplot)
(setq org-image-actual-width nil)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-directory "~/Seafile/My Library/notes")
(setq org-journal-dir "~/Seafile/My Library/notes/journal")
(setq org-agenda-files
	(file-expand-wildcards (concat org-directory "/*.org")))
(setq org-agenda-window-setup 'current-window)
(setq org-default-notes-file (concat org-directory "/capture.org"))
(defun get-journal-file-this-year ()
  "Return filename for today's journal entry."
  (let ((yearly-name (concat "/" (format-time-string "%Y") ".org")))
    (expand-file-name (concat org-journal-dir yearly-name))))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline
                           (lambda ()
                             (concat org-directory "/todos.org"))
                           "Allgemein")
         "* TODO %?")
        ("m" "Todo MOIA" entry (file+headline
                                (lambda ()
                                  (concat org-directory "/moia.org"))
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
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (ledger . t)
   (python . t)
   (shell . t)))

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

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

; Whitspace Column
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
; =============================> Packages
(use-package cider)

(use-package clojure-mode
  :hook
  (clojure-mode . rainbow-delimiters-mode))

(use-package company
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (define-key evil-insert-state-map (kbd "C-o") 'company-complete)
  :hook
  (after-init . global-company-mode))

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
  (load-theme 'doom-tomorrow-night t))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-Y-yank-to-eol t)
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

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme
               '(textobjects
                 insert
                 navigation
                 additional
                 shift
                 todo
                 heading))))
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

(use-package go-mode)

(use-package go-tag)

(use-package gotests
  :load-path "~/.emacs.d/packages/GoTests-Emacs")

(use-package gnuplot)
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package hydra)
(use-package jsonnet-mode)

(use-package kubel)
(load-file "~/.emacs.d/kubel/kubel-evil.el")

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
  (typescript-mode . lsp)
  :init
  (setq lsp-headerline-breadcrumb-enable t)
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

(use-package ob-async)
(use-package python-mode)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (setq
   org-super-agenda-groups
   '(
     (:name "Today"
            :time-grid t
            :todo "TODAY"
            :deadline today)
     (:name "High Priority"
            :priority "A"
            :deadline past
            :order 0)
     (:name "With Deadline"
            :deadline future)
     (:name "Haushalt"
            :category "Haushalt")
     (:name "Work"
            :category "MOIA")
     (:name "Freizeit Coding"
            :category "Freizeit Coding")
     (:name "Waiting"
            :todo "WAITING"
            :order 99)
     (:name "To Read"
            :category "Read"
            :order 100)
     (:name "To Watch"
            :category "Watch"
            :order 101)
     (:name "Geburtstage und Feiertage"
            :category ("Geburtstage" "Feiertage")
            :order 102)
     )))

(use-package rainbow-delimiters)
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

(use-package typescript-mode
  :init
  (setq typescript-indent-level 2)
  )
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
  (projectile-run-term "zsh"))

(defun snow/jsonnet-reformat-buffer ()
  "Reformat entire buffer using the Jsonnet format utility."
  (interactive)
  (call-process-region (point-min) (point-max) "jsonnetfmt" t t nil "-"))
(put 'dired-find-alternate-file 'disabled nil)

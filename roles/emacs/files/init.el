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
  :custom
  (auth-sources '(password-store)))

(use-package cider)

(use-package clojure-mode)

(use-package company
  :custom
  (company-dabbrev-downcase nil)
  (company-selection-wrap-around t)
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
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
  :custom
  (dashboard-startup-banner 'logo)
  (tab-bar-new-tab-choice "*dashboard*")
  (dashboard-items '((agenda . 5)
                     (projects . 5)
                     (recents  . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package dired-single)
(use-package dockerfile-mode)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one-light t)
  (load-theme 'doom-tomorrow-night t))

(use-package erc
  :custom
  (erc-prompt-for-password nil)
  (erc-modules '(autojoin fill notifications stamp track))
  (erc-autojoin-timing 'ident)
  (erc-autojoin-channels-alist '(("libera.chat" "#systemcrafters" "#emacs")))
  (erc-rename-buffers t)
  (erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY"))
  (erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "AWAY"))
  (erc-timestamp-only-if-changed-flag nil)
  (erc-timestamp-format "%H:%M ")
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-fill-prefix "      ")
  (erc-fill-column 120)
  :config
  (setq erc-prompt-for-nickserv-password nil))

(use-package erc-hl-nicks
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks))

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
  :custom
  (eshell-prompt-function 'snow/eshell-prompt))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :bind (:map esh-autosuggest-active-map
   ("C-l" . 'company-complete-selection))
  :custom
  (esh-autosuggest-delay 0.5))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :custom
  (eshell-syntax-highlighting-global-mode +1))

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-want-Y-yank-to-eol t)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-tree)
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
  :custom
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
  :custom
  (flycheck-check-syntax-automatically '(save new-line mode-enabled)))

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
   "C-o" 'company-complete
   "C-y" 'yas-expand)


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
    "o"    '(:ignore t :which-key "Org Mode")
    "oa"   'org-agenda
    "oc"   'org-capture
    "or"   '(:ignore t :which-key "Roam")
    "ord"  '(:ignore t :which-key "Daily")
    "ordt" 'org-roam-dailies-capture-today
    "ordT" 'org-roam-dailies-goto-today
    "ordy" 'org-roam-dailies-capture-yesterday
    "ordY" 'org-roam-dailies-goto-yesterday
    "ordd" 'org-roam-dailies-capture-date
    "ordD" 'org-roam-dailies-goto-date
    "orf"  'org-roam-node-find
    "ort"  'org-roam-buffer-toggle

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

    "y" 'yas-insert-snippet

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
    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "ef" 'eval-defun
    "l" 'package-lint-current-buffer
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

  ;; mu4e-compose-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'mu4e-compose-mode-map
    "a" 'mml-attach-file
    "cc" 'message-goto-cc
    "bcc" 'message-goto-bcc)

  ;; org-mode
  (snow/local-leader-keys
    :states 'normal
    :keymaps 'org-mode-map
    "RET" 'org-open-at-point
    "g"   '(:ignore t :which-key "go to")
    "gg"  'counsel-org-goto
    "gp"  'org-previous-visible-heading
    "i"   'org-toggle-inline-images
    "l"   'org-insert-link
    "o"   'org-agenda-open-link
    "p"   'org-plot/gnuplot
    "r"   '(:ignore t :which-key "Org Roam")
    "ra"  'org-roam-alias-add
    "ri"  'org-roam-node-insert
    "t"   'org-set-tags-command
    ","   'org-ctrl-c-ctrl-c
    "v"   'snow/org-exec-codeblock-in-vterm
    "0"   'snow/org-start-presentation
    "$"   'org-archive-subtree
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
  :custom
  (git-link-open-in-browser t))

(use-package go-mode)

(use-package go-tag)

(use-package gotests
  :load-path "~/.emacs.d/packages/GoTests-Emacs")

(use-package gnuplot)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  :hook
  (yaml-mode . highlight-indent-guides-mode))

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
  :custom
  (mu4e-update-interval (* 30 60))
  (mu4e-get-mail-command "offlineimap")
  ;; refresh mail every 30 minutes
  (mu4e-compose-format-flowed t)
  (mu4e-drafts-folder "/Drafts")
  (mu4e-sent-folder "/Sent")
  (mu4e-refile-folder "/Archiv")
  (mu4e-trash-folder "/Trash")
  (user-mail-address "marcel.patzwahl@posteo.de")

  ;; smtp settings
  (smtpmail-default-smtp-server "posteo.de")
  (smtpmail-smtp-server "posteo.de")
  (smtpmail-smtp-user "marcel.patzwahl@posteo.de")
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (message-send-mail-function 'smtpmail-send-it)
  :config
  (mu4e t)
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t))

(use-package ob-async)
(use-package ob-typescript)

(use-package openwith
  :config
  (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler) ;; needed to not randomly open the attachment when trying to send it
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
  :custom
  ;; important first settings which is used by other configurations
  (org-directory "~/Sync/notes")
  ;; AGENDA SETTINGS
  (org-agenda-custom-commands
        '(("w" "Work Todos"
           ((agenda "" ((org-agenda-span 1)))
            (todo ""
                  ((org-agenda-overriding-header "\nUnscheduled TODOs")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
           ((org-agenda-compact-blocks t)
            (org-agenda-files '("~/Sync/notes/work.org" "~/Sync/notes/appointments.org" "~/Sync/notes/meetings.org"))))
          ("p" "Private Todos"
           ((agenda "" ((org-agenda-span 1)))
            (tags-todo "+PRIORITY=\"A\"-TODO=\"WAITING\""
                       ((org-agenda-overriding-header "\nHigh Priority")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
            (tags-todo "-PRIORITY=\"A\""
                  ((org-agenda-overriding-header "\nUnscheduled TODOs")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
            (todo "WAITING"
                  ((org-agenda-overriding-header "\nWAITING"))))
           ((org-agenda-compact-blocks t)
            (org-agenda-files '("~/Sync/notes/todos.org" "~/Sync/notes/appointments.org" "~/Sync/notes/meetings.org"))))))
  (org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-window-setup 'current-window)
  (org-archive-location "%s_archive::datetree/* Archived Tasks")
  (org-babel-python-command "python3")
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file (concat org-directory "/capture.org"))
  (org-ellipsis " ▾")
  (org-image-actual-width nil)
  (org-todo-keywords '((sequence "TODO(t)" "TODAY(y)" "WAITING(w)" "|" "DONE(d)")
                       (sequence "|" "CANCELLED(c)")))
  :config
  (advice-add 'org-open-at-point :before 'evil-set-jump)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
  (setq org-capture-templates
        '(("a" "Private Appointments" entry (file+headline
                              (lambda ()
                                (concat org-directory "/appointments.org"))
                              "Private")
           "* %?")
          ("f" "Fitness")
          ("fj" "Workout Journal Entry"
           entry (file+datetree (lambda () (concat org-directory "/fitness.org"))
                                "Gym" "Workout Journal")
           "* %U %?")
          ("fw" "Gewicht Eintrag" table-line
           (id "weight-table")
           "| %u | %^{Gewicht} | %^{Körperfettanteil} | %^{Körperwasser} | %^{Muskelmasse} | %^{Knochenmasse} |"  :immediate-finish t)
          ("t" "Todos")
          ("tt" "Todo" entry (file+headline
                              (lambda ()
                                (concat org-directory "/todos.org"))
                              "Inbox")
           "* TODO %?")
          ("w" "Work")
          ("wa" "Appointments" entry (file+headline
                              (lambda ()
                                (concat org-directory "/appointments.org"))
                              "Work")
           "* %?")
          ("wm" "Meeting" entry (file+headline
                                   (lambda ()
                                     (concat org-directory "/meetings.org"))
                                   "Work")
           (file"~/Sync/notes/templates/meeting.org"))
          ("wt" "Todo Work" entry (file+headline
                                   (lambda ()
                                     (concat org-directory "/work.org"))
                                   "Todos")
           "* TODO %?"))))

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

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Sync/notes/roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("b" "book notes" plain (file "~/Sync/notes/roam/templates/booknote.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
   :config
   (org-roam-db-autosync-mode))

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
  :custom
  (projectile-completion-system 'ivy)
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

(use-package tramp)

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package vterm)

(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook (function (lambda ()
					(setq evil-shift-width 2)))))

(use-package yasnippet
  :bind
  (:map yas-keymap
        ("C-y" . yas-next-field-or-maybe-expand))
  :config
  (yas-global-mode 1))


(defun snow/dired-open-locally ()
  "Make a local file copy of the remote file under the cursor in dired.
Opens it.  Mainly used to open pdfs or other complex formats from remote machines"
  (interactive)
  (let* ((filename (dired-get-filename nil t))
         (local-tmp-file (file-local-copy filename)))
    (find-file local-tmp-file)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "L" 'dired-display-file
    "M" 'snow/dired-open-locally))

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
    "Join ERC with default settings."
    (interactive)
    (erc-tls
     :server "irc.libera.chat"
     :port "6697"
     :nick "snowiow"))

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

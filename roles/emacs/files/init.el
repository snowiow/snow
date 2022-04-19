(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package
  :ensure t)

(server-start)

(setq tags-revert-without-query 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(setq tab-bar-show nil)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one-light t)
  (load-theme 'doom-tomorrow-night t))

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

(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(load "~/.emacs.d/modeline-dark.el")

(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil
                        :family "Iosevka Term"
                        :height 120)
  (set-face-attribute 'default nil
                      :family "Iosevka Term"
                      :height 140))

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setq backup-directory-alist `(("." . "~/tmp")))

(setq require-final-newline t)
(setq-default fill-column 80)
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

(setq scroll-margin 5)

(global-visual-line-mode t)

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

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(show-paren-mode t)

(electric-pair-mode 1)

(use-package rainbow-delimiters
  :after (clojure-mode emacs-lisp-mode)
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package linum-relative
  :custom
  (linum-relative-backend 'display-line-numbers-mode)
  :config
  (linum-relative-global-mode))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character))

(use-package popper
  :after (shackle project)
  :bind (("C-'"   . popper-toggle-latest)
         ("M-'"   . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :custom
  (popper-display-control nil)
  (popper-group-function #'popper-group-by-project)
  :init
  (setq popper-reference-buffers
        '("\\*info\\*"
          "\\*Ledger Report\\*"
          "\\*Messages\\*"
          compilation-mode
          eshell-mode
          help-mode
          helpful-mode
          magit-status-mode
          rg-mode
          vterm-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package shackle
  :config
  (setq shackle-rules '(
                        (compilation-mode :noselect t)
                        (("^\\*eshell.*?\\*" "^\\*vterm.*?\\*") :regexp t :other t :select t)
                        (" *transient*" :align below)
                        ))
  (setq shackle-default-rule '(:select t))
  (shackle-mode t))

(global-auto-revert-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror 'nomessage)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-Ancestor ((t (:background "#223448" :foreground "#4db5bd"))))
 '(ediff-current-diff-B ((t (:inherit ediff-current-diff-A :background "#223448" :foreground "#50a14f"))))
 '(ediff-current-diff-C ((t (:inherit ediff-current-diff-A :background "#223448" :foreground "dark gray")))))

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
       (tags-todo "-TODO=\"WAITING\""
                  ((org-agenda-overriding-header "\nUnscheduled TODOs")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
       (todo "WAITING"
             ((org-agenda-overriding-header "\nWAITING"))))
      ((org-agenda-compact-blocks t)
       (org-agenda-files '("~/Sync/notes/work.org" "~/Sync/notes/appointments.org" "~/Sync/notes/meetings.org" "~/Sync/notes/meetings.org_archive"))))
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
       (org-agenda-files '("~/Sync/notes/todos.org" "~/Sync/notes/appointments.org" "~/Sync/notes/meetings.org" "~/Sync/notes/meetings.org_archive"))))))
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
  (require 'org-habit)
  (advice-add 'org-open-at-point :before 'evil-set-jump)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
  (add-to-list 'org-modules 'habits)
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
          ("k" "Keyboard WPM" table-line
           (id "wpm-progress-ferris")
           "| %u | %^{WPM} | %^{Accuracy} | %^{Consistency}"  :immediate-finish t)
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
          ("wm" "Meetings")
          ("wmm" "New Meeting" entry (file+headline
                                      (lambda ()
                                        (concat org-directory "/meetings.org"))
                                      "Work")
           (file "~/Sync/notes/templates/meeting.org"))
          ("wmd" "Daily" entry (file+headline
                                (lambda ()
                                  (concat org-directory "/meetings.org"))
                                "DevOps Daily")
           (file  "templates/repeating-meeting.org"))
          ("wme" "Extended Sync" entry (file+headline
                                        (lambda ()
                                          (concat org-directory "/meetings.org"))
                                        "Extended Sync")
           (file  "templates/repeating-meeting.org"))
          ("wmf" "Refinement" entry (file+headline
                                     (lambda ()
                                       (concat org-directory "/meetings.org"))
                                     "Refinement")
           (file  "templates/repeating-meeting.org"))
          ("wmr" "Retro" entry (file+headline
                                (lambda ()
                                  (concat org-directory "/meetings.org"))
                                "Retro")
           (file  "templates/repeating-meeting.org"))
          ("wms" "Platform Sync between DataPlatform and PE" entry (file+headline
                                                                    (lambda ()
                                                                      (concat org-directory "/meetings.org"))
                                                                    "Platform Sync between DataPlatform and PE")
           (file  "templates/repeating-meeting.org"))
          ("wmt" "Tech BiWeekly" entry (file+headline
                                        (lambda ()
                                          (concat org-directory "/meetings.org"))
                                        "Tech BiWeekly")
           (file repeating-meeting-file))
          ("wt" "Todo Work" entry (file+headline
                                   (lambda ()
                                     (concat org-directory "/work.org"))
                                   "Todos")
           "* TODO %?"))))

(defun snow/rg-org (regexp)
  "Do a REGEXP search in org files in the org directory."
  (interactive "sRegexp: ")
  (rg regexp "*.org" org-directory))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(put 'dired-find-alternate-file 'disabled nil)

(defun snow/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/workspace/snow/roles/emacs/files/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'snow/org-babel-tangle-config)))

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

(use-package ob-async)
(use-package ob-typescript)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (eshell . t)
   (gnuplot . t)
   ;; (ledger . t)
   (python . t)
   (shell . t)
   (typescript . t)))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

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

(setq ispell-program-name "aspell")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(defun snow/evil-yank-highlight-advice (orig-fn beg end &rest args)
  "Highlight yanked region."
  (pulse-momentary-highlight-region beg end)
  (apply orig-fn beg end args))

(use-package evil
  :after undo-tree
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-want-Y-yank-to-eol t)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-tree)
  :config
  (advice-add 'evil-yank :around 'snow/evil-yank-highlight-advice)
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
                          forge
                          helpful
                          info
                          magit
                          mu4e
                          package-menu
                          pass
                          proced
                          rg
                          ripgrep
                          term
                          xref)))

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

(use-package general
  :after consult
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
    ;; applications
    "a" '(:ignore t :which-key "applications")
    "aa" '(:ignore t :which-key "aws")
    "aaa" 'aws
    "ac"  'calc
    "aal" 'aws-login
    "ak" 'kubel
    "am" 'mu4e
    "ap" 'pass

    "b" 'consult-buffer
    "c" (lambda ()
          (interactive)
          (find-file "~/workspace/snow/roles/emacs/files/init.org"))
    "e" 'dired-jump

    ;; find
    "f"  '(:ignore t :which-key "find")
    "fd" 'dired
    "ff" 'find-file
    "fi" 'consult-imenu
    "fr" 'rg
    "fs" 'consult-line

    ;; git
    "g"  '(:ignore t :which-key "Git")
    "gg" 'magit
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gd" 'magit-diff
    "gl" 'git-link
    "gw" 'browse-at-remote

    ;; help
    "h" '(:ignore t :which-key "Help")
    "ha" 'consult-apropos
    "hf" 'describe-function
    "hk" 'describe-key
    "hi" 'info
    "hp" 'describe-package
    "hs" 'describe-symbol
    "hv" 'describe-variable

    ;; language-server-protocol
    "l" '(:ignore t :which-key "LSP")
    "ld" 'lsp-find-definition
    "lf" 'lsp-format-buffer
    "li" 'lsp-organize-imports
    "ln" 'lsp-rename
    "lr" 'lsp-find-references
    "ls" 'lsp-describe-session
    "lt" 'consult-imenu

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
    "os"   'snow/rg-org

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
    ":"  'execute-extended-command
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
    "gg"  'consult-org-heading
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

(use-package hydra)

(defhydra hydra-scale-window (:timeout 4)
  "scale window"
  ("l" enlarge-window-horizontally "h+")
  ("h" shrink-window-horizontally "h-")
  ("k" enlarge-window "v+")
  ("j" shrink-window "v-")
  ("q" nil "finished" :exit t))

(defhydra hydra-scale-font (:timeout 4)
  "scale text"
  ("j" text-scale-increase "+")
  ("k" text-scale-decrease "-")
  ("q" nil "finished" :exit t))

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

(defun snow/erc ()
  "Join ERC with default settings."
  (interactive)
  (erc-tls
   :server "irc.libera.chat"
   :port "6697"
   :nick "snowiow"))

(use-package cider)
(use-package clojure-mode)

(use-package dart-mode
  :hook
  (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :custom
  (flutter-sdk-path "~/flutter/"))

(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))

(use-package lsp-dart
  :after lsp
  :hook
  (dart-mode . lsp))

(use-package dockerfile-mode)

(use-package package-lint)

(use-package go-mode)

(use-package go-tag)

(use-package gotests
  :load-path "~/.emacs.d/packages/GoTests-Emacs")

(use-package json-mode
  :config
  (add-hook 'json-mode-hook (function (lambda ()
                                        (setq evil-shift-width 2)))))

(use-package jsonnet-mode)

(use-package ledger-mode)

(use-package markdown-mode
  :after (flyspell-mode auto-fill-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . auto-fill-mode))

(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(use-package python-mode)

(use-package terraform-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode))

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package yaml-mode
  :after highlight-indent-guides-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook (function (lambda ()
                                        (setq evil-shift-width 2))))
  :hook
  (yaml-mode . highlight-indent-guides-mode))

;; (use-package vertico
;;   :init
;;   (vertico-mode)
;;   :custom
;;   (vertico-cycle t)
;;   :bind
;;   (:map vertico-map
;;         ("C-j" . vertico-next)
;;         ("C-k" . vertico-previous)
;;         ("C-^" . vertico-first)
;;         ("C-$" . vertico-last)
;;         ("C-e" . vertico-exit-input)))

(use-package icomplete
  :ensure nil
  :init
  (icomplete-vertical-mode t)
  :bind (:map icomplete-vertical-mode-minibuffer-map
              ("<return>" . 'icomplete-force-complete-and-exit))
  :config
  (define-key minibuffer-local-completion-map " " 'self-insert-command)
  (setq icomplete-show-matches-on-no-input t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult)

(use-package embark
  :bind
  (("C-a" . embark-act)       
   ("C-e" . embark-dwim)       
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(defun snow/dired-open-locally ()
  "Make a local file copy of the remote file under the cursor in dired and
                               opens it.  Mainly used to open pdfs or other complex formats From remote machines"
  (interactive)
  (let* ((filename (dired-get-filename nil t))
         (local-tmp-file (file-local-copy filename)))
    (find-file local-tmp-file)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "L" 'dired-display-file
    "M" 'snow/dired-open-locally))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save new-line mode-enabled)))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-mode
  :commands lsp
  :hook
  (go-mode . lsp)
  (python-mode . lsp)
  (javascript-mode . lsp)
  ;; (terraform-mode . lsp) ;; currently not working properly
  (typescript-mode . lsp)
  :init
  (setq lsp-headerline-breadcrumb-enable t)
  :config
  (setq lsp-file-watch-threshold 5000))

;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs)

(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun snow/project-try-local (dir)
  "Determine if DIR is a non-Git project.
       DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(add-hook 'project-find-functions 'snow/project-try-local)

(defun snow/project-to-tab-name (path)
  "Extract the last directory name from PATH to set it as the tab name."
  (file-name-nondirectory (directory-file-name path)))

(defun snow/project-switch-project (orig-fun &rest args)
  "Rename current tab to the selected project."
  (let* ((project-dir (or (car args) (project-prompt-project-dir)))
         (tab-name (snow/project-to-tab-name project-dir)))
    (tab-bar-rename-tab tab-name)
    (funcall orig-fun project-dir)))

(advice-add 'project-switch-project :around #'snow/project-switch-project)

(defun snow/eshell-prompt ()
  (let (
        (current-branch (magit-get-current-branch))
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
     (propertize "$ " 'face `(:foreground "white")))))

(defun snow/eshell-config ()
  (eshell-hist-initialize)
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  (define-key eshell-mode-map (kbd "<up>") 'eshell-previous-input)
  (define-key eshell-mode-map (kbd "<down>") 'eshell-next-input)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history))

(use-package eshell
  :hook
  (eshell-first-time-mode . snow/eshell-config)
  (eshell-pre-command . eshell-save-some-history)
  :custom
  (eshell-prompt-function 'snow/eshell-prompt)
                                        ; needs to match the custum prompt
  (eshell-prompt-regexp "^$ "))

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

(use-package tramp)

(use-package vterm)

(use-package auth-source-pass
  :ensure nil
  :config
  (auth-source-pass-enable)
  :custom
  (auth-sources '(password-store)))

(use-package browse-at-remote)

(use-package forge)

(use-package git-link
  :custom
  (git-link-open-in-browser t))

(use-package github-review)

(use-package magit)

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

(use-package kubel
  :config
  (setq kubel-use-namespace-list 'on))

(use-package kubel-evil)

(use-package yasnippet
  :bind
  (:map yas-keymap
        ("C-y" . yas-next-field-or-maybe-expand))
  :config
  (yas-global-mode 1))

(use-package ripgrep)
(use-package rg)

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

(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (tab-bar-new-tab-choice "*dashboard*")
  (dashboard-items '((agenda . 5)
                     (projects . 5)
                     (recents  . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package gnuplot)




(use-package pass)


(use-package proced
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update t))))

(use-package aws-mode
  :load-path "~/.emacs.d/packages/awscli"
  :custom
  (aws-vault t)
  (aws-output "yaml"))

(use-package aws-evil
  :after aws-mode
  :load-path "~/.emacs.d/packages/awscli")

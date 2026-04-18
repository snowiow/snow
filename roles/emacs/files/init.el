(defun not-android ()
    (not (eq system-type 'android)))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package
  :ensure t)

(use-package request
  :if (not-android))

(global-display-line-numbers-mode)

(use-package emacs
  :custom
  (xref-search-program 'ripgrep)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (tab-width 2)
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (windmove-default-keybindings))

(use-package polymode)
(use-package aio)

(setq tags-revert-without-query 1)

(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)

(if (eq system-type 'android)
    (menu-bar-mode t)
  (menu-bar-mode -1))

(if (eq system-type 'android)
    (progn
      (tool-bar-mode t)
      (set-frame-parameter nil 'tool-bar-position 'bottom))
  (tool-bar-mode -1))

(when (eq system-type 'android)
  (setq touch-screen-display-keyboard t))

(use-package helpful
  :bind
  ("C-h f" .  helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :if (not-android)
  :hook
  (dired-mode . all-the-icons-dired-mode))

(setq tab-bar-show nil)

(use-package doom-themes
   :custom
   (doom-themes-enable-bold t)
   (doom-themes-enable-italic t)
   :config
   (load-theme 'doom-one-light t))

(use-package acid-theme
  :load-path "~/.emacs.d/packages/emacs-acid-theme"
  :config
  (load-theme 'acid t))

 (defun snow/switch-theme ()
   "switches between dark and light theme"
   (interactive)
   (if (eq (car custom-enabled-themes) 'acid)
       (progn
         (disable-theme 'acid)
         (enable-theme 'doom-one-light)
         (load "~/.emacs.d/modeline-light.el"))
     (progn
       (disable-theme 'doom-one-light)
       (enable-theme 'acid)
       (load "~/.emacs.d/modeline-dark.el"))))

(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(load "~/.emacs.d/modeline-dark.el")

(defvar snow/fixed-width-font "Iosevka Term"
  "The font to use for monospaced (fixed width) text.")

(defvar snow/variable-width-font "Iosevka Aile"
  "The font to use for variable-pitch (document) text.")

(defvar snow/font-height 120
  "The default height for all fonts")

(defvar snow/font-height 120
  "The default height for all fonts")

(if (eq system-type 'gnu/linux)
    (progn (set-face-attribute 'default nil
                               :family snow/fixed-width-font
                               :height snow/font-height)
           (set-face-attribute 'fixed-pitch nil
                               :family snow/fixed-width-font
                               :height snow/font-height)
           (set-face-attribute 'variable-pitch nil
                               :family snow/variable-width-font
                               :height snow/font-height))
  (when (not-android)
    (set-face-attribute 'default nil
                        :family snow/fixed-width-font
                        :height snow/font-height)))

(setq mac-option-modifier 'super)
(setq mac-right-option-modifier nil)
(setq mac-command-modifier 'meta)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(setq backup-directory-alist `(("." . "~/tmp")))

(setq require-final-newline t)
(setq-default fill-column 80)
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

(setq scroll-margin 5)

(global-visual-line-mode t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package corfu
  :custom
  (corfu-cycle t)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(defun snow/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'tempel-expand
                     #'cape-dabbrev))))

(defun snow/elisp-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'elisp-completion-at-point
                                     #'tempel-expand
                                     #'cape-dabbrev))))

(use-package cape
  :bind ("M-/" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'eglot-managed-mode-hook #'snow/eglot-capf)
  (add-hook 'emacs-lisp-mode-hook #'snow/elisp-capf)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list (number-sequence 2 120 2))

(show-paren-mode t)

(electric-pair-mode 1)

(use-package embrace
  :bind
  (("C-," . embrace-commander)))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character))

(use-package string-inflection)

(use-package change-inner
    :bind
    (("M-i" . change-inner)
     ("M-o" . change-outer)))

(defun snow/mark-current-line ()
    "Mark the current line."
    (interactive)
    (beginning-of-line)
    (set-mark (point))
    (end-of-line))

(global-set-key (kbd "C-c l") 'snow/mark-current-line)

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
                        ("*Completions*" :align above :select t)
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

(defvar snow/android-notes-path "/content/storage/com.android.externalstorage.documents/primary:Sync%2Fnotes")
(defvar snow/notes-path "~/Sync/notes")

;; Set org-directory early so other packages can use it
(setq org-directory (if (eq system-type 'android)
                        snow/android-notes-path
                      snow/notes-path))

(use-package org
    :demand t
    :hook
    (org-after-todo-statistics . org-summary-todo)
    (org-mode . flyspell-mode)
    :bind
    ("C-c o a" . org-agenda)
    ("C-c o c" . org-capture)
    ("C-c o s" . snow/rg-org)
    (:map org-mode-map
                ("C-c S" . snow/org-start-presentation))
    (:map flyspell-mode-map
          ("M-TAB" . nil))
    :custom
    ;; AGENDA SETTINGS
    (org-agenda-files (file-expand-wildcards (concat org-directory "/roam/pages/agenda/*.org")))
    (org-agenda-skip-deadline-if-done t)
    (org-agenda-skip-deadline-prewarning-if-scheduled t)
    (org-agenda-skip-scheduled-if-deadline-is-shown t)
    (org-agenda-skip-scheduled-if-done t)
    (org-agenda-window-setup 'current-window)
    (org-archive-location "%s_archive::datetree/* Archived Tasks")
    (org-babel-python-command "python3")
    (org-confirm-babel-evaluate nil)
    (org-default-notes-file (concat org-directory "/capture.org"))
    (org-ellipsis " ")
    (org-image-actual-width nil)
    (org-todo-keywords '((sequence "TODO(t)" "TODAY(y)" "WAITING(w)" "|" "DONE(d)")
                         (sequence "|" "CANCELLED(c)")))
    ;; Open org links in the same buffer instead of splitting
    (org-link-frame-setup '((file . find-file)))
    :config
    (set-face-attribute 'org-ellipsis nil :background 'unspecified :box nil)
    (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
    (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
    (require 'org-faces)
    (require 'ox-md)
    ;; Hide emphasis markers on formatted text
    (setq org-hide-emphasis-markers t)
    ;; Resize Org headings
    (dolist (face '((org-level-1 . 1.25)
                    (org-level-2 . 1.2)
                    (org-level-3 . 1.15)
                    (org-level-4 . 1.1)
                    (org-level-5 . 1.05)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :font snow/variable-width-font :weight 'medium :height (cdr face)))
    ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

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
   (python . t)
   (shell . t)
   (typescript . t)))

(use-package org-modern
  :if (not-android)
  :after org
  :hook (org-mode . org-modern-mode))

(defun snow/org-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'org-roam-complete-everywhere
                     #'org-roam-complete-link-at-point
                     #'tempel-expand
                     #'cape-file
                     #'cape-dabbrev))))

(use-package org-roam
  :after org
  :demand t
  :init
  (setq org-roam-v2-ack t)
  :bind
  ("C-c o r d" . org-roam-dailies-map)
  ("C-c o r f" . org-roam-node-find)
  (:map org-mode-map
              ("C-c C-n C-i" . org-roam-node-insert))
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  (org-roam-dailies-directory "journals/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("b" "book notes" plain (file (concat org-roam-directory "/templates/booknote.org"))
      :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("d" "default" plain
      "%?"
      :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("j" "journal" plain "\n** %<%H:%M>: %?"
      :target (file+head+olp "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n"
                             ("Journal"))
      :unnarrowed t
      :immediate-finish nil)))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  (add-hook 'org-mode-hook #'snow/org-capf)
  ;; Load Custom Agendas
  (let ((custom-agenda-commands (expand-file-name "custom-agenda-commands.el" org-directory)))
    (if (file-exists-p custom-agenda-commands)
        (load custom-agenda-commands)
      (message (format "%s not found." custom-agenda-commands))))
  ;; Load Capture Templates
  (let ((capture-templates (expand-file-name "capture-templates.el" org-directory)))
    (if (file-exists-p capture-templates)
        (load capture-templates)
      (message (format "%s not found." capture-templates)))))

(use-package org-ql
  :after '(org org-roam)
  :config
  ;; Load org-ql views
  (let ((org-ql-views (expand-file-name "org-ql-views.el" org-directory)))
    (if (file-exists-p org-ql-views)
        (load org-ql-views)
      (message (format "%s not found." org-ql-views)))))

(use-package visual-fill-column
    :custom
    (visual-fill-column-width 120)
    (visual-fill-column-center-text nil))

(use-package hide-mode-line)

(defun snow/org-start-presentation ()
    (interactive)
    (org-tree-slide-mode 1)
    (setq text-scale-mode-amount 2)
    (text-scale-mode 1)
    (visual-line-mode 1)
    (visual-fill-column-mode 1)
    (display-line-numbers-mode -1)
    (visual-fill-column-center-text t)
    (org-display-inline-images)
    (hide-mode-line-mode 1)
    ;; Set up keybindings for presentation mode
    (define-key org-tree-slide-mode-map (kbd "<right>") 'org-tree-slide-move-next-tree)
    (define-key org-tree-slide-mode-map (kbd "<left>") 'org-tree-slide-move-previous-tree)
    (define-key org-tree-slide-mode-map (kbd "Q") 'snow/org-end-presentation))

(defun snow/org-end-presentation ()
    (interactive)
    (text-scale-mode 0)
    (org-tree-slide-mode 0)
    (visual-line-mode 0)
    (visual-fill-column-center-text nil)
    (display-line-numbers-mode 1)
    (hide-mode-line-mode 0))

(use-package org-tree-slide
    :defer t
    :after '(org visual-fill-column)
    :commands org-tree-slide-mode)

(use-package ox-hugo
  :after ox)

(setq ispell-program-name "aspell")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package hydra)

(defhydra hydra-scale-window (:timeout 4)
  "scale window"
  ("m" enlarge-window-horizontally "h+")
  ("i" shrink-window-horizontally "h-")
  ("n" enlarge-window "v+")
  ("e" shrink-window "v-")
  ("q" nil "finished" :exit t))

(defhydra hydra-scale-font (:timeout 4)
  "scale text"
  ("n" text-scale-increase "+")
  ("e" text-scale-decrease "-")
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

(use-package cue-mode)

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

(use-package dockerfile-mode)

(use-package package-lint)

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :hook
  (go-ts-mode . eglot-ensure)
  :config
  (setq go-ts-mode-indent-offset 4))

(use-package json-ts-mode
  :ensure nil
  :bind (:map json-ts-mode-map
              ("C-c C-l f" . json-pretty-print-buffer))
  :mode "\\.json\\'"
  :hook
  (json-ts-mode . eglot-ensure))

(use-package jsonnet-mode)

(use-package ledger-mode)

(use-package markdown-ts-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "multimarkdown")
  (setq markdown-command "pandoc -f markdown -t html")
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
  :hook
  (markdown-ts-mode . flyspell-mode))

(use-package plantuml-mode
 :config
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(use-package protobuf-mode
 :config
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode)))

(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(use-package python-mode)

(use-package terraform-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode)
  (terraform-mode . eglot-ensure))

(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
        (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2)
  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure))

(defun snow/yaml-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'cape-dabbrev
                     #'cape-keyword))))

(use-package yaml-ts-mode
  :ensure nil
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode))
  :hook
  (yaml-ts-mode . snow/yaml-capf)
  (yaml-ts-mode . highlight-indent-guides-mode)
  (yaml-ts-mode . eglot-ensure))

(use-package vertico
  :init
  (vertico-mode t)
  :config
  (setq vertico-count 10
        vertico-resize nil
        vertico-cycle t)
  (define-key minibuffer-local-completion-map " " 'self-insert-command))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))
                                       (eglot (styles orderless))
                                (eglot-capf (styles orderless)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult)

(use-package embark
  :custom
  (embark-quit-after-action nil)
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-." . embark-act)       
   ("C-;" . embark-dwim)       
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
  :commands (dired dired-jump))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save new-line mode-enabled))
  :config
  (define-derived-mode cfn-json-mode js-mode
    "CFN-JSON"
    "Simple mode to edit CloudFormation template in JSON format."
    (setq js-indent-level 2))

  (add-to-list 'magic-mode-alist
               '("\\({\n *\\)? *[\"']AWSTemplateFormatVersion" . cfn-json-mode))

  ;; Set up a mode for YAML based templates if yaml-ts-mode is installed
    (define-derived-mode cfn-yaml-mode yaml-ts-mode
      "CFN-YAML"
      "Simple mode to edit CloudFormation template in YAML format.")
  
    (add-to-list 'magic-mode-alist
                 '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode))

  ;; Set up cfn-lint integration if flycheck is installed
  ;; Get flycheck here https://www.flycheck.org/
  (when (featurep 'flycheck)
    (flycheck-define-checker cfn-lint
      "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

      :command ("cfn-lint" "-f" "parseable" source)
      :error-patterns ((warning line-start (file-name) ":" line ":" column
                                ":" (one-or-more digit) ":" (one-or-more digit) ":"
                                (id "W" (one-or-more digit)) ":" (message) line-end)
                       (error line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "E" (one-or-more digit)) ":" (message) line-end))
      :modes (cfn-json-mode cfn-yaml-mode))

    (add-to-list 'flycheck-checkers 'cfn-lint)
    (add-hook 'cfn-json-mode-hook 'flycheck-mode)
    (add-hook 'cfn-yaml-mode-hook 'flycheck-mode)))

(use-package eglot
 :ensure nil
 :config
 (add-to-list 'eglot-server-programs
              '(terraform-mode . ("terraform-ls" "serve"))))

(use-package project
  :ensure nil
  :bind (:map project-prefix-map
              ("R" . 'snow/rg-project)
              ("m" . 'magit-status))
  :config
  (when (eq system-type 'android)
    (project-remember-projects-under snow/android-notes-path)))

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
          (current-branch (when (fboundp 'magit-get-current-branch) (magit-get-current-branch)))
          (aws-vault (getenv "AWS_VAULT"))
          (k8s-context (shell-command-to-string "kubectl config current-context")))
      (concat
       "\n"
       (propertize (user-login-name) 'face 'font-lock-type-face)
       (propertize "@" 'face 'default)
       (propertize (system-name) 'face 'font-lock-function-name-face)
       (when current-branch
         (propertize (concat "  " current-branch) 'face 'font-lock-keyword-face))
       (when (boundp 'kubel-context)
         (propertize (concat " k8s: " k8s-context) 'face 'font-lock-warning-face))
       (when aws-vault
         (propertize (concat "  " aws-vault) 'face 'font-lock-string-face))
       "\n"
       (propertize (eshell/pwd) 'face 'font-lock-constant-face)
       "\n"
       (propertize "$ " 'face 'eshell-prompt))))

  (defun snow/eshell-config ()
    (eshell-hist-initialize)
    (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
    (define-key eshell-mode-map (kbd "<up>") 'eshell-previous-input)
    (define-key eshell-mode-map (kbd "<down>") 'eshell-next-input)
    (define-key eshell-mode-map (kbd "C-r") 'consult-history))

  (use-package eshell
    :hook
    (eshell-first-time-mode . snow/eshell-config)
    (eshell-pre-command . eshell-save-some-history)
    :custom
    (eshell-prompt-function 'snow/eshell-prompt)
    (eshell-prompt-regexp "^$ "))

  (use-package esh-autosuggest
    :hook (eshell-mode . esh-autosuggest-mode)
    :custom
    (esh-autosuggest-delay 0.5))

  (use-package eshell-syntax-highlighting
    :if (not-android)
    :after esh-mode
    :custom  
    (eshell-syntax-highlighting-global-mode +1))

(defun snow/eshell-new-frame ()
    "Open a new frame with a new vterm session."
    (interactive)
    (let ((new-frame (make-frame)))
      (select-frame new-frame)
      (eshell (generate-new-buffer-name "*eshell*"))))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-default-user "snow")
  (tramp-default-host "cloudpi"))

(use-package vterm
  :if (not-android))

(defun snow/vterm-new-frame ()
  "Open a new frame with a new vterm session."
  (interactive)
  (let ((new-frame (make-frame)))
    (select-frame new-frame)
    (vterm (generate-new-buffer-name "*vterm*"))))

(use-package eat
  :load-path "~/.emacs.d/packages/emacs-eat"
  :hook
  (eshell-mode . eat-eshell-mode))

(use-package auth-source-pass
  :if (not-android)
  :ensure nil
  :config
  (auth-source-pass-enable)
  :custom
  (auth-sources '(password-store)))

(use-package browse-at-remote
  :bind
  ("C-c g w" . browse-at-remote))

(use-package forge
  :if (not-android))

(use-package git-link
  :if (not-android)
  :custom
  (git-link-open-in-browser t))

(use-package github-review
  :if (not-android))

(use-package emacsql)
(use-package magit
  :init
  (setq magit-show-long-lines-warning nil)
  :bind
  ("C-c g g" . magit-status)
  ("C-c g c" . magit-clone)
  ("C-c g b" . magit-blame))

;; Keybinding for commit message buffers
(with-eval-after-load 'git-commit
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c b") 'snow/branch-name-to-commit-msg))))

(use-package mu4e
  :if (not-android)
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.12.9"
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
  :bind
  (:map kubel-mode-map
        ("N" . kubel-set-namespace))
  :config
  (setq kubel-use-namespace-list 'on))

(use-package tempel
:bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
       ("M-*" . tempel-insert))

:init

;; Setup completion at point
(defun tempel-setup-capf ()
(setq-local completion-at-point-functions
            (cons #'tempel-expand completion-at-point-functions))))

(use-package ripgrep
  :if (not-android))

(use-package rg
  :if (not-android)
  :config
  (rg-define-search snow/rg-org
    :query ask
    :format regexp
    :files "*.org"
    :case-fold-search smart
    :dir org-directory
    :confirm prefix)
  (rg-define-search snow/rg-project
    :query ask
    :format regexp
    :files ""
    :case-fold-search smart
    :dir (if (project-current) (project-root (project-current))
           default-directory)
    :confirm prefix
    :flags ("--hidden -g !.git")))

(use-package agent-shell
    :ensure t
    :ensure-system-package
    ;; Add agent installation configs here
    ((claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp")))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c a" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package xdg-launcher
      :load-path "~/.emacs.d/packages/xdg-launcher")

(defun snow/app-launcher ()
  "Start xdg-launcher in a floating Emacs frame for app launching.
Frame is created by emacsclient -c for proper Wayland focus."
  (interactive)
  (switch-to-buffer (get-buffer-create " *app-launcher*"))
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local cursor-type nil)
  (let ((window-min-height 1)
        (vertico-count 10)
        (vertico-resize nil))
    (unwind-protect
        (xdg-launcher-run-app)
      (delete-frame))))

(defun snow/setup-wayland-clipboard (&optional frame)
  (when (and (display-graphic-p frame)
             (getenv "WAYLAND_DISPLAY"))
    (setq interprogram-cut-function
          (lambda (text)
            (start-process "wl-copy" nil "wl-copy" "--" text)))
    (setq interprogram-paste-function
          (lambda ()
            (let ((clipboard (string-trim (shell-command-to-string "wl-paste -n 2>/dev/null"))))
              (unless (string-empty-p clipboard)
                clipboard))))))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'snow/setup-wayland-clipboard)
  (snow/setup-wayland-clipboard))

(use-package openwith
  :if (not-android)
  :config
  (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler) ;; needed to not randomly open the attachment when trying to send it
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("pdf"))
               "open"
               '(file))))
  (openwith-mode t))

(defun snow/yank-markdown-as-org ()
  "Yank Markdown text as Org.

This command will convert Markdown text in the top of the `kill-ring'
and convert it to Org using the pandoc utility."
  (interactive)
  (save-excursion
    (with-temp-buffer
      (yank)
      (shell-command-on-region
       (point-min) (point-max)
       "pandoc -f markdown -t org --wrap=preserve" t t)
      (kill-region (point-min) (point-max)))
    (yank)))

(defun snow/dashboard-filter-agenda-today-or-earlier ()
    "Exclude agenda items scheduled after today.
Return nil to include the entry, return point to exclude it."
    (let ((scheduled-time (org-get-scheduled-time (point)))
          (end-of-today (org-time-string-to-time
                         (format-time-string "%Y-%m-%d 23:59:59" (current-time)))))
      ;; Exclude (return point) if scheduled time is after today
      (when (and scheduled-time
                 (time-less-p end-of-today scheduled-time))
        (point))))

  (use-package dashboard
    :if (not-android)
    :after org
    :custom
    (dashboard-startup-banner (expand-file-name "~/workspace/snow/img/banner.png"))
    (tab-bar-new-tab-choice "*dashboard*")
    (dashboard-projects-backend 'project-el)
    (dashboard-week-agenda nil)
    (dashboard-match-agenda-entry "+SCHEDULED<=\"<today>\"")
    (dashboard-items '((agenda . 5)
                       (projects . 5)
                       (recents  . 5)))
    :config
    (dashboard-setup-startup-hook))

  (use-package gnuplot
    :if (not-android))

  (use-package pass
    :if (not-android))

  (use-package proced
    :config
    (add-hook 'proced-mode-hook
              (lambda ()
                (proced-toggle-auto-update t))))

(use-package aws
  :load-path "~/.emacs.d/packages/aws.el"
  :commands (aws aws-login) 
  :custom
  (aws-login-method 'sso)
  (aws-output "yaml")
  (aws-organizations-account "Moia-Master:pe-infra-engineer-m"))

(use-package aws-evil
  :after aws
  :load-path "~/.emacs.d/packages/aws.el")

(defun snow/branch-name-to-commit-msg ()
 (interactive)
 (let* ((branch (magit-get-current-branch))
       (commit-msg (replace-regexp-in-string "MOIA \\([0-9]+\\) " "MOIA-\\1: "
           (string-replace "-" " "
           (string-replace "moia" "MOIA" branch)))))
    (insert commit-msg)))

(use-package org-roam-readwise
  :if (not-android)
  :load-path "~/.emacs.d/packages/org-roam-readwise"
  :after org-roam
  :custom
  (org-roam-readwise-output-location (concat org-roam-directory "/pages/readwise"))
  (org-readwise-sync-highlights t)
  (org-readwise-sync-reader nil)
  (org-readwise-auth-source 'auth-source-pass)
  (org-readwise-debug-level 2)
  (readwise-debug-level 2))

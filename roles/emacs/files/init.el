(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
; =============================> BuiltIns
; Increase/Decrease Text size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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
                    :height 110
                    :weight 'normal
                    :width 'normal)

; Mac OSX Settings make command meta
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
; Always add a final newline at the end of the file
(setq require-final-newline t)
; ============================> BuiltIn Packages
; Auto Fill Mode
(setq-default fill-column 80)

; Dired
(require 'dired)

; ERC
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
(setq erc-prompt-for-password nil)

; Org Mode
(require 'org)
(setq org-image-actual-width nil)
(setq org-directory "~/Seafile/My Library/notes")
(setq org-agenda-files
	(file-expand-wildcards (concat org-directory "/*.org")))
(setq org-default-notes-file (concat org-directory "/capture.org"))


; Whitspace Column
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)

; =============================> Packages
(setq use-package-always-ensure t)

(use-package company
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  :hook (after-init . global-company-mode))

(use-package counsel
  :after evil-leader
  :init
  (setq ivy-use-virtual-buffers t)
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
  (evil-leader/set-key
    "b"   'ivy-switch-buffer
    "hv"  'counsel-describe-variable
    "hf"  'counsel-describe-function
    "hf"  'counsel-describe-function
    "hia" 'info-apropos
    "hii" 'info
    ":"   'counsel-M-x
    ))

(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 5)
			  (recents  . 5)
			  (bookmarks . 5)
			  (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t    
	doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one-light t)
  (load-theme 'doom-vibrant t)
  )

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
  (evil-collection-init 'term))

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
  (evil-leader/set-key "ww"
    (lambda ()
      (interactive)
      (find-file "~/Seafile/My Library/notes/main.org")))
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
    "SPC" 'org-ctrl-c-ctrl-c))

(use-package evil-magit
  :after (evil-leader magit)
  :config
  (evil-leader/set-key
    "gg" 'magit))

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
  :config
  (global-evil-surround-mode 1))

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package linum-relative
  :config
  (linum-relative-global-mode))

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

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package ripgrep)

(use-package projectile
  :after evil-leader
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (evil-leader/set-key
    "o" 'projectile-find-file
    "7" 'projectile-ripgrep
    "po" 'projectile-switch-project
    "pt" 'projectile-run-term))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit markdown-mode ripgrep yasnippet use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

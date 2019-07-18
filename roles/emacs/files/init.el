(package-initialize)
(require 'package)
(require 'dired)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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

; ============================> BuiltIn Packages
; Auto Fill Mode
(setq-default fill-column 80)

; ERC
(defun my-erc ()
    "Join ERC with default settings"
    (interactive)
    (erc :server "irc.freenode.net" :port "6667" :nick "snowiow"))

(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
(setq erc-prompt-for-password nil)

; Org Mode
(require 'org)
(setq org-image-actual-width nil)
(setq org-directory "~/Seafile/My Library/notes")
(setq org-agenda-files
	(file-expand-wildcards (concat org-directory "/*.org")))
(setq org-default-notes-file (concat org-directory "/capture.org"))

; Term Mode
(defun my-term ()
  "Opens term without asking which shell to run"
  (interactive)
  (projectile-run-term "zsh"))

; Whitspace Column
(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)

; =============================> Packages
(use-package company
  :ensure t
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
  :ensure t
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
  :ensure t
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((projects . 5)
			  (recents  . 5)
			  (bookmarks . 5)
			  (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t    
	doom-themes-enable-italic t)
  :config
  (load-theme 'doom-vibrant t)
  )

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode)
  (evil-define-key 'visual emacs-lisp-mode-map
    "e" 'eval-region))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init 'term))

(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-leader
  :after evil
  :ensure t
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
  :ensure t
  :config
  (evil-leader/set-key
    "gg" 'magit
  ))

(use-package evil-org
  :after evil
  :ensure t
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
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package linum-relative
  :ensure t
  :config
  (linum-relative-global-mode))

(use-package magit
  :ensure t
  )

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . auto-fill-mode)
  (markdown-mode . whitespace-mode))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package ripgrep
  :ensure t)

(use-package projectile
  :after evil-leader
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (evil-leader/set-key
    "o" 'projectile-find-file
    "7" 'projectile-ripgrep
    "pt" 'projectile-run-term))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(load "~/.emacs.d/modeline.el")
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
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)

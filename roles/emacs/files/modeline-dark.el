(setq-default mode-line-format
              (list
               '(:eval (meow-indicator))
               '(:eval (when evil-mode-line-tag
                         (propertize
                          (concat " " (substring evil-mode-line-tag 2 3) "  ")
                          'face 'font-lock-keyword-face)))
               '(:eval (when vc-mode
		                 (propertize (concat " " (substring vc-mode 5))
			                         'face 'font-lock-negation-char-face)))
               '(:eval (ignore-errors
                         (propertize
                          (concat " " (all-the-icons-icon-for-buffer)))))
               (propertize " %b " 'face 'font-lock-negation-char-face)
               '(:eval (when (buffer-modified-p)
		                 (propertize "+" 'face 'font-lock-variable-name-face)))
               " ("
               (propertize "%02l" 'face 'font-lock-constant-face)
               ","
               (propertize "%02c" 'face 'font-lock-constant-face)
               ") ["
               (propertize "%p" 'face 'font-lock-constant-face)
               "/"
               (propertize "%I" 'face 'font-lock-constant-face)
               "]"
               " " 'display
               '(:eval (propertize
		                " " 'display
		                `((space :align-to (- (+ right right-fringe right-margin)
				                              ,(+ 5
                                                  (string-width (format-mode-line mode-name))
                                                  (string-width (
                                                                 concat
                                                                 "["
                                                                 (number-to-string (tab-bar--current-tab-index))
                                                                 ": "
                                                                 (alist-get 'name (tab-bar--current-tab))
                                                                 "]"))))))))
               (propertize " %m " 'face 'font-lock-string-face)
               "["
               '(:eval (propertize (number-to-string (tab-bar--current-tab-index))
                                   'face 'font-lock-constant-face))
               ": "
               '(:eval (propertize (alist-get 'name (tab-bar--current-tab))
                                   'face 'font-lock-constant-face))
               "]"))

(set-face-attribute 'mode-line nil
		            :background "#1e262e"
		            :foreground "#e0e0e0"
		            :box '(:line-width 8 :color "#1e262e")
		            :overline nil
		            :underline nil)

(set-face-attribute 'mode-line-inactive nil
		            :background "#161e26"
		            :foreground "#606870"
		            :box '(:line-width 8 :color "#161e26")
		            :overline nil
		            :underline nil)

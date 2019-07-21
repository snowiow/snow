(setq-default mode-line-format
    (list
    '(:eval (propertize
		(concat " " (substring evil-mode-line-tag 2 3) "  ")
			'face 'font-lock-keyword-face))
    '(:eval (when (not (string= "" vc-mode))
		(propertize (concat " " (substring vc-mode 5))
			    'face 'font-lock-negation-char-face)))
    (propertize " %b " 'face 'font-lock-defaults)
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
	    ;; (propertize
    " " 'display
    '(:eval (propertize
		" " 'display
		`((space :align-to (- (+ right right-fringe right-margin)
				    ,(+ 3 (string-width mode-name)))))))
    (propertize " %m " 'face 'font-lock-string-face)))

(set-face-attribute 'mode-line nil
		    :background "#21252B"
		    :foreground "white"
		    :box '(:line-width 8 :color "#21252B")
		    :overline nil
		    :underline nil)

(set-face-attribute 'mode-line-inactive nil
		    :background "#21252B"
		    :foreground "white"
		    :box '(:line-width 8 :color "#21252B")
		    :overline nil
		    :underline nil)

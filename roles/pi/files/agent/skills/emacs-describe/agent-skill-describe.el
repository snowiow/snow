(require 'cl-lib)

(cl-defun agent-skill-describe (&key query)
  "Look up QUERY across multiple Emacs documentation mechanisms.

Searches for QUERY as a function, variable, and via apropos,
returning all findings in a single string."
  (let ((sym (intern-soft query))
        (sections nil))
    ;; Function documentation
    (when (and sym (fboundp sym))
      (push (format "## Function: %s\n\n%s\n\nSignature: %s\n\n%s"
                    query
                    (cond ((subrp (symbol-function sym)) "Built-in function")
                          ((macrop sym) "Macro")
                          ((commandp sym) "Interactive command")
                          (t "Function"))
                    (or (help-function-arglist sym t) "()")
                    (or (documentation sym t) "No documentation available."))
            sections))
    ;; Variable documentation
    (when (and sym (boundp sym))
      (let ((val (symbol-value sym)))
        (push (format "## Variable: %s\n\nCurrent value: %s\n\n%s"
                      query
                      (let ((printed (format "%S" val)))
                        (if (> (length printed) 200)
                            (concat (substring printed 0 200) "...")
                            printed))
                      (or (documentation-property sym 'variable-documentation t)
                          "No documentation available."))
              sections)))
    ;; Face documentation
    (when (and sym (facep sym))
      (push (format "## Face: %s\n\n%s"
                    query
                    (or (documentation-property sym 'face-documentation t)
                        "No documentation available."))
            sections))
    ;; Key binding (if it looks like a key sequence)
    (when (string-match-p "\\`[CMSs]-\\|\\`<" query)
      (let* ((keyseq (ignore-errors (kbd query)))
             (binding (and keyseq (key-binding keyseq))))
        (when binding
          (push (format "## Key binding: %s\n\n%s runs `%s'\n\n%s"
                        query query binding
                        (or (documentation binding t)
                            "No documentation available."))
                sections))))
    ;; Apropos matches (up to 20)
    (let ((matches nil))
      (mapatoms
       (lambda (s)
         (when (and (string-match-p (regexp-quote query) (symbol-name s))
                    (not (eq s sym))
                    (or (fboundp s) (boundp s)))
           (push (format "  %s%s"
                         (symbol-name s)
                         (cond ((and (fboundp s) (boundp s))
                                " [function, variable]")
                               ((fboundp s) " [function]")
                               (t " [variable]")))
                 matches))))
      (when matches
        (let ((sorted (sort matches #'string<)))
          (push (format "## Related symbols\n\n%s%s"
                        (mapconcat #'identity (seq-take sorted 20) "\n")
                        (if (> (length sorted) 20)
                            (format "\n  ... and %d more" (- (length sorted) 20))
                          ""))
                sections))))
    (if sections
        (mapconcat #'identity (nreverse sections) "\n\n")
      (format "No documentation found for \"%s\"." query))))

(provide 'agent-skill-describe)

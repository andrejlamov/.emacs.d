(defun al-backward-kill-dwim ()
  "Kill region or backward-kill-word."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'backward-kill-word)))


(defun al-kill-word-dwim ()
  "Kill region or word."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-word)))


(defun al-eval-sexp-dwim ()
  "Eval region or sexp within matchin parens indicated by
paren-mode: either forward-sexp or last-sexp."
  (interactive)
  (let* ((string-after (ignore-errors (char-to-string (char-after)))))
    (if (string= string-after "(")
	(save-excursion ;;asdfasdf
	  (call-interactively 'forward-sexp)
	  (call-interactively 'eval-last-sexp))
      (call-interactively 'eval-last-sexp))))



(provide 'al-editor)



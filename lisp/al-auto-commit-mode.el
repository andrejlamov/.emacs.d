;; Add this to dir locals to activate auto commit in  a folder
;; ((nil . ((eval al-auto-commit-mode 1))))

(setq al-auto-commit-output-buffer "*al-auto-commit*")

(defun al-auto-commit-on-save ()
  (async-shell-command
   "git status --verbose && git add --all && git commit -m 'auto-commit-mode' --allow-empty && git pull && git push"
   al-auto-commit-output-buffer
   al-auto-commit-output-buffer))

;;;###autoload
(define-minor-mode al-auto-commit-mode
  nil
  :lighter " al-acm "
  (add-hook 'after-revert-hook 'al-auto-commit-on-save nil t)
  (add-hook 'after-save-hook 'al-auto-commit-on-save nil t))

(provide 'al-auto-commit-mode)


;; TODO: do stuff in other  window?
(require 's)
(require 'consult)

;;;###autoload
(defun al-shell-named (name)
  (interactive "sName: ")
  (shell (s-concat "*" name "*")))

;;;###autoload
(defun al-shell-some-cd-to-this-dir ()
  (interactive)
  (let ((cwd default-directory))
    (consult-buffer '(al-shell-consult-source))
    (end-of-buffer)
    (insert (s-concat "cd " cwd))
    (comint-send-input)))

;;;###autoload
(defvar al-shell-consult-source
  (list :name     "Shells"
        :category 'buffer
        :narrow   ?s
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :new
        (lambda (name)
          (with-current-buffer (get-buffer-create name)
            (shell (s-concat "*" name "*"))
            (consult--buffer-action (current-buffer))))
        :items
        (lambda ()
          (mapcar #'buffer-name
                  (seq-filter
                   (lambda (x)
                     (eq (buffer-local-value 'major-mode x) 'shell-mode))
                   (buffer-list))))))

(defun al-shell-consult-setup ()
    (add-to-list 'consult-buffer-sources 'al-shell-consult-source 'append))

(provide 'al-shell)

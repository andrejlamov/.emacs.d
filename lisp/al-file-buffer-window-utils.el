;;;###autoload
(defun al-switch-to-buffer-to-the-right (name &optional rest)
  "Open the buffer in a new window to the right"
  (interactive
   (list (read-buffer-to-switch "Siwtch to buffer to the right: ")))
  (split-window-right)
  (other-window 1)
  (switch-to-buffer name))

;;;###autoload
(defun al-find-file-to-the-right (path &optional rest)
  "Open then file in a new window to the right"
  (interactive
   (find-file-read-args "Find file to the right: "
			(confirm-nonexistent-file-or-buffer)))
  (split-window-right)
  (other-window 1)
  (switch-to-buffer (find-file path)))

;;;###autoload
(defun al-open-init-file ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))

;;;###autoload
(defun al-set-frame-font-height (height)
  (interactive "nHeight (in pt): ")
  (set-face-attribute 'default nil :height (* 10 height)))

;;;###autoload
(defun al-revert-buffer ()
  "Just revert, ok?"
  (interactive)
  (revert-buffer t t))

;;;###autoload
(defun al-rename-file-and-buffer (filename)
  "Rename given filename. Copied and modified from crux."
  (interactive "f")
  (if (not (and filename (file-exists-p filename)))
      (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
    (let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
           (containing-dir (file-name-directory new-name)))
      (make-directory containing-dir t)
      (cond
       ((vc-backend filename) (vc-rename-file filename new-name))
       (t
        (rename-file filename new-name t)
        (set-visited-file-name new-name t t))))))

(provide 'al-file-buffer-window-utils)

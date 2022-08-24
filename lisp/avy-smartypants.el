(require 'avy)
(require 'hydra)
(require 'cl-lib)
(require 'smartparens)

(setq avy-sp-open-rx (rx (or "(" "[" "{")))
(setq avy-sp-close-rx (rx (or ")" "]" "}")))

(defun avy-sp-noop () (interactive))

(cl-defun avy-sp-walk-candidates
    (&key
     (pre-op 'avy-sp-noop)
     (post-op 'avy-sp-noop)
     op
     (beg (window-start))
     (end (window-end)))
  (let ((candidates nil)
	(prev-point (point)))
    (save-excursion
      (funcall pre-op)
      (setq prev-point (point))
      (funcall op)
      (while (and (>= (point) beg) (<= (point) end) (not (= prev-point (point))))
	(push `((,(point) . ,(point)) . ,(selected-window)) candidates)
	(setq prev-point (point))
	(funcall op))
      (funcall post-op)
      (seq-reverse candidates))))

(defmacro avy-sp-walk-defun (name &rest candidate-fn-args)
  (let ((fn-name (intern (concat "avy-sp-" (symbol-name name)))))
    `(defun ,fn-name ()
       (interactive)
       (avy-with ,fn-name
                 (avy-process (avy-sp-walk-candidates ,@candidate-fn-args))))))

(defmacro avy-sp-re-defun (name &rest candidate-fn-args)
  (let ((fn-name (intern (concat "avy-sp-" (symbol-name name)))))
    `(defun ,fn-name ()
       (interactive)
       (avy-with ,fn-name
	 (avy-process (avy--regex-candidates ,@candidate-fn-args))))))


(avy-sp-walk-defun forward-sexp :op 'sp-forward-sexp)
(avy-sp-walk-defun backward-sexp :op 'sp-backward-sexp)
(avy-sp-walk-defun down-sexp :op 'sp-down-sexp)
(avy-sp-walk-defun backward-down-sexp :pre-op 'sp-backward-down-sexp :op 'sp-backward-sexp)
(avy-sp-walk-defun beg-of-sexp :op 'sp-beginning-of-sexp)
(avy-sp-walk-defun end-of-sexp :op 'sp-end-of-sexp)
(avy-sp-walk-defun up-sexp :op 'sp-up-sexp)
(avy-sp-walk-defun backward-up-sexp :pre-op 'sp-backward-up-sexp :op 'sp-forward-sexp)
(avy-sp-walk-defun forward-symbol :op 'sp-forward-symbol)
(avy-sp-walk-defun backward-symbol :op 'sp-backward-symbol)

(avy-sp-re-defun next-close avy-sp-close-rx (point) (window-end))
(avy-sp-re-defun next-open avy-sp-open-rx (point) (window-end))
(avy-sp-re-defun prev-close avy-sp-close-rx (window-start) (point))
(avy-sp-re-defun prev-open avy-sp-open-rx (window-start) (point))



(defhydra avy-smartypants-hydra  (:color pink)
  "Example smartypants hydra."

  ;; Bypass these but no others!
  ("q" avy-sp-noop)
  ("y" avy-sp-noop)
  ("(" avy-sp-noop)
  (")" avy-sp-noop)
  ("o" avy-sp-noop)
  ("p" avy-sp-noop)
  ("g" avy-sp-noop)
  ("j" avy-sp-noop)
  ("z" avy-sp-noop)
  ("x" avy-sp-noop)
  ("v" avy-sp-noop)
  ("n" avy-sp-noop)
  ("m" avy-sp-noop)

  
  ("]" avy-sp-next-close "next close" :column "simple")
  ("[" avy-sp-prev-open "prev open")
  ("}" avy-sp-prev-close "prev close") 
  ("{" avy-sp-next-open "next open")
  ("'" avy-goto-char-timer "char timer")
  ("\"" avy-pop-mark "pop")
  
  ("f" sp-forward-sexp "forward" :column "smartypants")
  ("b" sp-backward-sexp "back")
  ("d" sp-down-sexp "down")
  ("a" sp-backward-down-sexp "backward down")

  ("D" sp-beg-of-sexp "beg")
  ("A" sp-end-of-sexp "end")

  ("e" sp-up-sexp "up")
  ("u" sp-backward-up-sexp "backward up")

  ("F" sp-forward-symbol "forward symbol")
  ("B" sp-backward-symbol "backward symbol")

  ("h" sp-backward-slurp-sexp "bw slurp" :column "slurp & barf")
  ("H" sp-backward-barf-sexp "bw bar")
  ("l" sp-forward-slurp-sexp "slurp")
  ("L" sp-forward-barf-sexp "barf")

  ("t" sp-transpose-sexp "transposea" :column "edit")
  ("k" sp-kill-sexp "kill")
  ("w" sp-copy-sexp "copy")
  ("r" sp-raise-sexp "raise")
  ("s" sp-splice-sexp "splice" )
  ("M-s" sp-split-sexp "split" )
  ("c" sp-convolute-sexp "convolute")
  ("C-w" sp-backward-kill-sexp "backward kill")
  ("M-a" sp-absorb-sexp "absorb")
  ("M-e" sp-emit-sexp "emit")
  ("M-u" sp-unwrap-sexp "unwrap")
  
  ("C-g" nil :column "quit")
  ("i" nil))

(provide 'avy-smartypants)

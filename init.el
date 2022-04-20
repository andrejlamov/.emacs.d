;; Add ~/.emacs.d/lisp to load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; TODO: configure global whitespace mode to *lightly* *only* show trailing whitespace and if whitespace/tabs

;; Boostrap straight package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-lnast-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use use-package as our main package loader.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-compute-statistics t)


;; Auto save when idle
'(auto-save-visited-mode +1)
'(setq auto-save-visited-interval 0)

;; Switch betwen translit, swedish postfix and system default
(progn
  (defun al-input-swedish ()
    (interactive)
    (set-input-method "swedish-postfix"))

  (defun al-input-translit ()
    (interactive)
    (set-input-method "cyrillic-translit"))

  (defun al-input-default ()
    (interactive)
    (set-input-method nil))

  (bind-key* "M-SPC a l s" 'al-input-swedish)
  (bind-key* "M-SPC a l r" 'al-input-translit)
  (bind-key* "M-SPC a l e" 'al-input-default))



;; Fix the base look no matter what theme.  
(defun al-setup-look ()
  "Disable bars, scrolls and fringes. Setup font."
  (interactive)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (fringe-mode 0)
  (setq-default cursor-type 'bar)
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (blink-cursor-mode 0)

  (global-hl-line-mode 1)
  (setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
  (set-frame-font "-*-IBM Plex Mono-normal-italic-normal-*-14-*-*-*-m-0-iso10646-1"))


;; Treat words as sub-words.
(global-subword-mode +1)


;; Auto revert buffers
(global-auto-revert-mode +1)


;; Let tab first indent and then complete.
(setq tab-always-indent 'complete)


(define-key input-decode-map 
    (kbd "C-[") 
    [control-bracketleft])
(bind-key* [control-bracketleft] 'save-buffer)

;; Short prompts
(fset 'yes-or-no-p 'y-or-n-p)


;; Do not prompt about killing processes.
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))


;; Just follow links wihout asking.
(setq vc-follow-symlinks nil)


;; Use isearch regexp by default.
(bind-key* "C-s" 'isearch-forward-regexp)
(bind-key* "C-r" 'isearch-backward-regexp)


;; Truncate lines toggle.
(bind-key* "M-SPC a t" 'toggle-truncate-lines)

(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; The light modus theme.
(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-region '(bg-only no-extend))
  (modus-themes-load-themes)
  :config
  (al-setup-look)
  (modus-themes-load-operandi))


;; De facto string library.
(use-package s :defer t)


;; Handle backups and autosaves created by emacs.
(use-package no-littering
  :config
  ;; put autosaves in .emacs.d/var/auto-save
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; put customizations made from the ui to .emacs.d/custom.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))


;; Let gui emacs construct the exec-path same way as shell.

(use-package exec-path-from-shell
    :when (memq window-system '(mac ns x))
    :config (exec-path-from-shell-initialize))


;; Log what keys I use to detect what can be further optimized.
(use-package keyfreq
    :config
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)
    (setq keyfreq-excluded-commands '(self-insert-command)))


;; Collection of nice functions.
(use-package crux
  :defer t
  :bind* (("M-SPC f r" . crux-rename-file-and-buffer)
	  ("M-SPC B r" . crux-rename-file-and-buffer)
	  ("M-SPC f d" . 'crux-delete-file-and-buffer)
	  ("C-k" . crux-smart-kill-line))
  :bind (("M-o" . 'crux-smart-open-line)
	 ("M-O" . 'crux-smart-open-line-above)))


;; Writeroom mode for focusing.
(use-package writeroom-mode
  :bind* (("M-SPC w w" . 'writeroom-mode))
  :config
  (setq writeroom-width 120))


;; TODO: write your own shell popper!
(use-package shell-pop
  :bind* (("M-SPC a s" . shell))
  :bind (:map shell-mode-map
	      ("C-, o" . comint-clear-buffer))
  :config
  (require 'shell)
  ;; Fix shell opening in wrong window. 
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist))


;; My file & buffer functions 
(use-package al-file-buffer-window-utils
  :defer t
  :bind* (("M-SPC w s" . split-window-right)
	  ("M-SPC w m" . delete-other-windows)
	  ("M-SPC w b" . 'balance-windows)
	  ("M-SPC w z" . al-set-frame-font-height)
	  ("M-SPC f f" . find-file)
	  ("M-SPC f s" . save-buffer)
	  ("M-SPC a i" . al-open-init-file))
  :straight nil)


;; My "editor" functions.
;; TODO: eval-dwim macro to wrap eval functions from different modes (python, clojure etc)
(use-package al-editor
  :straight nil
  :bind (:map emacs-lisp-mode-map
	      ("C-x C-e" . al-eval-sexp-dwim)
	      ("C-, e b" . eval-buffer)
	      ("C-, e e" . al-eval-sexp-dwim))
  :bind* (("C-w" . al-backward-kill-dwim)
	  ("M-d" . al-kill-word-dwim)))



;; Hydra.
;; TODO: https://gitlab.com/jjzmajic/hercules.el
;; TODO: custom generate sticky hydras from keymaps?
(use-package hydra :defer t)


;; Context aware menu that works everywhere.
(use-package embark
  :demand t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (require 'al-file-buffer-window-utils)
  
  (define-key embark-buffer-map "O" #'al-switch-to-buffer-to-the-right)
  (define-key embark-file-map "O" #'al-find-file-to-the-right)
  (define-key embark-file-map "r" #'al-rename-file-and-buffer))



;; Vertical completion UI. 
(use-package vertico
  :init
  (vertico-mode)
  :config
  (add-to-list 'load-path "~/.emacs.d/straight/repos/vertico/extensions/")

  ;; Show vertico in a real buffer.
  '(require 'vertico-buffer)
  '(vertico-buffer-mode)
  '(setq vertico-buffer-display-action '(display-buffer-in-side-window (side . bottom) (window-height . 0.3)))

  ;; Avy actions on vertico alternativs.
  (require 'vertico-quick)
  (define-key vertico-map "\M-q" #'vertico-quick-insert)
  (define-key vertico-map "\C-q" #'vertico-quick-exit)

  ;; Resume to last vertico state.
  (require 'vertico-repeat)
  (bind-key* "M-SPC r" #'vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))
  

;; Practical commands (grep, find, git grep, buffer lists etc) that output via `completing-read'.
(use-package consult
  ;; M-SPC binding
  :bind* (("M-SPC s s" . consult-line)
	  ("M-SPC s g" . consult-git-grep)
	  ("M-SPC s f" . consult-find)
	  ("M-SPC s o" . consult-outline))
  :bind (
	 ;; C-c bindings (mode-specific-map)
	 ("M-p" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c b" . consult-bookmark)
	 ("C-c k" . consult-kmacro)
         
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame

	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)

	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)	  ;; orig. yank-pop
	 ("<help> a" . consult-apropos)	  ;; orig. apropos-command

	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)

	 ;; M-s bindings (search-map)
	 ("M-s f" . consult-find)
	 ("M-s F" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)

         ("M-s e" . consult-isearch-history)
	 :map comint-mode-map
	 ("M-p" . consult-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)) 
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Enhance with consult version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (setq consult-preview-key (kbd "M-."))
  
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
   
  (setq completion-in-region-function #'consult-completion-in-region))


;; Dash.
(use-package dash :defer t)


;; Avy.
(use-package avy
  :bind* (("C-j" . avy-goto-char-timer)
	  ("C-S-j" . avy-pop-mark)
	  ("M-j" . avy-goto-line)
	  ("C-z" . avy-goto-char-in-line))
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume))

  

;; Smartparens for working with sexps. ""
(use-package smartparens
  :bind (("C-l" . 'al-hydra-smartparens/body))
  :init
  (require 'smartparens-config)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  :config
  (setq al-open-rx (rx (or "(" "[" "{")))
  (setq al-close-rx (rx (or ")" "]" "}")))

  (defun al-next-close ()
    (interactive)
    (re-search-forward al-close-rx))
  (defun al-next-open ()
    (interactive)
    (re-search-forward al-open-rx))
  (defun al-prev-close ()
    (interactive)
    (re-search-backward al-close-rx))
  (defun al-prev-open ()
    (interactive)
    (re-search-backward al-open-rx))

  (defhydra al-hydra-smartparens (:color
				  pink)
    "Smartparens"
    ("M-n" al-next-close "next )]}" :column "Free move")
    ("M-N" al-next-open "next ([{")
    ("M-p" al-prev-open "prev ([{")
    ("M-P" al-prev-close "prev )]}")

    ("a" sp-beginning-of-sexp "beg" :column "Tree move")
    ("e" sp-end-of-sexp "end")
    ("f" sp-forward-sexp "forward")
    ("b" sp-backward-sexp "back")
    ("n" sp-down-sexp "down")
    ("N" sp-backward-down-sexp "bw down")
    ("p" sp-up-sexp "up")
    ("P" sp-backward-up-sexp "bw up")

    ("h" sp-backward-slurp-sexp "bw slurp" :column "Slurp & Barf")
    ("H" sp-backward-barf-sexp "bw bar")
    ("l" sp-forward-slurp-sexp "slurp")
    ("L" sp-forward-barf-sexp "barf")

    ("R" sp-rewrap-sexp "rewrap" :column "Wraping")
    ("u" sp-unwrap-sexp "unwrapt")
    ("U" sp-backward-unwrap-sexp "bw wrap")
    ("(" sp-wrap-round "()")
    ("{" sp-wrap-curly "{}")
    ("[" sp-wrap-square "[]")

    ("S" sp-split-sexp "split" :column "Juggling" )
    ("s" sp-splice-sexp "splice")
    ("r" sp-raise-sexp "raise")
    ("j" sp-join-sexp "join")
    ("t" sp-transpose-sexp "transpose")
    ("A" sp-absorb-sexp "absorb")
    ("E" sp-emit-sexp "emit")
    ("o" sp-convolute-sexp "convolute")

    ("c" sp-change-inner "ch inner" :exit t :column "Edit")
    ("C" sp-change-enclosing "ch outer" :exit t)

    ("M-d" sp-kill-sexp "kill")
    ("C-w" sp-backward-kill-sexp "bw kill")
    ("M-w" sp-copy-sexp "copy")

    ("v" er/expand-region "expand region")

    ("q" nil "quit" :column "quit")
    ("C-l" nil "quit")
    ("g" nil "quit")))


;; Winner undo.
(use-package winner
  :bind* (("C-c u" . winner-undo)
	  ("C-c r" . winner-redo)
	  ("M-SPC w u" . winner-undo))
  :init
  (winner-mode +1)
  :config
  (setq winner-boring-buffers-regexp "\\*Minibuf.*"))


;; Avy on windows.
(use-package ace-window
  :init
  (bind-key* "C-o" 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; Fuzzier completion for Vertico.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))


;; Save minibuffer, shell, kill-ring etc history to a file.
(use-package savehist
  :config
  (setq savehist-additional-variables
	'(kill-ring search-ring regexp-search-ring))

  :init
  (savehist-mode))


;; Show info in minibuffer & embark
(use-package marginalia ;;sdfdsf
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))


;; Embark and consult improvements. 
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Save buffer on lost focus.  
'(use-package super-save
  :config
  (super-save-mode +1))


;; "Semantic" region marker.
(use-package expand-region
  :bind* (("C-v" . set-mark-command)
	  ("M-v" . er/expand-region)))



;; Better looking help pages. 
(use-package helpful
    :bind
    (("C-h f" . helpful-callable)
     ("C-h v" . helpful-variable)
     ("C-h k" . helpful-key)
     ("C-c C-d" . helpful-at-point)
     ("C-h F" . helpful-function)
     ("C-h C" . helpful-command)))


;; Writable grep buffer.
(use-package wgrep
  :defer t)


;; Pulse when yank/pop
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))


;; Smarter completions.
(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
	 ("C-c p t" . complete-tag)        ;; etags
	 ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-symbol)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p i" . cape-ispell)
	 ("C-c p l" . cape-line)
	 ("C-c p w" . cape-dict)
	 ("C-c p \\" . cape-tex)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))


;; Magic.
(use-package magit
  :config
  (defun al-magit-fetch-rebase ()
    (interactive)
    (call-interactively 'magit-fetch-all)
    (call-interactively 'magit-rebase))
  
  (defun al-magit-fetch-reset ()
    (interactive)
    (call-interactively 'magit-fetch-all)
    (call-interactively 'magit-reset-hard))
  
  :bind* (("M-SPC g r" . al-magit-fetch-rebase)
	  ("M-SPC g R" . al-magit-fetch-reset)
	  ("M-SPC g b r" . magit-branch-reset)
	  ("M-SPC g l" . magit-log)
	  ("M-SPC g s" . magit-status)))


;; Json.
(use-package json-mode :defer t)


;; LSP.
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(web-mode . "javascriptreact"))
  )

(use-package lsp-tailwindcss
  :straight (lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  )


;; Web-mode.
(use-package web-mode
  :config
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")))
  :mode ("\\.tsx" "\\.jsx\\'" "\\.js\\'" "\\.html" "\\.css")
  :bind
  (:map web-mode-map
	("C-, e /" . 'web-mode-element-close)
	("C-, e a" . 'web-mode-element-content-select)
	("C-, e b" . 'web-mode-element-beginning)
	("C-, e c" . 'web-mode-element-clone)
	("C-, e d" . 'web-mode-element-child)
	("C-, e e" . 'web-mode-element-end)
	("C-, e f" . 'web-mode-element-children-fold-or-unfold)
	("C-, e i" . 'web-mode-element-insert)
	("C-, e I" . 'web-mode-element-insert-at-point)
	("C-, e k" . 'web-mode-element-kill)
	("C-, e m" . 'web-mode-element-mute-blanks)
	("C-, e n" . 'web-mode-element-next)
	("C-, e p" . 'web-mode-element-previous)
	("C-, e r" . 'web-mode-element-rename)
	("C-, e s" . 'web-mode-element-select)
	("C-, e t" . 'web-mode-element-transpose)
	("C-, e u" . 'web-mode-element-parent)
	("C-, e v" . 'web-mode-element-vanish)
	("C-, e w" . 'web-mode-element-wrap)
	("C-, e +" . 'web-mode-element-extract)
	("C-, e -" . 'web-mode-element-contract)))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (mark)
  (setq markdown-max-image-size '(1024 . 768))
  (setq markdown-display-remote-images t))


;; Yaml.
(use-package yaml-mode :defer t)


;; Dockerfile.
(use-package dockerfile-mode :defer t)


;; Handle docker containers.
(use-package docker
  :bind* (("M-SPC d d" . docker)
	  ("M-SPC d c" . docker-compose)))


(use-package good-scroll
  :bind* (("M-SPC w n" . al-scroll/body)
	  ("M-SPC w p" . al-scroll/body))
  :config
  (good-scroll-mode 1)
  (defhydra al-scroll (:color pink)
    "Good scroll"
    ("n" good-scroll-up)
    ("p" good-scroll-down)
    ("q" nil)
    )
  )

(use-package multiple-cursors
  :bind* (("M-l" . al-hydra-mc/body)
	  ("C->" . mc/mark-next-like-this)
	  ("C-<" . mc/mark-previous-like-this)
	  ("C-c C-<" . mc/mark-all-like-this))

  :config
  (defhydra al-hydra-mc (:color pink :hint nil)
    "
Multiple cursors
Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
[_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
[_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
[_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
[Click] Cursor at point       [_q_] Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("M-l" nil)
    ("q" nil)))


;; Show available keys (durring entering command).
;; TODO: can we have pretty names with use-packages bind and which key
;; https://github.com/justbur/emacs-which-key#key-based-replacement
(use-package which-key
  :config
  (setq which-key-idle-delay 0)
  (which-key-setup-side-window-bottom)
  (which-key-mode))


;; Projectile.
(use-package projectile
  :bind* (("M-SPC p" . 'projectile-command-map)))


;; Summary of projectile output into one consult.  
(use-package consult-projectile
  :bind* (("M-SPC b" . consult-projectile))
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))


;; Python virtual env that works. 
(use-package pyvenv
  :bind (:map python-mode-map
	      ("C-, a" . pyvenv-activate))
  :hook (python-mode . pyvenv-mode))


;; Python auto-complete and defenition.
(use-package anaconda-mode
  :hook (python-mode . anaconda-mode))


;; The built-in python mode with some tweaks.
(use-package python-mode
  :bind (:map python-mode-map
	      ("C-x C-e" . al-python-shell-send-dwim)
	      ("C-M-x" . python-shell-send-defun)
	      ("C-, e b" . python-shell-send-buffer)
	      ("C-, e e" . al-python-shell-send-dwim)
	      ("C-, e f" . python-shell-send-defun)
	      ("C-, s" . run-python)
	      ("C-, a" . pyvenv-activate)
	      ("C-<tab>" . 'python-indent-shift-right)
	      ("S-<tab>" . 'python-indent-shift-left)
	      )
  :config
  (defun al-python-shell-send-dwim ()
    (interactive)
    (if mark-active
	(call-interactively 'al-python-run-region-in-python-shell)
      (call-interactively 'al-python-run-stm-in-python-shell)))
  
  (defun al-python-run-stm-in-python-shell ()
    (interactive)
    (al-python-run-string-in-python-shell
     (buffer-substring
      (save-excursion (python-nav-beginning-of-statement))
      (save-excursion (python-nav-end-of-statement)))))

  (defun al-python-run-region-in-python-shell ()
    (interactive)
    (al-python-run-string-in-python-shell
     (buffer-substring (region-beginning) (region-end))))

  (defun al-python-run-string-in-python-shell (string)
    (interactive)
    (save-window-excursion
      (python-shell-switch-to-shell)
      (end-of-buffer)
      (insert string)
      (comint-send-input)))

  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"))


;; Guess the indent.
(use-package dtrt-indent
  :config
  (setq js-indent-level 2)
  (dtrt-indent-global-mode))


;; Prettier.
(use-package prettier
  :defer t
  :init
  (dir-locals-set-class-variables 'prettier-js
                                '((web-mode . ((eval . (prettier-mode t))))))
  (dir-locals-set-directory-class "~/code/bikg-triage-svc/frontend/" 'prettier-js))


;; Visual undo.
(use-package undo-tree
  :bind* (("C-x u" . undo-tree-visualize)
	  ("M-SPC u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1))

;; Roam.
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam/"))
  :bind* (("M-SPC o l" . 'org-roam-buffer-toggle)
          ("M-SPC o f" . 'org-roam-node-find)
          ("M-SPC o g" . 'org-roam-graph)
	  ("M-SPC o a" . 'al-agenda)
          ("M-SPC o i" . 'org-roam-node-insert)
          ("M-SPC o c" . 'org-roam-capture)
	  ("M-SPC o d" . 'org-roam-dailies-goto-today)
          ("M-SPC o j" . 'org-roam-dailies-capture-today))
  :config
  (setq org-agenda-files '("~/org-roam/" "~/org-roam/daily"))
  ;; Lets try hopscotch-like
  (setq org-todo-keywords
	'(
	  (sequence "MUST-TODAY(t)" "MUST-LATER(l)"  "SHOULD-TODAY(T)" "SHOULD-LATER(L)" "COULD(c)" "|" "DONE(d)" )))

  (defun al-agenda ()
    (interactive)
    (org-agenda nil "n"))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocolp
  (require 'org-roam-protocol))

(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-image-dir "~/org-roam/.org-downloads/"))

;; Python black formatter.
;; TODO: setup per project like prettier.
(use-package blacken
  :hook (python-mode . blacken-mode))


(use-package eyebrowse
  :bind* (("M-SPC w 1" . 'eyebrowse-switch-to-window-config-1)
	  ("M-SPC w 2" . 'eyebrowse-switch-to-window-config-2)
	  ("M-SPC w 3" . 'eyebrowse-switch-to-window-config-3)
	  ("M-SPC w 4" . 'eyebrowse-switch-to-window-config-4)
	  ("M-SPC w r" . 'eyebrowse-rename-window-config))
  :config
  (eyebrowse-mode 1))

(use-package al-auto-commit-mode
  :straight nil)


;; Wrap lines in a nicer way.
(use-package adaptive-wrap
  :config
  (setq-default adaptive-wrap-extra-indent 2) ;; TODO: set smarter per mode?
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  (global-visual-line-mode +1))


(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))


(use-package sql
  :straight nil
  :bind*  (("M-SPC a p" . 'al-sql-postgres-connect))
  :bind (:map sql-mode-map

	      ("C-, e l" . 'sql-send-line-and-next)
	      ("C-x C-e" . 'al-sql-send)
	      ("C-, e e" . 'al-sql-send))
  :config
  (defun al-sql-send ()
    (interactive)
    (if mark-active
	(call-interactively 'sql-send-region)
      (call-interactively 'sql-send-paragraph)))
  (defun al-sql-postgres-connect ()
    (interactive)
    (sql-set-product "postgres" )
    (sql-set-sqli-buffer)))

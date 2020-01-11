;; Evil Setup
(setq evil-want-C-i-jump nil)
(use-package evil :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up-then-center)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down-then-center)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd ">") 'better-evil-shift-right)
  (define-key evil-visual-state-map (kbd "<") 'better-evil-shift-left)
  (advice-add 'evil-search-next :after #'my-center-line))

(use-package evil-collection :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary :ensure t)
(require 'evil-commentary)
(evil-commentary-mode)

(use-package key-chord :ensure t
  :config
  (require 'key-chord)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "hh" 'evil-normal-state))

;; General Setup
(use-package general :ensure t
  :config
  (general-evil-setup t)

  (general-define-key
   :states '(normal insert emacs visual)
   :prefix "SPC"
   :non-normal-prefix "C-s"
   ;; Special actions
   "x" '(helm-M-x :which-key "M-x")
   "'" '(other-window :which-key "other window")
   "l" '(evil-window-right :which-key "evil-right")
   "h" '(evil-window-left :which-key "evil-left")
   "j" '(evil-window-down :which-key "evil-down")
   "k" '(evil-window-up :which-key "evil-up")
   "SPC a" '(avy-goto-char-2 :which-key "select two-chars")
   ;; General (e)
   "ed" '(describe-key :which-key "describe key")
   "eq" '(save-buffers-kill-terminal :which-key "save and kill buffer")
   "et" '(launch-terminal :which-key "launch terminal")
   "nh" '(evil-ex-nohighlight :which-key "nohl")
   "ri" '(reload-init :which-key "reload init")
   ;; Open (o)
   "or" '(ranger :which-key "ranger")
   ;; Yanking (y)
   "ya" '(yank-all-stay :which-key "yank all")
   "ys" '(yank-from-start-stay :which-key "yank from the start")
   "ye" '(yank-to-end-stay :which-key "yank to the end")
   ;; Buffers (b)
   "bA" '(eval-buffer-then-report :which-key "eval buffer")
   "bb" '(er-switch-to-previous-buffer :which-key "goto last buffer")
   "bn" '(next-buffer :which-key "next buffer")
   "bp" '(previous-buffer :which-key "previous buffer")
   "bs" '(save-buffer :which-key "save buffer")
   "bx" '(kill-this-buffer :which-key "kill buffer")
   "be" '(helm-projectile-find-file :which-key "buffer edit")
   "bh" '(fzf-counsel-home :which-key "buffer edit from home")
   "bc" '(helm-projectile-switch-to-buffer :which-key "buffer edit")
   ;; Panes (a)
   "av" '(vsplit :which-key "vsplit")
   "ah" '(hsplit :which-key "hsplit")
   "ar" '(vsplit-33 :which-key "vsplit-33")
   "ad" '(hsplit-33 :which-key "hsplit-33")
   "ax" '(delete-window :which-key "delete window")
   "at" '(terminal-vsplit :which-key "terminal vsplit")
   "aa" '(toggle-window-split :which-key "rotate"))

  ;; Org Mode
  (general-define-key
   :states '(normal insert emacs visual)
   :keymaps 'org-mode-map
   :prefix "SPC"
   :non-normal-prefix "C-s"
   ;; Others
   "ih" '(insert-org-mode-header :which-key "insert org header")))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(use-package helm :ensure t :config)
(use-package helm-projectile :ensure t)

(use-package ido-completing-read+ :ensure t)
(use-package smex :ensure t)

;; Functions
(defun my-center-line (&rest _)
  (evil-scroll-line-to-center nil))

(defun evil-scroll-up-then-center (count)
  (interactive "P")
  (evil-scroll-up count)
  (evil-scroll-line-to-center count))

(defun evil-scroll-down-then-center (count)
  (interactive "P")
  (evil-scroll-down count)
  (evil-scroll-line-to-center count))

(defun mark-inner-paragraph ()
  (interactive)
  (mark-paragraph)
  (next-line)
  (beginning-of-line))

(defun yank-all-stay ()
  (interactive)
  (evil-set-marker ?m)
  (mark-whole-buffer)
  (call-interactively 'evil-yank)
  (evil-goto-mark ?m))

(defun yank-from-start-stay ()
  (interactive)
  (evil-set-marker ?m)
  (end-of-line)
  (call-interactively 'set-mark-command)
  (beginning-of-buffer)
  (call-interactively 'evil-yank)
  (evil-goto-mark ?m))

(defun yank-to-end-stay ()
  (interactive)
  (evil-set-marker ?m)
  (call-interactively 'set-mark-command)
  (end-of-buffer)
  (call-interactively 'evil-yank)
  (evil-goto-mark ?m))

(defun better-evil-shift-right (beg end)
  (interactive "r")
  (evil-shift-right beg end)
  (evil-normal-state)
  (evil-visual-restore))

(defun better-evil-shift-left (beg end)
  (interactive "r")
  (evil-shift-left beg end)
  (evil-normal-state)
  (evil-visual-restore))

(defun counsel-fzf-home ()
  (interactive)
  (counsel-fzf "" "~"))

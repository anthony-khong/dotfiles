;; Startup Optimisation
(setq gc-cons-threshold 100000000)

(require 'package)

(add-to-list 'package-archives '("org"        		.    	"http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa"    	.    	"http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" .    	"http://stable.melpa.org/packages/"))
(package-initialize)

;; Packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(eval-when-compile
  (require 'use-package))

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

(defun evil-scroll-up-then-center (count)
  (interactive "P")
  (evil-scroll-up count)
  (evil-scroll-line-to-center count))

(defun evil-scroll-down-then-center (count)
  (interactive "P")
  (evil-scroll-down count)
  (evil-scroll-line-to-center count))

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

(use-package avy
  :ensure t
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package general :ensure t
  :config
  (general-evil-setup t)

  (general-define-key
   :states '(normal visual)
   :prefix "C-c"
   "C-c" '(send-to-tmux :which-key "run region"))

  (general-define-key
   :states '(normal insert emacs visual)
   :prefix "SPC"
   :non-normal-prefix "C-s"
   ;; Special actions
   "x" '(counsel-M-x :which-key "M-x")
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
   "ff" '(counsel-projectile-find-file :which-key "fuzzy find file")
   "fd" '(counsel-projectile-find-dir :which-key "fuzzy find directory")
   "nh" '(evil-ex-nohighlight :which-key "nohl")
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
   "bf" '(ivy-switch-buffer :which-key "switch buffer")
   ;; Panes (a)
   "av" '(vsplit :which-key "vsplit")
   "ah" '(hsplit :which-key "hsplit")
   "ar" '(vsplit-33 :which-key "vsplit-33")
   "ad" '(hsplit-33 :which-key "hsplit-33")
   "ax" '(delete-window :which-key "delete window")
   "at" '(terminal-vsplit :which-key "terminal vsplit")
   ;; Send (s)
   "ss" '(send-to-terminal-buffer :which-key "send to terminal buffer"))

  ;; Python
  (general-define-key
   :states '(normal insert emacs visual)
   :keymaps 'python-mode-map
   :prefix "SPC"
   :non-normal-prefix "C-s"
   "gd" '(elpy-goto-definition-other-window :which-key "go to definition")
   "ga" '(elpy-goto-assignment-other-window :which-key "go to assignment")
   "c" '(elpy-check :which-key "lint")
   "d" '(elpy-doc :which-key "documentation")
   "ne" '(elpy-flymake-next-error :which-key "next error")
   "pe" '(elpy-flymake-previous-error :which-key "previous error")))

(use-package ivy :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
              ("C-'" . ivy-avy)) ; C-' to ivy-avy
  :config
  (setq ivy-use-virtual-buffers t)    ; extend searching to bookmarks and …
  (setq ivy-height 20)                ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ")) ; count format, from the ivy help page

(use-package counsel :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c f"   . counsel-git)       ; search for files in git repo
   ("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ("C-c l"   . counsel-locate)))  ; search for files or else using locate

(use-package counsel-projectile :ensure t
  :config (counsel-projectile-mode)
  :diminish
  (projectile-mode . "")
  (counsel-projectile-mode . ""))

(use-package flx :ensure t
  :config (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package which-key :ensure t
  :init (which-key-mode)
  :diminish (which-key-mode . ""))

(use-package xclip :ensure t)
(xclip-mode 1)

(use-package emamux :ensure t)

(use-package tmux-pane :ensure t)
(require 'tmux-pane)
(tmux-pane-mode t)

(use-package rainbow-delimiters :ensure t
  :init (rainbow-delimiters-mode))

(use-package powerline :ensure t)
(require 'powerline)

(use-package airline-themes :ensure t)
(require 'airline-themes)
(airline-themes-set-modeline)

(use-package monokai-theme :ensure t)
(load-theme 'monokai t)
(set-background-color "unspecified-bg")
(setq default-frame-alist '((background-color . "unspecified-bg")))

(use-package company :ensure t
  :init (company-mode)
  :diminish (company-mode . "")
  :config
  (global-company-mode 1)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (eval-after-load 'company
    '(progn
       (define-key company-active-map (kbd "TAB") 'company-complete-selection))))
(require 'company)

(use-package volatile-highlights :ensure t
  :config (volatile-highlights-mode t)
  :diminish (volatile-highlights-mode . ""))

(use-package ranger :ensure t
  :commands (ranger)
  :config (setq ranger-cleanup-eagerly t))

(use-package diminish :ensure t)
(require 'diminish)

(use-package linum-relative :ensure t
  :init
  (setq linum-format "%4d \u2502")
  (setq linum-relative-format "%4s \u2502")
  (global-linum-mode t)
  (set-face-background 'linum "unspecified-bg"))
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(linum-relative-on)

(use-package yasnippet :ensure t
  :init (yas-global-mode 1)
  :diminish (yas-minor-mode . ""))

(use-package yasnippet-snippets :ensure t)

(use-package magit :ensure t)
(require 'magit)

(use-package evil-magit :ensure t)
(require 'evil-magit)

;; (use-package git-gutter :ensure t)
;; (require 'git-gutter)
;; (global-git-gutter-mode +1)
;; (git-gutter:linum-setup)

;;;; Emacs Lisp
(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :diminish (parinfer-mode . "")
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

;;;; Python
(use-package elpy :ensure t
  :config
  (elpy-enable)
  (setq python-shell-interpreter "python"
     	python-shell-interpreter-args "-i --simple-prompt"))

;;;; Clojure
(use-package clojure-mode :ensure t)
(use-package cider :ensure t)

;;;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Passive Configurations
(menu-bar-mode -1)
(setq vc-follow-symlinks t)
(electric-indent-mode +1)
(setq backup-directory-alist `(("." . "~/.saves")))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;; Continuos scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq scroll-margin 3)

;;;; Clipboard
(defun noct:conditionally-toggle-xclip-mode ()
  (if (display-graphic-p)
      (if (bound-and-true-p xclip-mode)
          (xclip-mode -1))
    (xclip-mode)))
(noct:conditionally-toggle-xclip-mode)
(add-hook 'focus-in-hook
          #'noct:conditionally-toggle-xclip-mode)

;; Functions
(defun my-center-line (&rest _)
  (evil-scroll-line-to-center nil))

(defun er-switch-to-previous-buffer ()
  "https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun hsplit ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun hsplit-33 ()
  (interactive)
  (split-window-vertically (floor (* 0.68 (window-height))))
  (other-window 1))

(defun vsplit ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun vsplit-33 ()
  (interactive)
  (split-window-horizontally (floor (* 0.68 (window-width))))
  (other-window 1))

(defun eval-buffer-then-report ()
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

(defun launch-terminal ()
  (interactive)
  (term "/bin/zsh"))

(defun mark-inner-paragraph ()
  (interactive "r")
  (mark-paragraph)
  (next-line)
  (beginning-of-line))

(defun terminal-vsplit ()
  (interactive)
  (vsplit-33)
  (launch-terminal)
  (other-window 1))

(defun send-to-terminal-buffer (beg end)
  (interactive "r")
  (mark-inner-paragraph)
  (process-send-region "terminal" beg end))

(defun send-to-tmux (beg end)
  (interactive "r")
  (mark-inner-paragraph)
  (emamux:run-region beg end)
  (evil-normal-state))

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

;; Emacs Lisp
(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(emamux:default-orientation (quote horizonal))
 '(emamux:runner-pane-height 35)
 '(package-selected-packages
   (quote
    (evil-magit magit yasnippet-snippets elpy yasnippet counsel-projectile fiplr counsel evil-collection fzf avy git-gutter evil-snipe rainbow-delimiters company eyebrowse anotehu evil-mode use-package evil-visual-mark-mode))))

(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

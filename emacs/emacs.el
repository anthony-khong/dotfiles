(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
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
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
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

;; TODO
;; (use-package evil-snipe :ensure t)
;; (require 'evil-snipe)
;; (evil-snipe-mode +1)
;; (evil-snipe-override-mode +1)

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
   ;; General (e)
   "ex" '(counsel-M-x :which-key "M-x")
   "ed" '(describe-key :which-key "M-x")
   "eq" '(save-buffers-kill-terminal :which-key "C-x C-c")
   "et" '(launch-terminal :which-key "launch terminal")
   "ff" '(find-file :which-key "new buffer edit")
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
   ;;;; Splits
   "av" '(vsplit :which-key "vsplit")
   "ah" '(hsplit :which-key "hsplit")
   "ar" '(vsplit-33 :which-key "vsplit-33")
   "ad" '(hsplit-33 :which-key "hsplit-33")
   "ax" '(delete-window :which-key "delete window")
   "at" '(terminal-vsplit :which-key "terminal vsplit")
   ;;;; Movements
   "'" '(other-window :which-key "other window")
   "l" '(evil-window-right :which-key "evil-right")
   "h" '(evil-window-left :which-key "evil-left")
   "j" '(evil-window-down :which-key "evil-down")
   "k" '(evil-window-up :which-key "evil-up")
   ;; Send (s)
   "ss" '(send-to-terminal-buffer :which-key "send to terminal buffer")))
   ;; "ss" '(emamux:run-region :which-key "send tmux")
   

(use-package xclip :ensure t)
(xclip-mode 1)

(use-package emamux :ensure t)
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
    (git-gutter evil-snipe rainbow-delimiters company eyebrowse anotehu evil-mode use-package evil-visual-mark-mode))))

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

;; (use-package doom-themes :ensure t)
;; (require 'doom-themes)
;; (setq doom-themes-enable-bold t
;;       doom-themes-enable-italic t)
;; (load-theme 'doom-one t)
(use-package monokai-theme :ensure t)
(load-theme 'monokai t)
(set-background-color "unspecified-bg")
(setq default-frame-alist '((background-color . "unspecified-bg")))

(use-package company :ensure t
  :init (company-mode))
(require 'company)
(global-company-mode 1)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-selection)))

(use-package volatile-highlights :ensure t
  :config
  (volatile-highlights-mode t))

(use-package ranger :ensure t
  :commands (ranger)
  :config (setq ranger-cleanup-eagerly t))

(use-package linum-relative :ensure t
  :init
  (setq linum-relative-format "%3s ")
  (global-linum-mode t)
  (set-face-background 'linum "unspecified-bg"))
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(linum-relative-on)

(use-package diminish :ensure t)
(require 'diminish)

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

(use-package which-key :ensure t
  :init (which-key-mode))

;; TODO
;; (use-package git-gutter :ensure t)
;; (require 'git-gutter)
;; (global-git-gutter-mode +1)

;;;; Emacs Lisp
(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
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

;; Emacs Lisp
(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.


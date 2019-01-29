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

(use-package evil :ensure t)
(require 'evil)
(evil-mode t)
(setq evil-search-module 'evil-search)

(use-package tmux-pane :ensure t)
(require 'tmux-pane)
(tmux-pane-mode t)

(use-package powerline :ensure t)
(require 'powerline)

(use-package airline-themes :ensure t)
(require 'airline-themes)
(airline-themes-set-modeline)

(use-package monokai-theme :ensure t)
(load-theme 'monokai t)
(setq default-frame-alist '((background-color . "unspecified-bg")))

(use-package linum-relative :ensure t)
(require 'linum-relative)
(setq linum-relative-format "%3s ")
(global-linum-mode t)
(linum-relative-on)
(set-face-background 'linum "unspecified-bg")

(use-package helm :ensure t)
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

(use-package clojure-mode :ensure t)
(use-package cider :ensure t)

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

;; Key Bindings
;; TODO: Make SPC a prefix + make these command generaliseable
(define-key evil-normal-state-map (kbd ", e") 'kill-this-buffer)
(define-key evil-normal-state-map (kbd ", A") 'eval-buffer)
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "t p") 'previous-buffer)
(define-key evil-normal-state-map (kbd "t n") 'next-buffer)
(defun er-switch-to-previous-buffer ()
  "https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(define-key evil-normal-state-map (kbd "t t") 'er-switch-to-previous-buffer)
(define-key evil-normal-state-map (kbd "t f") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "tabe") 'find-file)

(defun shell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))
(define-key evil-normal-state-map (kbd "SPC s") 'shell-other-window)

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
 '(package-selected-packages
   (quote
    (eyebrowse anotehu evil-mode use-package evil-visual-mark-mode))))
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
 

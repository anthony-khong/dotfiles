;; Packages

;;;; Visualisation
(use-package monokai-theme :ensure t)
(load-theme 'monokai t)
(set-background-color "unspecified-bg")
(setq default-frame-alist '((background-color . "unspecified-bg")))

(use-package linum-relative :ensure t
  :init
  (setq linum-relative-backend 'display-line-numbers-mode)
  (global-display-line-numbers-mode)
  (set-face-background 'line-number "unspecified-bg"))
(require 'linum-relative)
(linum-relative-on)

(use-package powerline :ensure t)
(require 'powerline)
(powerline-default-theme)
(setq powerline-display-buffer-size nil)
(setq powerline-display-mule-info nil)
(setq powerline-display-hud nil)

(use-package airline-themes :ensure t)
(require 'airline-themes)
(airline-themes-set-modeline)

(use-package rainbow-delimiters :ensure t
  :init (rainbow-delimiters-mode))

(use-package git-gutter :ensure t
  :diminish (git-gutter-mode . "")
  :config
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (setq git-gutter:modified-sign "~"))
(require 'git-gutter)
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
(set-face-foreground 'git-gutter:modified "yellow")
(set-face-background 'git-gutter:added "unspecified-bg")
(set-face-background 'git-gutter:deleted "unspecified-bg")
(set-face-background 'git-gutter:modified "unspecified-bg")
(global-git-gutter-mode +1)

(use-package volatile-highlights :ensure t
 :config (volatile-highlights-mode t)
 :diminish (volatile-highlights-mode . ""))

(use-package diminish :ensure t
 :diminish
 (undo-tree-mode . "")
 (auto-revert-mode . ""))
(require 'diminish)

(use-package which-key :ensure t
  :init (which-key-mode)
  :diminish (which-key-mode . ""))

(use-package ranger :ensure t
  :commands (ranger)
  :config (setq ranger-cleanup-eagerly t))

(use-package magit :ensure t)
(require 'magit)

(use-package xclip :ensure t)
(xclip-mode 1)

(use-package whitespace :ensure t)
(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package tagedit :ensure t)

;; Configs
(setq enable-local-variables nil)
(setq vc-follow-symlinks t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(setq inhibit-compacting-font-caches t)
(setq-default tab-width 4)
(setq-default truncate-lines nil
              indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face t)))))
(add-to-list 'interpreter-mode-alist
             '("bash" . sh-mode))
(add-to-list 'company-backends '(company-capf company-dabbrev))

(electric-indent-mode +1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(global-prettify-symbols-mode +1)
(menu-bar-mode -1)
(modify-syntax-entry ?_ "w")
(set-frame-parameter (selected-frame) 'buffer-predicate #'buffer-file-name)
(show-paren-mode 1)

;;;; Continuous Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-margin 8
   scroll-step 1
   scroll-conservatively 10000
   scroll-preserve-screen-position 1)

;;;; Clipboard
(defun noct:conditionally-toggle-xclip-mode ()
 (if (display-graphic-p)
   (if (bound-and-true-p xclip-mode)
       (xclip-mode -1))
   (xclip-mode)))
(noct:conditionally-toggle-xclip-mode)
(add-hook 'focus-in-hook
          #'noct:conditionally-toggle-xclip-mode)

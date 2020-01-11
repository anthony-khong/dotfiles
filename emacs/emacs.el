;; Startup Optimisation
(setq gc-cons-threshold 100000000)

(require 'package)

(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; Packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(eval-when-compile
  (require 'use-package))
(use-package auto-package-update :ensure t)
(use-package benchmark-init :ensure t)

(use-package avy
  :ensure t
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

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

;(use-package yasnippet :ensure t
 ;:init
 ;(setq yas-snippet-dirs  '("~/dotfiles/emacs/snippets"))
 ;(yas-global-mode 1)
 ;:diminish (yas-minor-mode . ""))

;(use-package yasnippet-snippets :ensure t)

;;;; Org Mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib)

;;;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown"))

;; Functions
(defun reload-init ()
  "Reloads init file"
  (interactive)
  (load-file "~/.emacs")
  (message "Emacs reloaded!"))

(defun insert-org-mode-header ()
  (interactive)
  (insert "MY PROJECT -*- mode: org -*-"))

;; Load Scripts
(setq el-files '("vim"
                 "tmux"
                 "passive"
                 "buffers-and-panes"
                 "navigation"
                 ;; Languages
                 "dockerfile"
                 "python"
                 "lisp"))

(mapcar (lambda (f)
          (load-file (concat "~/dotfiles/emacs/" f ".el")))
        el-files)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-rg hy-mode clojure-mode-extra-font-locking py-autopep8 doom-themes auto-package-update evil-tabs org-plus-contrib evil-magit magit yasnippet-snippets elpy yasnippet counsel-projectile fiplr counsel evil-collection fzf avy git-gutter evil-snipe rainbow-delimiters company eyebrowse anotehu evil-mode use-package evil-visual-mark-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "unspecified-bg")))))

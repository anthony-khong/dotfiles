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

;(use-package ivy :ensure t
  ;:diminish (ivy-mode . "") ; does not display ivy in the modeline
  ;:init (ivy-mode 1)        ; enable ivy globally at startup
  ;:bind (:map ivy-mode-map  ; bind in the ivy buffer
             ;("C-'" . ivy-avy)) ; C-' to ivy-avy
  ;:config
  ;(setq ivy-re-builders-alist
        ;'((ivy-switch-buffer . ivy--regex-plus)
          ;(t . ivy--regex-fuzzy)))    ; Fuzzy matching in M-x
  ;(setq ivy-use-virtual-buffers t)    ; extend searching to bookmarks and …
  ;(setq ivy-height 20)                ; set height of the ivy window
  ;(setq ivy-count-format "(%d/%d) ")) ; count format, from the ivy help page

;(use-package counsel :ensure t
  ;:bind*                           ; load counsel when pressed
  ;(("M-x"     . counsel-M-x)       ; M-x use counsel
   ;("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ;("C-x C-r" . counsel-recentf)   ; search recently edited files
   ;("C-c f"   . counsel-git)       ; search for files in git repo
   ;("C-c s"   . counsel-git-grep)  ; search for regexp in git repo
   ;("C-c /"   . counsel-ag)        ; search for regexp in git repo using ag
   ;("C-c l"   . counsel-locate)))  ; search for files or else using locate
;(setq counsel-async-ignore-re '("*Dropbox*"))

;(use-package counsel-projectile :ensure t
  ;:config (counsel-projectile-mode)
  ;:diminish
  ;(projectile-mode . "")
  ;(counsel-projectile-mode . ""))

;(use-package ido :ensure t
  ;:config
  ;(setq ido-auto-merge-delay-time 99999999)
  ;(setq ido-everywhere t)
  ;(setq ido-virtual-buffers t)
  ;(setq ido-enable-flex-matching t)
  ;(ido-mode))

;(use-package ibuffer-vc :ensure t
 ;:config
 ;(add-to-list 'ibuffer-never-show-predicates "^\\*"))
;(add-hook 'ibuffer-mode-hook
         ;'(lambda ()
              ;(ibuffer-switch-to-saved-filter-groups "home")))

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

;(use-package magit :ensure t)
;(require 'magit)

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

(defun counsel-fzf-home ()
  (interactive)
  (counsel-fzf "" "~"))

(defun insert-org-mode-header ()
  (interactive)
  (insert "MY PROJECT -*- mode: org -*-"))

;; Load Scripts
(setq el-files '("vim"
                 "tmux"
                 "passive"
                 "buffers-and-panes"
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
    (hy-mode clojure-mode-extra-font-locking py-autopep8 doom-themes auto-package-update evil-tabs org-plus-contrib evil-magit magit yasnippet-snippets elpy yasnippet counsel-projectile fiplr counsel evil-collection fzf avy git-gutter evil-snipe rainbow-delimiters company eyebrowse anotehu evil-mode use-package evil-visual-mark-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "unspecified-bg")))))

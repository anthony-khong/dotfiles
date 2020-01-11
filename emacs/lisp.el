;;;; Emacs Lisp
(use-package paredit :ensure t)
(use-package lispy :ensure t)
(use-package smart-tabs-mode :ensure t)
(use-package smart-yank :ensure t)
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
    (add-hook 'hy-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))
(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))


;;;; Clojure
(use-package clojure-mode :ensure t)
(use-package cider :ensure t
  :pin melpa-stable
  :config
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package clojure-mode-extra-font-locking :ensure t)
(require 'clojure-mode-extra-font-locking)

(add-hook 'clojure-mode-hook
          (lambda ()
             (require 'clojure-mode)
             (require 'clojure-mode-extra-font-locking)
             (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
             (font-lock-add-keywords
              nil
              '(("(\\(facts?\\)"
                 (1 font-lock-keyword-face))
                ("(\\(background?\\)"
                 (1 font-lock-keyword-face))))
             (define-clojure-indent (fact 1))
             (define-clojure-indent (facts 1))))

(general-define-key
  :states '(normal visual)
  :keymaps 'clojure-mode-map
  :prefix "SPC"
  :non-normal-prefix "C-s"
  "cj" '(cider-jack-in :which-key "jack in")
  "cl" '(cider-load-buffer :which-key "load buffer")
  "cc" '(cider-connect :which-key "connect")
  "cn" '(cider-repl-set-ns :which-key "set repl ns")
  "ct" '(cider-test-run-test :which-key "run test")
  "cb" '(cider-switch-to-repl-buffer :which "repl buffer")
  "cd" '(cider-doc :which-key "doc")
  "cf" '(cider-find-var :which-key "find var")
  "cp" '(cider-pop-back :which-key "pop back")
  "ces" '(cider-eval-sexp-at-point :which-key "eval sexp")
  "cet" '(cider-eval-defun-at-point :which-key "eval top")
  "cer" '(cider-eval-region :which-key "eval region")
  "p(" '(paredit-wrap-round :which-key "wrap (")
  "p[" '(paredit-wrap-square :which-key "wrap [")
  "p{" '(paredit-wrap-curly :which-key "wrap {")
  "pk" '(paredit-kill :which-key "paredit kill")
  "ps" '(paredit-forward-slurp-sexp :which-key "forward slurp")
  "pb" '(paredit-forward-barf-sexp :which-key "forward barf")
  "pS" '(paredit-backward-slurp-sexp :which-key "backward slurp")
  "pB" '(paredit-backward-barf-sexp :which-key "backward barf")
  "ps" '(paredit-split-sexp :which-key "split sexp")
  "pj" '(paredit-join-sexps :which-key "join sexps")
  "SPC l" '(paredit-forward-down :which-key "paredit forward")
  "SPC h" '(paredit-backward-up :which-key "paredit backward")
  "pt" '(transpose-sexps :which-key "transpose sexps")
  "pT" '(reverse-transpose-sexps :which-key "reverse transpose sexps"))

(general-define-key
  :states '(normal)
  :keymaps 'clojure-mode-map
  :prefix "C-c"
  "C-c" '(send-paragraph-to-tmux :which-key "eval top level"))

(general-define-key
  :states '(visual)
  :keymaps 'clojure-mode-map
  :prefix "C-c"
  "C-c" '(send-paragraph-to-tmux :which-key "eval region"))

(general-define-key
  :states '(normal)
  :keymaps 'cider-repl-mode-map
  :prefix "SPC"
  "n" '(cider-repl-next-input :which-key "repl next")
  "p" '(cider-repl-previous-input :which-key "repl previous")
  "cb" '(cider-repl-clear-buffer :which-key "clear buffer")
  "co" '(cider-repl-clear-output :which-key "clear output"))

(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  ;; when transpose-sexps can no longer transpose, it throws an error and code
  ;; below this line won't be executed. So, we don't have to worry about side
  ;; effects of backward-sexp and forward-sexp.
  (backward-sexp (1+ arg))
  (forward-sexp 1))


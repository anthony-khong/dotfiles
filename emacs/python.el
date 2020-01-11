(use-package elpy :ensure t
  :config
  (elpy-enable)
  (setq python-shell-interpreter 
        "ipython"
        python-shell-interpreter-args 
        "--simple-prompt --pprint"))
(delete `elpy-module-highlight-indentation elpy-modules)

(use-package py-autopep8 :ensure t)
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(use-package hy-mode :ensure t)

(general-define-key
  :states '(normal visual)
  :keymaps 'python-mode-map
  :prefix "C-c"
  "C-c" '(send-paragraph-to-tmux-ipython :which-key "run region"))

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
  "pe" '(elpy-flymake-previous-error :which-key "previous error"))


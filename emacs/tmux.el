(use-package tmux-pane :ensure t)
(require 'tmux-pane)
(tmux-pane-mode t)

;; Vim Slime
(defun send-kill-ring-to-tmux ()
  (interactive)
  (write-region (format "%s" (car kill-ring)) nil "~/.slime_paste")
  (shell-command "tmux load-buffer ~/.slime_paste")
  (shell-command "tmux paste-buffer -d -t 1")
  (shell-command "cat /dev/null > ~/.slime_paste"))

(defun send-text-to-tmux (text)
  (shell-command (format "tmux send-keys -t 1 %s" text)))

(defun send-paragraph-to-tmux ()
  (interactive)
  (mark-paragraph)
  (call-interactively 'evil-yank)
  (send-kill-ring-to-tmux)
  (send-text-to-tmux "Enter"))

(defun send-paragraph-to-tmux-ipython ()
  (interactive)
  (mark-paragraph)
  (call-interactively 'evil-yank)
  (send-text-to-tmux "'%cpaste -q' Enter")
  (send-kill-ring-to-tmux)
  (send-text-to-tmux "KP- KP-")
  (send-text-to-tmux "Enter"))


(general-define-key
  :states '(normal visual)
  :prefix "C-c"
  "C-c" '(send-paragraph-to-tmux :which-key "run region"))

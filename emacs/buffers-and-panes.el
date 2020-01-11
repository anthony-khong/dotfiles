;; Buffers
(defun er-switch-to-previous-buffer ()
  "https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun eval-buffer-then-report ()
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))


;; Panes
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

(defun toggle-window-split ()
  ;; From https://www.emacswiki.org/emacs/ToggleWindowSplit
  (interactive)
  (if (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges))
                                  (car next-win-edges))
                             (<= (cadr this-win-edges)
                                 (cadr next-win-edges))))
           (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
     (delete-other-windows)
     (let ((first-win (selected-window)))
         (funcall splitter)
         (if this-win-2nd (other-window 1))
         (set-window-buffer (selected-window) this-win-buffer)
         (set-window-buffer (next-window) next-win-buffer)
         (select-window first-win)
         (if this-win-2nd (other-window 1))))))

(defun launch-terminal ()
  (interactive)
  (term "/bin/zsh"))

(defun terminal-vsplit ()
  (interactive)
  (vsplit-33)
  (launch-terminal)
  (other-window 1))

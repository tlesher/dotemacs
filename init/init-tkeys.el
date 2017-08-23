;;;; Defines useful bindings to hang off "C-c c".

;;; Utility functions
(defun insert-todo (arg)
  "Insert 'TODO(username): ' at point, using correct commenting
syntax for current buffer."
  (interactive "*P")
  (let ()
    (comment-dwim arg)
    (insert "TODO(" (user-login-name) "): \n")
    (forward-char -1)))

;; From http://stackoverflow.com/questions/3669511/the-function-to-show-current-files-full-path-in-mini-buffer
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

;;; Tim's key bindings (tkeys).
(define-prefix-command 'tkey-map)
(define-key tkey-map (kbd "f") 'show-file-name)
(define-key tkey-map (kbd "F") 'copy-full-path-to-kill-ring)
(define-key tkey-map (kbd "k") 'delete-region)
(define-key tkey-map (kbd "t") 'insert-todo)
(global-set-key (kbd "C-c c") 'tkey-map)

;; Random non-tkey keys that shouldn't be here.
(global-set-key (kbd "C-c C-x C-c") 'save-buffers-kill-emacs)


(provide 'init-tkeys)

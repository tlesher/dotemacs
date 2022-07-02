(defvar tl-messages-archive-file (concat user-emacs-directory "messages-archive"))
(defun tl-archive-messages ()
  (interactive)
  (with-current-buffer "*Messages*"
    (widen)
    (append-to-file (point-min) (point-max) tl-messages-archive-file)))
(add-hook 'kill-emacs-hook 'tl-archive-messages)
(provide 'init-archive-messages)

(defvar messages-archive-file (concat user-emacs-directory "messages-archive"))

(defun archive-messages ()
  (interactive)
  (with-current-buffer "*Messages*"
    (widen)
    (append-to-file (point-min) (point-max) messages-archive-file)))

(archive-messages)

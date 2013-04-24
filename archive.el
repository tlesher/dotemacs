(defvar messages-archive-file "~/.emacs.d/messages-archive")
(defun archive-messages ()
  (interactive)
  (with-current-buffer "*Messages*"
    (widen)
    (append-to-file (point-min) (point-max) messages-archive-file)))

(archive-messages)

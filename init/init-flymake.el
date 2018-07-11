;; (when (load "flymake" t)
;;   (add-hook 'find-file-hook 'flymake-find-file-hook)
;;   (require 'flymake-cursor)
;;   (global-set-key [f10] 'flymake-goto-prev-error)
;;   (global-set-key [f11] 'flymake-goto-next-error)

;;   ;; Instead of raising a dialog, just warn via *Messages*
;;   (defun flymake-display-warning (warning)
;;     (message warning))
;;   )

(provide 'init-flymake)

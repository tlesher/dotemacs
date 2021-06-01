;; timed-require: Set to t to print time for each (require) in this file.
(defvar timed-require-max-depth 5
  "*The maximum depth to track nested require calls; if 1, just print summaries.")
(defvar timed-require-depth 0)  ;; Current depth; do not edit.
(defun timed-require (orig-fn &rest args)
  (let ((start-time (float-time)))
    (if (and (> timed-require-max-depth 1)
             (< timed-require-depth timed-require-max-depth))
        (message "%s > require %s" (make-string timed-require-depth ?\s) args))
    (incf timed-require-depth)
    (let ((res (apply orig-fn args)))
      (decf timed-require-depth)
      (if (< timed-require-depth timed-require-max-depth)
          (message "%srequire %s (%.4fs)"
                   (concat
                    (make-string timed-require-depth ?\s)
                    (if (eq timed-require-max-depth 1) "" " < "))
                   args (- (float-time) start-time))))))
;; 1000 is far too few when using timed-require
(setq message-log-max 5000)

;; Hook require.
(advice-add 'require :around #'timed-require)

;; Unhook require after startup.
(add-hook 'emacs-startup-hook (lambda() (advice-remove 'require #'timed-require)))

(provide 'timed-require)

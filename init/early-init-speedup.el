;; calcifer: Emacs ready in 0.77 seconds with 8 garbage collections.

;; bru: Emacs ready in 0.73 seconds with 20 garbage collections.

;; bru@1 0.98/33
;; bru@2 0.98/33
;; bru@4 0.93/27
;; bru@8 0.87/24
;; bru@16 0.90/22
;; bru@32 0.77/21
;; bru@64 0.73/20
;; bru@128 0.77/20

;; (setq tdl-init-gc-cons-threshold-multiplier 64
;;       tdl-orig-gc-cons-threshold gc-cons-threshold
;;       tdl-init-gc-cons-threshold (* tdl-init-gc-cons-threshold-multiplier
;; 				    gc-cons-threshold)
;;       gc-cons-threshold tdl-init-gc-cons-threshold)

;; (message (format "Setting init gc threshold multiplier to %d." tdl-init-gc-cons-threshold-multiplier))

;; Use ALL the memory!
(setq tdl-orig-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum)

(defun tdl/post-startup-restore-gc ()
  "Return to original gc cons threshold after startup."
  (setq gc-cons-threshold tdl-orig-gc-cons-threshold)
  (let ((elapsed (float-time (time-subtract after-init-time before-init-time))))
    (message "Emacs ready in %.2f seconds with %d garbage collections."
             elapsed gcs-done)))
(add-hook 'emacs-startup-hook #'tdl/post-startup-restore-gc)

(provide 'early-init-speedup)

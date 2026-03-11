;; Emacs startup optimization:
;; Make startup faster by increasing the garbage collection threshold (thereby
;; reducing the frequency of garbage collection), then reduce again to make gc pauses faster.
;; The default gc threshold is 800 kilobytes.
;; Last test:
;; OUTDATED-REDO: Emacs ready in 2.07 seconds with 18 garbage collections.

;; OUTDATED-REDO: calcifer: Emacs ready in 0.77 seconds with 8 garbage collections.

;; bru: Emacs ready in 0.61 seconds with 2 garbage collections.

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


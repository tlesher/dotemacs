;; Emacs startup optimization:
;; Make startup faster by increasing the garbage collection threshold (thereby
;; reducing the frequency of garbage collection), then reduce again to make gc pauses faster.
;; The default gc threshold is 800 kilobytes.
;;
;; Last tests: (use emacs -nw)
;;
;; sb: Emacs ready in 1.20 seconds with 2 garbage collections.
;; bru: Emacs ready in 0.61 seconds with 2 garbage collections.

;; OUTDATED-REDO: calcifer: Emacs ready in 0.77 seconds with 8 garbage collections.


;; Use ALL the memory!
(setq tdl-orig-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda()
            (setq gc-cons-threshold tdl-orig-gc-cons-threshold)))

;; Print startup time.
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


(provide 'early-init-speedup)


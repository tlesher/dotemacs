;; Emacs startup optimization:
;; Make startup faster by increasing the garbage collection threshold (thereby
;; reducing the frequency of garbage collection), then reduce again to make gc pauses faster.
;; The default gc threshold is 800 kilobytes.
;; Last test:
;; sb: Emacs ready in 2.07 seconds with 18 garbage collections.

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

;; Return to original gc cons threshold after startup.
(setq tdl-init-gc-cons-threshold-multiplier 64
      tdl-orig-gc-cons-threshold gc-cons-threshold
      tdl-init-gc-cons-threshold (* tdl-init-gc-cons-threshold-multiplier
				    gc-cons-threshold)
      gc-cons-threshold tdl-init-gc-cons-threshold)

(message (format "Setting init gc threshold multiplier to %d." tdl-init-gc-cons-threshold-multiplier))

(add-hook 'emacs-startup-hook (lambda() (setq gc-cons-threshold tdl-orig-gc-cons-threshold)))

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


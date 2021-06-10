;;; init-ui.el --- Emacs UI initialization

;;; Commentary:

;;; Code:

;; UI setup code moved to early-init.el under emacs 27+
(when (>= 28 emacs-major-version)
  (menu-bar-mode 0)
  (with-demoted-errors
      ;; not present in emacs-nox
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      )
  (line-number-mode t)
  (column-number-mode t)
  (show-paren-mode 1)
  )

(setq visible-bell t)
(setq transient-mark-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)

(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'ws-highlight-tabs)
(add-hook 'font-lock-mode-hook 'ws-highlight-trailing-whitespace)

(global-linum-mode)
(global-set-key "\C-c\C-l" 'linum-mode)

;; Why, yes, I know what a scratch buffer is.
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "tlesher")

;; Jiggle the cursor after switching buffers, for better visibility.
;; (require 'jiggle)
;; (jiggle-mode 1)
;; (jiggle-searches-too 1)

(use-package fill-column-indicator
  :config
  (setq fci-rule-column 80)
  (setq fci-rule-color "#889988")
  (setq fci-rule-use-dashes t)
  (setq fci-dash-pattern .8)
  (add-hook 'python-mode-hook 'turn-on-fci-mode)
  (add-hook 'borg-mode-hook 'turn-on-fci-mode)
  (add-hook 'cc-mode-hook 'turn-on-fci-mode)
  (add-hook 'font-lock-mode-hook 'turn-on-fci-mode)
  )

;; Enable "dangerous" commands I use
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Don't go full ugly when using a theme on a low-fi terminal (like ssh).
(use-package color-theme-approximate
  :config
  (color-theme-approximate-on))

;; Use diminish mode to clean up modeline.
(require 'diminish)
(eval-after-load "jiggle" '(diminish 'jiggle-mode))

;; Shamelessly stolen and slightly modified from Jonathan Rockway in
;; g/emacs-users:
;; https://groups.google.com/a/google.com/d/msg/emacs-users/09pEJXszcJQ/_x3UGHvlFAAJ
;;
;; We need C-x C-c bound to s-b-k-t for emacsclient -t sessions, but when
;; it kills my main X session (with 9 windows or whatever), it is really
;; annoying.
;; (defadvice save-buffers-kill-terminal (around dont-kill-my-x-session-kthx)
;;   "Don't kill the emacs frame on C-x C-c."
;;   (if (or (eq window-system 'x) (eq window-system 'w32))
;;       (if (bound-and-true-p server-clients)
;;           (apply 'server-switch-buffer (server-done))
;;         (message
;;          (format "I'm afraid I can't do that, %s." (user-login-name))))
;;     ad-do-it))
;; (ad-activate 'save-buffers-kill-terminal)

(provide 'init-ui)

;;; init-ui.el ends here

;;; init-ui.el --- Emacs UI initialization

;;; Commentary:

;;; Code:

;; UI setup code moved to early-init.el under emacs 27+
(when (>= 28 emacs-major-version)
  (load-file (concat user-emacs-directory "/init/early-init-ui.el")))

(setq visible-bell t)
(setq transient-mark-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)

(use-package highlight-chars
  :config
  (add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)
  (add-hook 'font-lock-mode-hook 'hc-highlight-tabs))

(use-package hide-lines
  :config
  (global-set-key "\C-ch" 'hide-lines)
  (global-set-key "\C-c\C-h" 'hide-lines-show-all)
  ;; TODO(tlesher): for some reason, hide-lines fails if used before
  ;; hide-lines-show-all.
  (hide-lines-show-all))

(global-linum-mode)
(global-set-key "\C-c\C-l" 'linum-mode)

;; Why, yes, I know what a scratch buffer is.
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "tlesher")

;; Jiggle the cursor after switching buffers, for better visibility.
(use-package jiggle
  :defines jiggle-mode
  :commands jiggle-mode
  :diminish
  :config
  (jiggle-mode 1)
  (jiggle-searches-too 1))

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

(provide 'init-ui)

;;; init-ui.el ends here

;;; Emacs UI initialization
;;; Loaded early to avoid annoying flicker

(menu-bar-mode 0)
(ignore-errors ;; not present in emacs-nox
  (scroll-bar-mode 0)
  (tool-bar-mode 0))
(line-number-mode t)
(column-number-mode t)
(show-paren-mode 1)
(setq visible-bell t)
(setq transient-mark-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)

(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'ws-highlight-tabs)
(add-hook 'font-lock-mode-hook 'ws-highlight-trailing-whitespace)

(global-linum-mode)
(global-set-key "\C-c\C-l" 'linum-mode)

;; (require 'color-theme)
;; (load-theme 'zenburn)

(require 'jiggle)
(jiggle-mode 1)
(jiggle-searches-too 1)

(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-color "#889988")
(setq fci-rule-use-dashes 1)
(setq fci-dash-pattern .2)
(add-hook 'python-mode-hook 'turn-on-fci-mode)
(add-hook 'borg-mode-hook 'turn-on-fci-mode)
(add-hook 'cc-mode-hook 'turn-on-fci-mode)
;; (add-hook 'font-lock-mode-hook 'turn-on-fci-mode)

;; Enable "dangerous" commands I use
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Use diminish mode to clean up modeline.
(require 'diminish)
(eval-after-load "jiggle" '(diminish 'jiggle-mode))

(provide 'init-ui)

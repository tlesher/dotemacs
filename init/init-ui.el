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

(require 'color-theme)
(load-theme 'zenburn)

;; Enable "dangerous" commands I use
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(provide 'init-ui)

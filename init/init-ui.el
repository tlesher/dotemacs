;;; Emacs UI initialization
;;; Loaded early to avoid annoying flicker

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(line-number-mode t)
(column-number-mode t)
(show-paren-mode 1)
(setq visible-bell t)
(setq transient-mark-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)

(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-trailing-whitespace)

(global-linum-mode)
(global-set-key "\C-c\C-l" 'linum-mode)

;; Enable "dangerous" commands I use
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; "Special" display quarantining
;; Stolen from https://github.com/garybernhardt/dotfiles/blob/master/.emacs
(setq special-display-regexps
        '("^\\*Completions\\*$"
          "^\\*Help\\*$"
          "^\\*grep\\*$"
          "^\\*Apropos\\*$"
          "^\\*elisp macroexpansion\\*$"
          "^\\*local variables\\*$"
          "^\\*Compile-Log\\*$"
          "^\\*Quail Completions\\*$"
          "^\\*Occur\\*$"
          "^\\*frequencies\\*$"
          "^\\*compilation\\*$"
          "^\\*Locate\\*$"
          "^\\*Colors\\*$"
          "^\\*tumme-display-image\\*$"
          "^\\*SLIME Description\\*$"
          "^\\*.* output\\*$"           ; tex compilation buffer
          "^\\*TeX Help\\*$"
          "^\\*Shell Command Output\\*$"
          "^\\*Async Shell Command\\*$"
          "^\\*Backtrace\\*$"))
(setq grb-temporary-window (nth 2 (window-list)))
(defun grb-special-display (buffer &optional data)
  (let ((window grb-temporary-window))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))
(setq special-display-function #'grb-special-display)

(provide 'init-ui)

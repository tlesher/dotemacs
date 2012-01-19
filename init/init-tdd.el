;; Random things to make Python TDD a little nicer

; Sorta ganked from Gary Bernhardt
; https://github.com/garybernhardt/dotfiles/blob/master/.emacs

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

(defun display-in-temp-window (buffer &optional data)
  (let ((window temp-window))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))

(defun tdd-split ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally 80)
  (split-window)
  (other-window 1)
  (eshell)
  (other-window -3)
  (setq temp-window (car (window-list)))
  (setq special-display-function #'display-in-temp-window))

(defun tdd-unsplit ()
  (interactive)
  (delete-other-windows)
  (setq special-display-function #'special-display-popup-frame))

(provide 'init-tdd)

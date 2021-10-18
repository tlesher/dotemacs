(progn
  (menu-bar-mode 0)
  (when (fboundp 'scroll-bar-mode) ; not present in emacs-nox.
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)  ; not present in emacs-nox.
    (tool-bar-mode 0))
  (line-number-mode t)
  (column-number-mode t)
  (show-paren-mode 1))

;; This file is loaded before init.el in Emacs 27+.

(menu-bar-mode 0)
(with-demoted-errors
    ;; not present in emacs-nox
    (scroll-bar-mode 0)
  (tool-bar-mode 0)
  )
(line-number-mode t)
(column-number-mode t)
(show-paren-mode 1)

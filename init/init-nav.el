(add-to-list 'load-path (concat user-emacs-directory "lisp/emacs-nav-49"))

(autoload 'nav-toggle "nav" "Toggles the nav panel." t)
(global-set-key [f8] 'nav-toggle)

(provide 'init-nav)

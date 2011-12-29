;; Load deft if available
(when (require 'deft nil 'noerror)
  (setq
   deft-extension "rst"
   deft-directory "~/.emacs.d/deft/"
   deft-text-mode 'rst-mode)
  (global-set-key (kbd "<f9>") 'deft))

(provide 'init-deft)

;; This file is loaded before init.el in Emacs 27+.

;; Add all the .emacs.d subdirectories that will contain elisp to the load-path.
(dolist (path '("glisp" "lisp" "init"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

(require 'early-init-speedup)
(require 'early-init-ui)

;; (load-file (concat user-emacs-directory "/init/early-init-ui.el"))
;; (load-file (concat user-emacs-directory "/init/erarly-init-speedup.el"))

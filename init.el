(require 'cl)
(defvar *emacs-load-start* (current-time))

(labels 
    ((add-path (p)
               (add-to-list 'load-path (concat user-emacs-directory p))))
  (add-path "lisp")
  (add-path "init")
  (add-path "lisp/jira"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'init-ui)
(require 'init-autocomplete)
(require 'init-deft)
(require 'init-fill)
(ignore-errors (require 'init-org))
(require 'init-python)
(require 'init-utils)
(ignore-errors (require 'init-google))
(require 'init-nav)
(require 'init-p4)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; Disambiguate buffers visiting files with the same name
(require 'uniquify) 
(setq uniquify-buffer-name-style 'forward)

(when (load "flymake" t)
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  (require 'flymake-cursor)
  (global-set-key [f10] 'flymake-goto-prev-error)
  (global-set-key [f11] 'flymake-goto-next-error)
  
  ;; Instead of raising a dialog, just warn via *Messages*
  (defun flymake-display-warning (warning)
    (message warning))
  )

;; server-running-p returns ":other" on win32 if it's not sure,
;; so don't just check (unless (server-running-p))
(require 'server)
(unless (eq (server-running-p) 't) (server-start))

;;; Hide-lines
(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
(global-set-key "\C-ch" 'hide-lines)
(global-set-key "\C-c\C-h" 'show-all-invisible)

(ido-mode 1)


;; Use ibuffer as a better list-buffers
;; from http://xahlee.org/emacs/effective_emacs.html
(defalias 'list-buffers 'ibuffer)

;; Some aliases for search/replace
;; from https://sites.google.com/site/steveyegge2/effective-emacs
(defalias 'rs 'replace-string)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

(setq make-backup-files nil)

(setq c-basic-offset 4)

;; Add commonly-used files in registers, so I can C-x r j e to get to
;; init.el, for example.
;(set-register ?e '(file . (concat user-emacs-directory "init.el")))
(set-register ?e '(file . "~/.emacs.d/init.el"))

;; No tabs.
(setq-default indent-tabs-mode nil)

;; Enable "dangerous" commands I use
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key "\C-x\C-l" 'sort-lines)
(global-set-key "\C-x\C-z" 'fixup-whitespace)
(global-set-key [?\C-x ?\C-j] 'find-file-at-point)
(global-set-key [f5] 'compile)

(global-set-key [M-down] 'next-error)
(global-set-key [M-up] '(lambda () (interactive) (next-error -1)))

;; Avoid minimizing when I accidentally C-z
(global-unset-key [?\C-z])
(global-unset-key [?\C-x ?\C-z])

;; Quick window switching
(global-set-key "\C-o" 'other-window)
(defun prev-window()
  (interactive)
  (other-window -1))
(global-set-key "\M-o" 'prev-window)

(if (eq system-type 'windows-nt)
    (progn
      (defun explorer () 
        "Launch Windows Explorer in current directory and select current file"
        (interactive)
        (w32-shell-execute "open" "explorer"
                           (concat "/e,/select," (convert-standard-filename
                                                  buffer-file-name))))
      (global-set-key [f12] 'explorer)))

;; What has it gots in its packages?
(when (boundp 'package-archives)
  (add-to-list 'package-archives
               '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/")))


;; server-running-p returns ":other" on win32 if it's not sure,
;; so don't just check (unless (server-running-p))
(require 'server)
(unless (eq (server-running-p) 't) (server-start))

(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms)
             (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))

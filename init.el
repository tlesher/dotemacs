
(require 'cl)
(defvar *emacs-load-start* (current-time))

(labels ((add-path (p)
                   (add-to-list 'load-path (concat user-emacs-directory p))))
  (add-path "lisp")
  (add-path "init"))

(require 'init-autocomplete)
(require 'init-deft)
(require 'init-fill)
(require 'init-org)
(require 'init-python)
(require 'init-utils)
(require 'init-vocollect)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

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

(global-linum-mode)
(global-set-key "\C-c\C-l" 'linum-mode)

;; Use ibuffer as a better list-buffers
;; from http://xahlee.org/emacs/effective_emacs.html
(defalias 'list-buffers 'ibuffer)

;; Some aliases for search/replace
;; from https://sites.google.com/site/steveyegge2/effective-emacs
(defalias 'rs 'replace-string)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

;; Commented out because it causes init.el to load slowly
;; Appears to be caused by p4-mode trying to load all help
;; strings from the P4 server at initialization.
;; (require 'p4)

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

(global-set-key [?\C-x ?\C-j] 'find-file-at-point)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key [f5] 'compile)

;; More useful frame title
(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

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


(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms)
             (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))

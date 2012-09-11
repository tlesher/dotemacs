(require 'cl)
(defvar *emacs-load-start* (float-time))

(defun mark-load-time (comment)
  "Print a message with the current elapsed load time.
Use for debugging why emacs is slow to start."
  (message "%s: %.2fs" comment (- (float-time) *emacs-load-start*)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(labels
    ((add-path (p)
               (add-to-list 'load-path (concat user-emacs-directory p))))
  (add-path "lisp")
  (add-path "init"))

(require 'init-ui)
(require 'init-autocomplete)
(require 'init-deft)
(require 'init-fill)
(require 'init-org)
(require 'init-python)
(require 'init-utils)
(ignore-errors (require 'init-google))
(require 'init-nav)
(require 'init-p4)
(require 'init-flymake)
(require 'init-windows)

(require 'ack)
;; create the autosave dir if necessary, since emacs won't.
;; Do this after loading custom.el.
(make-directory "~/.emacs.d/tmp/autosaves/" t)

;;; Disambiguate buffers visiting files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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
(set-register ?e '(file . "~/.emacs.d/init.el"))

;; No tabs.
(setq-default indent-tabs-mode nil)

;; Random keybindings
;; TODO: figure out how to avoid clashing with the bindings in
;; init-google.
(global-set-key [f5] 'compile)
(global-set-key [(shift f5)] 'recompile)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key "\C-x\C-l" 'sort-lines)
(global-set-key "\C-x\C-z" 'fixup-whitespace)
(global-set-key [M-down] 'next-error)
(global-set-key [M-up] '(lambda () (interactive) (next-error -1)))
(global-set-key (kbd "C-S-f") 'find-file-at-point)
(global-set-key [?\C-z] 'undo)
;; Quick window switching; from GRB
;; (https://github.com/garybernhardt/dotfiles/blob/master/.emacs)
(global-set-key "\C-o" 'other-window)
(defun prev-window()
  (interactive)
  (other-window -1))
(global-set-key "\M-o" 'prev-window)
;; use windmove to switch between buffers and buffer-move to throw
;; them around.
(windmove-default-keybindings)
(require 'buffer-move)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)

;; I don't always (browse-url-of-buffer), but when I do, I prefer to use
;; Chrome.
(when (executable-find "google-chrome")
  (setq browse-url-generic-program (executable-find "google-chrome")
        browse-url-browser-function 'browse-url-generic))

;; What has it gots in its packages?
(when (boundp 'package-archives)
  (add-to-list 'package-archives
               '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/")))

(message ".emacs loaded in %.2fs" (- (float-time) *emacs-load-start*))

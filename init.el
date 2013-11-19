(require 'cl)
(defvar *emacs-load-start* (float-time))

;; For future reference: emacs startup in graphics mode on tlesher.pit
;; with init-google as of 2013-05-22 is 1.58-2.1s.

(defun mark-load-time (comment)
  "Print a message with the current elapsed load time.
Use for debugging slow emacs startup."
  (message "%s: %.2fs" comment (- (float-time) *emacs-load-start*)))

;; Load custom early.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Extend load path to select .emacs.d subdirectories.
(labels
    ((add-path (p)
               (add-to-list 'load-path (concat user-emacs-directory p))))
  (add-path "lisp")
  (add-path "lisp/yasnippet")
  (add-path "init")
  (add-path "lisp/iedit") ;; EXPERIMENTAL!
  )

(require 'init-archive-messages)
(require 'init-ui)
(require 'init-autocomplete)
(require 'init-deft)
(require 'init-fill)
(require 'init-org)
(require 'init-python)
(require 'init-utils)
(ignore-errors (require 'init-google))  ; don't crash when running
                                        ; outside teh gewgols.
(require 'init-nav)
(require 'init-p4)
(require 'init-flymake)
(require 'init-windows)

;;;; Miscellaneous settings.  Move these to init-* modules when they
;;;; grow large enough to stand on their own.

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Load yasnippet if it's available.
(ignore-errors
  (require 'yasnippet)
  (yas-global-mode 1)
  (setq ac-source-yasnippet nil))

;; create the autosave dir if necessary, since emacs won't.
;; Do this after loading custom.el. 
(make-directory "~/.emacs.d/tmp/autosaves/" t)

;;; Disambiguate buffers visiting files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(add-to-list 'uniquify-list-buffers-directory-modes 'shell-mode)
(add-to-list 'uniquify-list-buffers-directory-modes 'term-mode)
(add-to-list 'uniquify-list-buffers-directory-modes 'compilation-mode)

(require 'server)
;; server-running-p returns ":other" on win32 if it's not sure,
;; so don't just check (unless (server-running-p))
(unless (eq (server-running-p) 't) (server-start))
(defun server-start-dammit ()
  "Kill any existing server and start a new one in this process."
  (interactive)
  (server-force-delete)
  (server-start))

;;; Hide-lines
(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
(global-set-key "\C-ch" 'hide-lines)
(global-set-key "\C-c\C-h" 'show-all-invisible)

;; Whack ido mode for now... it's misbehaving while homedir is offline.
;; (defun ami-ido-yank ()
;;   (interactive)
;;   (insert-string
;;    (ido-completing-read "Yank what? "
;;                         (mapcar 'substring-no-properties kill-ring))))
;; (global-set-key "\C-cy" 'ami-ido-yank)

;; (ido-mode 1)
;; ;; Disable auto searching for files unless called explicitly.
;; ;;(setq ido-auto-merge-delay-time 99999)
;; (define-key ido-file-dir-completion-map (kbd "C-c C-s")
;;   (lambda()
;;     (interactive)
;;     (ido-initiate-auto-merge (current-buffer))))


;; Use ibuffer as a better list-buffers
;; from http://xahlee.org/emacs/effective_emacs.html
(defalias 'list-buffers 'ibuffer)

;; Some aliases for search/replace
;; from https://sites.google.com/site/steveyegge2/effective-emacs
(defalias 'rs 'replace-string)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'fg 'grep-find)
(defalias 'gf 'find-grep) ;; LOL

(setq make-backup-files nil)

(setq c-basic-offset 4)

;; Add commonly-used files in registers, so I can C-x r j e to get to
;; init.el, for example.
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?a '(file . "~/.config/awesome/rc.lua"))

;; No tabs.
(setq-default indent-tabs-mode nil)

;; Random keybindings
;; TODO: figure out how to avoid clashing with the bindings in
;; init-google.
(global-set-key [f5] 'compile)
(global-set-key [(shift f5)] 'recompile)
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-x\C-l" 'sort-lines)

(defun mn-just-one-space ()
  "When called for the first time Works just like `just-one-space'.
When called second time deletes all spaces, tabs and new lines after
the point."
  (interactive)
  (just-one-space (if (equal last-command this-command) -1 1)))

(defun tl-delete-newline-and-fixup-whitespace ()
  (interactive "*")
  (save-excursion
    (end-of-line)
    (delete-forward-char 1)
    (fixup-whitespace)))
(global-set-key "\C-x\C-x" 'delete-newline-and-fixup-whitespace)

(substitute-key-definition 'just-one-space 'mn-just-one-space
                           (current-global-map))
(global-set-key "\C-x\C-z" 'mn-just-one-space)
(global-set-key "\C-x\C-x" 'fixup-whitespace)
(global-set-key [M-down] 'next-error)
(global-set-key [M-up] '(lambda () (interactive) (next-error -1)))
;; Just like M-down/M-up, but for nonwindowed Emacs
(global-set-key (kbd "ESC <down>") 'next-error)
(global-set-key (kbd "ESC <up>") '(lambda () (interactive) (next-error -1)))
(global-set-key (kbd "C-S-f") 'find-file-at-point)
(global-set-key "\C-cf" 'find-file-at-point)
(global-set-key [?\C-z] 'undo)

;; Trying out some ESC ESC <key> key bindings for key bindings that
;; don't work on a tty.
(global-set-key (kbd "ESC ESC c")  'recompile)

;; use windmove to switch between buffers and buffer-move to throw
;; them around.
(windmove-default-keybindings)
(require 'buffer-move)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
;; This set of keys stinks on ice: ESC Ctrl-left, etc.
(global-set-key (kbd "ESC M-[ d") 'buf-move-left)
(global-set-key (kbd "ESC M-[ c") 'buf-move-right)
(global-set-key (kbd "ESC M-[ a") 'buf-move-up)
(global-set-key (kbd "ESC M-[ b") 'buf-move-down)
;; (global-set-key (kbd "M-[ a") 'forward-paragraph)
;; (global-set-key (kbd "M-[ b") 'back-

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
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("GELPA" . "http://internal-elpa.appspot.com/packages/")))

(setq bookmark-default-file "~/.emacs.d/bookmarks")
(setq bookmark-save-flag 1)

(defun insert-todo (arg)
  "Insert '// TODO(username): ' at point."
  (interactive "*P")
  (let ()
    (comment-dwim arg)
    (insert "TODO(" (user-login-name) "): \n")
    (forward-char -1)))
(global-set-key "\C-ct" 'insert-todo)


;; Neat hack from http://whattheemacsd.com/setup-shell.el-01.html:
;; In shell-mode, second press of C-d kills shell buffer.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; Abbrevs.
(setq default-abbrev-mode t)
(setq abbrev-file-name "~/.emacs.d/abbrev-defs" )
(setq save-abbrevs t)
(quietly-read-abbrev-file)

;; Key frequency mode
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; EXPERIMENTS MAY BITE
(require 'iedit)
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))
(global-set-key (kbd "C-;") 'iedit-dwim)
;; END EXPERIMENTS

(message ".emacs loaded in %.2fs" (- (float-time) *emacs-load-start*))

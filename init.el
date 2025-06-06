;; Emacs startup optimization:
;; Make startup faster by increasing the garbage collection threshold (thereby
;; reducing the frequency of garbage collection), then reduce again to make gc pauses faster.
;; The default gc threshold is 800 kilobytes.
;; Last test:
;; sb: Emacs ready in 2.07 seconds with 18 garbage collections.
;; calcifer: Emacs ready in 0.77 seconds with 8 garbage collections.

;; bru: Emacs ready in 0.73 seconds with 20 garbage collections.

;; bru@1 0.98/33
;; bru@2 0.98/33
;; bru@4 0.93/27
;; bru@8 0.87/24
;; bru@16 0.90/22
;; bru@32 0.77/21
;; bru@64 0.73/20
;; bru@128 0.77/20

(setq tdl-init-gc-cons-threshold-multiplier 64
      tdl-orig-gc-cons-threshold gc-cons-threshold
      tdl-init-gc-cons-threshold (* tdl-init-gc-cons-threshold-multiplier
				    gc-cons-threshold)
      gc-cons-threshold tdl-init-gc-cons-threshold)

(message (format "Setting init gc threshold multiplier to %d." tdl-init-gc-cons-threshold-multiplier))

(add-hook 'emacs-startup-hook (lambda() (setq gc-cons-threshold tdl-orig-gc-cons-threshold)))

;; Print startup time.
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Extend load path to select .emacs.d subdirectories.
(require 'cl-lib)

(cl-labels
    ((add-path (p)
       (add-to-list 'load-path (concat user-emacs-directory p))))
  (add-path "glisp")
  (add-path "lisp")
  (add-path "init"))

;; Uncomment to measure time for require statements. Keep this line before any
;; other require calls
;; (require 'timed-require)

;; Load custom early.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)


;; What has it gots in its packages?
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; TODO(tlesher): consider straight.el.

;; We need at least use-package to continue initializing.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(require 'use-package)

;; Force installation of packages used in use-package declarations.
(use-package diminish :ensure)
(use-package bind-key :ensure)
(use-package vterm :ensure)

;; TODO(tlesher): look at auto-complete or company mode

(require 'init-archive-messages)
(require 'init-ui)
(require 'init-fill)
(require 'init-flymake)
(require 'init-python)
(require 'init-utils)
(if (not (require 'init-google nil t))
    (message "init-google.el not found; skipping Google-specific configuration."))
;; (require 'init-org)
(require 'init-nav)
(require 'init-windows)
(require 'init-tkeys)

;;(require 'helm-config)

;; Prefer newer .el to older .elc, if both exist.
(setq load-prefer-newer t)

;;;; Miscellaneous settings.  Move these to init-* modules when they
;;;; grow large enough to stand on their own.

;; create the autosave dir if necessary, since emacs won't.
;; Do this after loading custom.el.
(make-directory (concat user-emacs-directory "tmp/autosaves/") t)

;;; Disambiguate buffers visiting files with the same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (add-to-list 'uniquify-list-buffers-directory-modes 'shell-mode)
  (add-to-list 'uniquify-list-buffers-directory-modes 'term-mode)
  (add-to-list 'uniquify-list-buffers-directory-modes 'compilation-mode))

(use-package projectile
  :ensure
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package server
  :config
  ;; server-running-p returns ":other" on win32 if it's not sure,
  ;; so don't just check (unless (server-running-p))
  (unless (eq (server-running-p) 't) (server-start))
  (defun server-start-dammit ()
    "Kill any existing server and start a new one in this process."
    (interactive)
    (server-force-delete)
    (server-start)))

(use-package crux
  :ensure)

;; Retrying ido mode based on http://www.masteringemacs.org/article/introduction-to-ido-mode.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

;; Ensure tmux passes through xterm keys.
(defadvice terminal-init-screen
    ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
    (before tmux activate)
  ;; Docstring.  This describes the advice and is made available inside emacs;
  ;; for example when doing C-h f terminal-init-screen RET
  "Apply xterm keymap, allowing use of keys passed through tmux."
  ;; This is the elisp code that is run before `terminal-init-screen'.
  (if (getenv "TMUX")
      (let ((map (copy-keymap xterm-function-map)))
        (set-keymap-parent map (keymap-parent input-decode-map))
        (set-keymap-parent input-decode-map map))))

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
(defalias 'ffow 'find-file-other-window)

(setq make-backup-files nil)

(setq c-basic-offset 4)

;; Add commonly-used files in registers, so I can C-x r j e to get to
;; init.el, for example.
(set-register ?e `(file . ,(concat user-emacs-directory "init.el")))
(set-register ?g `(file . ,(concat
			    user-emacs-directory "init/init-google.el")))
;; TODO: This is temporary, while I'm getting org-mode up to speed.
(set-register ?o `(file . ,(concat user-emacs-directory "init/init-org.el")))

;; No tabs.
(setq-default indent-tabs-mode nil)

;; Random keybindings
;; TODO: figure out how to avoid clashing with the bindings in
;; init-google.
(global-set-key [f5] 'compile)
(global-set-key [(shift f5)] 'recompile)
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-x\C-l" 'sort-lines)

(global-set-key (kbd "C-x g") 'browse-url-at-point)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "M-RET") 'comment-indent-new-line)

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
    (delete-char 1)
    (fixup-whitespace)))
(global-set-key "\C-x\C-x" 'tl-delete-newline-and-fixup-whitespace)
(substitute-key-definition 'just-one-space 'mn-just-one-space
                           (current-global-map))
(global-set-key "\C-x\C-z" 'mn-just-one-space)
;;(global-set-key "\C-x\C-x" 'fixup-whitespace)

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

;; ;; Set up browse-url-of-buffer and friends.
;; (cond
;;  ;; Use host browser when running on a chromebook vm
;;  ((executable-find "garcon-url-handler")
;;   (setq browse-url-generic-program (executable-find "garcon-url-handler")
;;         browse-url-browser-function 'browse-url-generic))
;;  ;; Otherwise look for Chrome
;;  ((executable-find "google-chrome")
;;   (setq browse-url-generic-program (executable-find "google-chrome")
;;         browse-url-browser-function 'browse-url-generic)))

(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))
(setq bookmark-save-flag 1)

;; golang stuff
(add-to-list 'exec-path "/usr/local/go/bin")
(add-hook 'go-mode-hook 'lsp-deferred)

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
(setq abbrev-mode t)
(setq abbrev-file-name (concat user-emacs-directory "abbrev-defs" ))
(setq save-abbrevs t)
(quietly-read-abbrev-file)

;; Key frequency mode
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; EXPERIMENTS MAY BITE

(add-hook
 'c++-mode-hook
 '(lambda()
    ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
    ;; matters.
    (font-lock-add-keywords
     nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; namespace names and tags - these are rendered as constants by cc-mode
           ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
           ;;  new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
           ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
           ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
           ;; c++11 string literals
           ;;       L"wide string"
           ;;       L"wide string with UNICODE codepoint: \u2018"
           ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
           ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
           ;;       R"(user-defined literal)"
           ;;       R"( a "quot'd" string )"
           ;;       R"delimiter(The String Data" )delimiter"
           ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
           ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
           (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

           ;; user-defined types (rather project-specific)
           ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
    ) t)

;; From http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Opens file at line from emacsclient.
;; From https://stackoverflow.com/a/23857738
(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "looks for filenames like file:line or file:line:position and reparses name in such manner that position in file"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) "")))
                               )
                            fn))) files))
  )

;; END EXPERIMENTS

(require 'cl)
(defvar *emacs-load-start* (float-time))

;; Set to t to print time for each (require) in this file.
(defvar *timed-require-enabled* nil)

;; For future reference: emacs startup in graphics mode on badwolf.pit
;; with init-google as of 2017-08-25 is 1.28-1.38s.
;;
;; TODO: wtf, init-google?
;; with init-google as of 2018-01-17 is 2.77s
;;
;; require use-package: 0.20s (0.00s)
;; require diminish: 0.20s (0.00s)
;; require bind-key: 0.20s (0.00s)
;; require init-archive-messages: 0.20s (0.00s)
;; require init-ui: 0.23s (0.02s)
;; require init-fill: 0.23s (0.00s)
;; Loading flymake...
;; Loading flymake...done
;; require init-flymake: 0.26s (0.03s)
;; require init-python: 0.26s (0.00s)
;; require init-rust: 0.27s (0.01s)
;; require init-utils: 0.27s (0.00s)
;; require init-google: 2.70s (2.43s)
;; require init-org: 2.71s (0.01s)
;; require init-nav: 2.71s (0.01s)
;; require init-p4: 2.72s (0.01s)
;; require init-windows: 2.73s (0.01s)
;; require init-tkeys: 2.73s (0.01s)
;; require helm-config: 2.75s (0.01s)
;; require uniquify: 2.75s (0.00s)
;; require server: 2.75s (0.00s)
;; require buffer-move: 2.76s (0.01s)
;; require keyfreq: 2.77s (0.01s)
;; .emacs loaded in 2.77s


(defun mark-load-time (comment)
  "Print a message with the current elapsed load time.
Use for debugging slow emacs startup."
  (message "%s: %.2fs" comment (- (float-time) *emacs-load-start*)))

(defun timed-require (library)
  "Print a message around a require showing load time."
  (let ((start-time (float-time)))
    (require library)
    (when *timed-require-enabled*
      (message "require %s: %.2fs (%.2fs)" (symbol-name library) (- (float-time) *emacs-load-start*) (- (float-time) start-time)))))

;; Load custom early.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Extend load path to select .emacs.d subdirectories.
(labels
    ((add-path (p)
               (add-to-list 'load-path (concat user-emacs-directory p))))
  (add-path "glisp")
  (add-path "lisp")
  (add-path "lisp/use-package")
  (add-path "init")
  (add-path "lisp/iedit") ;; EXPERIMENTAL!
  )

;; What has it gots in its packages?
(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(eval-when-compile
  (timed-require 'use-package))
(timed-require 'diminish)                ;; if you use :diminish
(timed-require 'bind-key)                ;; if you use any :bind variant

;; TODO(tlesher): look at auto-complete or company mode

(timed-require 'init-archive-messages)
(timed-require 'init-ui)
(timed-require 'init-fill)
(timed-require 'init-flymake)
(timed-require 'init-python)
(timed-require 'init-rust)
(timed-require 'init-utils)

;; don't crash when running outside teh gewgols.
(with-demoted-errors
    (timed-require 'init-google))
(timed-require 'init-org)

(timed-require 'init-nav)
(timed-require 'init-p4)
(timed-require 'init-windows)
(timed-require 'init-tkeys)

(timed-require 'helm-config)

;;;; Miscellaneous settings.  Move these to init-* modules when they
;;;; grow large enough to stand on their own.

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; create the autosave dir if necessary, since emacs won't.
;; Do this after loading custom.el.
(make-directory "~/.emacs.d/tmp/autosaves/" t)

;;; Disambiguate buffers visiting files with the same name
(timed-require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(add-to-list 'uniquify-list-buffers-directory-modes 'shell-mode)
(add-to-list 'uniquify-list-buffers-directory-modes 'term-mode)
(add-to-list 'uniquify-list-buffers-directory-modes 'compilation-mode)

(timed-require 'server)
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
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?a '(file . "~/.config/awesome/rc.lua"))
(set-register ?g '(file . "~/.emacs.d/init/init-google.el"))
(set-register ?n '(file . "~/x/nthings.org"))

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

;; use windmove to switch between buffers and buffer-move to throw
;; them around.
(windmove-default-keybindings)
(timed-require 'buffer-move)
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

(setq bookmark-default-file "~/.emacs.d/bookmarks")
(setq bookmark-save-flag 1)


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
(setq abbrev-file-name "~/.emacs.d/abbrev-defs" )
(setq save-abbrevs t)
(quietly-read-abbrev-file)

;; Key frequency mode
(timed-require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; EXPERIMENTS MAY BITE

;; (timed-require 'iedit)
;; (defun iedit-dwim (arg)
;;   "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
;;   (interactive "P")
;;   (if arg
;;       (iedit-mode)
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         ;; this function determines the scope of `iedit-start'.
;;         (if iedit-mode
;;             (iedit-done)
;;           ;; `current-word' can of course be replaced by other
;;           ;; functions.
;;           (narrow-to-defun)
;;           (iedit-start (current-word) (point-min) (point-max)))))))
;; (global-set-key (kbd "C-;") 'iedit-dwim)

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


;; END EXPERIMENTS


(message ".emacs loaded in %.2fs" (- (float-time) *emacs-load-start*))

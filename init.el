
(require 'cl)
(defvar *emacs-load-start* (current-time))

(labels ((add-path (p)
         (add-to-list 'load-path (concat "~/.emacs.d/" p))))
  (add-path "lisp")
  (add-path "init")
)

(require 'init-vocollect)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'server)
(unless (server-running-p) (server-start))

(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
(global-set-key "\C-ch" 'hide-lines)
(global-set-key "\C-c\C-h" 'show-all-invisible)

(ido-mode 1)

(global-linum-mode)

;; Commented out because it causes init.el to load slowly
;; Appears to be caused by p4-mode trying to load all help
;; strings from the P4 server at initialization.
;(require 'p4)

(setq-default org-startup-indented t)
(org-indent-mode t)

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
(set-register ?e '(file . "~/.emacs.d/init.el"))

;; Load deft if available
(when (require 'deft nil 'noerror)
  (setq
   deft-extension "rst"
   deft-directory "~/.emacs.d/deft/"
   deft-text-mode 'rst-mode)
  (global-set-key (kbd "<f9>") 'deft))


;; No tabs.
(setq-default indent-tabs-mode nil)

;; Enable "dangerous" commands I use
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key [?\C-x ?\C-j] 'find-file-at-point)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; More useful frame title
(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Avoid minimizing when I accidentally C-z
(global-unset-key [?\C-z])
(global-unset-key [?\C-x ?\C-z])

(if (eq system-type 'windows-nt)
    (progn 
      (defun explorer () "Launch the windows explorer in the current directory and selects current file" 
        (interactive)
        (w32-shell-execute "open" "explorer"
                           (concat "/e,/select," (convert-standard-filename
                                                  buffer-file-name))))
      (global-set-key [f12] 'explorer)))

; TODO: Figure out how to tell c-mode about map macros:
; BEGIN_MSG_MAP(), MESSAGE_HANDLER(), END_MSG_MAP, etc.
(c-add-style "vocollect" vocollect-c-style)


;; Python mode settings
(defun tdl-python-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq py-indent-offset 4)
  (define-key python-mode-map "\C-m" 'newline-and-indent))
(add-hook 'python-mode-hook 'tdl-python-mode-setup)

;; pymacs
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")

;; Bicycle repair man
;(pymacs-load "bikeemacs" "brm-")
;(brm-init)

; (pymacs-load "ropemacs" "rope-")
;; (defun load-ropemacs ()
;;   "Load pymacs and ropemacs"
;;   (interactive)
;;   (require 'pymacs)
;;   (pymacs-load "ropemacs" "rope-")
;;   ;; Automatically save project python buffers before refactorings
;;   (setq ropemacs-confirm-saving 'nil)
;;   )


;(add-to-list 'load-path "~/.site-lisp/pycomplexity/")

;(require 'pycomplexity)
;(add-hook 'python-mode-hook
;          (function (lambda ()
;                      (pycomplexity-mode)
;                      (linum-mode))))

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

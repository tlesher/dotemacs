(setq-default org-startup-indented t)
(org-indent-mode t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; For some reason org-jira expects this function, which doesn't
;; appear to exist on Emacs 24 for Windows.
;;(when (eq system-type 'windows-nt)
;;  (defun browse-url-default-browser (url)
;;    (interactive)
 ;;   (browse-url-default-windows-browser url))
;;)

(require 'org-jira)

(provide 'init-org)

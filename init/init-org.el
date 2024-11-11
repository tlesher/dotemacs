(setq-default org-startup-indented t)
(setq org-indent-mode t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)" "SUPERSEDED(s)")))
(setq org-link-abbrev-alist
      '(("go" . "http://goto.corp.google.com/"))
      )

(setq org-capture-templates
      (doct '(("Journal" :keys "j"
               :file "~/org/journal.org"
               :function org-reverse-datetree-goto-date-in-file
               :template ("* 📆 Schedule"
                          ""
                          ""
                          "| Time      | Event                    | Location |"
                          "|-----------+--------------------------+----------|"
                          "* 🔥 Commits"
                          "** TODO"
                          ""
                          "* ✅ [[file:$HOME/org/todo.org][To Do]]"
                          ""
                          "* 🚫 Not To Do"
                          "- [ ] "
                          ""
                          "* 🏁 WIP Wrapup (where I left off)"
                          "** TODO SHUT DOWN"
                          ""
                          "* 📝 Notes"
                          "** ."
                          )))))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c t") 'org-tidy-toggle))

(global-set-key "\C-c1"
                (org-capture :KEYS "j"))

;; org-tidy looks nice but it seems to totally screw up drawer show/hide.

(use-package org-tidy
    :ensure t
    :config
    (add-hook 'org-mode-hook #'org-tidy-mode))
;; TODO figure out why tidy doesn't work on new files.


(use-package git-auto-commit-mode
  :ensure t)

(use-package doct
  :ensure t)

(provide 'init-org)

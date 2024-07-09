(setq-default org-startup-indented t)
(setq org-indent-mode t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)




(defun open-journal-file ()
  (let* ((today (format-time-string "%Y-%m-%d"))
         (path (concat (getenv "HOME") "/org/journal/" (format-time-string "%Y-%m-%d") ".org"))
         (hdr-list (list
                    ":PROPERTIES:"
                    (concat ":ID: journal-" (format-time-string "%Y-%m-%d"))
                    ":END:"
                    ""
                    (concat "#+title: " (format-time-string "%Y-%m-%d (%A)"))
                    ""
                    (concat "[[https://calendar.google.com/calendar/u/0/r/day/2024/06/27][Calendar]] "
                            "| [[http://go/tlesher-howskills][How Skills running doc]] | "
                            "[[https://docs.google.com/spreadsheets/d/1Uzho3ypkTY6kxM0qdzX4ABLBr0zSMwjSei_QBMEbtf4/edit#gid=1247661671][Morning Planning]]")
                    ""
                    "* 📆 Schedule"
                    ""
                    "| Time      | Event                    | Location |"
                    "|-----------+--------------------------+----------|"
                    ""                  
                    "* 🔥 Commits"
                    "** TODO "
                    ""
                    "* ✅ To Do"
                    "** TODO "
                    ""
                    "* 🚫 Not To Do"
                    "- [ ] "
                    ""
                    "* 🏁 WIP Wrapup (where I left off)"
                    "** TODO SHUT DOWN"
                    ""
                    "* 📝 Notes"
                    "** ."
                    ))
         (hdr (apply 'concat
                     (mapcar (lambda (s) (concat s "\n"))
                             hdr-list)))
         (has-hdr (lambda ()
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward "Schedule" nil t)))))
    (message (concat "opening " path " ..."))
    (find-file path)
    (unless (funcall has-hdr)
      (save-excursion
        (goto-char (point-min))
        (insert hdr)))
    (message "Enjoy your journaling!"))
  ) 

(setq org-roam-dailies-capture-templates
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c t") 'org-tidy-toggle))

(global-set-key "\C-c1"
                (lambda ()
                  (interactive)
                  (open-journal-file)))

(use-package org-tidy
    :ensure t
    :config
    (add-hook 'org-mode-hook #'org-tidy-mode))
;; TODO figure out why tidy doesn't work on new files.


(use-package git-auto-commit-mode
    :ensure t
    )
(provide 'init-org)

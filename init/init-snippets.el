;; Snippets of functions I keep using

(defun rootdir-p (dn)
  (if (eq system-type 'windows-nt)
      (if (string-match "^[a-zA-Z]\:/$" (expand-file-name dn)) t nil)
      (equal (expand-file-name dn) "/")))

(provide 'init-snippets)
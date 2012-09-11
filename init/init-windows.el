(if (eq system-type 'windows-nt)
    (progn
      (defun explorer ()
        "Launch Windows Explorer in current directory and select current file"
        (interactive)
        (w32-shell-execute "open" "explorer"
                           (concat "/e,/select," (convert-standard-filename
                                                  buffer-file-name))))
      (global-set-key [f12] 'explorer)))

(provide 'init-windows)
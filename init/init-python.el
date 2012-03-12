;; Python mode settings
(defun tdl-python-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq py-indent-offset 4)
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  (setq compile-command "py.test")
  ;; (pymacs-load "ropemacs" "rope-")
  )
(add-hook 'python-mode-hook 'tdl-python-mode-setup)

(when (load "flymake" t)
  (progn
    (defun flymake-pyflakes-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
			 'flymake-create-temp-inplace))
	     (local-file (file-relative-name
			  temp-file
			  (file-name-directory buffer-file-name))))
	(list "pycheckers"  (list local-file))))
    (add-to-list 'flymake-allowed-file-name-masks
		 '("\\.py\\'" flymake-pyflakes-init))))

;; pymacs
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")

;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pycheckers"  (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

(provide 'init-python)

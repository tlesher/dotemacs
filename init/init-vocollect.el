;; Always open VTX files as UTF-8
(modify-coding-system-alist 'file ".*\.vtx" 'utf-8)

(defconst vocollect-c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 4)
    (c-offsets-alist
     (substatement-open . 0)
     (label             . -)
     (inline-open       . 0))
    )
  "Vocollect Mobile Software style")

(provide 'init-vocollect)
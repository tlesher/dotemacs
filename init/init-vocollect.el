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

; TODO: Figure out how to tell c-mode about map macros:
; BEGIN_MSG_MAP(), MESSAGE_HANDLER(), END_MSG_MAP, etc.

(c-add-style "vocollect" vocollect-c-style)

(provide 'init-vocollect)

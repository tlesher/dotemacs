;; Don't (require 'p4); it causes init.el to load slowly.
;; Appears to be caused by p4-mode trying to load all help
;; strings from the P4 server at initialization.

;;; TODO: add other p4 commands here
(autoload 'p4-edit "p4" "Open the file for editing." "t")

(provide 'init-p4)

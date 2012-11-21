;; TODO: Fix the default font via DPI tweaking, not customize
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/tmp/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp/backups/"))))
 '(flymake-gui-warnings-enabled nil)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(org-agenda-files (quote ("~/organizer.org"))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))) nil (quote (flymake-warnline ((((class color)) (:underline "yellow"))))))
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-warnline ((t (:underline "gold")))))
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-warnline ((t (:underline "gold")))
 '(ediff-current-diff-A ((((class color) (min-colors 16)) (:background "Pink"))))
 '(ediff-current-diff-Ancestor ((((class color) (min-colors 16)) (:background "#ffbbdd"))))
 '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "#ccffcc"))))
 '(ediff-current-diff-C ((((class color) (min-colors 16)) (:background "#ffffcc"))))
 '(ediff-even-diff-A ((((class color) (min-colors 16)) (:background "light grey"))))
 '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "Grey"))))
 '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "Grey"))))
 '(ediff-even-diff-C ((((class color) (min-colors 16)) (:background "light grey"))))
 '(ediff-fine-diff-A ((((class color) (min-colors 16)) (:background "#eeeeff"))))
 '(ediff-fine-diff-Ancestor ((((class color) (min-colors 16)) (:background "#bbffbb"))))
 '(ediff-fine-diff-B ((((class color) (min-colors 16)) (:background "#aaffff"))))
 '(ediff-fine-diff-C ((((class color) (min-colors 16)) (:background "Turquoise"))))
 '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "Grey"))))
 '(ediff-odd-diff-Ancestor ((((class color) (min-colors 16)) (:background "gray40"))))
 '(ediff-odd-diff-B ((((class color) (min-colors 16)) (:background "light grey"))))
 '(ediff-odd-diff-C ((((class color) (min-colors 16)) (:background "Grey")))))
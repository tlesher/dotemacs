;; TODO: Fix the default font via DPI tweaking, not customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/tmp/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp/backups/"))))
 '(custom-safe-themes (quote ("ed56c2dde993afe54e768a243889b97bf9f8b135dd28e30e1b556d4400713e5e" "9667f8c36b9389fbbd07d605865e760ae23065508bbdc29b0ece99581ca365a9" "e6a66e95dcbe4ea2dbc02dd6e56656c42c6bfd11bd0c8ace18f7fbbd39c20d10" "6476be2ff7ace87c1cffc7b4645654aceb51331b5f48689740ebed0f036e6711" "e5f1d256e0da3f1cce5e542dc768afcb22cd193ea4150737206b9ae0cb172637" "1c7f421b2d1e14af2057bcb85091921bdeeb84f46bedc6ee94b90daf0f4c96f5" "4031c1ea0bb235b75a048bd92f3bf3aa984c9f7cc5b408f00f62ed99a6eecc09" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(fci-rule-color "#383838")
 '(flymake-gui-warnings-enabled nil)
 '(global-font-lock-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(org-agenda-files (quote ("~/organizer.org")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))) nil (quote (flymake-warnline ((((class color)) (:underline "yellow"))))))
;;  '(flymake-errline ((t (:underline "red"))))
;;  '(flymake-warnline ((t (:underline "gold"))))
;;  '(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button) :family "Verdana")))))
;;  '(flymake-errline ((t (:underline "red"))))
;;  '(flymake-warnline ((t (:underline "gold")))
;;  '(ediff-current-diff-A ((((class color) (min-colors 16)) (:background "Pink"))))
;;  '(ediff-current-diff-Ancestor ((((class color) (min-colors 16)) (:background "#ffbbdd"))))
;;  '(ediff-current-diff-B ((((class color) (min-colors 16)) (:background "#ccffcc"))))
;;  '(ediff-current-diff-C ((((class color) (min-colors 16)) (:background "#ffffcc"))))
;;  '(ediff-even-diff-A ((((class color) (min-colors 16)) (:background "light grey"))))
;;  '(ediff-even-diff-Ancestor ((((class color) (min-colors 16)) (:background "Grey"))))
;;  '(ediff-even-diff-B ((((class color) (min-colors 16)) (:background "Grey"))))
;;  '(ediff-even-diff-C ((((class color) (min-colors 16)) (:background "light grey"))))
;;  '(ediff-fine-diff-A ((((class color) (min-colors 16)) (:background "#eeeeff"))))
;;  '(ediff-fine-diff-Ancestor ((((class color) (min-colors 16)) (:background "#bbffbb"))))
;;  '(ediff-fine-diff-B ((((class color) (min-colors 16)) (:background "#aaffff"))))
;;  '(ediff-fine-diff-C ((((class color) (min-colors 16)) (:background "Turquoise"))))
;;  '(ediff-odd-diff-A ((((class color) (min-colors 16)) (:background "Grey"))))
;;  '(ediff-odd-diff-Ancestor ((((class color) (min-colors 16)) (:background "gray40"))))
;;  '(ediff-odd-diff-B ((((class color) (min-colors 16)) (:background "light grey"))))
;;  '(ediff-odd-diff-C ((((class color) (min-colors 16)) (:background "Grey")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

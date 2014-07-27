;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#E5786D" "#95E454" "#CAE682" "#8AC6F2" "#333366" "#CCAA8F" "#F6F3E8"])
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(anzu-use-mimego t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("9d5a88f5a74e0be7c259f3ece3104e578ae585abba5a62d0136d1103a38bb449" "2b484c630af2578060ee43827f4785e480e19bab336d1ccb2bce5c9d3acfb652" "ea4035bd249cc84f038158d1eb17493623c55b0ca92d9f5a1d036d2837af2e11" "9fd20670758db15cc4d0b4442a74543888d2e445646b25f2755c65dcd6f1504b" default)))
 '(ecb-options-version "2.40")
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(init-loader-show-log-after-init (quote error-only))
 '(org-agenda-files (quote ("~/gtd/main.org")))
 '(robe-highlight-capf-candidates nil)
 '(rspec-use-rake-when-possible nil)
 '(safe-local-variable-values (quote ((require-final-newline . t))))
 '(vbasense-tli-files
   (quote
    ("c:\\Program Files (x86)\\Microsoft Office\\OFFICE14\\EXCEL.EXE" "c:/Program Files (x86)/Common Files/Microsoft Shared/VBA/VBA7/VBE7.DLL" "c:/Program Files (x86)/Common Files/Microsoft Shared/VBA/VBA6/VBE6EXT.OLB" "c:/Program Files (x86)/Common Files/Microsoft Shared/OFFICE14/MSO.DLL" "C:\\Windows\\SysWOW64\\stdole2.tlb")))
 '(visual-basic-mode-indent 2)
 '(yas-trigger-key "TAB"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-control-face ((t (:background "color-234" :foreground "brightwhite" :underline t))))
 '(elscreen-tab-current-screen-face ((t (:background "color-243" :foreground "brightwhite"))))
 '(elscreen-tab-other-screen-face ((t (:background "color-235" :foreground "brightwhite" :underline t))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "dark gray"))))
 '(font-lock-comment-face ((t (:foreground "dark gray"))))
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(helm-selection ((t (:background "color-163" :underline t))))
 '(helm-source-header ((t (:background "color-18" :foreground "white" :weight bold :height 1.3 :family "Sans Serif"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))) t)
 '(markdown-pre-face ((t (:foreground "brightmagenta"))) t)
 '(minibuffer-prompt ((t (:foreground "brightblue")))))

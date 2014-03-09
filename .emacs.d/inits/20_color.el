;; ------------------------------------------------------------------------
;; face-display Setting
;; ------------------------------------------------------------------------
;;; 色を設定する
;;; 設定自体は M-x list-face-displaysから.emacsに自動生成されたものをcopy
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("2b484c630af2578060ee43827f4785e480e19bab336d1ccb2bce5c9d3acfb652" "ea4035bd249cc84f038158d1eb17493623c55b0ca92d9f5a1d036d2837af2e11" "9fd20670758db15cc4d0b4442a74543888d2e445646b25f2755c65dcd6f1504b" default)))
 '(ecb-options-version "2.40")
 '(safe-local-variable-values (quote ((require-final-newline . t))))
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))) t)
 '(markdown-pre-face ((t (:foreground "brightmagenta"))) t)
 '(minibuffer-prompt ((t (:foreground "brightblue")))))

;; ------------------------------------------------------------------------
;; Name     : Emacs Color theme
;; Function :
;; History  : 2014.1.14 Add
;; Install  : https://code.google.com/p/gnuemacscolorthemetest/
;; ------------------------------------------------------------------------
(require 'color-theme)
(color-theme-initialize)

;; ------------------------------------------------------------------------
;; Name     : Almost Monokai
;; Function : Beautiful Color theme
;; History  : 2014.1.14 Add
;; Install  : https://raw2.github.com/zanson/color-theme-almost-monokai/master/color-theme-almost-monokai.el
;; ------------------------------------------------------------------------
;;(load-file "~/.emacs.d/elisp/color-theme/themes/color-theme-almost-monokai.el")
;;(color-theme-almost-monokai)

;; ------------------------------------------------------------------------
;; Name     : Molokai
;; Function : Most popular color theme
;; History  : 2014.1.14 Add
;; Install  : https://raw2.github.com/hbin/molokai-theme/master/molokai-theme-kit.el
;; ------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq molokai-theme-kit t)
;;(require 'molokai-theme)
(load-theme 'molokai t)

;; ------------------------------------------------------------------------
;; Name     : PowerLine
;; Function : Most popular color theme
;; History  : 2014.1.14 Add
;; Install  : http://www.emacswiki.org/emacs/powerline.el
;; ------------------------------------------------------------------------
;;(require 'cl)
;;(require 'powerline)

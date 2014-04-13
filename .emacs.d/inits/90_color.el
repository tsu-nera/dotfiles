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
(add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/repo/molokai-theme")
(require 'molokai-theme-kit)
(setq molokai-theme-kit t)

(load-theme 'molokai t)
;; Linux環境はスケスケに上書きする
;; (when linux-p
;;   (load-theme 'my-molokai t)
;; )
;; ;; Cygwinはちゃんと塗りつぶし
;; (when cygwin-p
;;   (load-theme 'molokai t)
;; )

;; ------------------------------------------------------------------------
;; Name     : PowerLine
;; Function : Most popular color theme
;; History  : 2014.1.14 Add
;; Install  : http://www.emacswiki.org/emacs/powerline.el
;; ------------------------------------------------------------------------
;;(require 'cl)
;;(require 'powerline)

;; ------------------------------------------------------------------------
;; Name     : rainbow-mode
;; Function : 色つけしてくれる 
;; History  : 2014.3.13 Add
;; ------------------------------------------------------------------------
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

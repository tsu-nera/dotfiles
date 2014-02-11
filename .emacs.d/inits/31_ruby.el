;; ------------------------------------------------------------------------
;; Ruby
;; ------------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; Name     : ruby-mode
;; Install  :
;; Function : Ruby開発環境を提供
;; ------------------------------------------------------------------------
;(require 'ruby-mode)
;(autoload 'ruby-mode "ruby-mode"
;    "Mode for editing ruby source files" t)
;(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
;(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; -----------------------------------------------------------------------
;; Name     : enhanced-ruby-mode
;; Install  : https://github.com/zenspider/enhanced-ruby-mode
;; Function : Ruby開発環境を提供
;; ------------------------------------------------------------------------
(require 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

;; -----------------------------------------------------------------------
;; Name     : SmartCompile
;; Install  : ;http://www.emacswiki.org/emacs/download/smart-compile.el
;; Function : コマンドからコンパイル実行
;; ------------------------------------------------------------------------
 (require 'smart-compile)
 (define-key enh-ruby-mode-map (kbd "C-c c") 'smart-compile)
 (define-key enh-ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))
 (setq compilation-window-height 15) ;; default window height is 15

;; -----------------------------------------------------------------------
;; Name     : ruby-electric.el --- electric editing commands for ruby files
;; Install  :
;; Function : かっこやdo endなどの対応関係を自動で補正してくれる
;; ------------------------------------------------------------------------
(require 'ruby-electric)
  (add-hook 'enh-ruby-mode-hook '(lambda () (ruby-electric-mode t)))
  (setq ruby-electric-expand-delimiters-list nil)

;; -----------------------------------------------------------------------
;; Name     : ruby-electric.el --- electric editing commands for ruby files
;; Install  :
;; Function : かっこやdo endなどの対応関係を自動でハイライトしてくれる
;; ------------------------------------------------------------------------
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; -----------------------------------------------------------------------
;; Name     : RcodeTools
;; Install  : el-get
;; Function : Ruby開発のツール群
;; ------------------------------------------------------------------------
(require 'rcodetools)
(setq rct-find-tag-if-available nil)
(defun enh-ruby-mode-hook-rcodetools ()
  (define-key enh-ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key enh-ruby-mode-map "\C-c\C-d" 'xmp)
  (define-key enh-ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key enh-ruby-mode-map "\C-c\C-f" 'rct-ri)
  )
(add-hook 'enh-ruby-mode-hook 'enh-ruby-mode-hook-rcodetools)

;; -----------------------------------------------------------------------
;; Name     : ruby-refactor
;; Install  : github(el-get 登録済み)
;;              https://github.com/ajvargo/ruby-refactor.git
;; Function : Ruby用リファクタリングツール
;; ------------------------------------------------------------------------
(require 'ruby-refactor)
(add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch)

;; -----------------------------------------------------------------------
;; Name     : inf-ruby
;; Install  : el-get
;; Function : emacsからirbを操作
;; ------------------------------------------------------------------------
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

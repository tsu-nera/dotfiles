;; ------------------------------------------------------------------------
;; Ruby
;; ------------------------------------------------------------------------
;; Setting rbenv path
;; emacs から rbenv でいれたRubyを利用する。
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:"
		       (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims")
		(cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))
;; -----------------------------------------------------------------------
;; Name     : ruby-mode
;; Install  :
;; Function : Ruby開発環境を提供
;; ------------------------------------------------------------------------
(require 'ruby-mode)
(autoload 'ruby-mode "ruby-mode"p
    "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))

;; -----------------------------------------------------------------------
;; Name     : enhanced-ruby-mode
;; Install  : https://github.com/zenspider/enhanced-ruby-mode
;; Function : Ruby開発環境を提供
;; ------------------------------------------------------------------------
; must be added after any path containing old ruby-mode
;;(require 'enh-ruby-mode)
;;(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
;;(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))

;;(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;;(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
;;(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
;;(add-to-list 'auto-mode-alist '("[Rr]akefile$" . enh-ruby-mode))
;;(add-to-list 'load-path "~/.emacs.d/elips/el-get/enh-ruby-mode")
;;(add-to-list 'interpreter-mode-alist '("ruby" enh-ruby-mode))

;;(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

;; -----------------------------------------------------------------------
;; Name     : SmartCompile
;; Install  : ;http://www.emacswiki.org/emacs/download/smart-compile.el
;; Function : コマンドからコンパイル実行
;; ------------------------------------------------------------------------
 (require 'smart-compile)
 (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
 (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))
 (setq compilation-window-height 15) ;; default window height is 15

;; -----------------------------------------------------------------------
;; Name     : ruby-electric.el --- electric editing commands for ruby files
;; Install  :
;; Function : かっこやdo endなどの対応関係を自動で補正してくれる
;; ------------------------------------------------------------------------
(require ' ruby-electric)
  (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
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
(defun ruby-mode-hook-rcodetools ()
  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key ruby-mode-map "\C-c\C-d" 'xmp)
  (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri)
  )
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

;; -----------------------------------------------------------------------
;; Name     : ruby-refactor
;; Install  : github(el-get 登録済み)
;;              https://github.com/ajvargo/ruby-refactor.git
;; Function : Ruby用リファクタリングツール
;; ------------------------------------------------------------------------
(require 'ruby-refactor)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

;; -----------------------------------------------------------------------
;; Name     : inf-ruby
;; Install  : el-get
;; Function : emacsからirbを操作
;; ------------------------------------------------------------------------
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; -----------------------------------------------------------------------
;; Name     : robocop 
;; Install  : checkstyleのようなスタイルの解析
;; Function : gem install rubocop
;; ------------------------------------------------------------------------
;;(require 'rubocop)
;;(add-hook 'ruby-mode 'rubocop-mode)

;; -----------------------------------------------------------------------
;; Name     : autotest
;; Install  : wget 
;; Function : rspec自動実行
;; ------------------------------------------------------------------------
;; 動かん！
;; (require 'autotest)

;; -----------------------------------------------------------------------
;; Name     : rspec-mode
;; Install  : el-get
;; Function : rspec拡張
;; ------------------------------------------------------------------------
(require 'rspec-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))
(custom-set-variables '(rspec-use-rake-flag nil))
(custom-set-faces)

;; -----------------------------------------------------------------------
;; Name     : rcov-overlay
;; Install  : wget http://www.emacswiki.org/emacs/download/rcov-overlay.el
;; Function : rcov連携
;; ------------------------------------------------------------------------
;; 動かん!
;; (require 'rcov-overlay)

;; -----------------------------------------------------------------------
;; Name     : robe-mode
;; Install  : el-get
;; Function : かしこい補完
;; ------------------------------------------------------------------------
 ; robe
(autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
(autoload 'robe-ac-setup "robe-ac" "robe auto-complete" nil nil)
(add-hook 'ruby-mode-hook
	            '(lambda ()
		       (robe-mode)
		       (robe-ac-setup)
;;		       (inf-ruby-setup-keybindings)
		       ))

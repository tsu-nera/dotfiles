;; ------------------------------------------------------------------------
;; Name     : el-get.el
;; Function : eLisp管理
;; Install  : cd public_repos 
;;            git clone git@github.com:dimitri/el-get.git
;; ------------------------------------------------------------------------
;; ダウンロードしていないときはダウンロード
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

;; ダウンロードしたelisp置き場
(setq el-get-dir "~/.emacs.d/el-get/repo")

;; レシピ置き場
;;(setq el-get-recipe-path (list "~/.emacs.d/el-get/recipes/emacswiki"
;;			       "~/.emacs.d/el-get/recipes"))
;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/user-recipes")
;;(add-to-list 'el-get-recipe-path '("~/.emacs.d/el-get/user-recipes/myemacswiki"))
;;(add-to-list 'el-get-recipe-path '("~/.emacs.d/el-get/user-recipes"))

(setq el-get-user-package-directory "~/.emacs.d/el-get/init-files")

(require 'el-get)

;; 自動でインストールするものたち
(el-get 'sync
	'helm
	'yasnippet
	'vbasense
	'visual-basic-mode
	'ruby-block
	'robe-mode
	'ruby-electric
	'rspec-mode
	'ruby-refactor
	'rcodetools
	'anything-rdefs
	'inf-ruby
	'auto-complete
	'auto-complete-ruby
	'yaml-mode
	'popwin
	'flycheck
	'flycheck-color-mode-line
	'anzu
	'auto-highlight-symbol
	'highlight-symbol
	'multiple-cursors
	'color-theme
	;; 'smart-compile 独自改造したものをelispにおいた
	'helm-descbinds
	'anything
	'gist
	'flymake
	'helm-gist
	'helm-c-yasnippet
	'markdown-mode
	'expand-region
	'powerline 
	;;'molokai-theme オリジナルを利用するので
	'cool-mode
	'plantuml-mode
	'metaweblog
	'xml-rpc-el
	'tempbuf
	'org2blog
	'org-mode
	'org-pomodoro
	'rainbow-mode
	)

;; ------------------------------------------------------------------------
;; Name     : package.el
;; Function :
;; History  : 2014/01/16 add
;; Install  : http://www.emacswiki.org/emacs/download/auto-install.el
;; ------------------------------------------------------------------------
(require 'package)
;; Add package-archives
;; Melpa: githubからelispを落とすリポジトリを追加
;; これで、 M-x list-packagesで melpaが利用できる。
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
;; Initialize
(package-initialize)

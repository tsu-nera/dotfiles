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

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======

>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======

>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======

>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======

>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======

>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
;; ダウンロードしたelisp置き場
(setq el-get-dir "~/.emacs.d/el-get/repo")

;; レシピ置き場
;;(setq el-get-recipe-path (list "~/.emacs.d/el-get/recipes/emacswiki"
;;			       "~/.emacs.d/el-get/recipes"))
;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/user-recipes")

;; 一時temp
(setq el-get-user-package-directory "~/.emacs.d/el-get/init-files")

(require 'el-get)
(require 'el-get-emacswiki)

;; 自動でインストールするものたち
(el-get 'sync
	'helm
	'yasnippet
	'metaweblog
	'xml-rpc-el
	'org2blog
	'auto-complete
	'popwin
	'tempbuf
	'flycheck
	'flycheck-color-mode-line
	'anzu
	'auto-highlight-symbol
	'highlight-symbol
	'multiple-cursors
	'plantuml-mode
	'color-theme
	'org-mode
	'smart-compile
	'org-pomodoro
	'helm-descbinds
	'auto-install
	'ruby-block
	'auto-complete-ruby
	'robe-mode
	'gist
	'yaml-mode
	'flymake
	'helm-gist
	'inf-ruby
	'helm-c-yasnippet
	'markdown-mode
	'expand-region
	'ruby-electric
	'rspec-mode
	'ruby-refactor
	'powerline
	;;'molokai-theme オリジナルを利用するので
	'anything
	'rcodetools
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
	'anything-rdefs
=======
>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======
>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======
>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======
>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======
>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
	)

;; ------------------------------------------------------------------------
;; Name     : auto-install
;; Function :
;; History  :
;; Install  : http://www.emacswiki.org/emacs/download/auto-install.el
;; ------------------------------------------------------------------------
(when(require 'auto-install nil t)
  ;;インストールディレクトリを設定する  初期値は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  ;;; 起動時にnetwork unreachableってでるので、とりあえず封印 13/05/26
  ;;(auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定を行う
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にす
  (auto-install-compatibility-setup))

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

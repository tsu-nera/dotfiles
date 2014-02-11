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

;; ------------------------------------------------------------------------
;; Name     : el-get.el
;; Function : eLisp管理
;; Install  : cd public_repos 
;;            git clone git@github.com:dimitri/el-get.git
;; ------------------------------------------------------------------------
(setq el-get-dir "~/.emacs.d/elisp/el-get/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; レシピ置き場
(add-to-list 'el-get-recipe-path
	     (concat (file-name-directory load-file-name) "/el-get/recipes"))
;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path
	     "~/.emacs.d/elisp/el-get/el-get/user-recipes")))

(el-get 'sync 'helm 'yasnippet)

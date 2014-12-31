;; ========================================================================
;; Emacs Configuration files
;; ========================================================================

;; ------------------------------------------------------------------------
;; Name     : el-get
;; ------------------------------------------------------------------------
;; el-get ロードパス設定
(add-to-list 'load-path (locate-user-emacs-file "el-get/repo/el-get"))

;; ダウンロードしたelisp置き場
(setq el-get-dir "~/.emacs.d/el-get/repo")

;; ダウンロードしていないときはダウンロード
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; 初期化処理用
(setq el-get-user-package-directory "~/.emacs.d/el-get/init-files")

;; レシピ置き場
;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/user-recipes")

;; emacs wiki関係は一時封印している。会社だとエラーするので。
;;(setq el-get-recipe-path (list "~/.emacs.d/el-get/recipes/emacswiki"
;;			       "~/.emacs.d/el-get/recipes"))
;; (setq-default el-get-dir (locate-user-emacs-file "el-get")
;;               el-get-emacswiki-base-url
;;               "http://raw.github.com/emacsmirror/emacswiki.org/master/")

;; ------------------------------------------------------------------------
;; Name     : org-mode
;; ------------------------------------------------------------------------
;; org-mode/lisp, org-mode/contribe/lispをロードパスに追加する
(defconst dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(defconst config-dir "~/.emacs.d/inits/")
(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org-mode" el-get-dir)))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir)))))
  (setq load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))

(require 'org)

;; ------------------------------------------------------------------------
;; Name     : bundle.el
;; Function : an el-get wapper
;; Refs     : 
;; https://github.com/tarao/bundle-el
;; ------------------------------------------------------------------------
;; bundleを el-getで取得する
(add-to-list 'el-get-sources
	     '(:name bundle :type github :pkgname "tarao/bundle-el"))
(el-get 'sync 'bundle)

;; -----------------------------------------------------------------------
;; Name     : init-loader
;; Install  : git clone https://github.com/emacs-jp/init-loader
;; Function : init.el分割管理
;; ------------------------------------------------------------------------
(bundle emacs-jp/init-loader)

;; バイトコンパイルする
;; 初めのバイトコンパイルは手動で実施する必要がある
(setq init-loader-byte-compile t)

;; このエラーメッセージがでないようにするおまじない
;; gnutls.c: [1] Note that the security level 
;; http://whiteanthrax.pkf.jp/emacs/75/
(setq gnutls-min-prime-bits 1024)

;; エラー発生時にだけlogを開く
;; (setq init-loader-show-log-after-init t)
(setq init-loader-show-log-after-init 'error-only)

;; -----------------------------------------------------------------------
;; Name     : babel-loader
;; Function : inits.org分割管理
;; Refs     : https://github.com/takaishi/babel-loader.el
;; ------------------------------------------------------------------------
(bundle takaishi/babel-loader.el)
(require 'org-element)
(require 'babel-loader)

;; Refs;
;; http://uwabami.junkhub.org/log/?date=20111213
;; 指定された org ファイルの basename を取得
;; 取得した basename + ".el" と元の org ファイルのタイムスタンプを比較
;; .el のタイムスタンプが古かったら, org 中の
;;  +begin_src emacs-lisp - +end_src 部分をファイル名.el というファイルに抽出
;; タイムスタンプが新しい or .el の抽出を行なったら .el を読む

;; ignore warinig
;; byte-compile warning の無視
;; http://tsengf.blogspot.jp/2011/06/disable-byte-compile-warning-in-emacs.html
;; ignore byte-compile warnings 
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete 
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))

;; インデント保持
;; これをしないと 変換された elispファイルのインデントがずれる.
(setq org-src-preserve-indentation t)

;; inits配下のorgファイルをelcに変換して読み込み.
(bl:load-dir "~/.emacs.d/inits/")
(put 'downcase-region 'disabled nil)

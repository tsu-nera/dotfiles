;; ========================================================================
;; Emacs Configuration files
;; ========================================================================

;; ------------------------------------------------------------------------
;; Name     : el-get
;; ------------------------------------------------------------------------
(add-to-list 'load-path (locate-user-emacs-file "el-get/repo/el-get"))
;; ダウンロードしたelisp置き場
(setq el-get-dir "~/.emacs.d/el-get/repo")

;; (setq-default el-get-dir (locate-user-emacs-file "el-get")
;;               el-get-emacswiki-base-url
;;               "http://raw.github.com/emacsmirror/emacswiki.org/master/")

;; ダウンロードしていないときはダウンロード
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

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

;; -----------------------------------------------------------------------
;; Name     : custom
;; Func     : customize の出力先
;; ------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/inits/02_custom.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/inits/02_custom.el"))
    (load (expand-file-name custom-file) t nil nil))

;; ------------------------------------------------------------------------
;; Name     : bundle.el
;; Function : an el-get wapper
;; Refs     : 
;; https://github.com/tarao/bundle-el
;; ------------------------------------------------------------------------
;; bundleを el-getで取得する
(add-to-list 'el-get-sources '(:name bundle :type github :pkgname "tarao/bundle-el"))
(el-get 'sync 'bundle)

;; -----------------------------------------------------------------------
;; Name     : init-loader
;; Install  : git clone https://github.com/emacs-jp/init-loader
;; Function : init.el分割管理
;; ------------------------------------------------------------------------
(bundle emacs-jp/init-loader)

;; ログファイルを表示
(setq init-loader-show-log-after-init t)

;; バイトコンパイルする
;; 初めのバイトコンパイルは手動で実施する必要がある
(setq init-loader-byte-compile t)

;; このエラーメッセージがでないようにするおまじない
;; gnutls.c: [1] Note that the security level 
;; http://whiteanthrax.pkf.jp/emacs/75/
(setq gnutls-min-prime-bits 1024)

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
;;  #+begin_src emacs-lisp 〜 #+end_src 部分をファイル名.el というファイルに抽出
;; タイムスタンプが新しい or .el の抽出を行なったら .el を読む

;; インデント保持
(setq org-src-preserve-indentation t)

(bl:load-dir "~/.emacs.d/inits/") 

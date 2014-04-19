;;------------------------------------------------------------------------
;; Def Macro
;;------------------------------------------------------------------------

;; Special Thanks
;; http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html
;;  add-hookを簡潔に
(defmacro add-hook-fn (name &rest body)
    `(add-hook ,name #'(lambda () ,@body)))

;; global-set-keyも簡潔に
(defmacro global-set-key-fn (key args &rest body)
  `(global-set-key ,key (lambda ,args ,@body)))

;;  複数の要素をリストに追加
(defmacro append-to-list (to lst)
  `(setq ,to (append ,lst ,to)))

;; ライブラリがあるときだけrequireする
;; マクロ定義
(defmacro req (lib &rest body)
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body))

;; autoloadで遅延ロード
(defmacro lazyload (func lib &rest body)
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib nil t)) func)
     (eval-after-load ,lib
       '(progn
	  ,@body))))

;; Special Thanks
;; http://d.hatena.ne.jp/sr10/20121128/1354083749
;; ライブラリを遅延評価
(defmacro lazy-load-eval (feature &optional functions &rest body)
  "Define each FUNCTIONS to autoload from FEATURE.
FEATURE is a symbol. FUNCTIONS is a list of symbols. If FUNCTIONS is nil,
the function same as FEATURE is defined as autoloaded function. BODY is passed
 to `eval-after-load'.
When this macro is evaluated, this returns the path to library if FEATURE
found, otherwise returns nil."
  (let* ((libname (symbol-name (eval feature)))
	 (libpath (locate-library libname)))
    (and libpath
	 `(progn
	    ,@(mapcar (lambda (f)
			(or (fboundp f)
			    `(autoload (quote ,f)
				 ,libname
			       ,(concat "Autoloaded function defined in \""
					libpath
					"\".")
			       t)))
		      (or (eval functions)
			  `(,(eval feature))))
	    (eval-after-load ,feature
	      (quote (progn
		       ,@body)))
	    ,libpath))))

;;------------------------------------------------------------------------
;; Def Function
;;------------------------------------------------------------------------
;; Special Thanks
;; http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html

;; @ load-path
;; for Emacs 23 under
(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))
;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "inits" "el-get")

;; elpaは外した。el-getで一元管理したいので。
;; public_reposは対象から外した。el-getで一元管理したいので。

;; -----------------------------------------------------------------------
;; Function : 環境による場合分けの方法
;;   http://d.hatena.ne.jp/tomoya/20090811/1250006208
;; ------------------------------------------------------------------------
(defun x->bool (elt) (not (not elt)))

;; emacs-version predicates
(setq emacs22-p (string-match "^22" emacs-version)
      emacs23-p (string-match "^23" emacs-version)
      emacs23.0-p (string-match "^23\.0" emacs-version)
      emacs23.1-p (string-match "^23\.1" emacs-version)
      emacs23.2-p (string-match "^23\.2" emacs-version))

;; system-type predicates
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (eq window-system 'ns)
      carbon-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      colinux-p (when linux-p
		  (let ((file "/proc/modules"))
		    (and
		     (file-readable-p file)
		     (x->bool
		      (with-temp-buffer
			(insert-file-contents file)
			(goto-char (point-min))
			(re-search-forward "^cofuse\.+" nil t))))))
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

;; -----------------------------------------------------------------------
;; Name     : 左右のバッファをF2で交換する
;; Function : http://d.hatena.ne.jp/supermassiveblackhole/20100625/1277436024
;; ------------------------------------------------------------------------
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)

;; -----------------------------------------------------------------------
;; Name     : custom
;; Func     : customize の出力先
;; ------------------------------------------------------------------------
(setq custom-file "~/.emacs.d/inits/02_custom.el")
(if (file-exists-p (expand-file-name "~/.emacs.d/inits/02_custom.el"))
    (load (expand-file-name custom-file) t nil nil))

;; -----------------------------------------------------------------------
;; Name     : init-loader
;; Install  : git clone https://github.com/emacs-jp/init-loader
;; Function : init.el分割管理
;; ------------------------------------------------------------------------
;; このエラーメッセージがでないようにするおまじない
;; gnutls.c: [1] Note that the security level 
;; http://whiteanthrax.pkf.jp/emacs/75/
(setq gnutls-min-prime-bits 1024)

;; init-loader
;; init-loader は git submoduleで入れる。
;; TODO 自動でインストールする方法を考える
(require 'init-loader)

;;; 設定ファイルのあるフォルダを指定
(setq inits_dir (expand-file-name "~/.emacs.d/inits/"))
(init-loader-load inits_dir)

;; ログファイルを表示
(setq init-loader-show-log-after-init t)
;; バイトコンパイルする
(setq init-loader-byte-compile t)

;; initsフォルダのみ、保存時に自動コンパイルして即反映させる
;; http://fukuyama.co/emacsd
(defun auto-save-byte-compile-file ()
  "Do `byte-compile-file' and reload setting immediately, When elisp file saved only in inits folder."
  (interactive)
  (when (or (equal default-directory inits_dir)
	    (equal default-directory (abbreviate-file-name inits_dir)))
    (byte-compile-file buffer-file-name t)
    ))
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook 'auto-save-byte-compile-file nil t)))

;; ------------------------------------------------------------------------
;; Name     : setup
;; Function : Emacs高速起動のためのマクロライブラリ
;;            EmacsWiki
;; ------------------------------------------------------------------------
;; (require 'setup)

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
(add-to-load-path "elisp" "inits" "el-get" "elpa")

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

;; ------------------------------------------------------------------------
;; General key bind
;; ------------------------------------------------------------------------
;;(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-h")     'backward-delete-char)
(global-set-key (kbd "C-c d")   'delete-indentation)
(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "C-S-i")   'indent-region)
(global-set-key (kbd "C-m")     'newline-and-indent)
(global-set-key (kbd "C-t")     'next-multiframe-window)
(global-set-key (kbd "M-<RET>") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-S-t")   'previous-multiframe-window)
;; (global-set-key (kbd "C-M-r")   'replace-regexp)
;; (global-set-key (kbd "C-r")     'replace-string)
(global-set-key (kbd "C-/")     'undo)

;; ------------------------------------------------------------------------
;; General Value
;; Ref:
;; http://www.cozmixng.org/~kou/emacs/dot_emacs
;; ------------------------------------------------------------------------

;; file名の補間で大文字小文字を区別しない
(setq completion-ignore-case t)

;; 同名ファイルの区別
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; バッファ再読み込み
(global-auto-revert-mode 1)

;; ウィンドウマネージャ環境での起動時間カイゼン
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; git管理のシンボリックリンクで質問されないためのおまじない。
;; 参考: http://openlab.dino.co.jp/2008/10/30/212934368.html
;;; avoid "Symbolic link to Git-controlled source file;; follow link? (yes or no)
(setq git-follow-symlinks t)

;; byte-compile warningの無視
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
;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; フォント設定
;; Ricty
;; http://d.hatena.ne.jp/kitokitoki/20110502/p2
(add-to-list 'default-frame-alist '(font . "ricty-13"))

;; ------------------------------------------------------------------------
;; デフォルトブラウザ設定
;; ------------------------------------------------------------------------
(when linux-p
  (setq browse-url-generic-program (executable-find "conkeror"))
  (setq browse-url-browser-function 'browse-url-generic)
)

(when windows-p
; Windows環境のデフォルト
 (setq browse-url-browser-function 'browse-url-default-windows-browser)
)

;; 今のポイントしているURLを開く
(global-set-key (kbd "C-c u") 'browse-url-at-point)
;; ------------------------------------------------------------------------
;; Emacs Client
;; ------------------------------------------------------------------------
;; server start for emacs-client
;; http://d.hatena.ne.jp/syohex/20101224/1293206906
(require 'server)
(unless (server-running-p)
  (server-start))

;; -----------------------------------------------------------------------
;; Function : EmacsとXのクリップポードを共有
;; Install  : http://tubo028.hatenablog.jp/entry/2013/09/01/142238
;; ------------------------------------------------------------------------
;; OSとのクリップボード共有
(cond (window-system
(setq x-select-enable-clipboard t)
))

;; 上記では、emacs -nwでは動作しない。
(when linux-p
  ;; クリップボードと同期
  (setq interprogram-paste-function
	(lambda ()
	  (shell-command-to-string "xsel -p -o")))
  (setq interprogram-cut-function
	(lambda (text &optional rest)
	  (let* ((process-connection-type nil)
		 (proc (start-process "xsel" "*Messages*" "xsel" "-p" "-i")))
	    (process-send-string proc text)
	    (process-send-eof proc))))
  )

;; -----------------------------------------------------------------------
;; Function : ミニバッファに入るときに日本語入力無効にする
;;  http://www11.atwiki.jp/s-irie/pages/21.html
;; Install  : 
;;  sudo add-apt-repository ppa:irie/elisp
;;  sudo apt-get update
;;  sudo apt-get install ibus-el
;;  いれたけど、うまく動かない。
;; ------------------------------------------------------------------------
;; (require 'ibus)
;; (add-hook 'after-init-hook 'ibus-mode-on)

;; ;; IBusの状態によってカーソル色を変化させる
;; (setq ibus-cursor-color '("red" "blue" "limegreen"))

;; ;; isearch 時はオフに
;; (add-hook 'isearch-mode-hook 'ibus-disable)

;; ;; mini buffer ではオフに
;; (add-hook 'minibuffer-setup-hook 'ibus-disable)

;; ;; インクリメンタル検索中のカーソル形状を変更する
;; (setq ibus-isearch-cursor-type 'hollow)

;; ;; カーソルの位置に予測候補を表示
;; (setq ibus-prediction-window-position t)

;; ;; Undo の時に確定した位置まで戻る
;; (setq ibus-undo-by-committed-string t)

;; -----------------------------------------------------------------------
;; Name     : whitespace
;; Install  : build-in
;; Function : 80行めを光らせる
;; ------------------------------------------------------------------------
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'ruby-mode-hook 'whitespace-mode)

;; ------------------------------------------------------------------------
;; Name     : popwin
;; Function : ポップアップ表示
;; History  : 2014.1.15 Add
;; Install  : package.el経由
;; ------------------------------------------------------------------------
(when (require 'popwin)
  (setq helm-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config '(("*compilation*" :noselect t)
					;;("helm" :regexp t :height 0.4)
					("anything" :regexp t :height 0.4)
					)))
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*Org .+\*$" :regexp t) popwin:special-display-config)
(push '("*rspec-compilation*" :regexp t) popwin:special-display-config)
(push '("*Oz Compiler*" :regexp t) popwin:special-display-config)
(push '("^CAPTURE-.+\*.org$" :regexp t) popwin:special-display-config)
;; (push '("^\*terminal<.+" :regexp t) popwin:special-display-config)

;; http://cx4a.blogspot.jp/2011/12/popwineldirexel.html

;; M-x dired-jump-other-window
(push '(dired-mode :position bottom) popwin:special-display-config)
;; M-!
(push "*Shell Command Output*" popwin:special-display-config)
;; M-x compile
(push '(compilation-mode :noselect t) popwin:special-display-config)

(push '(direx:direx-mode :position left :width 40 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x j") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-x 4 j") 'dired-jump-other-window)

;; -----------------------------------------------------------------------
;; Name     : ffap.el
;; Function : 現在の位置のファイル・URLを開く
;; History  : 2014/02/02 add
;; Install  : build-in
;; ------------------------------------------------------------------------
;; (ffap-bindings)

;; -----------------------------------------------------------------------
;; Name     : tempbuf.el
;; Function : 使っていないバッファを削除
;; History  : 2014/02/02 add
;; Install  : emacs wiki
;; ------------------------------------------------------------------------
(require 'tempbuf)
;; ファイルを開いたら有効
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;; Dired modeならば有効
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;; -----------------------------------------------------------------------
;; Name     : EmacsでTODOをハイライト
;; Install  : http://stackoverflow.com/questions/8551320/
;;            highlighting-todos-in-all-programming-modes
;; ------------------------------------------------------------------------
(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
           '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; -----------------------------------------------------------------------
;; Name     : netrc
;; Install  : build-in
;; Function : パスワード管理
;; パスワード自体は ~/.netrcに書き込む。dropboxで同期
;; ------------------------------------------------------------------------
(require 'netrc)

;; -----------------------------------------------------------------------
;; Name     : stripe-buffer
;; Install  : el-get
;; Function : しましま表示
;; ------------------------------------------------------------------------
(require 'stripe-buffer)
(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
;;(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)

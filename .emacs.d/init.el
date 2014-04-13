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
(add-to-load-path "elisp" "public_repos" "elpa" "inits" "el-get" "themes")

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
;; Name     : init-loader
;; Install  : M-x install-elisp
;;     http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
;; Function : init.el分割管理
;; ------------------------------------------------------------------------
;; このエラーメッセージがでないようにするおまじない
;; gnutls.c: [1] Note that the security level 
;; http://whiteanthrax.pkf.jp/emacs/75/
(setq gnutls-min-prime-bits 1024)

;; init-loader
;; init-loader は git submoduleで入れる。
(require 'init-loader)
;; 設定ディレクトリ
(init-loader-load "~/.emacs.d/inits")
;; ログファイルを表示
(setq init-loader-show-log-after-init t)

;; ------------------------------------------------------------------------
;; Name     : auto-complete
;; URL      : http://www.emacswiki.org/emacs/auto-complete-extension.el
;; Function : 自動補完を実現するelisp
;; History  : 13/10/14
;; ------------------------------------------------------------------------
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

(setq ac-auto-show-menu 0.5)
(setq ac-menu-height 20)

(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (setq ac-dwim nil)
  ;;  (set-face-background 'ac-selection-face "steelblue")
  ;;  (set-face-background 'ac-menu-face "skyblue")
  (setq ac-auto-start t)
  (global-set-key "\M-/" 'ac-start)
  (setq ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
  (add-hook 'ruby-mode-hook
	    (lambda ()
	      (require 'rcodetools)
	      (require 'auto-complete-ruby)
	      ;; (load-auto-complete)
	      (make-local-variable 'ac-omni-completion-sources)
	      (setq ac-omni-completion-sources
		    '(("\\.\\=" . (ac-source-rcodetools)))))))

;;; C-c c で compile コマンドを呼び出す
(define-key mode-specific-map "" 'compile)

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
;; ------------------------------------------------------------------------
;; Name     : migemo
;; Function : 日本語をロ-マ字検索
;; History  : 2014.1.25 Add
;; Install  : sudo apt-get install cmigemo
					;
;; ------------------------------------------------------------------------
(when (and (executable-find "cmigemo")
	   (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
  )
(setq migemo-command "cmigemo")
(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")

;; ------------------------------------------------------------------------
;; Name     : keisen
;; Function : 罫線を引く
;; History  : 2014.1.29 Add
;; Install  : www.pitecan.com/Keisen/keisen.el
;;; ------------------------------------------------------------------------
;;(require 'keisen)
;;; Control+矢印キーで罫線を引く場合
;; (global-set-key (kbd "C-M-f") 'keisen-right-move)
;;(global-set-key (kbd "C-M-b") 'keisen-left-move)
;;(global-set-key (kbd "C-M-p") 'keisen-up-move)
;;(global-set-key (kbd "C-M-n") 'keisen-down-move)

;;(global-set-key [(C-right)] 'keisen-right-move)
;;(global-set-key [(C-left)] 'keisen-left-move)
;;(global-set-key [(C-up)] 'keisen-up-move)
;;(global-set-key [(C-down)] 'keisen-down-move)

;; -----------------------------------------------------------------------
;; Name     : ffap.el
;; Function : 現在の位置のファイル・URLを開く
;; History  : 2014/02/02 add
;; Install  : build-in
;; ------------------------------------------------------------------------
(ffap-bindings)

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
;; Name     : yasnippet
;; Function : スニペット管理
;; History  : 2014/02/11
;; Install  : elpa
;; ------------------------------------------------------------------------
;; 問い合わせを簡略化 yes/no を y/n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'yasnippet)
(yas/load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;; -----------------------------------------------------------------------
;; Name     : anzu
;; Install  : el-get
;; Function : インタラクティブ検索、置換
;;            http://qiita.com/syohex/items/56cf3b7f7d9943f7a7ba
;;            https://github.com/syohex/emacs-anzu
;; ------------------------------------------------------------------------
(require 'anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
		    :foreground "yellow" :weight 'bold)

;; -----------------------------------------------------------------------
;; Name     : expand-region
;; Install  : el-get
;; Function : リージョンを広げる
;; http://d.hatena.ne.jp/syohex/20120117/1326814127
;; ------------------------------------------------------------------------
;;(require 'expand-region)
;;(global-set-key (kbd "C-@") 'er/expand-region)
;;(global-set-key (kbd "C-M-@") 'er/contract-region)

;; transient-mark-modeが nilでは動作しませんので注意
;;(transient-mark-mode t)
;; -----------------------------------------------------------------------
;; Name     : multiple-cursors
;; Install  : el-get
;; Function : sublime textみたいに、複数行を編集
;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
;; ------------------------------------------------------------------------
(require 'multiple-cursors)

(global-set-key (kbd "C-c C-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)

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
;; Name     : custom
;; Install  : 自動的に挿入される
;; ------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(anzu-use-mimego t)
 '(custom-safe-themes (quote ("2b484c630af2578060ee43827f4785e480e19bab336d1ccb2bce5c9d3acfb652" "ea4035bd249cc84f038158d1eb17493623c55b0ca92d9f5a1d036d2837af2e11" "9fd20670758db15cc4d0b4442a74543888d2e445646b25f2755c65dcd6f1504b" default)))
 '(ecb-options-version "2.40")
 '(org-agenda-files (quote ("~/gtd/main.org")))
 '(rspec-use-rake-when-possible nil)
 '(safe-local-variable-values (quote ((require-final-newline . t))))
 '(vbasense-tli-files (quote ("c:\\Program Files (x86)\\Microsoft Office\\OFFICE14\\EXCEL.EXE" "c:/Program Files (x86)/Common Files/Microsoft Shared/VBA/VBA7/VBE7.DLL" "c:/Program Files (x86)/Common Files/Microsoft Shared/VBA/VBA6/VBE6EXT.OLB" "c:/Program Files (x86)/Common Files/Microsoft Shared/OFFICE14/MSO.DLL" "C:\\Windows\\SysWOW64\\stdole2.tlb")))
 '(visual-basic-mode-indent 2)
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))) t)
 '(markdown-pre-face ((t (:foreground "brightmagenta"))) t)
 '(minibuffer-prompt ((t (:foreground "brightblue")))))

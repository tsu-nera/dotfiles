;; ------------------------------------------------------------------------
;; Name     : setup
;; Function : Emacs高速起動のためのマクロライブラリ
;;            EmacsWiki
;; ------------------------------------------------------------------------
(require 'setup)

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
(global-set-key (kbd "C-M-r")   'replace-regexp)
(global-set-key (kbd "C-r")     'replace-string)
(global-set-key (kbd "C-/")     'undo)

;; ------------------------------------------------------------------------
;; General Value
;; ------------------------------------------------------------------------
;;; ツールバーを消す
;;; (tool-bar-mode -1)

;; emacs -nw で起動した時にメニューバーを消す
(if window-system (menu-bar-mode 1) (menu-bar-mode -1))

;;; 対応する括弧を光らせる。
(show-paren-mode 1)

;;; モードラインに時間を表示する
(display-time)
(setq display-time-day-and-date t)
;;; 現在の関数名をモードラインに表示
(which-function-mode 1)

(global-linum-mode t)   ;; 行番号の表示
;;(global-hl-line-mode 1) ;; 現在行に色をつける

;;; 画像ファイルを表示
(auto-image-file-mode t)
;; (setq-default mode-line-format
;; 	      '("-"
;; 		mode-line-mule-info
;; 		mode-line-modified
;; 		" "
;; 		mode-line-buffer-identification
;; 		" "
;; 		global-mode-string
;; 		" %[("
;; 		mode-name
;; 		mode-line-process
;; 		minor-mode-alist
;; 		"%n" ")%]"
;; 		(which-func-mode ("" which-func-format "-"))
;; 		"-%-"
;; 		)
;; 	      )

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
;;; avoid "Symbolic link to Git-controlled source file;; follow link? (yes or no)"
(setq git-follow-symlinks t)

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
;; Name     :  パスの設定
;; Install  :
;; Function : http://sakito.jp/emacs/emacsshell.html#emacs
;; ------------------------------------------------------------------------
;; (let* ((zshpath (shell-command-to-string
;; 		          "/usr/bin/env zsh -c 'printenv PATH'"))
;;               (pathlst (split-string zshpath ":")))
;;     (setq exec-path pathlst)
;;       (setq eshell-path-env zshpath)
;;         (setenv "PATH" zshpath))

;; パスの引き継ぎ
;; exec-path-from-shell from el-get
(exec-path-from-shell-initialize)

;; -----------------------------------------------------------------------
;; Name     :  シェルの設定
;; Install  :
;; Function : http://sakito.jp/emacs/emacsshell.html#emacs
;; ------------------------------------------------------------------------
;;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; http://d.hatena.ne.jp/mooz/20090613/p1
;; コントロールシーケンスを利用した色指定が使えるように
(require 'ansi-color)
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
    "Set `ansi-color-for-comint-mode' to t." t)

(add-hook 'shell-mode-hook
	  '(lambda ()
	     ;; zsh のヒストリファイル名を設定
	     (setq comint-input-ring-file-name "~/.histfile")
	     ;; ヒストリの最大数
	     (setq comint-input-ring-size 1024)
	     ;; 既存の zsh ヒストリファイルを読み込み
	     (comint-read-input-ring t)
	     ;; zsh like completion (history-beginning-search)
	     (local-set-key "\M-p" 'comint-previous-matching-input-from-input)
	     (local-set-key "\M-n" 'comint-next-matching-input-from-input)
	     ;; 色の設定
	     (setq ansi-color-names-vector
		   ["#000000"           ; black
		    "#ff6565"           ; red
		    "#93d44f"           ; green
		    "#eab93d"           ; yellow
		    "#204a87"           ; blue
		    "#ce5c00"           ; magenta
		    "#89b6e2"           ; cyan
		    "#ffffff"]          ; white
		   )
	     (ansi-color-for-comint-mode-on)
	     )
	  )
;; utf-8
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)


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
;; Name     : EmacsでTODOをハイライト
;; Install  : http://stackoverflow.com/questions/8551320/
;;            highlighting-todos-in-all-programming-modes
;; ------------------------------------------------------------------------
(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
           '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))


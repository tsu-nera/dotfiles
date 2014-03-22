<<<<<<< HEAD
<<<<<<< HEAD
=======

>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
=======

>>>>>>> 4470c724dede7b905611a286407e2b0987c595bf
;; general key bind
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

;;; ツールバーを消す
(tool-bar-mode -1)

(menu-bar-mode -1)
;;; emacs -nw で起動した時にメニューバーを消す
;; (if window-system (menu-bar-mode 1) (menu-bar-mode -1))

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
;; ------------------------------------------------------------------------
;; Emacs Client
;; ------------------------------------------------------------------------
;; server start for emacs-client
;; http://d.hatena.ne.jp/syohex/20101224/1293206906
(require 'server)
(unless (server-running-p)
  (server-start))

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
;; Function : EmacsとXのクリップポードを共有
;; Install  : http://tubo028.hatenablog.jp/entry/2013/09/01/142238
;; ------------------------------------------------------------------------
(when linux-p
(if (display-graphic-p)
    ((progn )
      ;; if on window-system
      (setq x-select-enable-clipboard t)
      (global-set-key "\C-y" 'x-clipboard-yank))
  ;; else (on terminal)
  (setq interprogram-paste-function
	(lambda ()
	  (shell-command-to-string "xsel -b -o")))
  (setq interprogram-cut-function
	(lambda (text &optional rest)
	  (let* ((process-connection-type nil)
		 (proc (start-process "xsel" "*Messages*" "xsel" "-b" "-i")))
	    (process-send-string proc text)
	    (process-send-eof proc)))))

(setq x-select-enable-clipboard t);; OSとのクリップボード共有
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
(let* ((zshpath (shell-command-to-string
		          "/usr/bin/env zsh -c 'printenv PATH'"))
              (pathlst (split-string zshpath ":")))
    (setq exec-path pathlst)
      (setq eshell-path-env zshpath)
        (setenv "PATH" zshpath))
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


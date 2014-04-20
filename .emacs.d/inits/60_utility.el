;;-----------------------------------------------------------------------
;; Emacsですべてこなす
;; Emacsは世界
;; Emacsは人生
;; このelispこそ、Emacsのすごさを示すもの
;;------------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Name     : webkit
;; Install  : el-get
;; Function : web browser
;; ------------------------------------------------------------------------
;; 動かない。。。
;; (require 'webkit)

;; -----------------------------------------------------------------------
;; Name     : wanderlust
;; Install  :el-get
;; Function : emacsのメーラ
;; ------------------------------------------------------------------------
(setq ssl-certificate-verification-policy 1) ; この行がないとimapサーバに繋がらない
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; -----------------------------------------------------------------------
;; Name     : esup
;; Function : Emacs 起動時のプロファイラ
;; ------------------------------------------------------------------------
(autoload 'esup "esup" "Emacs Start Up Profiler." nil)

;; -----------------------------------------------------------------------
;; Name     :  シェルの設定(ansi-termをしよう)
;; Install  :
;; Function : http://sakito.jp/emacs/emacsshell.html#emacs
;;            Emacs上のシェル
;; ------------------------------------------------------------------------
;; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      ;; Emacs + Cygwin を利用する人は Zsh の代りにこれにしてください
      ;; (executable-find "f_zsh")
      ;; Emacs + Cygwin を利用する人は Bash の代りにこれにしてください
      ;; (executable-find "f_bash") 
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))

;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

;; エスケープを綺麗に表示する(lsとか)
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
	    (setq comint-input-ring-file-name "~/.zsh-histry")
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

;; Emacs が保持する terminfo を利用する
;; (setq system-uses-terminfo nil)

;; shellのキーバインド
(global-set-key (kbd "C-c t") '(lambda ()
                                (interactive)
                                (ansi-term shell-file-name)))

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

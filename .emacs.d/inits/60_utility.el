;;-----------------------------------------------------------------------
;; Emacsですべてこなす
;; Emacsは世界
;; Emacsは人生
;; このelispこそ、Emacsのすごさを示すもの
;;------------------------------------------------------------------------

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

;; -----------------------------------------------------------------------
;; Name     : howm
;; Install  : el-get
;; Function : Evernoteを越えるメモ管理ツール
;; Refs
;; http://www.gfd-dennou.org/member/uwabami/cc-env/emacs/howm_config.html
;; http://d.hatena.ne.jp/TakashiHattori/20120627/1340768058
;; ------------------------------------------------------------------------
;; *.org を開いたら howm-mode も起動する
;;(add-hook 'org-mode-hook 'howm-mode)

;; howm のメモを置くディレクトリ(任意)
(setq howm-directory "~/gtd/howm") ;; メニュー表示しない
(setq howm-menu-top nil)
;; メニューの言語設定
(setq howm-menu-lang 'ja)
;; howm ファイル名を設定する。org-mode を起動するため拡張子は .org にする。
(setq howm-file-name-format "%Y%m%d-%H%M%S.org")
(setq howm-view-title-header "*") ;; ← howm のロードより前に書くこと

;; キーバインドは C-a C-aにする
(global-unset-key (kbd "C-x C-a"))
(setq howm-prefix (kbd "C-x C-a"))

;;(autoload 'howm "howm" " Hitori Otegaru Wiki Modoki" nil)
(require 'howm)
(add-hook 'howm-mode-hook 'helm-howm)
;; (require 'helm-howm)

;; -----------------------------------------------------------------------
;; Name     : magit
;; Install  : el-get
;; Function : Emacsの Git Client
;; Refs
;; https://github.com/magit/magit
;; http://qiita.com/takc923/items/c7a11ff30caedc4c5ba7
;; チーとシーと
;; http://daemianmack.com/magit-cheatsheet.html
;; ------------------------------------------------------------------------
(autoload 'magit "magit" "An Emacs mode for Git" nil)
(setq magit-git-executable "git")
(setq magit-emacsclient-executable "emacsclient")

(define-key global-map (kbd "C-c m") 'magit-status)

;; -----------------------------------------------------------------------
;; Name     : pdf-tools
;; Install  : recipe
;;            sudo apt-get install libpoppler-glib-dev
;; Function : PDF Viewer
;; Refs
;;   http://sheephead.homelinux.org/2014/03/17/7076/
;; ------------------------------------------------------------------------
;; あとでautoloadに改善する。
(require 'pdf-tools)
(require 'pdf-annot) 
(require 'pdf-history) 
(require 'pdf-info) 
(require 'pdf-isearch) 
(require 'pdf-links) 
(require 'pdf-misc) 
(require 'pdf-occur) 
(require 'pdf-outline) 
(require 'pdf-render) 
(require 'pdf-sync) 
(require 'tablist-filter)
(require 'tablist)

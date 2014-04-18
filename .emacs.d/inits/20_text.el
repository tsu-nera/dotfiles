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


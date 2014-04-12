;; ------------------------------------------------------------------------
;; Name     : helm
;; Function : 置換機能
;; History  : 2014.1.15 Add
;; Install  : https://github.com/emacs-helm/helm
;; ------------------------------------------------------------------------
(eval-when-compile (require 'cl))

;;; ミニバッファで C-h でヘルプでないようにする
(load "term/bobcat")
(when (fboundp 'terminal-init-bobcat)
  (terminal-init-bobcat))

(require 'helm-config)
(require 'helm-command)
(require 'helm-descbinds)
;(require 'helm-recentf)
;(require 'helm-c-moccur)
;(require 'helm-migemo)
(require 'gist)
(require 'helm-gist)
(autoload 'helm-github-issues "helm-github-issues"
  "Helm interface for github issues" nil)

(setq helm-idle-delay             0.3
      helm-input-idle-delay       0.3
      helm-candidate-number-limit 200)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-r") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
					;(let ((key-and-func
;;       `((,(kbd "C-r")   helm-for-files)
;;         (,(kbd "C-^")   helm-c-apropos)
;;         (,(kbd "C-;")   helm-resume)
;;         (,(kbd "M-s")   helm-occur)
;;         (,(kbd "M-x")   helm-M-x)
;;         (,(kbd "M-y")   helm-show-kill-ring)
;;         (,(kbd "M-z")   helm-do-grep)
;;         (,(kbd "C-S-h") helm-descbinds)
;;        )))
;;  (loop for (key func) in key-and-func
;;        do (global-set-key key func)))

;; ------------------------------------------------------------------------
;; Name     : Anything
;; Install  : (auto-install-bitch "anything")
;; Function : 自動補完
;; History  : 13/10/14 Install
;; ------------------------------------------------------------------------
;(when (require 'anything nui t)
;;  (setq
;;; 候補を表示するまでの時間
;;   anything-idle-delay 0.3
;;; タイプして再描写するまでの時間
;;   anything-input-idle-delay 0.2
;;; 候補の最大表示数
;;   anything-candidate-number-limit 100
;;; 体感速度アップ
;;   anything-quick-update t
;;; 候補選択をアルファベット順
;;   anything-enable-shortcuts 'alphabet)

;; anything の補間強化
;;  (require 'anything-match-plugin nil t)
					;)
(require 'anything)
(require 'anything-rdefs)

(setq ar:command "~/.rbenv/shims/rdefs")
(define-key ruby-mode-map "\C-\M-o" 'anything-rdefs)

;;(require 'anything-rake) 動かない。

;;;; color-moccur.elの設定
;; (require 'color-moccur)
;;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
;;(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
;;(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
;;  (setq moccur-use-migemo t))

;;;; anything-c-moccurの設定
;;(require 'anything-c-moccur)
;;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
;;(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
;;      anything-c-moccur-higligt-info-line-flag t ;; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
;;      anything-c-moccur-enable-auto-look-flag t ;; 現在選択中の候補の位置を他のwindowに表示する
;;      anything-c-moccur-enable-initial-pattern t) ;; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;;;; キーバインドの割当(好みに合わせて設定してください)
;;(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
;;(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
;;(add-hook 'dired-mode-hook ;dired
;;	  '(lambda ()
;;	     (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

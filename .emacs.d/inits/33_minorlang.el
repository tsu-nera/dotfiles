;; -----------------------------------------------------------------------
;; Name     :  Oz
;; Function :  マルチパラダイム言語
;; ------------------------------------------------------------------------
(add-hook 'oz-mode-hook
	  (lambda ()
	    (define-key oz-mode-map "\C-c\C-b" 'oz-feed-buffer)
	    (define-key oz-mode-map "\C-c\C-l" 'oz-feed-line)
	    (define-key oz-mode-map "\C-c\C-r" 'oz-feed-region)))
;; -----------------------------------------------------------------------
;; Name     :  Cool
;; ------------------------------------------------------------------------
(autoload 'cool-mode "cool-mode" "Major mode for editing COOL programs" t)
(setq auto-mode-alist
            (append '(("\\.cl\\'" . cool-mode)) auto-mode-alist))

;; -----------------------------------------------------------------------
;; Name     :  VBA
;; ------------------------------------------------------------------------
(require 'vbasense)

;; キーバインド
(setq vbasense-popup-help-key "C-:")
(setq vbasense-jump-to-definition-key "C->")

;; 必要に応じて適宜カスタマイズして下さい。以下のS式を評価することで項目についての情報が得られます。
;; (customize-group "vbasense")

;; 推奨設定を行う
(vbasense-config-default)(require 'vbasense)

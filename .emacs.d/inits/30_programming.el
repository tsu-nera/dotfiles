;; -----------------------------------------------------------------------
;; Name     : highight-symbol/auto-highlight-symbol
;; Install  : el-get
;; Function : シンボルをハイライト
;; http://shibayu36.hatenablog.com/entry/2013/12/30/190354
;; ------------------------------------------------------------------------
(require 'auto-highlight-symbol-config)
(require 'highlight-symbol)
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

;;(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
;;(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

;; -----------------------------------------------------------------------
;; Name     : flymake
;; Function : 静的文法チェック
;; History  : 2014/02/06
;; Install  : package.el
;; ------------------------------------------------------------------------
(require 'flymake)

;; GUIの警告は表示しない
(setq flymake-gui-warnings-enabled nil)

;; 全てのファイルで flymakeを有効化
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; M-p/M-n で警告/エラー行の移動
(global-set-key "\M-p" 'flymake-goto-prev-error)
(global-set-key "\M-n" 'flymake-goto-next-error)

;; 警告エラー行の表示
(global-set-key "\C-cd" 'flymake-display-err-menu-for-current-line)

(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
		       temp-file
		       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(push '("\\.c$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (flymake-mode t)))(require 'flymake)

(add-hook 'c-mode-hook
	  '(lambda ()
	     (flymake-mode t)))(require 'flymake)

;;(require 'flymake-ruby)
;;(add-hook 'enh-ruby-mode-hook 'flymake-ruby-load)

;; -----------------------------------------------------------------------
;; Name     : flycheck
;; Function : 静的文法チェック
;; History  : 2014/02/06
;; Install  : package.el
;; ------------------------------------------------------------------------
(require 'flycheck)
;;(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; Ruby
(add-hook 'ruby-mode-hook 'flycheck-mode)


(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; definition for flycheck
(flycheck-define-checker ruby-rubocop
  "A Ruby syntax and style checker using the RuboCop tool.
   See URL `http://batsov.com/rubocop/'."
  :command ("rubocop" "--format" "emacs" "--silent"
	    (config-file "--config" flycheck-rubocoprc)
	    source)
  :error-patterns
  ((warning line-start
	    (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
	    line-end)
   (error line-start
	  (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
	  line-end))
  :modes (ruby-mode))

;; definition for flycheck
(flycheck-define-checker ruby-rubylint
  "A Ruby syntax and style checker using the rubylint tool."
  :command ("ruby-lint" source)
  :error-patterns
  ((warning line-start
	    (file-name) ":" line ":" column ": " (or "C" "W") ": " (message)
	    line-end)
   (error line-start
	  (file-name) ":" line ":" column ": " (or "E" "F") ": " (message)
	  line-end))
  :modes (ruby-mode))

;; -----------------------------------------------------------------------
;; Name     : github関連
;; ------------------------------------------------------------------------
(require 'helm-github-issues)
(require 'helm-open-github)

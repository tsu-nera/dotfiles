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
;; 起動時にproxyパスワードが求められるので一旦封印する
;; (require 'helm-github-issues)
;; (require 'helm-open-github)

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
;; Name     : magit
;; Install  : el-get
;; Function : Emacsの Git Client
;; Refs
;; https://github.com/magit/magit
;; http://qiita.com/takc923/items/c7a11ff30caedc4c5ba7
;; チーとシーと
;; http://daemianmack.com/magit-cheatsheet.html
;; ------------------------------------------------------------------------
(autoload 'magit "magit" "An Emacs mode for Git" t t)
(autoload 'magit-svn "magit-svn" "An Emacs mode for Subversion" t t)

(setq magit-git-executable "git")
(setq magit-emacsclient-executable "emacsclient")

(define-key global-map (kbd "C-c m") 'magit-status)

;; ------------------------------------------------------------------------
;; Name     : ediff
;; Function : emacsようdiffツール
;; build-in :
;; http://www.emacswiki.org/emacs/EdiffMode
;;; ------------------------------------------------------------------------
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; 縦に分割
(setq ediff-split-window-function 'split-window-horizontally)
;; ウィンドウサイズによっては横分割
(setq ediff-split-window-function (if (> (frame-width) 150)
				      'split-window-horizontally
				    'split-window-vertically))

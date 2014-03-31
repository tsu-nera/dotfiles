;;------------------------------------------------------------------------
;; Global settigngs 
;;------------------------------------------------------------------------
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
(add-to-load-path "elisp" "public_repos" "elpa" "inits" "el-get")
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
;; emacs-evernote-mode
;; ------------------------------------------------------------------------
;; (require 'evernote-mode)
;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)
;; (setq evernote-username "fox10225fox")  ;; Evernote アカウント名

;; ------------------------------------------------------------------------
;; Rst-mode (for Sphinx)
;; ------------------------------------------------------------------------
;;; RSTモードを見やすくする。
(setq frame-background-mode 'dark)

;;; rst.elを読み込み
(require 'rst)
;;; *.rst, *.restファイルをrst-modeでOpen
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode)
		) auto-mode-alist))

;;; 全部スペースでインデントしましょう
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))

(add-hook 'rst-mode-hook
	  (lambda ()
	    (setq rst-slides-program "open -a Firefox")
	    ))

;; ------------------------------------------------------------------------
;; others
;; ------------------------------------------------------------------------
;; git管理のシンボリックリンクで質問されないためのおまじない。
;; 参考: http://openlab.dino.co.jp/2008/10/30/212934368.html
;;; avoid "Symbolic link to Git-controlled source file;; follow link? (yes or no)"
(setq git-follow-symlinks t)

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
;; Name     : Markdown Mode
;; Function : Use Markdown
;; History  : 2014.1.11 Add
;; Install  : http://jblevins.org/projects/markdown-mode/markdown-mode.el
;; ------------------------------------------------------------------------
(autoload
  'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
;; associate .md file to markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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
;; Name     : conkeror
;; Function : web browser based on emacs key bind
;; History  : 2014.1.24 Add
;; Install  : http://www.emacswiki.org/emacs/Conkeror
;; ------------------------------------------------------------------------
(setq browse-url-generic-program (executable-find "conkeror"))
(setq browse-url-browser-function 'browse-url-generic)

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
;;(ffap-bindings)

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
 '(yas-trigger-key "TAB"))

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
;; Name     : wanderlust
;; Install  :el-get
;; Function : emacsのメーラ
;; ------------------------------------------------------------------------
;; (autoload 'wl "wl" "Wanderlust" t)
;; (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
;; (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; (require 'starttls)
;; (require 'ssl)
;; (setq ssl-certificate-verification-policy 1) 
;; (setq ssl-program-name "openssl")
;; (setq starttls-negotiation-by-kill-program t)

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
;; Name     : plantuml
;; Install  : 
;; ------------------------------------------------------------------------
;; (require 'plantuml-mode)
;; (add-to-list 'auto-mode-alist '("\\.puml$" . plantuml-mode))
;; (add-to-list 'auto-mode-alist '("\\.plantuml$" . plantuml-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))) t)
 '(markdown-pre-face ((t (:foreground "brightmagenta"))) t)
 '(minibuffer-prompt ((t (:foreground "brightblue")))))

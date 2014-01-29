;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; general key bind
;;______________________________________________________________________
(global-set-key (kbd "C-c a")   'align)
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

;; -----------------------------------------------------------------------
;; Name     :
;; Function :
;; History  :
;; Install  :
;; ------------------------------------------------------------------------
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
;;(add-to-load-path "elisp" "conf" "public_repos" "elisp/ruby")
(add-to-load-path "elisp" "conf" "public_repos" "elpa")

					; 行番号の表示
(global-linum-mode t)

;; ------------------------------------------------------------------------
;; Name     : auto-install
;; Function :
;; History  :
;; Install  : http://www.emacswiki.org/emacs/download/auto-install.el
;; ------------------------------------------------------------------------
(when(require 'auto-install nil t)
  ;;インストールディレクトリを設定する  初期値は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  ;;; 起動時にnetwork unreachableってでるので、とりあえず封印 13/05/26
  ;;(auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定を行う
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にす
  (auto-install-compatibility-setup))

;; ------------------------------------------------------------------------
;; Name     : package.el
;; Function :
;; History  : 2014/01/16 add
;; Install  : http://www.emacswiki.org/emacs/download/auto-install.el
;; ------------------------------------------------------------------------
(require 'package)
;; Add package-archives
;; Melpa: githubからelispを落とすリポジトリを追加
;; これで、 M-x list-packagesで melpaが利用できる。
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Initialize
(package-initialize)

;; ------------------------------------------------------------------------
;; face-display Setting
;; ------------------------------------------------------------------------
;;; 色を設定する
;;; 設定自体は M-x list-face-displaysから.emacsに自動生成されたものをcopy
(custom-set-variables
 ;;; custom-set-variables was added by Custom.
 ;;; If you edit it by hand, you could mess it up, so be careful.
 ;;; Your init file should contain only one such instance.
 ;;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("2b484c630af2578060ee43827f4785e480e19bab336d1ccb2bce5c9d3acfb652" "ea4035bd249cc84f038158d1eb17493623c55b0ca92d9f5a1d036d2837af2e11" "9fd20670758db15cc4d0b4442a74543888d2e445646b25f2755c65dcd6f1504b" default))))
(custom-set-faces
 ;;; custom-set-faces was added by Custom.
 ;;; If you edit it by hand, you could mess it up, so be careful.
 ;;; Your init file should contain only one such instance.
 ;;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))))
 '(markdown-pre-face ((t (:foreground "brightmagenta"))))
 '(minibuffer-prompt ((t (:foreground "brightblue")))))


;; ------------------------------------------------------------------------
;; Name     : Anything
;; Install  : (auto-install-bitch "anything")
;; Function : 自動補完
;; History  : 13/10/14 Install
;; ------------------------------------------------------------------------
;; TODO 使い方がわからないので、封印しておく。
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

;;;; color-moccur.elの設定
(require 'color-moccur)
;;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
(setq moccur-split-word t)
;;; migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
  (setq moccur-use-migemo t))

;;;; anything-c-moccurの設定
(require 'anything-c-moccur)
;;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ;; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ;; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t) ;; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;;;; キーバインドの割当(好みに合わせて設定してください)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
(add-hook 'dired-mode-hook ;dired
	  '(lambda ()
	     (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))
;; ------------------------------------------------------------------------
;; emacs-evernote-mode
;; ------------------------------------------------------------------------
(require 'evernote-mode)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)
(setq evernote-username "fox10225fox")  ;; Evernote アカウント名

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
;; Emacs Client
;; ------------------------------------------------------------------------
;; server start for emacs-client
;; http://d.hatena.ne.jp/syohex/20101224/1293206906
(require 'server)
(unless (server-running-p)
  (server-start))


;; ------------------------------------------------------------------------
;; org2blog
;; ------------------------------------------------------------------------
;; Emacs から WordPressに投稿するLisp
;; https://github.com/punchagan/org2blog
;; xml-rspも入れた
;; http://launchpadlibrarian.net/40270196/xml-rpc.el
;; metaweblogも入れた
;; git://github.com/punchagan/metaweblog.el.git
					;(require 'metaweblog)
					;(require 'org2blog-autoloads)
					;(setq org2blog/wp-blog-alist
;;       '(("SternStunden" ;;; ブログの名前
;;          :url "http://hmi-me.ciao.jp/sternstunden/xmlrpc.php";;; xmlrcp path
;;         :username "admin" ;;; ユーザ名
;;; :password "hoge" ;;; パスワードは封印
;;         :default-categories ("daily") )))


;; ------------------------------------------------------------------------
;; others
;; ------------------------------------------------------------------------
;; git管理のシンボリックリンクで質問されないためのおまじない。
;; 参考: http://openlab.dino.co.jp/2008/10/30/212934368.html
;;; avoid "Symbolic link to Git-controlled source file;; follow link? (yes or no)"
;; (setq git-follow-symlinks t)

;; ------------------------------------------------------------------------
;; Name     : auto-complete
					;u URL      : http://www.emacswiki.org/emacs/auto-complete-extension.el
;; Function : 自動補完を実現するelisp
;; History  : 13/10/14
;; ------------------------------------------------------------------------
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; ------------------------------------------------------------------------
;; C/C++
;; ------------------------------------------------------------------------
;;; C-c c で compile コマンドを呼び出す
(define-key mode-specific-map "c" 'compile)

;; ------------------------------------------------------------------------
;; Ruby
;; ------------------------------------------------------------------------
;;;  ruby-mode
					;http://shibayu36.hatenablog.com/entry/2013/03/18/192651
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;;; ruby-electric.el --- electric editing commands for ruby files
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;;;; ruby-block.el --- highlight matching block
;;(require 'ruby-block)
;;(ruby-block-mode t)
;;(setq ruby-block-highlight-toggle t)

;;; rcodetools
;;(require 'rcodetools)
;;(setq rct-find-tag-if-available nil)
;;(defun ruby-mode-hook-rcodetools ()
;;;  (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
;;;  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
;;;  (define-key ruby-mode-map "\C-c\C-d" 'xmp)
;;;  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))
;;(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

					;(require 'anything-rcodetools)
					;(setq rct-get-all-methods-command "PAGER=cat fri -l")
;;; See docs
					;(define-key anything-map [(control ?)] 'anything-execute-persistent-action)

;;; smart-compile
;;; http://www.emacswiki.org/emacs/download/smart-compile.el
(require 'smart-compile)
(define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
(define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))
(setq compilation-window-height 15) ;; default window height is 15

;; ------------------------------------------------------------------------
;; Name     : Markdown Mode
;; Function : Use Markdown
;; History  : 2014.1.11 Add
;; Install  : http://jblevins.org/projects/markdown-mode/markdown-mode.el
;; ------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
;; associate .md file to markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ------------------------------------------------------------------------
;; Name     : Emacs Color theme
;; Function :
;; History  : 2014.1.14 Add
;; Install  : https://code.google.com/p/gnuemacscolorthemetest/
;; ------------------------------------------------------------------------
(require 'color-theme)
(color-theme-initialize)

;; ------------------------------------------------------------------------
;; Name     : Almost Monokai
;; Function : Beautiful Color theme
;; History  : 2014.1.14 Add
;; Install  : https://raw2.github.com/zanson/color-theme-almost-monokai/master/color-theme-almost-monokai.el
;; ------------------------------------------------------------------------
					;(load-file "~/.emacs.d/elisp/color-theme/themes/color-theme-almost-monokai.el")
					;(color-theme-almost-monokai)

;; ------------------------------------------------------------------------
;; Name     : Molokai
;; Function : Most popular color theme
;; History  : 2014.1.14 Add
;; Install  : https://raw2.github.com/hbin/molokai-theme/master/molokai-theme-kit.el
;; ------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq molokai-theme-kit t)
(load-theme 'molokai t)


;; ------------------------------------------------------------------------
;; Name     : PowerLine
;; Function : Most popular color theme
;; History  : 2014.1.14 Add
;; Install  : http://www.emacswiki.org/emacs/powerline.el
;; ------------------------------------------------------------------------
(require 'powerline)

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
					;(require 'helm-c-moccur)
					;(require 'helm-migemo)

(setq helm-idle-delay             0.3
      helm-input-idle-delay       0.3
      helm-candidate-number-limit 200)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-r") 'helm-occur)
(global-set-key (kbd "C-x r") 'helm-resentf)
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
;; Name     : popwin
;; Function : ポップアップ表示
;; History  : 2014.1.15 Add
;; Install  : package.el経由
;; ------------------------------------------------------------------------
(when (require 'popwin)
  (setq helm-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config '(("*compilatoin*" :noselect t)
					;;                                        ("helm" :regexp t :height 0.4)
					("anything" :regexp t :height 0.4)
					)))
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
(require 'keisen)
;;; Control+矢印キーで罫線を引く場合
(global-set-key (kbd "\S-f") 'keisen-right-move)
(global-set-key (kbd "\S-b") 'keisen-left-move)
(global-set-key (kbd "\S-p") 'keisen-up-move)
(global-set-key (kbd "\S-n") 'keisen-down-move)
			  

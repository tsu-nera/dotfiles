;; -----------------------------------------------------------------------
;; Name     : CEDET
;; Function : 統合開発環境
;; History  : 2014/02/04 add 
;; Install  : http://www.logilab.org/blogentry/173886
;; ------------------------------------------------------------------------
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: Tou must place this *before* any CEDET component (including
;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
(load-file "/home/tsu-nera/.emacs.d/cedet-bzr/trunk/cedet-devel-load.el")

(semantic-mode 1)  ;; Enable Semantic
;; (global-ede-mode 1);; Enable EDE (Project Management) features
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion

(setq semantic-default-submodes
            '(
	      global-semantic-idle-scheduler-mode
	      global-semantic-idle-completions-mode
	      global-semanticdb-minor-mode
	      global-semantic-decoration-mode
	      global-semantic-highlight-func-mode
	      global-semantic-stickyfunc-mode
	      global-semantic-mru-bookmark-mode
	      ))
		
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

;; -----------------------------------------------------------------------
;; Name     : 
;; Function : EmacsとXのクリップポードを共有
;; Install  : http://tubo028.hatenablog.jp/entry/2013/09/01/142238
;; ------------------------------------------------------------------------
(if (display-graphic-p)
    (progn
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

;;------------------------------------------------------------------------
;; Global settigngs 
;;------------------------------------------------------------------------
(global-linum-mode t)   ;; 行番号の表示
;; (global-hl-line-mode 1) ;; 現在行に色をつける

;; general key bind
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
;; Name     : el-get.el
;; Function : eLisp管理
;; Install  : cd public_repos 
;;            git clone git@github.com:dimitri/el-get.git
;; ------------------------------------------------------------------------
(setq el-get-dir "~/.emacs.d/elisp/el-get/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(el-get 'sync 'helm 'yasnippet)
;; ------------------------------------------------------------------------
;; face-display Setting
;; ------------------------------------------------------------------------
;;; 色を設定する
;;; 設定自体は M-x list-face-displaysから.emacsに自動生成されたものをcopy
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("2b484c630af2578060ee43827f4785e480e19bab336d1ccb2bce5c9d3acfb652" "ea4035bd249cc84f038158d1eb17493623c55b0ca92d9f5a1d036d2837af2e11" "9fd20670758db15cc4d0b4442a74543888d2e445646b25f2755c65dcd6f1504b" default)))
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face))) t)
 '(markdown-pre-face ((t (:foreground "brightmagenta"))) t)
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
;; Emacs Client
;; ------------------------------------------------------------------------
;; server start for emacs-client
;; http://d.hatena.ne.jp/syohex/20101224/1293206906
(require 'server)
(unless (server-running-p)
  (server-start))

;; ------------------------------------------------------------------------
;; Name     : org2blog
;; Function : Emacsからブログ投稿
;:            Emacs から WordPressに投稿するLisp
;; Install  :
;; History  : 2014.02.09 パスワードレスにした
;; ------------------------------------------------------------------------
;; https://github.com/punchagan/org2blog
;; xml-rspも入れた
;; http://launchpadlibrarian.net/40270196/xml-rpc.el
;; metaweblogも入れた
;; git://github.com/punchagan/metaweblog.el.git
(require 'metaweblog)
(require 'org2blog-autoloads)
(require 'netrc) ;; or nothing if already in the load-path
(setq blog (netrc-machine (netrc-parse "~/.netrc") "Futurismo" t))
(setq org2blog/wp-blog-alist
    '(("Futurismo"
	 :url "http://futurismo.biz/xmlrpc.php"
	 :username (netrc-get blog "login")
	 :password (netrc-get blog "password"))))

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

;; ------------------------------------------------------------------------
;; C/C++
;; ------------------------------------------------------------------------
;;; C-c c で compile コマンドを呼び出す
(define-key mode-specific-map "c" 'compile)

;; ------------------------------------------------------------------------
;; Ruby
;; ------------------------------------------------------------------------
;; ruby-mode
;; http://shibayu36.hatenablog.com/entry/2013/03/18/192651
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;;; ruby-electric.el --- electric editing commands for ruby files
;; Emacs24ではうまく動かない。。 ruby-insert-endがなくなったそう。
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;; (setq ruby-electric-expand-delimiters-list nil)

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
;;(require 'anything-rcodetools)
;;(setq rct-get-all-methods-command "PAGER=cat fri -l")
;;; See docs
;;(define-key anything-map [(control ?)] 'anything-execute-persistent-action)

;;; smart-compile
;;; http://www.emacswiki.org/emacs/download/smart-compile.el
;; (require 'smart-compile)
;; (define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
;; (define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))
;; (setq compilation-window-height 15) ;; default window height is 15

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
;;(load-file "~/.emacs.d/elisp/color-theme/themes/color-theme-almost-monokai.el")
;;(color-theme-almost-monokai)

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
;(require 'helm-recentf)
;(require 'helm-c-moccur)
;(require 'helm-migemo)

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
;; Name     : popwin
;; Function : ポップアップ表示
;; History  : 2014.1.15 Add
;; Install  : package.el経由
;; ------------------------------------------------------------------------
(when (require 'popwin)
  (setq helm-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config '(("*compilatoin*" :noselect t)
					;;("helm" :regexp t :height 0.4)
					("anything" :regexp t :height 0.4)
					)))
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
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
;; (global-set-key (kbd "C-M-f") 'keisen-right-move)
;;(global-set-key (kbd "C-M-b") 'keisen-left-move)
;;(global-set-key (kbd "C-M-p") 'keisen-up-move)
;;(global-set-key (kbd "C-M-n") 'keisen-down-move)

(global-set-key [(C-right)] 'keisen-right-move)
(global-set-key [(C-left)] 'keisen-left-move)
(global-set-key [(C-up)] 'keisen-up-move)
(global-set-key [(C-down)] 'keisen-down-move)

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
;; Name     : Emacs Code Browser
;; Function : 
;; History  : 2014/02/05
;; Install  : github
;;            git clone https://github.com/emacsmirror/ecb
;; ------------------------------------------------------------------------
(require 'ecb)
;;(require 'ecb-autoloads)

;;(require 'xrefactory)
(defvar xref-current-project nil) ;; can be also "my_project_name"
(defvar xref-key-binding 'global) ;; can be also 'local or 'none
(setq load-path (cons "/home/tsu-nera/repo/xref/emacs" load-path))
(setq exec-path (cons "/home/tsu-nera/repo/xref" exec-path))
(load "xrefactory")

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


;; -----------------------------------------------------------------------
;; Name     : org-capture
;; Function : アイデアをキャプチャーする
;; History  : 2014/02/25
;; Install  : build-in
;; ------------------------------------------------------------------------
(require 'org-capture)
(setq org-capture-templates
      '(
;;	("t" "Task" entry (file+headline nil "Inbox")
;;	 "** TODO %?\n %T\n %a\n %i\n")
;;	("b" "Bug" entry (file+headline nil "Inbox")
;;	 "** TODO %?   :bug:\n  %T\n %a\n %i\n")
;;	("m" "Meeting" entry (file+headline nil "Meeting")
;;	 "** %?\n %U\n %a\n %i\n")
	("i" "Idea" entry (file+headline nil "~/diary/org/idea.org")
	 "** %?\n %U\n %i\n %a\n %i\n")
	("w" "Twitter" entry (file+datetree "~/diary/org/twitter.org")
	 "** %U %?\n")
	)
      )
(global-set-key (kbd "C-c c") 'org-capture)
;; -----------------------------------------------------------------------
;; Name     : yasnippet
;; Function : スニペット管理
;; History  : 2014/02/11
;; Install  : elpa
;; ------------------------------------------------------------------------
(require 'cl)
;; 問い合わせを簡略化 yes/no を y/n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'yasnippet)
;;(setq yas-snippet-dirs
;;      '("~/.emacs.d/snippets"
;;        ))
(yas-global-mode 1)

;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
;; (setqだとtermなどで干渉問題ありでした)
;; もちろんTAB以外でもOK 例えば "C-;"とか
(custom-set-variables '(yas-trigger-key "TAB"))

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)


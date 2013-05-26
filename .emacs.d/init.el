;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;------------------------------------------------------------------------
;; @ load-path
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
(add-to-load-path "elisp" "conf" "public_repos" "elisp/ruby")

; ------------------------------------------------------------------------
; face-display Setting
; ------------------------------------------------------------------------
;; 色を設定する
;; 設定自体は M-x list-face-displaysから.emacsに自動生成されたものをcopy
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(minibuffer-prompt ((t (:foreground "brightblue")))))

; ------------------------------------------------------------------------
; auto-install
; http://www.emacswiki.org/emacs/download/auto-install.el
; ------------------------------------------------------------------------
(when(require 'auto-install nil t)
  ;;インストールディレクトリを設定する  初期値は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  ;; 起動時にnetwork unreachableってでるので、とりあえず封印 13/05/26
  ;; (auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定を行う
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

; ------------------------------------------------------------------------
; emacs-evernote-mode
; ------------------------------------------------------------------------
(require 'evernote-mode)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)
(setq evernote-username "fox10225fox")  ; Evernote アカウント名

; ------------------------------------------------------------------------
; Rst-mode (for Sphinx)
; ------------------------------------------------------------------------
;; RSTモードを見やすくする。
(setq frame-background-mode 'dark)

;; rst.elを読み込み
(require 'rst)
;; *.rst, *.restファイルをrst-modeでOpen
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
		("\\.rest$" . rst-mode)
		) auto-mode-alist))

;; 全部スペースでインデントしましょう
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))

(add-hook 'rst-mode-hook
	  (lambda ()
	      (setq rst-slides-program "open -a Firefox")
	        ))

; ------------------------------------------------------------------------
; Emacs Client
; ------------------------------------------------------------------------
; server start for emacs-client
; http://d.hatena.ne.jp/syohex/20101224/1293206906
(require 'server)
(unless (server-running-p)
  (server-start))

; ------------------------------------------------------------------------
; Ruby 
; ------------------------------------------------------------------------
;;  ruby-mode 
; http://shibayu36.hatenablog.com/entry/2013/03/18/192651
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; エラーして} が入力できない。とりあえず封印
;; Symbol's value as variable is void: last-command-char
;; ruby-electric.el --- electric editing commands for ruby files
;;(require 'ruby-electric)
;;(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;;(setq ruby-electric-expand-delimiters-list nil)

;;; ruby-block.el --- highlight matching block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; rcodetools
(require 'rcodetools)
(setq rct-find-tag-if-available nil)
(defun ruby-mode-hook-rcodetools ()
  (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key ruby-mode-map "\C-c\C-d" 'xmp)
  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

;(require 'anything-rcodetools)
;(setq rct-get-all-methods-command "PAGER=cat fri -l")
;; See docs
;(define-key anything-map [(control ?)] 'anything-execute-persistent-action)

;; smart-compile                                
;; http://www.emacswiki.org/emacs/download/smart-compile.el
(require 'smart-compile) 
(define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
(define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))
(setq compilation-window-height 15) ; default window height is 15

; ------------------------------------------------------------------------
; org2blog
; ------------------------------------------------------------------------
; Emacs から WordPressに投稿するLisp
; https://github.com/punchagan/org2blog
; xml-rspも入れた
; http://launchpadlibrarian.net/40270196/xml-rpc.el
; metaweblogも入れた
; git://github.com/punchagan/metaweblog.el.git
(require 'metaweblog)
(require 'org2blog-autoloads)
(setq org2blog/wp-blog-alist
       '(("SternStunden" ;; ブログの名前
          :url "http://hmi-me.ciao.jp/sternstunden/xmlrpc.php";; xmlrcp path
         :username "admin" ;; ユーザ名 
	 ;; :password "hoge" ;; パスワードは封印
         :default-categories ("daily") )))

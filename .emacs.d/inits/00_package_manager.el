;; ------------------------------------------------------------------------
;; Name     : el-get.el
;; Function : eLisp管理
;; Install  : cd public_repos 
;;            git clone git@github.com:dimitri/el-get.git
;; ------------------------------------------------------------------------
;; ダウンロードしていないときはダウンロード
;; TODO どうもうまく動作していないようだ。調査する。
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

;; ダウンロードしたelisp置き場
(setq el-get-dir "~/.emacs.d/el-get/repo")

;; レシピ置き場
;;(setq el-get-recipe-path (list "~/.emacs.d/el-get/recipes/emacswiki"
;;			       "~/.emacs.d/el-get/recipes"))
;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/user-recipes")
;;(add-to-list 'el-get-recipe-path '("~/.emacs.d/el-get/user-recipes/myemacswiki"))
;;(add-to-list 'el-get-recipe-path '("~/.emacs.d/el-get/user-recipes"))

(setq el-get-user-package-directory "~/.emacs.d/el-get/init-files")

;; auto install el-get.el
(defvar my/el-get-packages
  '(
    ;; window
    popwin
    ;; text
    yasnippet
    auto-complete
    anzu
    auto-highlight-symbol
    highlight-symbol
    multiple-cursors
    ;; smart-compile 独自改造したものをelispにおいた
    expand-region
;;    setup
    migemo
    ;; utility
    howm
    magit
    pdf-tools
    direx
    stripe-buffer
    wanderlust
    multi-term
    emacs-w3m
    metaweblog
    xml-rpc-el
    tempbuf
    esup
    exec-path-from-shell
    calfw
    ;; helm
    helm
    helm-gist
    helm-c-yasnippet
    helm-descbinds
    anything
    helm-github-issues
    helm-open-github
    ;; helm-shell-history
    ;; org-mode
    org2blog
    org-mode
    org-pomodoro
    org-gcal
    org-pandoc
    ;; color
    color-theme
    powerline 
    molokai-theme
    ;; programming
    gist
    flymake
    flycheck
    flycheck-color-mode-line
    ;; ruby
    ruby-block
    robe-mode
    ruby-electric
    rspec-mode
    ruby-refactor
    rcodetools
    inf-ruby
    auto-complete-ruby
    anything-rdefs
    ;; visual-basic
    vbasense
    visual-basic-mode
    ;; minor-mode
    rainbow-mode
    yaml-mode
    markdown-mode
    cool-mode
    plantuml-mode
    ;; elscreen
    ;; この2つは特別なので一番最後におく
;;    init-loader
;;    el-get
    )
  "A list of packages to install from el-get at launch.")

;; syncを外してみる。
;; proxy配下で起動が遅くなる気がする。
;; (el-get 'sync my/el-get-packages)

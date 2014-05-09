;; -----------------------------------------------------------------------
;; Function : 環境による場合分けの方法
;;   http://d.hatena.ne.jp/tomoya/20090811/1250006208
;; ------------------------------------------------------------------------
(defun x->bool (elt) (not (not elt)))

;; emacs-version predicates
(setq emacs22-p (string-match "^22" emacs-version)
      emacs23-p (string-match "^23" emacs-version)
      emacs23.0-p (string-match "^23\.0" emacs-version)
      emacs23.1-p (string-match "^23\.1" emacs-version)
      emacs23.2-p (string-match "^23\.2" emacs-version))

;; system-type predicates
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (eq window-system 'ns)
      carbon-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      colinux-p (when linux-p
		  (let ((file "/proc/modules"))
		    (and
		     (file-readable-p file)
		     (x->bool
		      (with-temp-buffer
			(insert-file-contents file)
			(goto-char (point-min))
			(re-search-forward "^cofuse\.+" nil t))))))
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

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
(defvar my/el-get-packages-common
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
    ;; utility
    howm
    direx
    stripe-buffer
    tempbuf
    esup
    exec-path-from-shell
    ;; helm
    helm
    helm-c-yasnippet
    helm-descbinds
    anything
    ;; helm-shell-history
    ;; org-mode
    org-mode
    org-pomodoro
    org-pandoc
    ;; color
    color-theme
    powerline 
    molokai-theme
    ;; programming
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
    elscreen
    )
  "A list of packages to install from el-get at launch")

(defvar my/el-get-packages-for-linux
  '(
    migemo
    pdf-tools
    magit
    wanderlust
    emacs-w3m  ;; cvs command not found
    )
  "A list of packages that can build only linux, fail for windows")

(defvar my/el-get-packages-private
  '(
    metaweblog
    xml-rpc-el
    calfw
    helm-gist
    helm-github-issues
    helm-open-github
    org2blog
    org-gcal
    gist
    )
  "A list of packages that is not necessory for my work.")

;; auto install el-get.el
(defvar my/el-get-packages-all
  (append 
  my/el-get-packages-common
  my/el-get-packages-private
  (when linux-p my/el-get-packages-for-linux)
  )
  "A list of packages to install from el-get at launch.")

(el-get 'sync my/el-get-packages-all)
;; (el-get 'sync my/el-get-packages-work)
;; (el-get 'sync my/el-get-packages-private)

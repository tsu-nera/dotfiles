(require 'org-install)
(require 'org-pomodoro)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; アジェンダ表示の対象ファイル
(setq org-agenda-files '("~/gtd/main.org"))

;; key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-c\C-x\C-@" 'org-clock-out)

;; リンクをconkerorで開く
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/home/tsu-nera/bin/conkeror")

;; TODO状態
;;(setq org-todo-keywords
;;      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))

;; DONEの時刻を記録
(setq org-log-done 'time)


;;; org-clock configuration
(setq org-clock-out-remove-zero-time-clocks t
      org-clock-out-when-done t
      org-clock-modeline-total 'today
      org-clock-persist t
      org-clock-persistence-insinuate)
;;(setq org-clock-clocked-in-display 'both)

;(eval-after-load "org-faces"
;  '(set-face-attribute 'org-mode-line-clock nil
;		       :inherit nil))

;;(setq org-clock-heading-function
;;             (lambda ()
;	       (replace-regexp-in-string
;;		 "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
;;		  (nth 4 org-heading-components))))
;;(org-defkey org-agenda-mode-map [(tab)]
;;	    '(lambda () (interactive)
;;	       (org-agenda-goto)
;;	       (with-current-buffer "*Org Agenda*"
;;		 (org-agenda-quit))))
;;

;; NextActionタグを設定
(defvar my-doing-tag "next")
;; nextタグをトグルする
(defun my-toggle-doing-tag ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (save-restriction
        (unless (org-at-heading-p)
          (outline-previous-heading))
        (if (string-match (concat ":" my-doing-tag ":") (org-get-tags-string))
            (org-toggle-tag my-doing-tag 'off)
          (org-toggle-tag my-doing-tag 'on))
        (org-reveal)))))
(global-set-key (kbd "<f11>") 'my-toggle-doing-tag)

; 時間になったら音をならす
(setq org-clock-sound "/usr/share/sounds/LinuxMint/stereo/desktop-login.ogg")

;; wanderlustのメールを追跡できる
(setq org-return-follows-link t)

;; -----------------------------------------------------------------------
;; Function ; 見積り設定
;; ------------------------------------------------------------------------
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%40ITEM(Task) %17Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote ((
      "Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00"))))

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

;; C-oの置き換え tmuxで c-oは使っているので
(define-key org-mode-map "\C-co" 'org-open-at-point)

;; -----------------------------------------------------------------------
;; NextActionの設定
;; http://qiita.com/takaxp/items/4dfa11a81e18b29143ec
;; ------------------------------------------------------------------------
;; タグの色変更
(setq org-tag-faces '(("next" :foreground "#FF0000")))
(defun my-sparse-doing-tree ()
    (interactive)
      (org-tags-view nil "next"))
(define-key org-mode-map (kbd "C-c 3") 'my-sparse-doing-tree)

;; -----------------------------------------------------------------------
;; Name     : plantuml
;; Install  : http://www.emacswiki.org/emacs/IanYang
;; ------------------------------------------------------------------------
;; (require 'org-exp-blocks)
;; (require 'org-export-blocks-format-plantuml)
;;(require 'ob-plantuml)
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
(defun org-mode-init ()
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages '(plantuml . t))))
(add-hook 'org-mode-hook 'org-mode-init)

;; ------------------------------------------------------------------------
;; Name     : org2blog
;; Function : Emacsからブログ投稿
;:            Emacs から WordPressに投稿するLisp
;; Install  :
;; ------------------------------------------------------------------------
;; https://github.com/punchagan/org2blog
;; xml-rspも入れた
;; http://launchpadlibrarian.net/40270196/xml-rpc.el
;; metaweblogも入れた
;; git://github.com/punchagan/metaweblog.el.git
(require 'metaweblog)
(require 'org2blog-autoloads)
;;(require 'netrc) ;; or nothing if already in the load-path
;;(setq blog (netrc-machine (netrc-parse "~/.netrc") "EverClassic" t))
;;(setq blog (netrc-machine (netrc-parse "~/.netrc") "Futurismo" t))
(setq org2blog/wp-blog-alist
;;(setq org2blog/wp-blog-alist
      '(("Futurismo"
	 :url "http://futurismo.biz/xmlrpc.php"
	 :username "admin"
	 ;;:username (netrc-get blog "login")
	 ;;:password (netrc-get blog "password")
	 )
	;; ("EverClassic"
	;;  :url "http://everclassic.biz/xmlrpc.php"
	;;  :username "admin"
	;;  ;;:username (netrc-get blog "login")
	;;  ;;:password (netrc-get blog "password"))
	;;  )
	)
      )

;; DONEをすべてアーカイブ
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;; ------------------------------------------------------------------------
;; Name     : mobileOrg
;; Function : iphoneとorg-modeの同期。Dropboxを利用
;; Install  :
;; ------------------------------------------------------------------------
;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/dropbox/アプリ/MobileOrg")

;; 起動と終了時に同期
;; org-mobile-directryが存在しないとハングするためなしにした
;; そのうちなんか考える
;; (add-hook 'after-init-hook 'org-mobile-pull)
;; (add-hook 'kill-emacs-hook 'org-mobile-push)

;; moble sync
;; http://stackoverflow.com/questions/8432108/how-to-automatically-do-org-mobile-push-org-mobile-pull-in-emacs
(defvar org-mobile-sync-timer nil)
(defvar org-mobile-sync-idle-secs (* 60 10))
(defun org-mobile-sync ()
    (interactive)
      (org-mobile-pull)
        (org-mobile-push))
(defun org-mobile-sync-enable ()
    "enable mobile org idle sync"
      (interactive)
        (setq org-mobile-sync-timer
	      (run-with-idle-timer org-mobile-sync-idle-secs t
				   'org-mobile-sync)));
(defun org-mobile-sync-disable ()
    "disable mobile org idle sync"
      (interactive)
        (cancel-timer org-mobile-sync-timer))
(org-mobile-sync-enable)

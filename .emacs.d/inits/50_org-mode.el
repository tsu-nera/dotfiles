;; -----------------------------------------------------------------------
;; org-mode
;; 
;; こまったらここを見れば日本語訳がある
;; http://orgmode.jp/
;; ------------------------------------------------------------------------
(require 'org-install)
;; (require 'org-pomodoro)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; アジェンダ表示の対象ファイル
(setq org-agenda-files '("~/gtd/inbox.org"
			 "~/gtd/main.org"
			 "~/gtd/schedule.org"))

;; key bindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-c\C-x\C-@" 'org-clock-out) ;; tmuxで C-oは利用しているため
(define-key org-mode-map "\C-co" 'org-open-at-point) ;; C-oの置き換え tmuxで c-oは使っているので
(global-set-key "\C-cC" 'cfw:open-org-calendar)

;; -----------------------------------------------------------------------
;; Name     : tiny-function
;; ------------------------------------------------------------------------
;; wanderlust
;; wanderlustのメールを追跡できる
(setq org-return-follows-link t)

;; -----------------------------------------------------------------------
;; Name     : tiny-function
;; ------------------------------------------------------------------------
;; DONEをすべてアーカイブ
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;; ショートカットGTD
(defun gtd ()
  (interactive)
     (find-file "~/gtd/main.org")
     )

;;========================================================================
;; タスク管理・GTD
;;========================================================================
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

;;========================================================================
;; 時間計測・見積り
;;========================================================================
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%40ITEM(Task) %17Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote ((
      "Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00"))))

;; TODO状態
;;(setq org-todo-keywords
;;      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))

;; DONEの時刻を記録
(setq org-log-done 'time)

;; -----------------------------------------------------------------------
;; Name     : org-clock
;; http://orgmode.org/manual/Resolving-idle-time.html#Resolving-idle-time
;; ------------------------------------------------------------------------
;; emacs resume 時に時間計測再会
(org-clock-persistence-insinuate)

;; Sometimes I change tasks I'm clocking quickly
;; - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history
;; when exiting Emacs, load it on startup
(setq org-clock-persist (quote history))
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; 空き時間の解決 15分
;; 半端時間を絶えずチェックしているファイルのリストは、M-x org-resolve-clocks
;; http://orgmode.org/manual/Resolving-idle-time.html#Resolving-idle-time
(setq org-clock-idle-time 10)

;; what's this??
;;(setq org-clock-modeline-total 'today)
;;(setq org-clock-persist t)
;;(setq org-clock-clocked-in-display 'both)

; 時間になったら音をならす
;;(setq org-clock-sound "/usr/share/sounds/LinuxMint/stereo/desktop-login.ogg")
;;(setq org-clock-sound t)

;; -----------------------------------------------------------------------
;; Name     : org-clock-by-tags
;; Function : タグごとにclocktableを集計
;; TODO そのうち elispで分離
;; http://stackoverflow.com/questions/17353591/timetable-grouped-by-tag
;; #+BEGIN: clocktable-by-tag :maxlevel 2 :tags ("p1" "p2")
;;                            :tstart "2013-06-27" :tend "2013-06-28"
;; ------------------------------------------------------------------------
(defun clocktable-by-tag/shift-cell (n)
  (let ((str ""))
    (dotimes (i n)
      (setq str (concat str "| ")))
    str))

(defun clocktable-by-tag/insert-tag (params)
  (let ((tag (plist-get params :tags)))
    (insert "|--\n")
    (insert (format "| %s | *Tag time* |\n" tag))
    (let ((total 0))
;;      (mapcar
      (mapc
       (lambda (file)
	 (let ((clock-data (with-current-buffer (find-file-noselect file)
			     (org-clock-get-table-data (buffer-name) params))))
	   (when (> (nth 1 clock-data) 0)
	     (setq total (+ total (nth 1 clock-data)))
	     (insert (format "| | File *%s* | %.2f |\n"
			     (file-name-nondirectory file)
			     (/ (nth 1 clock-data) 60.0)))
	     (dolist (entry (nth 2 clock-data))
	       (insert (format "| | . %s%s | %s %.2f |\n"
			       (org-clocktable-indent-string (nth 0 entry))
			       (nth 1 entry)
			       (clocktable-by-tag/shift-cell (nth 0 entry))
			       (/ (nth 3 entry) 60.0)))))))
       (org-agenda-files))
      (save-excursion
	(re-search-backward "*Tag time*")
	(org-table-next-field)
	(org-table-blank-field)
	(insert (format "*%.2f*" (/ total 60.0)))))
    (org-table-align)))

(defun org-dblock-write:clocktable-by-tag (params)
  (insert "| Tag | Headline | Time (h) |\n")
  (insert "|     |          | <r>  |\n")
  (let ((tags (plist-get params :tags)))
    (mapcar (lambda (tag)
	      (setq params (plist-put params :tags tag))
	      (clocktable-by-tag/insert-tag params))
	    tags)))

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
	("i" "Inbox" entry (file+datetree "~/gtd/inbox.org")
	 "** TODO %?\n")
	("w" "Diary" entry (file+datetree "~/gtd/main.org")
	 "** %T %?\n")
	("e" "Clock-in" entry (clock)
	 "* %T %?\n")
	("m" "Memo" plain
         (file (concat org-directory (format-time-string "/howm/%Y%m%d-%H%M%S.org")))
         "* MEMO <%<%Y-%m-%d>> %?\n   %i\n  %a\n\n"
         :prepend t
         :unnarrowed t
         :kill-buffer t
         )
	)
      )

;; calfwとの連携
;; http://sheephead.homelinux.org/2014/03/15/7035/#
;;cfw:org-capture-templateはcalfw-orgを
;;requireする前に評価しておいてください。
(setq cfw:org-capture-template
      '("c" "calfw2org" entry 
        (file "~/gtd/schedule.org")
        "*  %?\n %(cfw:org-capture-day)"))

;; capture てんぷれの書き方
;; http://orgmode.org/manual/Template-expansion.html#Template-expansion

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
;; ------------------------------------------------------------------------
(require 'metaweblog)
(require 'org2blog-autoloads)
(setq futurismo (netrc-machine (netrc-parse "~/.netrc") "futurismo" t))
;; (setq blog (netrc-machine (netrc-parse "~/.netrc") "EverClassic" t))

(setq org2blog/wp-blog-alist
;;(setq org2blog/wp-blog-alist
      '(("Futurismo"
	 :url "http://futurismo.biz/xmlrpc.php"
	 :username (netrc-get futurismo "login")
	 :password (netrc-get futurismo "password")
	 )
	;; ("EverClassic"
	;;  :url "http://everclassic.biz/xmlrpc.php"
	;;  :username "admin"
	;;  ;;:username (netrc-get blog "login")
	;;  ;;:password (netrc-get blog "password"))
	;;  )
	)
      )

;; ------------------------------------------------------------------------
;; Name     : mobileOrg
;; Function : iphoneとorg-modeの同期。Dropboxを利用
;; Install  :
;; ------------------------------------------------------------------------
;; Set to the location of your Org files on your local system
(setq org-directory "~/gtd")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/gtd/flagged.org")
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

;; -----------------------------------------------------------------------
;; Name     : wanderlust
;; ------------------------------------------------------------------------
;; wanderlustのメールを追跡できる
(setq org-return-follows-link t)

;; -----------------------------------------------------------------------
;; Name     : calfw-org
;; Function : カレンダー連携
;; ------------------------------------------------------------------------
(require 'calfw-org)
;; 対象ファイル
(setq cfw:org-icalendars '("~/gtd/schedule.org"))
;; First day of the week
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday

(defun cfw:open-calendar ()
  (interactive)
  (let ((cp
         (cfw:create-calendar-component-buffer
          :view 'month
          :contents-sources
          (list 
           (cfw:org-create-file-source
            "仕事" "~/gtd/schedule.org" "#268bd2")
           ;;(cfw:org-create-file-source
	   ;; "遊び" "~/yaschedule.org" "#859900")
	   )
	  )))
    (switch-to-buffer (cfw:cp-get-buffer cp))))

;; -----------------------------------------------------------------------
;; Name     : org-gcal
;; Function : google calendar
;; ------------------------------------------------------------------------
(require 'org-gcal)
;; passwordは netrcへ
(setq GoogleCal (netrc-machine (netrc-parse "~/.netrc") "org-gcal" t))
(setq org-gcal-client-id (netrc-get GoogleCal "login")
      org-gcal-client-secret (netrc-get GoogleCal "password")
      org-gcal-dir "~/org"
      org-gcal-file-alist '(("fox10225fox@gmail.com" .  "~/gtd/schedule.org")
			    ;;("your-mail@gmail.com" .  "~/schedule.org")
                            ;;("another-mail@gmail.com" .  "~/task.org")
			    )
      )

;; -----------------------------------------------------------------------
;; Name     : org-pandoc
;; Function : エクスポート
;; ------------------------------------------------------------------------
(require 'ox-pandoc)
(setq org-pandoc-output-format 'rst)

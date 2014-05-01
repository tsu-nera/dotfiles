;; -----------------------------------------------------------------------
;; org-mode
;; 
;; こまったらここを見れば日本語訳がある
;; http://orgmode.jp/
;; ------------------------------------------------------------------------
(require 'org-install)
;; (require 'org-pomodoro)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; key bindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-c\C-x\C-@" 'org-clock-out) ;; tmuxで C-oは利用しているため
(define-key org-mode-map "\C-co" 'org-open-at-point) ;; C-oの置き換え tmuxで c-oは使っているので
(global-set-key "\C-cC" 'cfw:open-org-calendar)
(global-set-key "\C-c\C-x\C-z" 'org-resolve-clocks)

;; 色ツケ
(setq org-src-fontify-natively t)

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

;;========================================================================
;; タスク管理・GTD
;;========================================================================
;; -----------------------------------------------------------------------
;; NextActionの設定
;; http://qiita.com/takaxp/items/4dfa11a81e18b29143ec
;; ------------------------------------------------------------------------
(defvar my-next-tag "next")
(defvar my-unplan-tag "unplan")

;; タグの色変更
;; (setq org-tag-faces '(("next" :foreground "#FF0000")))
(setq org-tag-faces '(("next" :foreground "orange")))

;; Nextタグをトグルする
;; (defun my-toggle-tag (my-tag)
;;   (interactive)
;;   (when (eq major-mode 'org-mode)
;;     (save-excursion
;;       (save-restriction
;;         (unless (org-at-heading-p)
;;           (outline-previous-heading))
;;         (if (string-match (concat ":" my-tag ":") (org-get-tags-string))
;;             (org-toggle-tag my-tag 'off)
;;           (org-toggle-tag my-tag 'on))
;;         (org-reveal)))))

(defun my-toggle-next-tag ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (save-restriction
        (unless (org-at-heading-p)
          (outline-previous-heading))
        (if (string-match (concat ":" my-next-tag ":") (org-get-tags-string))
            (org-toggle-tag my-next-tag 'off)
          (org-toggle-tag my-next-tag 'on))
        (org-reveal)))))

;; (defun my-toggle-unplan-tag ()
;;   my-toggle-tag(my-unplan-tag))

(global-set-key (kbd "C-x <f2>") 'my-toggle-next-tag)
;;(global-set-key (kbd "C-x <f3>") 'my-toggle-unplan-tag)

;;========================================================================
;; org-agenda
;;========================================================================
;; アジェンダ表示の対象ファイル
(setq org-agenda-files '("~/gtd/inbox.org"
			 "~/gtd/main.org"
			 "~/gtd/unplan.org"
			 "~/gtd/schedule.org"))


;; アジェンダデフォルトはday
(setq org-agenda-span 'day)

;; 時間表示が1桁の時、0をつける
(setq org-agenda-time-leading-zero t)

;;========================================================================
;; 時間計測・見積り
;;========================================================================
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format
      "%40ITEM(Task) %17Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
(setq org-global-properties (quote ((
      "Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00"))))

;; DONEの時刻を記録
(setq org-log-done 'time)

;; セレクションメニューから状態の変更を行えるようにする
(setq org-use-fast-todo-selection t)

(setq org-clock-in-resume t)

(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; 測定した時間が0の場合消去する
(setq org-clock-out-remove-zero-time-clocks t)

;;アジェンダのclockreport用パラメータ
(setq org-agenda-clockreport-parameter-plist
      '(:maxlevel 5 :block t :tstart t :tend t :emphasize t :link t :narrow 80 :indent t :formula nil :timestamp t :level 5 :tcolumns nil :formatter nil))

;; カラムビューで表示する項目
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

;; -----------------------------------------------------------------------
;; Name     : org-clock
;; http://orgmode.org/manual/Resolving-idle-time.html#Resolving-idle-time
;; ------------------------------------------------------------------------
;; emacs resume 時に時間計測再会
(org-clock-persistence-insinuate)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Sometimes I change tasks I'm clocking quickly
;; - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; clock out when moving task to a done state
;; タスクが完了した時に時間測定も停止する
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history
;; when exiting Emacs, load it on startup
;; Emacsが再起動したときにタスクの時間計測を再開する
;; Emacsが終了する時に測定中の計測と全ての測定履歴を保存する
(setq org-clock-persist (quote history))

;; 空き時間の解決 
;; 半端時間を絶えずチェックしているファイルのリストは、M-x org-resolve-clocks
;; http://orgmode.org/manual/Resolving-idle-time.html#Resolving-idle-time
(setq org-clock-idle-time 20)

;: 時間測定の履歴数
(setq org-clock-history-length 36)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

; 時間になったら音をならす
;;(setq org-clock-sound "/usr/share/sounds/LinuxMint/stereo/desktop-login.ogg")
;;(setq org-clock-sound t)

;; 必ず時間見積り
(defadvice org-clock-in (before is-set-effort-before-clock-in)
   (let ((effort (org-entry-get (point) "Effort")))
     (unless effort
       (error "[Error: Is not set a effort!]"))))
(ad-activate-regexp "is-set-effort-before-clock-in")


;; -----------------------------------------------------------------------
;; Name  : org-clock-in-quick
;;       : 指定したタスクをclockinするためのショートカット
;;       : はじめてつくった自作defun!!
;; http://orgmode.org/manual/Resolving-idle-time.html#Resolving-idle-time
;; ------------------------------------------------------------------------
;; Refs
;; https://github.com/danieroux/emacs/blob/master/external/bh-org-mode.el
(defvar bh/organization-task-id-gtd "b66237b9-95dd-4863-bc36-bd4dbc435eca")
(defvar bh/organization-task-id-rest "192d0802-8ed7-4c51-ad3f-04f6ae4e75f6")

(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in '(16)))))

;; ショートカット clock-in
(defun gtd ()
  (interactive)
  (find-file "~/gtd/main.org")
  (bh/clock-in-task-by-id bh/organization-task-id-gtd)
  )

(defun rest ()
  (interactive)
  (find-file "~/gtd/main.org")
  (bh/clock-in-task-by-id bh/organization-task-id-rest)
  )

;; (defun bh/clock-in-default-task ()
;;   (save-excursion
;;     (org-with-point-at org-clock-default-task
;;       (org-clock-in))))

;; (defun bh/clock-in-organization-task-as-default ()
;;   (interactive)
;;   (org-with-point-at (org-id-find bh/organization-task-id 'marker)
;;     (org-clock-in '(16))))

;; (defun bh/clock-out-maybe ()
;;   (when (and bh/keep-clock-running
;;              (not org-clock-clocking-in)
;;              (marker-buffer org-clock-default-task)
;;              (not org-clock-resolving-clocks-due-to-idleness))
;;     (bh/clock-in-parent-task)))

;;(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

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
	("i" "Inbox" entry (file+datetree "~/gtd/inbox.org")
	 "** TODO %?\n")
	("u" "Unplan" entry (file+datetree "~/gtd/unplan.org")
	 "** TODO %? :unplan:\n")
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

;; -----------------------------------------------------------------------
;; Name     : org-open-linkをdiredで
;; http://sheephead.homelinux.org/
;; ------------------------------------------------------------------------
(org-add-link-type "file+emacs+dired" 'org-open-file-with-emacs-dired)
(add-hook 'org-store-link-functions 'org-dired-store-link)

(defun org-open-file-with-emacs-dired (path)
  "Open in dired."
  (let ((d (file-name-directory path))
    (f (file-name-nondirectory path)))
    (dired d)
    (goto-char (point-min))
    (search-forward f nil t)))

(defun org-dired-store-link ()
  "Store link to files/directories from dired."
  (require 'dired-x)
  (when (eq major-mode 'dired-mode)
    (let* ((f (dired-filename-at-point))
           (link (concat "file+emacs+dired" ":" f))
           (desc (concat f " (dired)")))
      (org-add-link-props :link link :description desc)
      link)))

;;
;; text-adjust.el 日本語の文章を整形する. 
;;
;;  By 小松弘幸  Hiroyuki Komatsu <komatsu@taiyaki.org>
;;
;; このコードは GPL に従って配布可能です. (This is GPLed software.)
;; 
;; ■インストール方法
;; 1) 適当なディレクトリにこのファイルと mell.el をおく.
;;    (~/elisp/ 内においたとする). mell.el の一元配布元は
;;    http://www.taiyaki.org/elisp/mell/ です.
;;
;; 2) .emacs に次の 2 行を追加する.
;; (setq load-path (cons (expand-file-name "~/elisp") load-path))
;; (load "text-adjust")
;; 
;; ■使い方
;; 1) M-x text-adjust を実行すると文章が整形される.
;; 2) 使用可能な関数の概要.
;;     text-adjust-codecheck : 半角カナ, 規格外文字を「〓」に置き換える.
;;     text-adjust-hankaku   : 全角英数文字を半角にする.
;;     text-adjust-kutouten  : 句読点を「, 」「. 」に置き換える.
;;     text-adjust-space     : 全角文字と半角文字の間に空白を入れる.
;;     text-adjust           : これらをすべて実行する.
;;     text-adjust-fill      : 句読点優先で, fill-region をする.
;;    適応範囲はリージョンがある場合はその範囲を,
;;    なければ mark-paragraph で得られた値. 
;;
;;     *-region : 上記関数をリージョン内で実行する.
;;     *-buffer : 上記関数をバッファ内で実行する.
;; 
;;
;; ■Tips
;; 1) 次のように設定すると, text-adjust-fill-region 実行時に, 
;;  左マージンが考慮される.
;;  | (setq adaptive-fill-regexp "[ \t]*")
;;  | (setq adaptive-fill-mode t)
;;
;; 2) ？！や全角空白を半角へ変換しないようにするには.
;;  text-adjust-hankaku-except に文字を追加すれば可能になります.
;;  | (setq text-adjust-hankaku-except "　？！＠ー〜、，。．")
;;

(require 'mell)

(defvar text-adjust-hankaku-except "＠ー〜、，。．"
  "text-adjust-hankaku で半角にされたくない文字列. 正規表現ではない.")

;; text-adjust-rule のフォーマットは
;; (("左端文字列" "対象文字列" "右端文字列") "変換文字列") という構成の
;; リストです. "左端文字列", "対象文字列", "右端文字列" は正規表現で
;; 記述可能でこの 3 つ を連結した文字列にマッチした個所を変換対象とし, 
;; "対象文字列" を "変換文字列" へ変換します.
;;
;; ■例1
;; (("男湯" " " "女湯")   "|壁|")
;; 変換前 = "男湯 女湯",  変換後 = "男湯|壁|女湯"
;;
;; ■例2
;; ((("\\cj"        "" "[0-9a-zA-Z]")   " ")
;;  (("[0-9a-zA-Z]" "" "\\cj")          " "))
;; 変換前 = "YouはShoooock!",  変換後 = "You は Shoooock!"
;;
;; "変換文字列" では "{", "}" を用いた独自記法によって対象文字列を
;; 参照することが可能です. "{1}", "{2}", "{3}" はそれぞれ順に "左端文字列",
;; "対象文字列", "右端文字列" の全体を表わし, "{2-3}" は "対象文字列" の
;; 3 番目の正規表現の括弧に対応します. また, "{1}" と "{1-0}" は同値です.
;;
;; ■例3
;; (("月" "火水木" "金") "{1}{2}{3}")
;; 変換前 = "月火水木金", 変換後 = "月月火水木金金"
;;
;; ■例4
;; (("" "\\(.ン\\)\\(.ン\\)" "") "{2-2}{2-1}")
;; 変換前 = "夜明けのガンマン", 変換後 = "夜明けのマンガン"
;;
;; text-adjust-mode-skip-rule は各モードに特化した特殊変換ルールで, 
;; 主に変換をさせたくない個所をスキップする目的で用意されています.
;; text-adjust-rule-space, text-adjust-rule-kutouten, 
;; text-adjust-rule-codecheck のそれぞれの先頭に追加されたのち, 実行されます.


;; 日本語用正規表現 (M-x describe-category を参照)
;\\cK カタカナ
;\\cC 漢字
;\\cH ひらがな
;\\cS 全角記号
;\\cj 日本語 (上記全部)
;\\ck 半角カナ
(defvar text-adjust-rule-space 
  '((("\\cj\\|)" "" "[[(0-9a-zA-Z+]")   " ")
    (("[])/!?0-9a-zA-Z+]" "" "(\\|\\cj") " "))
  "置換する空白の変換ルール.")

(defvar text-adjust-rule-kutouten-hperiod
  '((("\\cA\\|\\ca" "．" "\\cA\\|\\ca")   ".")
    (("" "[、，] ?\\([)」』]?\\) *" "$")  "{2-1},")
    (("" "[、，] ?\\([)」』]?\\) ?" "")   "{2-1}, ")
    (("" "[。．] ?\\([)」』]?\\) *" "$")  "{2-1}.")
    (("" "[。．] ?\\([)」』]?\\) ?" "")   "{2-1}. ")
    )
  "「,.」用, 句読点の変換ルール.")

(defvar text-adjust-rule-kutouten-zperiod
  '((("" "、 ?\\([)」』]?\\)" "")     "{2-1}，")
    (("" "。 ?\\([)」』]?\\)" "")     "{2-1}．")
    (("\\cj" ", ?\\([)」』]?\\)" "")   "{2-1}，")
    (("\\cj" "\\. ?\\([)」』]?\\)" "") "{2-1}．"))
  "「，．」用, 句読点の変換ルール.")

(defvar text-adjust-rule-kutouten-zkuten
  '((("" "， ?\\([)」』]?\\)" "")     "{2-1}、")
    (("" "． ?\\([)」』]?\\)" "")     "{2-1}。")
    (("\\cj" ", ?\\([)」』]?\\)" "")   "{2-1}、")
    (("\\cj" "\\. ?\\([)」』]?\\)" "") "{2-1}。"))
  "「、。」用, 句読点の変換ルール.")

(defvar text-adjust-rule-kutouten text-adjust-rule-kutouten-hperiod
  "置換する句読点の変換ルール.
nil の場合, バッファごとに選択可能.")

(defvar text-adjust-rule-codecheck
  '((("" "\\ck\\|\\c@" "") "〓")
    ))

(defvar text-adjust-mode-skip-rule '((sgml-mode . ((("<" "[^>]*" ">") "{2}")
						   ))))

;(defvar text-adjust-fill-regexp ", \\|\\. \\|! \\|\\? \\|を\\| ")
;(defvar text-adjust-fill-regexp "[,.!?] \\|[を ]"
(defvar text-adjust-fill-regexp "[,!] \\|[を ]"
  "この正規表現の次で優先して改行する.")
(defvar text-adjust-fill-start 60
  "各行とも, この値から fill-column までの値までが\
 text-adjust-fill の有効範囲.")

(global-set-key [(meta zenkaku-hankaku)] 'text-adjust)


;;;; text-adjust
(defun text-adjust (&optional force-kutouten-rule)
  "日本語文章を整形する.
各関数 text-adjust-codecheck, text-adjust-hankaku, text-adjust-kutouten,
text-adjust-space を順に実行することにより,
英数字交じりの日本語文章を整形する.
リージョンの指定があった場合はその範囲を, なければ mark-paragraph によって
得られた範囲を対象にする."
  (interactive "P")
  (save-excursion
    (or (transient-region-active-p)
	(mark-paragraph))
    (text-adjust-region (region-beginning) (region-end) force-kutouten-rule)))

(defun text-adjust-buffer (&optional force-kutouten-rule)
  "バッファ内で関数 text-adjust を実行する."
  (interactive "P")
  (text-adjust-region (point-min) (point-max) force-kutouten-rule))

(defun text-adjust-region (from to &optional force-kutouten-rule) 
  "リージョン内で関数 text-adjust を実行する."
  (interactive "r\nP")
  (text-adjust-kutouten-read-rule force-kutouten-rule)
  (save-restriction
    (narrow-to-region from to)
    (text-adjust-codecheck-region (point-min) (point-max))
    (text-adjust-hankaku-region (point-min) (point-max))
    (text-adjust-kutouten-region (point-min) (point-max))
    (text-adjust-space-region (point-min) (point-max))
;    (text-adjust-fill)
    ))


;;;; text-adjust-codecheck
;;;; jischeck.el より引用
;;
;; jischeck.el 19960827+19970214+19980406
;;     By TAMURA Kent <kent@muraoka.info.waseda.ac.jp>
;;      + akira yamada <akira@linux.or.jp>
;;      + Takashi Ishioka <ishioka@dad.eec.toshiba.co.jp>

;; JIS X 0208-1983 で無効な範囲(数値は ISO-2022-JP での値):
;;  1,2 Byte目が 0x00-0x20, 0x7f-0xff
;;  1 Byte目:  0x29-0x2f, 0x75-0x7e
;;
;; 細かいところでは:
;;  222f-2239, 2242-2249, 2251-225b, 226b-2271, 227a-227d
;;  2321-232f, 233a-2340, 235b-2360, 237b-237e
;;  2474-247e,
;;  2577-257e,
;;  2639-2640, 2659-267e,
;;  2742-2750, 2772-277e,
;;  2841-287e,
;;  4f54-4f7e,
;;  7425-747e,
;;
;;;; 引用終わり.

;;;; 1 byte 目が 0x29-0x2f, 0x75-0x7e の文字にのみ対応.
(or (if running-xemacs
	(defined-category-p ?@)
      (category-docstring ?@))
    (let ((page 41))
      (define-category ?@ "invalid japanese char category")
      (while (<= page 126)
	(if running-xemacs
	    (modify-category-entry `[japanese-jisx0208 ,page] ?@)
	  (modify-category-entry (make-char 'japanese-jisx0208 page) ?@))
	(setq page 
	      (if (= page 47) 117 (1+ page))))))

(defun text-adjust-codecheck (&optional from to)
  "無効な文字コードを text-adjust-codecheck-alarm に置き換える.

リージョンの指定があった場合はその範囲を, なければ mark-paragraph によって
得られた範囲を対象にする."
  (interactive)
  (save-excursion
    (or (transient-region-active-p)
	(mark-paragraph))
    (text-adjust-codecheck-region (region-beginning) (region-end))))

(defun text-adjust-codecheck-buffer ()
  "バッファ内で関数 text-adjust-jischeck を実行する."
  (interactive)
  (text-adjust-codecheck-region (point-min) (point-max)))

(defun text-adjust-codecheck-region (from to)
  "リージョン内で関数 text-adjust-jischeck を実行する."
  (interactive "r")
  (text-adjust--replace text-adjust-rule-codecheck from to))


;;;; text-adjust-hankaku
(defun text-adjust-hankaku ()
  "全角英数文字を半角にする.

リージョンの指定があった場合はその範囲を, なければ mark-paragraph によって
得られた範囲を対象にする."
  (interactive)
  (save-excursion
    (or (transient-region-active-p)
	(mark-paragraph))
    (text-adjust-hankaku-region (region-beginning) (region-end))))

(defun text-adjust-hankaku-buffer ()
  "バッファ内で関数 text-adjust-hankaku を実行する."
  (interactive)
  (text-adjust-hankaku-region (point-min) (point-max)))

(defun text-adjust-hankaku-region (from to) 
  "リージョン内で関数 text-adjust-hankaku を実行する."
  (interactive "r")
  (require 'japan-util)
  (save-excursion
    (let ((tmp-table (text-adjust--copy-char-table char-code-property-table)))
      (text-adjust--modify-char-table ?　 (list 'ascii "  "))
      (mapcar '(lambda (c) (text-adjust--modify-char-table c nil))
       (string-to-list text-adjust-hankaku-except))
      (japanese-hankaku-region from to t)
      (setq char-code-property-table 
	    (text-adjust--copy-char-table tmp-table)))))

(defun text-adjust--modify-char-table (range value)
  (if running-xemacs
      (put-char-table range value char-code-property-table)
    (set-char-table-range char-code-property-table range value)))

(defun text-adjust--copy-char-table (table)
  (if running-xemacs
      (copy-char-table table)
    (copy-sequence table)))


;;;; text-adjust-kutouten
(defun text-adjust-kutouten (&optional forcep)
  "句読点を変換する.
句点を text-adjust-kuten-from から text-adjust-kuten-to の値に,
読点を text-adjust-touten-from から text-adjust-touten-to の値に変換する.

リージョンの指定があった場合はその範囲を, なければ mark-paragraph によって
得られた範囲を対象にする."
  (interactive)
  (save-excursion
    (or (transient-region-active-p)
	(mark-paragraph))
    (text-adjust-kutouten-region (region-beginning) (region-end) forcep)))

(defun text-adjust-kutouten-buffer (&optional forcep)
  "バッファ内で関数 text-adjust-kutouten を実行する."
  (interactive "P")
  (text-adjust-kutouten-region (point-min) (point-max) forcep))

(defun text-adjust-kutouten-region (from to &optional forcep)
  "リージョン内で関数 text-adjust-kutouten を実行する."
  (interactive "r\nP")
  (text-adjust-kutouten-read-rule forcep)
  (text-adjust--replace text-adjust-rule-kutouten from to))

(defun text-adjust-kutouten-read-rule (&optional forcep)
  "変換後の句読点を選択する."
  (interactive)
  (if (and text-adjust-rule-kutouten (not forcep) (not (interactive-p)))
      text-adjust-rule-kutouten
    (make-local-variable 'text-adjust-rule-kutouten)
    (setq text-adjust-rule-kutouten
	  (symbol-value
	   (let ((kutouten-alist 
		  '(("kuten-zenkaku"  . text-adjust-rule-kutouten-zkuten)
		    ("zenkaku-kuten"  . text-adjust-rule-kutouten-zkuten)
		    ("、。"           . text-adjust-rule-kutouten-zkuten)
		    ("period-zenkaku" . text-adjust-rule-kutouten-zperiod)
		    ("zenkaku-period" . text-adjust-rule-kutouten-zperiod)
		    ("，．"           . text-adjust-rule-kutouten-zperiod)
		    ("period-hankaku" . text-adjust-rule-kutouten-hperiod)
		    ("hankaku-period" . text-adjust-rule-kutouten-hperiod)
		    (",."             . text-adjust-rule-kutouten-hperiod))))
	     (cdr (assoc
		   (completing-read "句読点の種類: " kutouten-alist
				    nil t ",.")
		   kutouten-alist)))))))

;;;; text-adujst-space
(defun text-adjust-space ()
  "半角英数と日本語の間に空白を挿入する.
text-adjust-japanese で定義された日本語文字を示す正規表現と,
text-adjust-ascii で定義された半角英数文字を示す正規表現との間に
空白を挿入する.

リージョンの指定があった場合はその範囲を, なければ mark-paragraph によって
得られた範囲を対象にする."
  (interactive)
  (save-excursion
    (or (transient-region-active-p)
	(mark-paragraph))
    (text-adjust-space-region (region-beginning) (region-end))))

(defun text-adjust-space-buffer () 
  "バッファ内で関数 text-adjust-space を実行する."
  (interactive)
  (text-adjust-space-region (point-min) (point-max)))
  
(defun text-adjust-space-region (from to) 
  "リージョン内で関数text-adjust-spaceを実行する."
  (interactive "r")
  (text-adjust--replace text-adjust-rule-space from to))


;;;; text-adjust-fill
(defun text-adjust-fill ()
  "句読点での改行を優先して, fill-region を実行する.
各行の text-adjust-fill-start から, fill-column までの間に,
text-adjust-fill-regexp が最後に含まれているところで改行する.

リージョンの指定があった場合はその範囲を, なければ mark-paragraph によって
得られた範囲を対象にする."
  (interactive)
  (save-excursion
    (or (transient-region-active-p)
	(mark-paragraph))
    (text-adjust-fill-region (region-beginning) (region-end))))

(defun text-adjust-fill-buffer () 
  "バッファ内で関数 text-adjust-fill を実行する."
  (interactive)
  (text-adjust-fill-region (point-min) (point-max)))
  
(defun text-adjust-fill-region (from to) 
  "リージョン内で関数 text-adjust-fill を実行する."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (let ((kinsoku-tmp kinsoku-ascii)
	    (prefix (if adaptive-fill-mode (fill-context-prefix from to) "")))
	(setq kinsoku-ascii t)
	(fill-region (point-min) (point-max))
	(goto-char (point-min))
	(while (/= (line-end-position) (point-max))
	  (move-to-column text-adjust-fill-start)
	  (if (and (re-search-forward
		    (concat "\\(" text-adjust-fill-regexp 
			    "\\) *[^" text-adjust-fill-regexp "]*$")
		    (line-end-position) t))
	      (progn
		(goto-char (match-end 1))
		(delete-horizontal-space)
		(if (eolp)
		    (beginning-of-line 2)
		  (progn
		    (insert (concat "\n" prefix))
		    (beginning-of-line)
		    )))
	    (beginning-of-line 2))
	  (narrow-to-region (point) (point-max))
	  (fill-region (point-min) to nil nil t)
	  (goto-char (point-min)))
	(delete-horizontal-space)
	(setq kinsoku-ascii kinsoku-tmp)))))


;;;; text-adjust engine
(defun text-adjust--replace (rule from to)
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let* ((rule-pattern 
	      (text-adjust--make-rule-pattern 
	       (append (cdr (assoc major-mode text-adjust-mode-skip-rule)) 
		       rule)))
	     (regexp (nth 0 rule-pattern))
	     (target (nth 1 rule-pattern))
	     (counts (nth 2 rule-pattern)))
	(while (re-search-forward regexp nil t)
	  (let ((n 1) (m 0) right-string)
	    ; 該当パターンまですすめる
	    (while (not (match-beginning n))
	      (setq n (+ n 3 (mapadd (nth m counts)))
		    m (1+ m)))
	    ; 該当パターンと置換する
	    (let* ((tmp n)
		   (total-counts 
		    (cons n (mapcar (lambda (x) (setq tmp (+ tmp x 1)))
				    (nth m counts))))
		   (right-string (match-string (nth 2 total-counts))))
	      (replace-match 
	       (concat 
		;; 該当パターンの左側
		(match-string n)
		;; 該当パターンのまん中 (置換部分)
		(mapconcat
		 (lambda (x) 
		   (if (stringp x) x
		     (match-string (+ (nth (1- (car x)) total-counts) 
				      (cdr x)))))
		 (nth m target) "")
		;; 該当パターンの右側
		right-string))
	      ;; "あaあa" のように一文字ずつで並んでいる時の対処
	      (backward-char (length right-string))))))
      )))

(defun text-adjust--make-rule-pattern (rule)
  (let ((regexp (mapconcat 
		 (lambda (x) 
		   (format "\\(%s\\)\\(%s\\)\\(%s\\)"
			   (nth 0 (car x)) (nth 1 (car x)) (nth 2 (car x))))
		 rule "\\|"))
	(target (mapcar 
		 (lambda (x)
		   (text-adjust--parse-replace-string (nth 1 x)))
		 rule))
	(counts (mapcar
		 (lambda (x)
		   (list (count-string-match "\\\\(" (nth 0 (car x)))
			 (count-string-match "\\\\(" (nth 1 (car x)))
			 (count-string-match "\\\\(" (nth 2 (car x)))))
		 rule)))
    (list regexp target counts)))

(defun text-adjust--parse-replace-string (rule)
  (let ((n 0) m list)
    (while (string-match "\\([^{]*\\){\\([^}]+\\)}" rule n)
      (setq n (match-end 0))
      (let ((match1 (match-string 1 rule))
	    (match2 (match-string 2 rule)))
	(cond ((string-match "^[0-9]+\\(-[0-9]+\\)?$" match2)
	       (or (string= match1 "") (setq list (cons match1 list)))
	       (let* ((tmp (split-string match2 "-"))
		      (num (cons (string-to-number (car tmp))
				 (string-to-number (or (nth 1 tmp) "0")))))
		 (setq list (cons num list))))
	      (t
	       (setq list (cons match2 (cons match1 list)))))))
    (reverse (cons (substring rule n) list))))


(provide 'text-adjust)
; $Id: text-adjust.el,v 1.1.1.1 2002/08/25 14:24:48 komatsu Exp $

;; -----------------------------------------------------------------------
;; Name     : 左右のバッファをF2で交換する
;; Function : http://d.hatena.ne.jp/supermassiveblackhole/20100625/1277436024
;; ------------------------------------------------------------------------
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)

;; -----------------------------------------------------------------------
;; Name     : 縦横のバッファをF3で交換する
;; http://masutaka.net/chalow/2011-05-19-1.html
;; ------------------------------------------------------------------------
(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割<->横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let ((before-height)
        (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally))
    (other-window 1)
    (switch-to-buffer other-buf)
    (other-window -1)))
(global-set-key [f3] 'window-toggle-division)

;; -----------------------------------------------------------------------
;; Name     : windownの動的リサイズ
;; http://d.hatena.ne.jp/mooz/20100119/p1
;; ------------------------------------------------------------------------
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))

(define-key global-map "\C-q" (make-sparse-keymap))

;; window-resizer は C-q C-r (resize) で
(global-set-key "\C-q\C-r" 'window-resizer)

(global-set-key "\C-ql" 'windmove-right)
(global-set-key "\C-qh" 'windmove-left)
(global-set-key "\C-qj" 'windmove-down)
(global-set-key "\C-qk" 'windmove-up)

;;; ツールバーを消す
(tool-bar-mode -1)
;; emacs -nw で起動した時にメニューバーを消す
;;(if window-system (menu-bar-mode 1) (menu-bar-mode -1))
;; -> 常にけす
(menu-bar-mode -1)
;;; スクロールバーを消す
(set-scroll-bar-mode nil)

;;; 対応する括弧を光らせる。
(show-paren-mode 1)

;;; 画像ファイルを表示する
(if window-system (auto-image-file-mode t)(auto-image-file-mode nil))

;;; モードラインに時間を表示する
(display-time)
(setq display-time-day-and-date t)
;;; 現在の関数名をモードラインに表示
(which-function-mode 1)
(global-linum-mode t)   ;; 行番号の表示
;;(global-hl-line-mode 1) ;; 現在行に色をつける

;;; 画像ファイルを表示
(auto-image-file-mode t)
;; (setq-default mode-line-format
;; 	      '("-"
;; 		mode-line-mule-info
;; 		mode-line-modified
;; 		" "
;; 		mode-line-buffer-identification
;; 		" "
;; 		global-mode-string
;; 		" %[("
;; 		mode-name
;; 		mode-line-process
;; 		minor-mode-alist
;; 		"%n" ")%]"
;; 		(which-func-mode ("" which-func-format "-"))
;; 		"-%-"
;; 		)
;; 	      )

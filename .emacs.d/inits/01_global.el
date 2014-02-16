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
;; Emacs Client
;; ------------------------------------------------------------------------
;; server start for emacs-client
;; http://d.hatena.ne.jp/syohex/20101224/1293206906
(require 'server)
(unless (server-running-p)
  (server-start))

;; -----------------------------------------------------------------------
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

;; -----------------------------------------------------------------------
;; Function : ミニバッファに入るときに日本語入力無効にする
;;  http://www11.atwiki.jp/s-irie/pages/21.html
;; Install  : 
;;  sudo add-apt-repository ppa:irie/elisp
;;  sudo apt-get update
;;  sudo apt-get install ibus-el
;;  いれたけど、うまく動かない。
;; ------------------------------------------------------------------------
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)

;; IBusの状態によってカーソル色を変化させる
(setq ibus-cursor-color '("red" "blue" "limegreen"))

;; isearch 時はオフに
(add-hook 'isearch-mode-hook 'ibus-disable)

;; mini buffer ではオフに
(add-hook 'minibuffer-setup-hook 'ibus-disable)

;; インクリメンタル検索中のカーソル形状を変更する
(setq ibus-isearch-cursor-type 'hollow)

;; カーソルの位置に予測候補を表示
(setq ibus-prediction-window-position t)

;; Undo の時に確定した位置まで戻る
(setq ibus-undo-by-committed-string t)

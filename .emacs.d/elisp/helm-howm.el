;;; helm-howm.el --- Helm completion for howm  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011 kitokitoki
;;               2012-2030 mori_dev

;; Author: kitokitoki <mori.dev.asdf@gmail.com>
;; Keywords: helm, howm
;; Prefix: hh:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation:

;; install requires libraries:
;; `migemo'                    http://0xcc.net/migemo/
;; `helm.el'               http://www.emacswiki.org/emacs/helm.el
;; `helm-config.el'        http://www.emacswiki.org/emacs/helm-config.el
;; `helm-match-plugin.el'  http://www.emacswiki.org/emacs/helm-match-plugin.el
;; `helm-migemo.el'        http://www.emacswiki.org/emacs/helm-migemo.el
;; `howm'                      http://howm.sourceforge.jp/index-j.html

;; `helm-howm.el'          http://github.com/kitokitoki/helm-howm (this file)

;;; Setting Sample

;; (require 'helm-howm)
;;
;; (setq hh:recent-menu-number-limit 600)
;; (global-set-key (kbd "C-2") 'hh:menu-command)
;; (global-set-key (kbd "C-3") 'hh:cached-howm-menu)
;;
;; (defun helm-buffers ()
;;   (interactive)
;;   (helm-other-buffer
;;    '(helm-c-source-buffers+-howm-title
;;      helm-c-source-recentf
;;      ...
;;      )
;;    "*Buffer+File*"))
;; (global-set-key (kbd "M-h") 'helm-buffers)
;;
;; or
;;
;; (setq helm-sources
;;       (list
;;         helm-c-source-buffers+-howm-title ;これを追加
;;         ;; helm-c-source-buffers はコメントアウト
;;         helm-c-source-recentf など
;;         ...
;;         ))

;; Change Log
;; 1.1.0: リファクタ helm-c-source-howm-recent の内部の無名関数に名前を付与
;; 1.0.9: prefix を helm-howm- から hh: へ変更
;; 1.0.8: 拡張子 .homn での判定処理を
;;        howm-directory 以下の howm-mode かに変更
;;        migemo をオプション化
;; 1.0.7: ファイル名ではなくタイトルを一覧表示する
;;        helm-c-source-buffers+-howm-title を追加
;; 1.0.6: 専用の helm-resume を作成
;; 1.0.5: メニューリストに検索などの項目を追加。メニューソースでの (migemo)を廃止
;; 1.0.4: アクション"Open Marked howm file", "Delete file(s)" を作成
;; 1.0.3: メニュー用のソースを新規作成
;; 1.0.2: ファイル削除、新ウィンドウで開く、新フレームで開くアクションを追加
;;        リファクタリング
;; 1.0.1: 新しいメモをつくる機能を追加, migemo 対応
;; 1.0.0: 新規作成

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-match-plugin)
(require 'helm-migemo nil t)
(require 'howm)
(require 'howm-menu)

(defvar hh:recent-menu-number-limit 10)
(defvar hh:persistent-action-buffer "*howm-tmp*")
(defvar hh:menu-buffer "*helm-howm-menu*")
(defvar hh:default-title "")
(defvar hh:use-migemo nil)

(defvar hh:howm-full-path-directory (expand-file-name howm-directory))


;;; Version

(defconst helm-howm-version "1.0.8"
  "The version number of the file helm-howm.el.")

(defun helm-howm-version (&optional here)
  "Show the helm-howm version in the echo area.
With prefix arg HERE, insert it at point."
  (interactive "P")
  (let ((version (format "helm-howm version %s" helm-howm-version)))
    (message version)
    (if here
      (insert version))))

;; see https://github.com/emacs-jp/emacs-jp.github.com/issues/30
(defun helm-migemo-match-fn (candidate)
  (or (string-match (regexp-quote helm-input) candidate)
      (string-match helm-pattern candidate)))

(defvar helm-c-source-howm-recent
  '((name    . "最近のメモ")
    (init    . helm-c-howm-recent-init)
    (candidates . (lambda()
    		    (hh:get-recent-title-list
    		     (howm-recent-menu hh:recent-menu-number-limit))))
    ;; (candidates-in-buffer)
    ;; (candidates . helm-candidates-in-buffer)
    (volatile)
    (match helm-migemo-match-fn)
    (candidate-number-limit . 9999)
    (action .
      (("Open howm file(s)" . hh:find-files)
       ("Open howm file in other window" .
          (lambda (candidate)
            (find-file-other-window
             (hh:select-file-by-title candidate))))
       ("Open howm file in other frame" .
          (lambda (candidate)
            (find-file-other-frame
             (hh:select-file-by-title candidate))))
       ("Create new memo" .
          (lambda (template)
            (hh:create-new-memo "")))
       ("Create new memo on region" .
          (lambda (template)
            (hh:create-new-memo (hh:set-selected-text))))
       ("Delete file(s)" . hh:delete-marked-files)))
    (persistent-action . helm-howm-persistent-action)
    (cleanup . helm-c-howm-recent-cleanup)))

(defun helm-c-howm-recent-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (insert (mapconcat 'identity
                       (hh:get-recent-title-list
                        (howm-recent-menu hh:recent-menu-number-limit))
                       "\n"))))

(defun helm-c-howm-recent-cleanup ()
  (helm-aif (get-buffer hh:persistent-action-buffer)
      (kill-buffer it)))

(defun helm-howm-persistent-action (candidate)
  (let ((buffer (get-buffer-create hh:persistent-action-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert-file-contents (hh:select-file-by-title candidate))
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      (howm-mode t)))

(when hh:use-migemo
  (push '(migemo) helm-c-source-howm-recent))

(defun hh:select-file-by-title (title)
  (cl-loop for recent-menu-x in (howm-recent-menu hh:recent-menu-number-limit)
        for list-item-file  = (first recent-menu-x)
        for list-item-name  = (second recent-menu-x)
        if (string-equal title list-item-name)
          return list-item-file))

(defun hh:find-files (candidate)
  (helm-aif (helm-marked-candidates)
      (dolist (i it)
        (find-file (hh:select-file-by-title i)))
    (find-file (hh:select-file-by-title candidate))))

(defun hh:get-recent-title-list (recent-menu-list)
  (cl-loop for recent-menu-x in recent-menu-list
        for list-item-name  = (second recent-menu-x)
        collect list-item-name))

(defun hh:create-new-memo (text)
  (let ((str text))
    (howm-create-file-with-title hh:default-title nil nil nil nil)
    (save-excursion
      (goto-char (point-max))
      (insert str))
    (goto-char (point-min))
    (end-of-line)))

(defun hh:delete-marked-files (candidate)
  (helm-aif (helm-marked-candidates)
      (if (y-or-n-p (format "Delete *%s Files " (length it)))
          (progn
            (dolist (i it)
              (set-text-properties 0 (length i) nil i)
              (delete-file
                (hh:select-file-by-title i)))
            (message "%s Files deleted" (length it)))
          (message "(No deletions performed)"))
    (set-text-properties 0 (length candidate) nil candidate)
    (if (y-or-n-p
         (format "Really delete file `%s' " (hh:select-file-by-title candidate)))
        (progn
          (delete-file
            (hh:select-file-by-title candidate))
          (message "1 file deleted"))
        (message "(No deletions performed)"))))

(defun hh:set-selected-text ()
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

(defvar hh:menu-list
      '(("c [メモを作成]" . "(hh:create-new-memo \"\")")
        ("cr[リージョンからメモを作成]" . "(hh:create-new-memo (hh:set-selected-text))")
        ("s [固定]" . "(howm-list-grep-fixed)")
        ("g [正規]" . "(howm-list-grep)")
        ("m [roma]" . "(howm-list-migemo)")
        ("y [予定]" . "(howm-list-todo)")
        ("t [Todo]" . "(howm-list-schedule)")))

(defvar helm-c-source-howm-menu
  '((name . "メニュー")
    (candidates . hh:menu-list)
    (type . sexp)))

(defun hh:cached-howm-menu ()
  (interactive)
  (let ((helm-display-function 'hh:display-buffer))
    (if (get-buffer hh:menu-buffer)
        (helm-resume hh:menu-buffer)
      (hh:menu-command))))


(defun hh:menu-command ()
  (interactive)
  (let ((helm-display-function 'hh:display-buffer))
    (helm-other-buffer
     '(helm-c-source-howm-menu
       helm-c-source-howm-recent)
     hh:menu-buffer)))

(defun hh:resume ()
  (interactive)
  (when (get-buffer hh:menu-buffer)
    (helm-resume hh:menu-buffer)))

(defun hh:display-buffer (buf)
  "左右分割で表示する"
  (delete-other-windows)
  (split-window (selected-window) nil t)
  (pop-to-buffer buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; howm のファイルは日付形式のため，複数開いていると見分けにくい。
;; helm-c-source-buffers+-howm-title では、一覧時にタイトルを表示する

(defvar helm-c-source-buffers+-howm-title
  '((name . "Buffers")
    (candidates . helm-c-buffer-list)
    (real-to-display . hh:title-real-to-display)
    (type . buffer)
    (candidate-transformer
         helm-c-skip-current-buffer
         helm-c-highlight-buffers
         helm-c-skip-boring-buffers)
    (persistent-action . helm-c-buffers+-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

;; helm-c-buffers-persistent-kill and helm-c-switch-to-buffer are defined at helm-config.el.
;; (defun helm-c-buffers+-persistent-action (candidate)
;;   (if current-prefix-arg
;;       (helm-c-buffers-persistent-kill candidate)
;;     (helm-c-switch-to-buffer candidate)))

(defun hh:title-real-to-display (file-name)
  (with-current-buffer (get-buffer file-name)
    (if (and howm-mode
             (hh:in-howm-dir-p file-name))
      (hh:title-get-title file-name)
    file-name)))

(defun hh:in-howm-dir-p (file-name)
  (hh:!! (string-match hh:howm-full-path-directory
                       (buffer-file-name (get-buffer file-name)))))

(defun hh:!! (arg)
  (not (not arg)))

(defun hh:title-get-title (buffer)
  (with-current-buffer buffer
    (let ((point (point-min)))
      (save-excursion
        (goto-char (point-min))
        (end-of-line)
        (buffer-substring point (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.x, (global-set-key (kbd "C-c e") (helm-howm-fixed-term-command "emacs"))
(defun hh:fixed-term-command (initial)
  (lexical-let ((initial initial))
    (lambda () (interactive) (helm 'helm-c-source-howm-recent initial))))


;; experimental code
;(hh:get-filename (list howm-directory))
;; (defun hh:get-filename (file-list)
;;     (cl-loop for x in file-list
;;           with path-list = nil
;;           when (file-directory-p x)
;;             for path-list =
;;               (append
;;                 (hh:get-filename
;;                  (cl-remove-if
;;                   (lambda(y) (string-match "\\.$\\|\\.svn" y))
;;                   (directory-files x t)))
;;                 path-list)
;;           else
;;             collect x into path-list
;;           end
;;           finally return path-list))

;; (defvar helm-c-source-howm-contents-grep
;;   `((name . "helm-howm-contents-grep")
;;     (grep-candidates . ,(hh:get-filename (list howm-directory)))
;;     (header-name . (lambda (x) (concat x ": " helm-pattern)))
;;     (candidate-number-limit . 99999)))
;; (helm 'helm-c-source-howm-contents-grep)

(provide 'helm-howm)

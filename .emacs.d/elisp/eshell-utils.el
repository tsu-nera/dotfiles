;;---------------------------------------------------
;; http://d.hatena.ne.jp/kitokitoki/20110222
;;---------------------------------------------------

;; emacs 起動時に eshell バッファも一つ用意する
;; (add-hook 'after-init-hook
;;           (lambda()
;;             (eshell)
;;             (switch-to-buffer "*scratch*")))

(defun eshell/toggle-buffer ()
  "eshell と直前のバッファを行き来する
   C-u 付きで呼ぶと 今いるバッファと同じディレクトリに cd して開く"
  (interactive)
  (let ((ignore-list '("*Help*" "*Minibuf-1*" "*Messages*"
                       "*terminal<1>*" "*terminal<2>*" "*terminal<3>*"))
        (dir default-directory))
    (labels
        ((_my-toggle-term (target)
           (if (null (member (buffer-name (second target)) ignore-list))
               (if (equal "*eshell*" (buffer-name (window-buffer)))
                   (switch-to-buffer (second target))
                 (switch-to-buffer "*eshell*")
                 (when current-prefix-arg
                   (cd dir)
                   (eshell-interactive-print (concat "cd " dir "\n"))
                   (eshell-emit-prompt)))
             (_my-toggle-term (cdr target)))))
      (_my-toggle-term (buffer-list)))))

;;---------------------------------------------------
;; eshell は 1 つしか生成できないので, 複数作成する.
;; http://stackoverflow.com/questions/2540997/create-more-than-one-eshell-instance-in-emacs
;;---------------------------------------------------
(defun eshell/make-new-eshell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

;;---------------------------------------------------
;; eshell/emacs
;;---------------------------------------------------
;; find-fileで十分
;; (defun eshell/emacs (&rest args)
;;   "Open a file in emacs. Some habits die hard."
;;   (interactive)
;;   (if (null args)
;;       ;; If I just ran "emacs", I probably expect to be launching
;;       ;; Emacs, which is rather silly since I'm already in Emacs.
;;       ;; So just pretend to do what I ask.
;;       (bury-buffer)
;;     ;; We have to expand the file names or else naming a directory in an
;;     ;; argument causes later arguments to be looked for in that directory,
;;     ;; not the starting directory
;;     (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;; Ctrl + L
(defun eshell/clear ()
  "Clear the current buffer, leaving one prompt at the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ))

;; written by Stefan Reichoer <reichoer@web.de>
(defun eshell/less (&rest args)
  "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	(let* ((line (string-to-number (match-string 1 (pop args))))
	       (file (pop args)))
	  (view-file file)
	  (goto-line line))
      (view-file (pop args)))))

;; (defun eshell/rpwd2home ()
;;   (interactive)
;;   (let ((abs-path (eshell/pwd))
;; 	(home-dir (getenv "HOME")))
;;     (if (string-match home-dir abs-path)
;; 	(replace-match "~" nil nil abs-path)
;;       abs-path)))

;; (defun eshell/rpwd ()
;;   (interactive)
;;   (let ((rdir (eshell/rpwd2home)))
;;     (car (last (split-string rdir "/")))))

;;---------------------------------------------------
;; sudo のあとも補完可能に
;; これはなんだっけ??
;;---------------------------------------------------
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-help "complete after sudo"))
    (pcomplete-here (pcomplete-here (eshell-complete-commands-list)))))

(provide 'eshell-utils)
;;; eshell-utils.el ends here

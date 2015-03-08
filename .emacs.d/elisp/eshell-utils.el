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
;; http://stackoverflow.com/questions/7733668/command-to-clear-shell-while-using-emacs-shell
(defun eshell/clear ()
  "Clear the current buffer, leaving one prompt at the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; written by Stefan Reichoer <reichoer@web.de>
(defun eshell/less (&rest args)
  "Invoke `view-file' on the file.
\"less +42 foo\" also goes to line 42 in the buffer."
  (interactive)
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	(let* ((line (string-to-number (match-string 1 (pop args))))
	       (file (pop args)))
	  (view-file file)
	  (goto-line line))
      (view-file (pop args)))))

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

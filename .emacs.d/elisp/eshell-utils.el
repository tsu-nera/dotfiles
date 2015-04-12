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
;; eshell/bmk - version 0.1.3
;; http://www.emacswiki.org/emacs/EshellBmk
;;---------------------------------------------------
(defun pcomplete/eshell-mode/bmk ()
  "Completion for `bmk'"
  (pcomplete-here (bookmark-all-names)))

(defun eshell/bmk (&rest args)
  "Integration between EShell and bookmarks.
For usage, execute without arguments."
  (setq args (eshell-flatten-list args))
  (let ((bookmark (car args))
        filename name)
    (cond
     ((eq nil args)
      (format "Usage: 
* bmk BOOKMARK to
** either change directory pointed to by BOOKMARK
** or bookmark-jump to the BOOKMARK if it is not a directory.
* bmk . BOOKMARK to bookmark current directory in BOOKMARK.
Completion is available."))
     ((string= "." bookmark)
      ;; Store current path in EShell as a bookmark
      (if (setq name (car (cdr args)))
          (progn
            (bookmark-set name)
            (bookmark-set-filename name (eshell/pwd))
            (format "Saved current directory in bookmark %s" name))
        (error "You must enter a bookmark name")))
     (t
       ;; Check whether an existing bookmark has been specified
       (if (setq filename (cdr (car (bookmark-get-bookmark-record bookmark))))
           ;; If it points to a directory, change to it.
           (if (file-directory-p filename)
               (eshell/cd filename)
             ;; otherwise, just jump to the bookmark 
             (bookmark-jump bookmark))
         (error "%s is not a bookmark" bookmark))))))

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



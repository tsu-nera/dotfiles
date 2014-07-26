;;;;; MELL - MELL Emacs Lisp Library.
;;
;; AUTHOR:  Hiroyuki Komatsu <komatsu@taiyaki.org>
;; LICENCE: GPL2
;; $Id: mell.el,v 1.4 2003/03/18 03:34:45 komatsu Exp $
;;

;; ------------------------------------------------------------
;; XEmacs と FSF Emacs の差異を吸収
;; ------------------------------------------------------------

;; Checking Emacs or XEmacs.
(if (not (boundp 'running-xemacs))
    (defconst running-xemacs nil))

;; line-end-position  for XEmacs
(if (not (fboundp 'line-end-position))
    (defun line-end-position (&optional arg)
      (point-at-eol (or arg 1))))

;; define-obsolete-function-alias for FSF Emacs
(if (not (fboundp 'define-obsolete-function-alias))
    (defun define-obsolete-function-alias (obsolete current)
      (defalias obsolete current)
      (make-obsolete obsolete current))
  )

;; add-local-hook
(or (fboundp 'add-local-hook)
    (defun add-local-hook (hook function &optional append)
      (make-local-hook hook)
      (add-hook hook function append t))
    )

;; remove-local-hook
(or (fboundp 'remove-local-hook)
    (defun remove-local-hook (hook function)
      (if (local-variable-p hook (current-buffer))
	  (remove-hook hook function t)))
    )
;; ------------------------------------------------------------
;; mell (basic)
;; ------------------------------------------------------------
(defcustom mell-working-buffer-name " *mell-buffer*"
  "Working buffer name for mell")
(defvar    mell-working-buffer nil
  "working buffer for mell")

(defun mell-check-value (value)
  (and (boundp value)
       (symbol-value value)))

(defun mell-defvar (symbol value &optional doc-string)
  (if (not (boundp symbol))
      (set symbol value))
  (if doc-string
      (put symbol 'variable-documentation doc-string))
  symbol)

(defun mell-defvar-locally (symbol initvalue &optional docstring)
  (mell-defvar symbol initvalue docstring)
  (make-variable-buffer-local symbol)
  symbol)

(defun mell-require (feature &optional filename noerror)
  (or (featurep feature)
      (if noerror
	  (condition-case nil
	      (require feature filename)
	    (file-error nil)
	    )
	(require feature filename)
	)))

(defun mell-point-at-bol (&optional point)
  (save-excursion
    (or point (goto-char point))
    (beginning-of-line)
    (point)
    ))

(defun mell-point-at-eol (&optional point)
  (save-excursion
    (or point (goto-char point))
    (end-of-line)
    (point)
    ))

(defun mell-point-at-bop (&optional point)
  (save-excursion
    (goto-char (or point (point)))
    (backward-paragraph 1)
    (point)))

(defun mell-point-at-eop (&optional point)
  (save-excursion
    (goto-char (or point (point)))
    (forward-paragraph 1)
    (point)))

(defun mell-column-at-point (point &optional buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (goto-char point)
    (current-column)
    ))

(defun mell-point-at-column (column &optional point buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (and point (goto-char point))
    (move-to-column column)
    (point)
    ))

;; mell-marker
(defun mell-marker-make (&optional position buffer type)
  (let ((marker (make-marker)))
    (or position
	(setq position (point)))
    (set-marker marker position buffer)
    (set-marker-insertion-type marker type)
    marker
    ))

(defun mell-marker-set (marker &optional position buffer type)
  (or (and (boundp marker) (markerp (symbol-value marker)))
      (set marker (make-marker)))
  (or position
      (setq position (point)))
  (set-marker (symbol-value marker) position buffer)
  (set-marker-insertion-type (symbol-value marker) type)
  (eval marker)
  )

;; ------------------------------------------------------------

;; mell-sublist
(if (functionp 'sublist)
    (defalias 'mell-sublist 'sublist)
  (defun mell-sublist (list start &optional end)
    (if (< start 0)
	(setq start (+ start (length list))))
    (if (null end)
	(nthcdr start (copy-sequence list))
      (and end (< end 0) 
	   (setq end (+ end (length list))))
      (let (sublist tmp)
	(if (> start end)
	    (progn (setq tmp start)
		   (setq start end)
		   (setq end tmp)))
	(while (< start end)
	  (setq end (1- end)
		sublist (cons (nth end list) sublist)))
	sublist)))
  )

;; mell-subarray
(if (functionp 'subarray)
    (defalias 'mell-subarray 'subarray)
  (defun mell-subarray (array start &optional end)
    (apply 'vector (mell-sublist (append array nil) start end)))
  )

;; mell-subseq
(if (functionp 'subseq)
    (defalias 'mell-subseq 'subseq)
  (defun mell-subseq (seq start &optional end) ;; For Emacs20
    (cond ((stringp seq) (substring seq start end))
	  ((listp seq)   (mell-sublist seq start end))
	  (t             (mell-subarray seq start end))
	  ))
  )

;; ------------------------------------------------------------
;; mell-mode
;; ------------------------------------------------------------

;; this function requires mell-alist.
(defun mell-set-minor-mode (name modeline &optional key-map)
  (make-variable-buffer-local name)
  (setq minor-mode-alist
	(mell-alist-add minor-mode-alist (list name modeline)))
  (and key-map
       (setq minor-mode-map-alist
	     (mell-alist-add minor-mode-map-alist (cons name key-map)))
       )
  )

;; ------------------------------------------------------------
;; mell-region
;; ------------------------------------------------------------

;; mell-region-face
(if running-xemacs
    (defconst mell-region-face 'zmacs-region)
  (defconst mell-region-face 'region)
  )

;; mell-region-active-p
(if running-xemacs
    (defun mell-region-active-p ()
      (region-active-p))
  (defun mell-region-active-p ()
    (mell-check-value 'mark-active))
  )

;; mell-transient-mode-p
(if running-xemacs
    (defun mell-transient-mode-p ()
      (mell-check-value 'zmacs-regions))
  (defun mell-transient-mode-p ()
    (mell-check-value 'transient-mark-mode))
  )

;; Define mell-transient-region-active-p
(defun mell-transient-region-active-p ()
  (and (mell-transient-mode-p)
       (mell-region-active-p)))

(define-obsolete-function-alias
  'transient-region-active-p 'mell-transient-region-active-p)

(defun mell-transient-region-stay ()
  (and running-xemacs
       (setq zmacs-region-stays t))
  )
;; ------------------------------------------------------------
;; mell-region (applications)
;; ------------------------------------------------------------

(defun mell-read-region-or-string ()
  "If active region exists, return the substring specified the region.
 Or read string from minibuffer."
  (interactive)
  (if (mell-transient-region-active-p)
      (buffer-substring (mark) (point))
    (read-string "String: " (current-word))
    ))

(define-obsolete-function-alias
  'read-region-or-string 'mell-read-region-or-string)

(defun mell-paragraph-string (&optional point)
  (buffer-substring (mell-point-at-bop point) (mell-point-at-eop point)))

(defun mell-delete-paragraph (&optional point)
  (delete-region (mell-point-at-bop point) (mell-point-at-eop point)))

(defun mell-read-region-or-paragraph ()
  "If active region exists, return the substring specified the region.
 Or return paragraph on the cursor automatically."
  (interactive)
  (if (mell-transient-region-active-p)
      (buffer-substring (mark) (point))
    (mell-paragraph-string)
    ))

(defun mell-call-function-region-or-string (function &optional args-list)
  (apply function
	 (prog1 (mell-read-region-or-string)
	   (and (mell-transient-region-active-p)
		(delete-region (point) (mark)))
	   )
	 args-list))

(defun mell-call-function-region-or-paragraph (function &optional args-list)
  (if (or (mell-transient-region-active-p)
	  (y-or-n-p "Use current paragraph? "))
      (apply function
	     (prog1 (mell-read-region-or-paragraph)
	       (if (mell-transient-region-active-p)
		   (delete-region (point) (mark))
		 (mell-delete-paragraph))
	       )
	     args-list)
    ""))

(defun mell-narrow-to-transient-region (&optional begin end)
  "If active region exists, narrow-to-region"
  (setq begin (or begin (and (transient-region-active-p) (region-beginning)))
	end   (or end   (and (transient-region-active-p) (region-end))))
  (if (and begin end)
      (progn
	(narrow-to-region begin end)
	(goto-char begin)
	))
  )
  
(defun mell-region-get-rectangle-list (start end &optional buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (let* (rectangle-alist
	   (column-min (min (mell-column-at-point start)
			    (mell-column-at-point end)))
	   (column-max (max (mell-column-at-point start)
			    (mell-column-at-point end)))
	   (point-min (min (mell-point-at-column column-min start)
			   (mell-point-at-column column-min end)))
	   (point-max (max (mell-point-at-column column-max start)
			   (mell-point-at-column column-max end)))
	   )
      (goto-char point-min)
      (while (< (point) point-max)
	(move-to-column column-min)
	(setq rectangle-alist 
	      (cons (cons (point) (mell-point-at-column column-max))
		    rectangle-alist))
	(forward-line 1)
	)
      (reverse rectangle-alist)
      )))
      
(put 'mell-region-rectangle-while 'lisp-indent-function 1)
(defmacro mell-region-rectangle-while (rectangle &rest body)
  `(let ((rectangle-markers
	  (mell-region-get-rectangle-marker-list
	   (nth 0 ,rectangle) (nth 1 ,rectangle) (nth 2 ,rectangle)))
	 )
     (mapcar
      (lambda (region)
	(let ((line-beginning (car region))
	      (line-end (cdr region)))
	  ,@body
	  ))
      rectangle-markers)
     (mapcar
      (lambda (region)
	(set-marker (car region) nil)
	(set-marker (cdr region) nil)
	)
      rectangle-markers)
     ))

(defun mell-region-get-rectangle-marker-list (start end &optional buffer)
  (mapcar
   '(lambda (region)
      (cons (mell-marker-make (car region)) (mell-marker-make (cdr region)))
      )
   (mell-region-get-rectangle-list start end buffer))
  )

(defun mell-region-rectangle-right-edge-p (start end)
  (save-excursion
    (let ((list (mell-region-get-rectangle-list start end))
	  (result t))
      (while (and list
		  (progn (goto-char (cdr (car list)))
			 (eolp)))
	(setq list (cdr list))
	)
      (null list)
      )))

(put 'mell-save-region 'lisp-indent-function 0)
(defmacro mell-save-region (&rest body)
  `(let ((mark (mark))
	(active-p (mell-transient-region-active-p))
	(cur-buffer (current-buffer))
	global-mark-ring mark-ring
	kill-ring kill-ring-yank-pointer
	overlay)
    (if active-p
	(setq overlay (mell-sign-region-highlight (mark) (point)))
      )
    ,@body
    (if active-p
	(progn
	  (mell-transient-region-activate)
	  (mell-sign-region-highlight-off overlay)
	  ))
    ))

;; ------------------------------------------------------------
;; mell-match
;; ------------------------------------------------------------

(defun mell-match-count-string (regexp string)
  (save-match-data
    (let ((i 0) (n 0))
      (while (and (string-match regexp string i) (< i (match-end 0)))
	(setq i (match-end 0))
	(setq n (1+ n)))
      n)))
  
(if running-xemacs
    (defun mell-match-count-region (regexp start end &optional buffer)
      (mell-match-count-string regexp (buffer-substring start end buffer))
      )
  (defun mell-match-count-region (regexp start end &optional buffer)
    (save-excursion
      (and buffer (set-buffer buffer))
      (mell-match-count-string regexp (buffer-substring start end))
      ))
  )

(define-obsolete-function-alias
  'count-string-match 'mell-match-count-string)

;; ------------------------------------------------------------
;; mell-alist
;; ------------------------------------------------------------
(defun mell-alist-add! (alist new-cons)
  (if (null alist)
      (error "mell-alist-add! can not deal nil as an alist.")
    (let ((current-cons (assoc (car new-cons) alist)))
      (if current-cons
	  (setcdr current-cons (cdr new-cons))
	(if (car alist)
	    (nconc alist (list new-cons))
	  (setcar alist new-cons))
	)
      alist)))
  
(defun mell-alist-add (alist new-cons)
  (if (null alist)
      (list new-cons)
    (let ((return-alist (copy-alist alist)))
      (mell-alist-add! return-alist new-cons)
      return-alist)))
  
(defun mell-alist-delete (alist key)
  (if key
      (let (return-alist)
	(mapcar '(lambda (x)
		   (or (equal key (car x))
		       (setq return-alist (cons x return-alist))))
		alist)
	(if return-alist
	    (reverse return-alist)
	  (list nil)))
    alist)
  )

(define-obsolete-function-alias
  'delete-assoc 'mell-alist-delete)

(defun mell-alist-combine (var-list val-list)
  (let ((i 0))
    (mapcar '(lambda (var) 
	       (prog1 (cons var (nth i val-list))
		 (setq i (1+ i))))
	    var-list)))

(define-obsolete-function-alias
  'mell-make-alist 'mell-alist-combine)

;; ------------------------------------------------------------
;; mell-list
;; ------------------------------------------------------------

(defun mell-list-member-get-nth (element list)
  (let ((rest-list (member element list)))
    (if rest-list
	(- (length list) (length rest-list))
      )))

(defun mell-list-mapfunc (func list &optional value)
  (while list
    (setq value (funcall func value (car list))
	  list (cdr list)))
  value)

(define-obsolete-function-alias
  'mapfunc 'mell-list-mapfunc)

(defun mell-list-mapadd (number-list)
  (mell-list-mapfunc '+ number-list 0))

(define-obsolete-function-alias
  'mapadd 'mell-list-mapadd)

;; elmo-uniq-list (from wanderlust) より.
(defun mell-list-uniq (list)
  "Distractively uniqfy elements of LIST."
  (let ((tmp list))
    (while tmp (setq tmp
                     (setcdr tmp
                             (and (cdr tmp)
                                  (delete (car tmp)
                                          (cdr tmp)))))))
  list)


;; ------------------------------------------------------------
;; mell-key-binding
;; ------------------------------------------------------------

(defun mell-key-binding-minor-mode-list (key)
  (delq nil
	(mapcar 
	 '(lambda (x) (lookup-key x key))
	 (current-minor-mode-maps))
  ))
(define-obsolete-function-alias
  'minor-mode-key-binding-list 'mell-key-binding-minor-mode-list)

(defun mell-key-binding-next-minor-mode (keymap)
  (car (delq nil
	     (mapcar '(lambda (x) (lookup-key x (this-command-keys)))
		     (cdr (member keymap (current-minor-mode-maps))))
	     )))
(define-obsolete-function-alias
  'next-minor-mode-key-binding 'mell-key-binding-next-minor-mode)

(defun mell-key-binding-next (&optional keymap command-keys)
  (let ((mode-maps (if keymap (member keymap (current-minor-mode-maps))
		     (current-minor-mode-maps)))
	(command-keys (or command-keys (this-command-keys))))
    (or (car (cdr (delq nil
			(mapcar '(lambda (x) (lookup-key x command-keys))
				mode-maps))))
	(mell-key-binding-local command-keys)
	(mell-key-binding-global command-keys)
	)))

(defun mell-key-binding-local (keys)
  (let ((result (local-key-binding keys)))
    (if (numberp result)
	(local-key-binding (mell-subseq keys 0 result))
      result)
    ))

(defun mell-key-binding-global (keys)
  (let ((result (global-key-binding keys)))
    (if (numberp result)
	(global-key-binding (mell-subseq keys 0 result))
      result)
    ))

(defun mell-call-next-interactively (&optional keymap command-keys)
  (call-interactively (or (mell-key-binding-next keymap command-keys)
			  'self-insert-command)
		      ))

;; ------------------------------------------------------------
;; mell-time
;; ------------------------------------------------------------

(defun mell-time-get-interval (time1 time2)
  (if (or (> (- (nth 0 time1) (nth 0 time2)) 0)
	  (> (- (nth 1 time1) (nth 1 time2)) 1000))
      1000000000 ;; 桁あふれへのいんちき対処
    (+ (* 1000000 (- (nth 1 time1) (nth 1 time2)))
       (- (nth 2 time1) (nth 2 time2))))
  )


;; ------------------------------------------------------------
;; mell-color
;; ------------------------------------------------------------
(defun mell-color-find (color-name &optional alt-tty-color-num)
  (if window-system color-name
    (and (functionp 'find-tty-color)
	 (or (and color-name (find-tty-color color-name))
	     (nth alt-tty-color-num (tty-color-list))))
    ))

(defun mell-color-get-cursor ()
  (if (featurep 'xemacs)
      (face-background-name 'text-cursor) ;; Emacs だと void
    (cdr (assoc 'cursor-color (frame-parameters)))
    ))

(defun mell-color-get-background ()
  (if (featurep 'xemacs)
      (face-background-name 'default) ;; Emacs だと nil
    (cdr (assoc 'background-color (frame-parameters)))
    ))

(provide 'mell)



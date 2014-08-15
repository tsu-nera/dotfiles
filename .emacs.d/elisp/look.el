85;95;0c;; Copyright (C) 1994 Mikio Nakajima <PBC01764@niftyserve.or.jp>
;; Helped a lot by Mr. Seiichi Namba <GBC00441@niftyserve.or.jp>
;; Version 1.0 (1994/11/16)
;; modified by Matsuda Shigeki <matsu@math.s.chiba-u.ac.jp> (2001/01/16)

;; look.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; look.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;;;   I used `look.el' created by Mr. Namba, and I was very impressed with it.
;;; But I thought it was very regrettable that with his version of `look.el'
;;; candidates after coming first one are shown in other window and a user is
;;; compelled to repeatedly type C-u, C-L -- bound to a function called
;;; (look-here) -- until a user get what he/she wants.

;;;   So I've created new interface for `look' command in Emacs.  It provides
;;; a user with a interface like so called `Front End Processor' of Japanese.
;;; That is, a user can select a words in candidates shown in a minibuffer by
;;; typing `A', `S', `D' or `F' key -- keys near the home position --
;;; attributed to a word they want.  If there are more candidates, Type `SPC'
;;; key for more, `X' or `DEL' key for less.  `ESC' or `Q' key for quitting
;;; selecting candidates mode and restarting normal edit in Emacs.  You may
;;; use small letters for all such commands.  Note that first candidates are
;;; inserted automatically first of all.

;;;   Mr. Namba helped me a lot for creating my version of `look.el' and grant
;;; me to use the name for my version, too.  He said that it must be very
;;; useful for either people who speak English as their native language or
;;; not, so I should contribute it to the FSF.	And I will do it.  Thank you
;;; very much, Mr. Namba!

;;; for Install:

;;;   Suggested key bind to (look-here) command is;
;;;	  (global-set-key "\M-L" 'look-here)
;;;   You may put the following lisp expression in your ~/.emacs;
;;;	  (autoload 'look-here "look" nil t nil)
;;;   Set applicable variable for your look to the variable
;;; `look-command-spec'.  See below.


(defvar look-command-spec 'gnu
  "*`gnu' if you use GNU look \(or other look which can handle regexp
search\), `minix' if you use minix look \(or other look which can handle [/']
character as a word delimiter and `nil' your look can handle neither regexp
nor such word delimiter." )

(defvar look-skip-characters nil)

(defun look-backward-word ()
  "Same as backward-word function except it ignores some characters for regexp
search or minix look's version search."
  (if (or (eq look-command-spec 'gnu) (eq look-command-spec 'minix))
      (skip-chars-backward look-skip-chars)
    (backward-word 1) ))

(defun look-here ()
  "Invoke `look' command by \(call-process\).  Set your cursor at the end of
an incomplete word and call this function, and you'll see first candidate for
such an incomplete word inserted in a current buffer and other candidates
shown in a minibuffer per four candidates.  Type `SPC' key for more
candidates, `X' or `DEL' key for the previous candidates, `ESC' or `Q' key for
quitting selecting candidates mode and restarting normal edit in Emacs, and
`A', `S', `D' or `F' key for selecting a word attributed to one of them.  You
may use small letters for all such commands."
  (interactive)
  (let* ((cbuf (current-buffer))
	(look-skip-chars (cond (look-skip-characters
				look-skip-characters)
			       ((eq look-command-spec 'gnu)
			        "^ \t\n`\"()。、「」『』（）")
			       ((eq look-command-spec 'minix)
				"a-zA-z/'")))
	 (incomp (save-excursion
		 (buffer-substring (point)
				   (progn (look-backward-word) (point)) )))
	 (regexp-p (string-match "[]\\[^$.*+?]" incomp))
	 (look-case
	  (let ((case-check
		 (if (and regexp-p (string-match "^\\^" incomp))
		     (substring incomp 1)
		   incomp )))
	    (if (and regexp-p (string-match "[][^$.*+?]" case-check))
		(setq case-check (substring case-check 0 (1- (match-end 0)))))
	    (cond ((string-equal case-check (downcase case-check)) 'down)
		  ((string-equal case-check (capitalize case-check)) 'cap)
		  ((string-equal case-check (upcase case-check)) 'up)
		  (t 'down) )))
	 selected-word )
    (set-buffer (get-buffer-create " *looking-here*"))
    (erase-buffer)
    (cond ((not regexp-p) (call-process "look" nil t nil incomp))
	  ((eq look-command-spec 'gnu)
;	   (call-process shell-file-name nil t nil
;			 shell-command-option "look -r" incomp )))
	   (call-process "look" nil t nil "-r" incomp)))
    (if (= (buffer-size) 0)
	(message "(Look command completed with no output)") ;; It's true...
      (let (next-word
	    word-list )
	(goto-char 1)
	(while (not (eobp))
	  (setq next-word
		(buffer-substring (point) (progn (forward-line 1)
						 (backward-char 1)
						 (point) )))
	  (or (string-equal next-word incomp) ;; other than the same word.
	      (setq word-list (append word-list (list next-word))) )
	  (forward-line 1)
	  (beginning-of-line) )
	(if (not (car word-list))
	    (message "(Look command completed with no output)")
	  ;; It's not true.  Nothing else exists other than the same word as
	  ;; an incomplete word which look.el completes.  But I think it's
	  ;; good.
	  (progn
	    (set-buffer cbuf)
	    (look-insert-word (car word-list)) ;; insert a first candidate.
	    (if (cdr word-list)
		(progn (setq word-list
			     (append (cdr word-list) (list (car word-list))))
		       (setq selected-word
			     (look-show-candidates (length word-list) 0) )
		       (if (not (eq selected-word 'exit))
			   (progn (set-buffer cbuf)
				  (look-insert-word selected-word) )))
	      (message "(Only one candidate for %s)" incomp) )))))))

(defun look-show-candidates (word-length offset)
;; Internal function of look-here.  Return candidates selected by a user.
;; Call itself recursively.  Tricky?
  (let ((counter 0)  ;; `word-list' and `incomp' variables used in this
	input msg )  ;; function are declared in look-here function and be
		     ;; in the scope of such function.
    (catch 'break
      (mapcar
       (function (lambda (string)
		   (if (< word-length (+ 1 counter offset))
		       (throw 'break nil)
		     (progn
		       (setq msg
			     (concat msg string ": "
                                     (nth (+ counter offset) word-list) "  " ))
                       (setq counter (1+ counter)) ))))
       '("A" "S" "D" "F") ))  ;; home position keys.
    (message (concat msg
		     (number-to-string (+ counter offset)) "/"
		     (number-to-string word-length)))
    (setq input (read-char))
    (catch 'exit
      (cond ((eq input 32) ;; SPC key
             (cond ((> word-length (+ counter offset))
                    (look-show-candidates word-length (+ offset 4)) )
                   (t (message "(No more candidate for %s)" incomp)
                      (sit-for 2)
                      (look-show-candidates word-length offset) )))
            ((or (eq input 127) (eq input 120)) ;; DEL or `x' key
             (cond ((eq offset 0)
                    (message "(No previous candidate for %s)" incomp)
                    (sit-for 2)
                    (look-show-candidates word-length 0) )
                   (t
                    (look-show-candidates word-length (- offset 4)) )))
            ;; `a' or `A'
            ((or (eq input 97) (eq input 65)) (nth (+ offset 0) word-list))
            ;; `s' or `S'
            ((or (eq input 115) (eq input 83)) (nth (+ offset 1) word-list))
            ;; `d' or `D'
            ((or (eq input 100) (eq input 68)) (nth (+ offset 2) word-list))
            ;; `f' or `F'
            ((or (eq input 102) (eq input 70)) (nth (+ offset 3) word-list))
            ;; ESC, `q' or `Q'
            ((or (eq input 27) (eq input 113) (eq input 81))
             (throw 'exit 'exit) )
            (t (message "illegal input.")
               (sit-for 2)
               (look-show-candidates word-length offset) )))))

(defun look-insert-word (insert-word)
;; Internal function of look-here.  Insert INSERT-WORD in a position of
;; incomplete word which look.el completes.  If an incomplete word is upper
;; case, convert an inserted word to upper case.  If capitalized, convert it
;; to capitalized.  Nothing if an incomplete word is down case.
  (delete-region (point) (progn (look-backward-word) (point)))
  (insert insert-word)
  (if (not (eq look-case 'down))
      (progn (backward-word 1)
             (cond ((eq look-case 'cap)
                    (capitalize-word 1) )
                   ((eq look-case 'up)
                    (upcase-word 1) )))))

(provide 'look)

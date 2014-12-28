;; from https://gist.github.com/cofi/3013327
(require 'helm)
(require 'flyspell)

(defun helm-flyspell-correct ()
  "Use helm for flyspell correction.
Adapted from `flyspell-correct-word-before-point'."
  (interactive)
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let ((cursor-location (point))
	(word (flyspell-get-word))
	(opoint (point)))
    (if (consp word)
	(let ((start (car (cdr word)))
	      (end (car (cdr (cdr word))))
	      (word (car word))
	      poss ispell-filter)
	  ;; now check spelling of word.
	  (ispell-send-string "%\n")	;put in verbose mode
	  (ispell-send-string (concat "^" word "\n"))
	  ;; wait until ispell has processed word
	  (while (progn
		   (accept-process-output ispell-process)
		   (not (string= "" (car ispell-filter)))))
	  ;; Remove leading empty element
	  (setq ispell-filter (cdr ispell-filter))
	  ;; ispell process should return something after word is sent.
	  ;; Tag word as valid (i.e., skip) otherwise
	  (or ispell-filter
	      (setq ispell-filter '(*)))
	  (if (consp ispell-filter)
	      (setq poss (ispell-parse-output (car ispell-filter))))
	  (cond
	   ((or (eq poss t) (stringp poss))
	    ;; don't correct word
	    t)
	   ((null poss)
	    ;; ispell error
	    (error "Ispell: error in Ispell process"))
	   (t
	    ;; The word is incorrect, we have to propose a replacement.
	    (flyspell-do-correct (helm-comp-read "Correction: "
						 (append
						  (third poss)
						  '(("Save word"        . save)
						    ("Accept (session)" . session)
						    ("Accept (buffer)"  . buffer)))
						 :name (format "%s [%s]" word (or ispell-local-dictionary
										  ispell-dictionary
										  "Default"))
						 :must-match t
						 :alistp t)

				 poss word cursor-location start end opoint)))
	  (ispell-pdict-save t)))))

(provide 'helm-flyspell-correct)

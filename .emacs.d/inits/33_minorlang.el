;; -----------------------------------------------------------------------
;; Name     :  Oz
;; Function :  マルチパラダイム言語
;; ------------------------------------------------------------------------
(add-hook 'oz-mode-hook
	  (lambda ()
	    (define-key oz-mode-map "\C-c\C-b" 'oz-feed-buffer)
	    (define-key oz-mode-map "\C-c\C-l" 'oz-feed-line)
	    (define-key oz-mode-map "\C-c\C-r" 'oz-feed-region)))
;; -----------------------------------------------------------------------
;; Name     :  Cool
;; ------------------------------------------------------------------------
(autoload 'cool-mode "cool-mode" "Major mode for editing COOL programs" t)
(setq auto-mode-alist
            (append '(("\\.cl\\'" . cool-mode)) auto-mode-alist))
	  

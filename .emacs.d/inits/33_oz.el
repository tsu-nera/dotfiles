;; -----------------------------------------------------------------------
;; Oz
;; -----------------------------------------------------------------------
;; Name     : 
;; Install  :
;; Function : 
;; ------------------------------------------------------------------------
(add-hook 'oz-mode-hook
	  (lambda ()
	    (define-key oz-mode-map "\C-c\C-b" 'oz-feed-buffer)
	    (define-key oz-mode-map "\C-c\C-l" 'oz-feed-line)
	    (define-key oz-mode-map "\C-c\C-r" 'oz-feed-region)))

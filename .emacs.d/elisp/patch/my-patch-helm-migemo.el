;; helm-migemoを動作さ競るためのパッチ
;; http://rubikitch.com/2014/12/19/helm-migemo/
(defun helm-compile-source--candidates-in-buffer (source)
  (helm-aif (assoc 'candidates-in-buffer source)
      (append source
	      `((candidates
		 . ,(or (cdr it)
			(lambda ()
			  ;; Do not use `source' because other plugins
			  ;; (such as helm-migemo) may change it
			  (helm-candidates-in-buffer (helm-get-current-source)))))
		(volatile) (match identity)))
    source))

(provide 'my-patch-helm-migemo)

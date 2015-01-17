(require 'eww)
(defvar eww-data)
(defun eww-current-url ()
  (if (boundp 'eww-current-url)
      eww-current-url                   ;emacs24.4
    (plist-get eww-data :url)))         ;emacs25
(defun eww-current-title ()
  (if (boundp 'eww-current-title)
      eww-current-title                   ;emacs24.4
    (plist-get eww-data :title)))

(require 'helm)
(require 'cl-lib)

(defun helm-eww-history-candidates ()
  (cl-loop with hash = (make-hash-table :test 'equal)
           for b in (buffer-list)
           when (eq (buffer-local-value 'major-mode b) 'eww-mode)
           append (with-current-buffer b
                    (clrhash hash)
                    (puthash (eww-current-url) t hash)
                    (cons
                     (cons (format "%s (%s) <%s>" (eww-current-title) (eww-current-url) b) b)
                     (cl-loop for pl in eww-history
                              unless (gethash (plist-get pl :url) hash)
                              collect
                              (prog1 (cons (format "%s (%s) <%s>" (plist-get pl :title) (plist-get pl :url) b)
                                           (cons b pl))
                                (puthash (plist-get pl :url) t hash)))))))
(defun helm-eww-history-browse (buf-hist)
  (if (bufferp buf-hist)
      (switch-to-buffer buf-hist)
    (switch-to-buffer (car buf-hist))
    (eww-save-history)
    (eww-restore-history (cdr buf-hist))))
(defvar helm-source-eww-history
  '((name . "eww history")
    (candidates . helm-eww-history-candidates)
    (migemo)
    (action . helm-eww-history-browse)))
(defvaralias 'anything-c-source-eww-history 'helm-source-eww-history)
(defun helm-eww-history ()
  (interactive)
  (helm :sources 'helm-source-eww-history
        :buffer "*helm eww*"))

(define-key eww-mode-map (kbd "H") 'helm-eww-history)
(define-key eww-mode-map (kbd "s") 'helm-eww-history)

(require 'helm)

(defvar helm-shell-history-file "~/.zsh-history")
(defvar helm-shell-history-command
  '(lambda (pattern)
     (concat "\\tac " helm-shell-history-file " | "
             "\\grep -e \"" pattern "\"  | "
             "\\sed 's/^: [0-9]*:[0-9];//'")))

(defun helm-shell-history ()
  (interactive)
  (helm :sources
        `(((name . "shell-history")
           (candidates . (lambda ()
                           (start-process "history-all" nil "/usr/bin/zsh" "-c"
;;			   (candidates-process "history-all" nil "/usr/bin/zsh" "-c"
;;			   (candidates-process "history-all" nil "/bin/sh" "-c"
                                          (funcall helm-shell-history-command
                                                   helm-pattern))))
           (candidate-number-limit . 500)
           (multiline)
           (nohighlight)
           (action . (lambda (line)
                       (term-send-raw-string line)))
           (requires-pattern . 3)
           (delayed)))
        :buffer "*helm shell history*"))

;; (defvar helm-c-shell-history-buffer "*helm shell history*")
;; (defvar helm-shell-history-command "history-all | awk '{print $4}'")

;; (defun helm-c-shell-history-init ()
;;   (with-current-buffer (helm-candidate-buffer 'global)
;;     (call-process-shell-command helm-shell-history-command nil t)))

;; (defun helm-c-shell-history-action (line)
;;   (insert line))

;; (defvar helm-c-shell-history-source
;;   '((name . "shell-history")
;;     (init . helm-c-shell-history-init)
;;     (candidates-in-buffer)
;;     (candidate-number-limit . 9999)
;;     (multiline)
;;     (action . helm-c-shell-history-action)
;;     (requires-pattern . 2)
;;     (delayed)))

;; (defun helm-shell-history ()
;;   (interactive)
;;   (helm :sources helm-c-shell-history-source
;;         :buffer helm-c-shell-history-buffer))

(provide 'helm-shell-history)

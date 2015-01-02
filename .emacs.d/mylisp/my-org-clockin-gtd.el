;;; mygtd.el --- My GTD Timer
;; 
;; Author: tsu-nera
;; Version: 0.01

;;; Code:
(require 'org-clock)

(defvar mygtd:organization-task-id-gtd "b66237b9-95dd-4863-bc36-bd4dbc435eca")
(defvar mygtd:organization-task-id-rest "192d0802-8ed7-4c51-ad3f-04f6ae4e75f6")
(defvar mygtd:rest-display-string "\n 休憩中だよ (*'~`*)")
(defvar mygtd:rest-buffer-name "*RestMessage*")
(defvar mygtd:gtd-file "~/gtd/main.org")

(defvar mygtd:rest-timer nil)
(defvar mygtd:rest-count-sec 0)

(defun mygtd:clock-in-task-by-id (id)
  "Clock in a task by id"
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in '(16)))))

(defun mygtd::tick ()
  "called per 1 sec and update timer"
  (mygtd:display-rest-message-to-buffer)
  (incf mygtd:rest-count-sec))

(defun mygtd:rest-display-mode ()
  "Sets a fixed width (monospace) font in current buffer"
  (setq buffer-face-mode-face '(:height 1000))
  (buffer-face-mode))

(defun mygtd:create-rest-message ()
  (princ (concat mygtd:rest-display-string "\n\n "
	  (mygtd:rest-time-to-string mygtd:rest-count-sec))))
 
(defun mygtd:display-rest-message-to-buffer ()
  (let ((temp-buffer-show-function 'switch-to-buffer))
    (with-output-to-temp-buffer mygtd:rest-buffer-name
      (mygtd:create-rest-message))
    (mygtd:rest-display-mode)))

(defun mygtd:rest-time-to-string (seconds)
  (format "%02d:%02d" (/ seconds 60) (mod seconds 60)))

(defun mygtd:reset-rest-timer ()
  (if mygtd:rest-timer
    (cancel-timer mygtd:rest-timer))
  (setq mygtd:rest-timer nil)
  (setq mygtd:rest-count-sec 0))

(add-hook 'org-clock-in-hook 'mygtd:reset-rest-timer)

;;;###autoload
(defun mygtd:gtd ()
  (interactive)
  (find-file mygtd:gtd-file)
  (mygtd:clock-in-task-by-id mygtd:organization-task-id-gtd))

(defun mygtd:rest ()
  (interactive)
  (if mygtd:rest-timer
      (error "Already start timer!!")
    (find-file mygtd:gtd-file)
    (mygtd:clock-in-task-by-id mygtd:organization-task-id-rest)
    (mygtd:display-rest-message-to-buffer)
    (setq mygtd:rest-timer (run-with-timer 0 1 'mygtd::tick))))
  
(provide 'my-org-clockin-gtd)

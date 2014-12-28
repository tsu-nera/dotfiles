(require 'org-clock)

(defvar my/organization-task-id-gtd "b66237b9-95dd-4863-bc36-bd4dbc435eca")
(defvar my/organization-task-id-rest "192d0802-8ed7-4c51-ad3f-04f6ae4e75f6")
(defvar my/rest-display-string "\n 休憩中だよ (*'~`*)")

(defun my/clock-in-task-by-id (id)
  "Clock in a task by id"
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find id 'marker)
		       (org-clock-in '(16)))))

(defun my/org-rest-display-mode ()
  "Sets a fixed width (monospace) font in current buffer"
  (setq buffer-face-mode-face '(:height 1000))
  (buffer-face-mode))

(defun my/rest-display ()
  "Display rest message on buffer"
  (let ((temp-buffer-show-function 'switch-to-buffer))
    (with-output-to-temp-buffer "*RestMessage*"  
      (princ my/rest-display-string)))
  (my/org-rest-display-mode)
  )

(defun my/gtd ()
  (interactive)
  (find-file "~/gtd/main.org")
  (my/clock-in-task-by-id my/organization-task-id-gtd)
  )

(defun my/rest ()
  (interactive)
  (find-file "~/gtd/main.org")
  (my/clock-in-task-by-id my/organization-task-id-rest)
  (my/rest-display)
  )

(provide 'my-org-clockin-gtd)

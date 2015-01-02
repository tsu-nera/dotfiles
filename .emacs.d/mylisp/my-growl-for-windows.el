;; https://gist.github.com/danielsz/ac19353e718dde3dea72
(defvar growlnotify-command "growlnotify")

(defun growl (title message)
  (shell-command (concat growlnotify-command " /t:" title " " message)))
  
(defun my-erc-nick-hook (match-type nick message)
 "Shows a growl notification, when user's nick was mentioned.
  If the buffer is; currently not visible, makes it sticky."
 (unless (posix-string-match "^\\** *Users on #" message)
   (growl
    (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
    message
    )))

(add-hook 'erc-text-matched-hook 'my-erc-nick-hook)

(provide 'my-growl-fow-windows)

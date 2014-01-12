;;; Pull notes from evernote and import them to org-mode

(require 'org)
(require 'evernote-mode)

(defvar org-evernote-notebook-name "Captures"
  "*Name of the Evernote notebook to pull captured items from.")

(defvar org-evernote-captured-tag "captured"
  "*Name of the Evernote tag which will indicate that an item has already been captured.")

(defvar org-evernote-file "~/org/from-evernote.org"
  "*Filename to which new Evernote notes will be appended.")

(defun file-string (file)
    "Read the contents of a file and return as a string."
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))

(defun org-evernote-timestamp-buffer (buf)
  "Time stamp buffer BUF, just to make sure its checksum will change."
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (re-search-forward
             "^\\([ \t]*\\)#\\+LAST_EVERNOTE_PULL:.*\n?" nil t)
            (progn
              (goto-char (match-end 1))
              (delete-region (point) (match-end 0)))
          (if (looking-at ".*?-\\*-.*-\\*-")
              (forward-line 1)))
        (insert "#+LAST_EVERNOTE_PULL: "
                (format-time-string "%Y-%m-%d %T") "\n")))))

(defun org-evernote-capture-note (note)
  (let* ((fullnote (enh-get-note-attr (enutil-aget 'guid note)))
         (note-content-file (enutil-aget 'contentFile fullnote))
         (note-title (enutil-aget 'title fullnote)))
    (save-excursion
      (goto-char (point-max))
      (insert "\n* TODO " note-title "\n")
      (enh-format-enml (file-string note-content-file) (current-buffer)))))

(defun org-evernote-tag-note (note tag)
  "Add a tag to a note.  Actually replaces all tags because I'm lazy."
  (enh-command-update-note-tags note (list tag)))

(defun org-evernote-pull ()
  "Pull new captured items from Evernote."
  (interactive)
  (enh-command-with-auth
   (let ((guid (enh-notebook-name-to-guid org-evernote-notebook-name))
         (captured-id (car (enh-tag-names-to-guids (list org-evernote-captured-tag)))))
     (save-excursion
       (find-file org-evernote-file)
       (loop for note in (enh-command-get-note-attrs-from-notebook-and-tag-guids guid nil)
             unless (member captured-id (cdr (assq 'tagGuids note)))
             do (and (org-evernote-capture-note note)
                     (org-evernote-tag-note note org-evernote-captured-tag)))
       (org-evernote-timestamp-buffer (current-buffer))
       (save-buffer)))))

(provide 'org-evernote)

(defun ascii-table ()
  "Display a list of the first 128 ASCII chars and keyboard equivalents."
  (interactive)
  (let ((char 0))
    (message "Making the ascii table...")
    (save-excursion
      (set-buffer (get-buffer-create "*ASCII Table*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (while (<= char 127)
	(insert (format "%d %s\t" char (single-key-description char)))
	(setq char (1+ char))
	(if (>= (count-lines (point-min) (point)) 13)
	    (goto-char (point-min))
	  (next-line 1))
	(end-of-line))
      (goto-char (point-min))
      ;; this may be overkill, but it is the quickest way I know to nuke
      ;; blank space at the end of all the lines in a buffer.
      (picture-mode) (picture-mode-exit)
      (setq buffer-read-only t))
    (display-buffer "*ASCII Table*")
    (message "Making the ascii table...done")))

(defun ascii-table-octal ()
  "Display a list of the first 128 ASCII chars and keyboard equivalents."
  (interactive)
  (let ((char 0))
    (message "Making the ascii table...")
    (save-excursion
      (set-buffer (get-buffer-create "*ASCII Table Octal*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (while (<= char 127)
	(insert (format "%o %s\t" char (single-key-description char)))
	(setq char (1+ char))
	(if (>= (count-lines (point-min) (point)) 13)
	    (goto-char (point-min))
	  (next-line 1))
	(end-of-line))
      (goto-char (point-min))
      ;; this may be overkill, but it is the quickest way I know to nuke
      ;; blank space at the end of all the lines in a buffer.
      (picture-mode) (picture-mode-exit)
      (setq buffer-read-only t))
    (display-buffer "*ASCII Table Octal*")
    (message "Making the ascii table...done")))

;-Andrew Feren  (feren@ctron.com)
; Cabletron Systems Inc.
; Durham, NH

;; from.el --- Show who messages in mailspools is from

;; Copyright (C) 2001 Henrik Enberg

;; Author: Henrik Enberg <henrik@enberg.org>
;; Created: 2001-08-26 11:47:54
;; Version: 0.3
;; X-RCS: $Id: from.el,v 1.4 2001/08/30 09:46:16 henrik Exp $
;; URL: http://www.enberg.org/emacs/
;; Keywords: mail

;; This file is not part of GNU Emacs.

;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

;;; Commentary:

;; This package lets you quickly see subjects and senders of messages in
;; your mailspools, without the overhead of starting up a mailreader.

;; To install, drop it in a directory on your `load-path', and add
;; the following to your .emacs:

;;	(autoload 'from "from" "Show who messages in mailspools is from." t)

;; Then customize the variable `from-mailspools' to point to the spools
;; you're interested in.

;;; Code:

(eval-when-compile (require 'cl))
(require 'mail-extr)
(require 'parse-time)
(require 'rfc2047)

(defgroup from nil
  "Show who messages in mailspools is from.

This package lets you quickly see subjects and senders of messages in
your mailspools, without the overhead of starting up a mailreader."
  :group 'mail)

(defcustom from-mailspools nil
  "A list of unix mailspools that will be checked for new mail."
  :type '(repeat file)
  :group 'from)

(defcustom from-decode-mime-strings t
  "If non-nil, decode MIME encoded message headers."
  :type 'boolean
  :group 'from)

(defcustom from-use-other-window t
  "If non-nil, display From buffer in other window."
  :type 'boolean
  :group 'from)

(defcustom from-quit-command 'bury-buffer
  "Function to call when doing `from-quit'.
If `from-use-other-window' is true, and more than one window is
visible the window will be deleted."
  :type '(choice (const :tag "Bury buffer" :value bury-buffer)
		 (const :tag "Kill buffer" :value kill-buffer))
  :group 'from)

(defcustom from-mode-hook nil
  "List of functions to run after entering `from-mode'."
  :type 'hook
  :group 'from)

(defcustom from-date-format "%a %b %e"
    "Format for display of date in the From buffer listings.
See `format-time-string' for details on how this works."
  :type 'string
  :group 'from)

(defcustom from-line-format "%-20,20n %d %-48,49s"
  "Format specification of lines in the From buffer.
This works in much the same way as with `format', with a few simple
changes in length and padding of fields.

Valid control-strings are:
%a	Address of sender
%D	Date of message
%d	Date of message in format specified by `from-date-format'.
%n	Name of sender or email address when name doesn't exist.
%m	Message-ID of message
%s	Subject string of message.

A numerical modifier between the `%' and the control char means to make
this field autoload least this long, left-padded with spaces if
necessary.  If the number is negative, padding will be done to the right
instead.

To protect against very long strings, a second number between `%' and
control char separated by a comma will truncate the string if longer.

Thus, a value of \"%-20,25n %d %50s\" will print the name at leas 20
chars wide padded to the right but never wider than 25 chars.  Date will
be printed as is, and the subject is printed at least 50 chars wide
padded to the left."
  :type 'string
  :group 'from)

(defcustom from-highlight-regexp nil
  "Regexp matching lines that should be highlighted.
This is applied to a whole line in the From buffer."
  :type 'regexp
  :group 'from)

(defcustom from-highlight-face 'bold
  "Face used to highlight lines in `from-mode'."
  :type 'face
  :group 'from)

(defvar from-buffer-highlighted-p nil)

(defvar from-format-spec-alist
  '((?a address)
    (?D raw-date)
    (?d fmt-date)
    (?m msg-id)
    (?n name)
    (?s subject)))

(defun from-parse-line-format ()
  "Return a lisp from that evals to a string matching `from-line-format'."
  (let ((case-fold-search nil)
	(fmt-list nil))
    (with-temp-buffer
      (erase-buffer)
      (insert from-line-format)
      (goto-char (point-min))
      (while (not (eobp))
	(if (looking-at
	     (concat "%\\(\\([-0-9]+\\)\\(,[-0-9]+\\)?\\)?\\(["
		     (mapcar 'car from-format-spec-alist) "]\\)"))
	    (let ((max-width 0)
		  min-width
		  fmt-char
		  field)
	      (when (match-string 2)
		(setq min-width (string-to-int (match-string 2))))
	      (when (match-string 3)
		(setq max-width (string-to-int
				 (substring (match-string 3) 1))))
	      (setq fmt-char (string-to-char (match-string 4)))
	      (setq field (nth 1 (assoc fmt-char from-format-spec-alist)))
	      (goto-char (match-end 0))
	      (push (let ((max (abs max-width)))
		      (if min-width
			  `(insert (format ,(format "%%%ds" min-width)
					   (if (> (length ,field) ,max)
					       (substring ,field 0 ,max)
					     ,field)))
			`(insert (format "%s" ,field))))
		    fmt-list))
	  ;; this sucks, should concatenate
	  (push `(insert ,(char-to-string (char-after))) fmt-list)
	  (forward-char 1)))
      (push `(insert "\n") fmt-list))
    `(progn
       ,@(nreverse fmt-list))))

(defun from-parse-mailspools ()
  "Extract message headers from files in `from-mailspools'."
  (interactive)
  (let ((case-fold-search nil)
	(files from-mailspools)
	(header-list nil))
    (with-temp-buffer
      ;; loop trough all files
      (dolist (i files)
	(when (and (> (nth 7 (file-attributes i)) 0)
		   (file-readable-p i))
	  (let (positions start end)
	    (erase-buffer)
	    (insert-file-contents-literally i)
	    ;; get positions of headers in file
	    (goto-char (point-min))
	    (while (not (eobp))
	      (when (re-search-forward "^From " nil t)
		(setq start (match-beginning 0))
		(if (re-search-forward "^\r?$" nil t)
		    (setq end (match-beginning 0))
		  (setq end (point-max)))
		(push (cons start end) positions))
	      (forward-line))
	    ;; extract intresting headers
	    (dolist (j positions)
	      (let (headers name value)
		(narrow-to-region (car j) (cdr j))
		(goto-char (point-min))
		(while (re-search-forward "^\\([-A-Za-z]*\\): \\(.*\\)" nil t)
		  (setq name (intern (downcase (match-string 1))))
		  (if from-decode-mime-strings
		      (setq value (rfc2047-decode-string (match-string 2)))
		    (setq value (match-string 2)))
		  (when (memq name '(date from message-id subject))
		    (push (cons name value) headers)))
		(push headers header-list)
		(widen)))))))
    (nreverse header-list)))

(defun from-insert-buffer (headers)
  "Insert HEADERS in buffer in format specified by `from-line-format'."
  (let ((fmt-form (from-parse-line-format))
	(buffer-read-only nil))
    (save-excursion
      (erase-buffer)
      (dolist (i headers)
	(let ((address (let ((addr (mail-extract-address-components
				    (cdr (assoc 'from i)))))
			 (nth 1 addr)))
	      (raw-date (cdr (assoc 'date i)))
	      (fmt-date (format-time-string from-date-format
					    (apply 'encode-time
						   (parse-time-string
						    (cdr (assoc 'date i))))))
	      (msg-id (cdr (assoc 'subject i)))
	      (name (let ((addr (mail-extract-address-components
				 (cdr (assoc 'from i)))))
		      (if (not (null (car addr)))
			  (car addr)
			(nth 1 addr))))
	      (subject (cdr (assoc 'subject i))))
	  (eval fmt-form))))))

(defun from-highlight-buffer ()
  "Highlight all lines in buffer matching `from-highlight-regexp'."
  (let ((face from-highlight-face)
	(buffer-read-only nil)
	beg end)
    (when from-highlight-regexp
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (beginning-of-line)
	  (setq beg (point))
	  (setq end (progn (end-of-line) (point)))
	  (when (string-match from-highlight-regexp
			      (buffer-substring beg end))
	    (add-text-properties beg end `(face ,face)))
	  (forward-line 1)))
      (setq from-buffer-highlighted-p t))))

(defun from-update-buffer ()
  "Update buffer with any new messages."
  (interactive)
  (when (eq major-mode 'from-mode)
    (let ((headers (from-parse-mailspools)))
      (from-insert-buffer headers)
      (when from-buffer-highlighted-p
	(from-highlight-buffer)))))

(defun from-toggle-highlight ()
  "Toggle highlighting of buffer lines."
  (interactive)
  (when (eq major-mode 'from-mode)
    (let ((buffer-read-only nil))
      (if (not from-buffer-highlighted-p)
	  (from-highlight-buffer)
	(remove-text-properties (point-min) (point-max) '(face))
	(setq from-buffer-highlighted-p nil)))))

(defun from-quit ()
  "Exit using the function defined by `from-quit-command'."
  (interactive)
  (when (eq major-mode 'from-mode)
    (if (eq from-quit-command 'kill-buffer)
	(kill-buffer (current-buffer))
      (funcall from-quit-command))
    (when (and from-use-other-window
	       (/= (count-windows) 1))
      (delete-window))))

;;;###autoload
(defun from ()
  "Show you who your mail is from."
  (interactive)
  (let ((buffer (get-buffer-create "*From*"))
	(headers (from-parse-mailspools)))
    (if (not headers)
	(message "No messages found.")
      (if from-use-other-window
	  (pop-to-buffer buffer)
	(switch-to-buffer buffer))
      (from-insert-buffer headers)
      (from-highlight-buffer)
      (from-mode))))

(define-derived-mode from-mode fundamental-mode "From"
  "Major mode used in `from' buffers.

Commands available in `from-mode':
  '\\[from-update-buffer]' - Update buffer with any new messages.
  '\\[from-toggle-highlight]' - Toggle highlighting of buffer lines.
  '\\[from-quit]' - Exit using the function defined by `from-quit-command'."
  (define-key from-mode-map (kbd "g") 'from-update-buffer)
  (define-key from-mode-map (kbd "t") 'from-toggle-highlight)
  (define-key from-mode-map (kbd "q") 'from-quit)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(provide 'from)

;;; from.el ends here

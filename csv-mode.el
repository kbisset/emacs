;;; csv-mode.el --- major mode for editing comma-separated value files

;; Copyright (C) 2003, 2004 Francis J. Wright

;; Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; Time-stamp: <10 April 2004>
;; URL: http://centaur.maths.qmul.ac.uk/Emacs/
;; Version: $Id: csv-mode.el,v 1.9 2004/04/10 15:42:42 fjw Exp $
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is intended for use with GNU Emacs 21, and implements
;; the following commands to process CSV (comma-separated value)
;; records in the region in an Emacs buffer:
;;
;; `csv-sort-fields' and `csv-sort-numeric-fields' sort respectively
;; lexicographically and numerically on a specified field or column.
;; They are based closely on, and use, code in `sort.el' and the user
;; interfaces are identical to those for the standard commands
;; `sort-fields' and `sort-numeric-fields'.
;;
;; `csv-kill-fields' and `csv-yank-fields' respectively kill and yank
;; fields or columns, although they do not use the normal kill ring.

;; Note 1: In general CSV data, fields can be delimited by
;; double-quote characters (and must if they contain commas).  This
;; implementation supports quoted fields provided the quotes are
;; double-quotes that are IMMEDIATELY preceded or followed by a comma
;; or newline.  It also supports separators other than commas, but the
;; separator must be a single character other than the double-quote
;; character.  The separator character can be changed ONLY by
;; CUSTOMIZING the variable `csv-separator', e.g. via the command
;; `customize-variable'.

;; Note 2: See also csv.el by Ulf Jasper <ulf.jasper at web.de>.  It
;; provides functions for reading/parsing Comma Separated Value files
;; and is available at http://de.geocities.com/ulf_jasper/emacs.html
;; (and in the gnu.emacs.sources archives).

;;; Installation:

;; Put this file somewhere that Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it (recommended), and put this in your .emacs file:
;;
;; (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;; (autoload 'csv-mode "csv-mode"
;;   "Major mode for editing comma-separated value files." t)
;;
;; Additionally, you may want to include the following:
;;
;; (autoload 'csv-sort-fields "csv-mode"
;;   "Sort CSV data lexicographically by field." t)
;; (autoload 'csv-sort-numeric-fields "csv-mode"
;;   "Sort CSV data numerically by field." t)
;; (autoload 'csv-kill-fields "csv-mode" "Kill CSV data by field." t)
;; (autoload 'csv-yank-fields "csv-mode" "Yank CSV data by field." t)

;;; History:

;; Begun on 15 November 2003 to provide lexicographic sorting of
;; simple CSV data by field and released as csv.el.  Facilities to
;; kill multiple fields and customize separator added 9 April 2004.
;; Converted to a major mode and renamed csv-mode.el on 10 April 2004,
;; partly at the suggestion of Stefan Monnier <monnier at
;; IRO.UMontreal.CA> to avoid conflict with csv.el by Ulf Jasper.

;;; To do (maybe):

;; Align columns by padding with white space.
;; Convert comma-separated to space- or tab-separated.

;;; Code:

(defcustom csv-separator ","
  "CSV field separator -- must be a *single-character* string.
Default is a comma, i.e. \",\"."
  ;; Suggested by Eckhard Neber <neber@mwt.e-technik.uni-ulm.de>
  :group 'convenience
  :type 'string
  :set (lambda (variable value)
	 (if (/= (length value) 1) (error))
	 (custom-set-default variable value)
	 (setq csv-skip-regexp (concat "^\n" value))))

(defvar csv-skip-regexp nil
  "Regexp used to skip CSV fields.  Default is \"^\\n,\".
Set by customizing `csv-separator' -- do not set directly!")

(define-derived-mode csv-mode text-mode "CSV"
  "Major mode for editing comma-separated value files.
CSV mode is derived from `text-mode', and runs `text-mode-hook' before
running `csv-mode-hook'.  It turns `auto-fill-mode' off by default.
CSV mode binds the following keyboard keys:

\\{csv-mode-map}"
  (turn-off-auto-fill))

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(define-key csv-mode-map [(control ?c) (control ?y)] 'csv-yank-fields)
(define-key csv-mode-map [(control ?c) (control ?k)] 'csv-kill-fields)
(define-key csv-mode-map [(control ?c) (control ?n)] 'csv-sort-numeric-fields)
(define-key csv-mode-map [(control ?c) (control ?s)] 'csv-sort-fields)

(easy-menu-define
  csv-menu
  csv-mode-map
  "CSV major mode menu keymap"
  '("CSV"
    ["Sort By Field Lexicographically" csv-sort-fields :active t
     :help "Sort lines in region lexicographically by the ARGth field."]
    ["Sort By Field Numerically" csv-sort-numeric-fields :active t
     :help "Sort lines in region numerically by the ARGth field."]
    ["Kill Fields (Columns)" csv-kill-fields :active t
     :help "Kill specified fields of each line in the region."]
    ["Yank Fields (Columns)" csv-yank-fields :active t
     :help "Yank fields as the ARGth field of each line in the region."]
    "--"
    ["Customize Field Separator" (customize-variable 'csv-separator) :active t
     :help "Open a customization buffer to change the field separator."]
    ))

(require 'sort)

(defun csv-sort-fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by commas and numbered from 1 up.
Null fields are allowed anywhere and sort before any non-null field.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "*p\nr")
  (sort-fields-1
   field beg end
   ;; STARTKEYFUN moves from the start of the record to the start of
   ;; the key.  It may return either a non-nil value to be used as the
   ;; key, or else the key is the substring between the values of
   ;; point after STARTKEYFUN and ENDKEYFUN are called.  If
   ;; STARTKEYFUN is nil, the key starts at the beginning of the
   ;; record.
   (lambda () (csv-sort-skip-fields field) nil)
   ;; ENDKEYFUN moves from the start of the sort key to the end of the
   ;; sort key.  ENDKEYFUN may be nil if STARTKEYFUN returns a value
   ;; or if it would be the same as ENDRECFUN.
   (lambda () (skip-chars-forward csv-skip-regexp))
   ))

(defun csv-sort-numeric-fields (field beg end)
  "Sort lines in region numerically by the ARGth field of each line.
Fields are separated by commas and numbered from 1 up.
Null fields are allowed anywhere and sort as zeros.
Specified field must contain a number in each line of the region,
which may begin with \"0x\" or \"0\" for hexadecimal and octal values.
Otherwise, the number is interpreted according to sort-numeric-base.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort."
  (interactive "*p\nr")
  (sort-fields-1 field beg end
		 (lambda ()
		   (csv-sort-skip-fields field)
		   (let* ((case-fold-search t)
			  (base
			   (if (looking-at "\\(0x\\)[0-9a-f]\\|\\(0\\)[0-7]")
			       (cond ((match-beginning 1)
				      (goto-char (match-end 1))
				      16)
				     ((match-beginning 2)
				      (goto-char (match-end 2))
				      8)
				     (t nil)))))
		     (string-to-number (buffer-substring (point)
							 (save-excursion
							   (forward-sexp 1)
							   (point)))
				       (or base sort-numeric-base))))
		 nil))

(defsubst csv-forward-field ()
  "Skip forward over one field."
  (if (eq (following-char) ?\")
      (forward-sexp)
    (skip-chars-forward csv-skip-regexp)))

(defsubst csv-backward-field ()
  "Skip backward over one field."
  (if (eq (preceding-char) ?\")
      (backward-sexp)
    (skip-chars-backward csv-skip-regexp)))

(defun csv-sort-skip-fields (n &optional yank)
  "Position point at the beginning of field N on the current line.
Fields are separated by commas\; null terminal field allowed.
Assumes point is initially at the beginning of the line.
YANK non-nil allows N to be 1 greater than the number of fields."
  (if (> n 0)
      ;; Skip across N - 1 fields.
      (let ((i (1- n)))
	(while (> i 0)
	  (csv-forward-field)
	  (if (eolp)
	      (or (and yank (= i 1))
		  (error "Line has too few fields: %s"
			 (buffer-substring
			  (save-excursion (beginning-of-line) (point))
			  (save-excursion (end-of-line) (point)))))
	    (forward-char))		; skip separator
	  (setq i (1- i))))
    (end-of-line)
    ;; Skip back across -N - 1 fields.
    (let ((i (1- (- n))))
      (while (> i 0)
	(csv-backward-field)
	(if (bolp)
	    (error "Line has too few fields: %s"
		   (buffer-substring
		    (save-excursion (beginning-of-line) (point))
		    (save-excursion (end-of-line) (point)))))
	(backward-char)			; skip separator
	(setq i (1- i)))
      ;; Position at the front of the field
      ;; even if moving backwards.
      (csv-backward-field))))

(defvar csv-killed-fields nil
  "The last column of fields killed by `csv-kill-fields'.")

(defun csv-kill-fields (fields beg end)
  "Kill specified fields of each line in the region.
The fields are stored for use by `csv-yank-fields'.
Fields are separated by commas and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the right.
A prefix argument specifies a single field, otherwise prompt for field list.
Fields can be specified in any order but are saved in increasing index order.
When called non-interactively, FIELDS is a single field index or a
list of field indices, and BEG and END specify the region to process."
  (interactive "*P\nr")
  (if fields
      ;; Ensure that fields is a list:
      (or (consp fields)
	  (setq fields (list fields)))
    ;; Read fields interactively, ignoring non-integers:
    (setq fields
	  (mapcar
	   (lambda (x) (car (read-from-string x)))
	   (split-string
	    (read-string "Fields (sequence of integers): ")
	    "[^-+0-9]+"))))
  ;; Kill the field(s):
  (setq csv-killed-fields nil)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (cdr fields)
	  (csv-kill-many-columns fields)
	(csv-kill-one-column (car fields)))))
  (setq csv-killed-fields (nreverse csv-killed-fields)))

(defmacro csv-kill-one-field (field killed-fields)
  "Kill field with index FIELD in current line.
Save killed field by `cons'ing onto KILLED-FIELDS.
Assumes point is at beginning of line.
Called by `csv-kill-one-column' and `csv-kill-many-columns'."
  `(progn
     ;; Move to start of field to kill:
     (csv-sort-skip-fields ,field)
     ;; Kill to end of field (cf. `kill-region'):
     (setq ,killed-fields
	   (cons
	    (delete-and-extract-region
	     (point)
	     (progn (csv-forward-field) (point)))
	    ,killed-fields))
     (if (eolp) (delete-char -1)	; delete trailing comma at eol
       (delete-char 1))))		; or following comma otherwise

(defun csv-kill-one-column (field)
  "Kill field with index FIELD in all lines in (narrowed) buffer.
Save killed fields in `csv-killed-fields'.
Assumes point is at `point-min'.  Called by `csv-kill-fields'."
  (while (not (eobp))
    (csv-kill-one-field field csv-killed-fields)
    (forward-line)))

(defun csv-kill-many-columns (fields)
  "Kill several fields in all lines in (narrowed) buffer.
FIELDS is a list of field indices.
Save killed fields in increasing index order in `csv-killed-fields'.
Assumes point is at `point-min'.  Called by `csv-kill-fields'."
  (if (eolp) (error "First record is empty"))
  ;; Convert non-positive to positive field numbers:
  (let ((n 2) (f fields))
    (csv-forward-field)
    (while (not (eolp))
      (forward-char)			; skip comma
      (csv-forward-field)
      (setq n (1+ n)))		    ; n = (fields in first record) + 1
    (while f
      (cond ((zerop (car f)) (setcar f 1))
	    ((< (car f) 0) (setcar f (+ f n))))
      (setq f (cdr f))))
  ;; Kill from right to avoid miscounting:
  (setq fields (sort fields '>))
  (while (not (eobp))
    (let ((fields fields) killed-fields field)
      (while fields
	(setq field (car fields)
	      fields (cdr fields))
	(beginning-of-line)
	(csv-kill-one-field field killed-fields)
	(setq csv-killed-fields
	      (cons (mapconcat 'identity killed-fields csv-separator)
		    csv-killed-fields)))
      (forward-line))))

(defun csv-yank-fields (field beg end)
  "Yank fields as the ARGth field of each line in the region.
The fields yanked are those last killed by `csv-kill-fields'.
Fields are separated by commas and null fields are allowed anywhere.
Field indices increase from 1 on the left or decrease from -1 on the right.
A prefix argument specifies a single field, otherwise prompt for field index.
When called non-interactively, FIELD is a single field index,
and BEG and END specify the region to process."
  (interactive "*P\nr")
  (if field
      (setq field (prefix-numeric-value field))
    (while (not (integerp field))
      (setq field (eval-minibuffer "Field (integer): "))))
  (if (<= field 0) (setq field (1+ field)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((fields csv-killed-fields))
	(while (not (eobp))
	  ;; Yank at start of specified field if possible, otherwise
	  ;; yank at end of record:
	  (if (zerop field)
	      (end-of-line)
	    (csv-sort-skip-fields field 'yank))
	  (and (eolp) (insert csv-separator))
	  (when fields
	    (insert (car fields))
	    (setq fields (cdr fields)))
	  (or (eolp) (insert csv-separator))
	  (forward-line))))))

(provide 'csv-mode)

;;; csv-mode.el ends here

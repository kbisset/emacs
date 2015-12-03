;;; ediary.el --- edit emacs diary files

;; Copyright (C) 2001 Thomas Gehrlein <thomas.gehrlein@t-online.de>

;; Emacs Lisp Archive Entry
;; Filename: ediary.el
;; Version: 0.1
;; Keywords: diary calendar
;; Author: Thomas Gehrlein <thomas.gehrlein@t-online.de>
;; Maintainer: Thomas Gehrlein <thomas.gehrlein@t-online.de>
;; Description: edit emacs diary files
;; URL: No URL, I don't have a website.
;; Compatibility: Emacs20, Emacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;; Quick start

;; Installing: Put ediary somewhere in your load-path and add the following to
;; the end of your .emacs (removing the leading `;;').

;; (require 'ediary)

;; Find your diary file.  "C-x f ~/diary RET" or typing "s" in the calendar
;; buffer are good starting points.  Do "M-x ediary-mode RET" and try it out.
;; If you like it, put the following line at the beginning of your diary file
;; (remove the leading ";;"):

;; -*- mode: ediary -*-

;;; documentation

;; ediary provides font-lock and a couple of useful function for diary files.
;; These functions allow to copy, delete, and move entries.  They work on
;; ordinary entries (currently only the format created automatically by
;; calendar/diary) and diary-blocks.  They also work on time entries in the 24
;; hour hh:mm format.  Do "C-h m" for a list of the function and the
;; key-bindings.  The names of the functions should be self explaining.  Here
;; is a list of the mnemonics for the key-bindings:

;; e = entry
;; b = block
;; t = time
;; < = earlier ; move both dates of a block
;; > = later
;; + = longer  ; move the end of a block
;; - = shorter

;; If you have comment, suggestions, improvements, please send me an email.
;; I'm especially interested in how you like the user interface.  Are the
;; key-bindings easy to remember?  Would you like other/new/different
;; functions?


;;; Bugs:

;; No known bugs.  If you find a bug, tell me about it.

;; Tested with Emacs 20.7.1


;;; TODO
;; - Improve documentation.
;; - Do the key-bindings make sense?
;; - Do we need more/other functions?
;; - Add support for fancy diary buffer.
;; - Improve font-lock support.  Add more colors.  Define fonts.
;; - defcustom instead of defvar.
;; - autoload ediary-mode.


;;; Random Thoughts

;; WIBNI the user didn't have to remember all these key strokes?  How about more
;; generic commands like ediary-thing-at-point-later or -earlier or -shorter.
;; These could be bound to C->, C-<, C--.  (Is it ok to use these key-bindings?
;; Control + char is reserved for the user.)  TAB and shift-TAB could be bound
;; to ediary-next-date-or-time and ediary-previous-date-or-time.  Then one
;; could navigate in the buffer and change the dates/times under point.

;; ediary contains lots of hard coded regexps.  I'd like to use regexp
;; variables, and I'd like them to be more generic.  Is there a tutorial for
;; elisp regexps?

;; Should ediary understand all possible date formats for diary entries?  It
;; would be nice, of course, but it's a lot of work.  The format created by
;; `insert-diary-entry' should be sufficient.

;; What about XEmacs?  I don't know anything about XEmacs.  Does this code
;; work in XEmacs?  If not, what needs to be changed?


;;; History

;; 0.1


;;; Code:

;;; require what we need
(require 'calendar)

;;; variables for the user
(defvar ediary-really-delete-entries-p
  nil
  "*nil means: do not remove deleted entries from the `diary-file'.

Delete them by prepending `ediary-deleted-string´ instead.")

(defvar ediary-copy-modified-p
  t
  "*Non-nil means: when an entry is modified, the old version is kept.

'ediary-modified-string´ is prepended to the old entry, so that it is ignored
by diary.")

(defvar ediary-deleted-string
  "DELETED "
  "*String used to mark deleted entries.

The string is put at the beginning of the entry.  The entry will then be
ignored when the `diary-file' is parsed.")

(defvar ediary-modified-string
  "MODIFIED "
  "*String used to mark modified entries.

The string is put at the beginning of the entry.  The entry will then be
ignored when the `diary-file' is parsed.")


;;; internal variables
(defvar ediary-version "0.1"
  "Version number of ediary.")

;; does calendar provide something similar?
(defvar ediary-short-month-name-list
  (mapcar
   (lambda (arg) (substring arg 0 3))
   calendar-month-name-array)
  "List of month names, truncated to 3 letters.")

;;; utility-functions
;; moving in the buffer
(defun ediary-boe ()
  "Return position of beginning of entry."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (looking-at "[ \|\t]")	;whitespace
      (forward-line -1))
    (point)))

(defun ediary-eoe ()
  "Return position of end of entry."
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (eobp)
      (forward-char 1))
    (while
	(looking-at "[ \|\t]")
      (end-of-line)
      (unless (eobp) (forward-line 1)))
    (point)))


;;; functions for time and date handling

;; This would be much easier if all non-Europeans used the European calendar
;; style.  But I won't do any a.m./p.m. stuff (not yet, anyway)

;; Currently ediary understands several time/date formats (see below).
;; For each of them there are several functions to check if point is on such a
;; string, to read this string from the buffer, to parse the string to a
;; number, to convert a number back to the appropriate format, to increment
;; it.

;; The time/date formats are:
;; 7 Sep 2001
;; Sep 8, 2001
;; 12 31 2001
;; 31 12 2001
;; 13:00
;; 13:00-17:15


;;; handle diary entry formats
(defun ediary-point-on-entry-date-p ()
  "If point is on the date string for a diary entry, return that string.

Return nil otherwise.
Recognized formats are:
7 Sep 2001
Sep 8, 2001
Other formats may or may not be added later."
  ;; (if european-calendar-style
  ;;       (ediary-point-on-european-entry-date-p)
  ;;     (ediary-point-on-american-entry-date-p)))
  ;; us or, so we can easily add more
  (interactive)
  (or (ediary-point-on-european-entry-date-p)
      (ediary-point-on-american-entry-date-p)))

(defun ediary-point-on-european-entry-date-p ()
  "If point is on a European format date string, return that string.

Otherwise return nil.  Understands formats like \"7 Sep 2001\""
  (save-excursion
    (backward-word 3)			; get before the date string
    ;; FIXME: use regexp variable
    (re-search-forward "[0-9]+ [A-ZÄÖÜ][a-zäöü]+ [0-9]+"))
  (if (and (<= (match-beginning 0) (point))
	   (< (point) (match-end 0)))
      (match-string 0)			; return the string
    nil))				; or nil

(defun ediary-point-on-american-entry-date-p ()
  "If point is on an American format date string, return that string.

Otherwise return nil.  Understands formats like \"Sep 7, 2001\""
  (save-excursion
    (backward-word 3)			; get before the date string
    ;; FIXME: use regexp variable
    ;; Do we need umlauts?
    (re-search-forward "[A-ZÄÖÜ][a-zäöü]+ [0-9]+, [0-9]+"))
  (if (and (<= (match-beginning 0) (point))
	   (< (point) (match-end 0)))
      (match-string 0)			; return the string
    nil))

;; Do we need this?  ediary-point-on-*-date-p return the strings?
;; (defun ediary-read-entry-date-under-point ()
;;   "Read the date string of a diary entry point is on and return it.

;; Recognized formats are:
;; 7 Sep 2001
;; Sep 8, 2001
;; Other formats may or may not be added later."
;;   ;; error check
;;   (if (not (ediary-point-on-entry-date-p))
;;       (error "Point is not on the date of a diary entry")
;;     (
;;     ;; read the string
;;     ()))

(defun ediary-parse-entry-date (string)
  "Parse a date of a diary entry.  Return a list (m d yyyy).
Recognized formats for STRING are:
7 Sep 2001
Sep 8, 2001
Other formats may or may not be added later."
  ;; how about
  ;; (condition-case
  ;;     ((looking-at "1 Sep 2001") (parse-deutsches-datum)
  ;;      (looking-at "Sep 1, 2001") (parse-amerikanisches-datum)
  ;;      (looking-at "1324 1234 1234 1234") (parse-enterprise-sternzeit))
  ;;     )

  (let (month day year date)

    ;; prepare string
    (with-temp-buffer
      (insert-string string)
      (goto-char (point-min))
      ;; get rid of "&"
      (when (looking-at "&")
	(delete-char 1))
      ;; read year, month, day as strings
      ;; the first thing in the buffer could be day or month
      (re-search-forward "[a-zA-Z0-9]+")
      (if european-calendar-style
	  (setq day (match-string 0))
	(setq month (match-string 0)))
      ;; the next thing in the buffer could be month or day
      (re-search-forward "[a-zäöüA-ZÄÖÜ0-9]+")
      (if european-calendar-style
	  (setq month (match-string 0))
	(setq day (match-string 0)))
      ;; next thing should be the year
      (re-search-forward "[0-9]+")
      (setq year (match-string 0)))

    ;; convert month, day, year
    (setq month
	  (- 13
	     (length
	      (member month ediary-short-month-name-list)))
	  day (string-to-number day)
	  year (string-to-number year)
	  date (list month day year))

    ;; sanity check and return the result
    (if (calendar-date-is-legal-p date)
	date
      (error "Could not parse date string correctly"))))

(defun ediary-format-entry-date (date)
  "Format DATE to local style.

DATE is a list (m d yyyy).  This function calls `calender-date-string'."
  (calendar-date-string date
			t		; abbreviate month name
			t))		; no day name

(defun ediary-increment-entry-date (date number)
  "Increment DATE by NUMBER days and return the result in the same format.

DATE is a string, NUMBER is an integer."
  (ediary-format-entry-date
   (calendar-gregorian-from-absolute
    (+ (calendar-absolute-from-gregorian
	(ediary-parse-entry-date date))
       number))))



;;;handle diary block entries
(defun ediary-point-on-block-date-p ()
  "If point is on the date string for a diary block entry, return that string.

Return nil otherwise.
Understands but cannot distinguish European and non European formats:
31 12 2001
3 31 2001"
  (save-excursion
    (backward-word 3)			; get before the date string
    ;; FIXME: use regexp variable
    ;; could be improved: day can be 1 to 31, month 1 to 12
    (re-search-forward "[0-9]+ [0-9]+ [0-9]+"))
  (if (and (<= (match-beginning 0) (point))
	   (< (point) (match-end 0)))
      (match-string 0)			; return the string
    nil))

(defun ediary-point-on-block-dates-p ()
  "If point is on a 2 dates string for a diary block entry, return that string.

Return nil otherwise.
Understands but cannot distinguish European and non European formats:
28 12 2001 31 12 2001
3 28 2001 3 31 2001"
  (save-excursion
    (backward-word 6)			; get before the date string
    ;; FIXME: use regexp variable
    ;; could be improved: day can be 1 to 31, month 1 to 12
    (re-search-forward "[0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+"))
  (if (and (<= (match-beginning 0) (point))
	   (< (point) (match-end 0)))
      (match-string 0)			; return the string
    nil))

(defun ediary-parse-block-date (string)
  "Parse a date of a diary block entry.  Return a list (m d yyyy).
Recognized formats for STRING are:
31 12 2001 (m d yyyy)
1 31 2001 (d m yyyy)
Other formats may or may not be added later."

  (let (month day year date)

    ;; prepare string
    (with-temp-buffer
      (insert-string string)
      (goto-char (point-min))

      ;; read year, month, day as strings
      ;; the first thing in the buffer could be day or month
      (re-search-forward "[0-9]+")
      (if european-calendar-style
	  (setq day (match-string 0))
	(setq month (match-string 0)))
      ;; the next thing in the buffer could be month or day
      (re-search-forward "[0-9]+")
      (if european-calendar-style
	  (setq month (match-string 0))
	(setq day (match-string 0)))
      ;; next thing should be the year
      (re-search-forward "[0-9]+")
      (setq year (match-string 0)))

    ;; convert month, day, year
    (setq month (string-to-number month)
	  day (string-to-number day)
	  year (string-to-number year)
	  date (list month day year))

    ;; sanity check and return the result
    (if (calendar-date-is-legal-p date)
	date
      (error "Could not parse date string correctly"))))


(defun ediary-format-block-date (date)
  "Format DATE to a local style string for diary block entries.

DATE is a list (m d yyyy)."
  ;; Could this be done with `format-time-string'?
  (if european-calendar-style
      (concat (number-to-string (car (cdr date))) ; day
	      " "
	      (number-to-string (car date)); month
	      " "
	      (number-to-string (car (cdr (cdr date))))) ;year
    (concat (number-to-string (car date))
	    " "
	    (number-to-string (car (cdr date)))
	    " "
	    (number-to-string (car (cdr (cdr date)))))))

(defun ediary-increment-block-date (date number)
  "Increment DATE by NUMBER days and return the result in the same format.

DATE is a string, NUMBER is an integer."
  (ediary-format-block-date
   (calendar-gregorian-from-absolute
    (+ (calendar-absolute-from-gregorian
	(ediary-parse-block-date date))
       number))))


;;;handle time strings "12:45"
(defun ediary-point-on-time-p ()
  "If point is on a time string like \"12:45\", return that string.

Return nil otherwise."
  (save-excursion
    (backward-word 2)			; get before the string
    ;; FIXME: use regexp variable
    (re-search-forward "[0-2][0-9]:[0-5][0-9]"))
  (if (and (<= (match-beginning 0) (point))
	   (< (point) (match-end 0)))
      (match-string 0)			; return the string
    nil))

(defun ediary-point-on-times-p ()
  "If point is on a time period string like \"12:45-13:45\",return that string.

Return nil otherwise."
  (save-excursion
    (backward-word 4)			; get before the date string
    ;; FIXME: use regexp variable
    (re-search-forward "[0-2][0-9]:[0-5][0-9]-[0-2][0-9]:[0-5][0-9]"))
  (if (and (<= (match-beginning 0) (point))
	   (< (point) (match-end 0)))
      (match-string 0)			; return the string
    nil))

(defun ediary-parse-time (string)
  "Parse a time STRING like \"12:45\".  Return the number of minutes."

  (let (hour min)
    (with-temp-buffer
      (insert-string string)
      (goto-char (point-min))

      ;; read hour and min
      (re-search-forward "[0-9]+")
      (setq hour (match-string 0))
      (re-search-forward "[0-9]+")
      (setq min (match-string 0)))
    
    (+ (* 60 (string-to-number hour))
       (string-to-number min))))

(defun ediary-format-time (min)
  "Format MIN to a time string like \"12:45\".  Hours are zero padded."
  (let*
      ;; hh (since we're using ints, an int is returned)
      ((hours (int-to-string (/ min 60)))
       (hours (if (= (length hours) 1)	;pad a "0", if length = 1
		  (concat "0" hours)
		hours))
       (mins (int-to-string (mod min 60)))
       (mins (if (= (length mins) 1)	;pad a "0", if length = 1
		 (concat "0" mins)
	       mins)))
    (concat hours ":" mins)))

(defun ediary-increment-time (time mins)
  "Increment TIME by MINS minutes and return the result in the same format.

TIME is a string, MINS is an integer."
  ;; 23:50 + 70 mins is not 25:00 -> use (mod)
  (ediary-format-time
   (mod
    (+ (ediary-parse-time time)
       mins)
    1440)))				; (* 24 60)



;;; kill, modify, copy entries etc.
(defun ediary-kill-entry ()
  "Kill the diary-entry under point.

This is done by killing the entry, or by prepending `ediary-deleted-string',
depending on the value of `ediary-really-delete-entries-p', which see."
  (interactive)
  (if ediary-really-delete-entries-p
    (kill-region (ediary-boe) (ediary-eoe))
    (save-excursion
      (goto-char (ediary-boe))
      (insert ediary-deleted-string))))
    

(defun ediary-modify-entry ()
  "Prepend `ediary-modified-string' to the entry under point.
Make a copy of the entry and insert it in the buffer for editing.
This function does nothing when `ediary-copy-modified-p' is nil."
  (interactive)
  (if ediary-copy-modified-p
      (let ((entry (buffer-substring (ediary-boe) (ediary-eoe))))
	;; insert modified-string
	(goto-char (ediary-boe))
	(when (looking-at ediary-modified-string)
	  (error "Attempt to modify a modified entry"))
	(insert ediary-modified-string)
	;; copy the entry
	(goto-char (ediary-eoe))
	(insert entry)
	;; back to beginning of the copied entry
	(ediary-previous-entry))
    ;; do nothing
    (error "Can't do anything if ediary-copy-modified-p is nil")))


(defun ediary-copy-entry ()
  "Copy the entry under point."
  (interactive)
  (let ((entry (buffer-substring (ediary-boe) (ediary-eoe))))
    (goto-char (ediary-eoe))
    (insert "\n" entry)
    (goto-char (ediary-boe))))


(defun ediary-toggle-marking ()
  "Make marking entry non-marking or make non-marking entry marking.

Remove or add \"&\" as necessary."
  (interactive)
  (save-excursion
    (goto-char (ediary-boe))
    (if (looking-at "&")
	(delete-char 1)
      (insert "&"))))



;;; operate on time/date of entries

(defun ediary-entry-later (&optional no-of-days)
  "Move the date of the entry under point to a date that is NO-OF-DAYS later.

NO-OF-DAYS defaults to 1.
Works only with the date format created by `insert-diary-entry'.
Support for other formats may or may not be added later."
  (interactive "p")

  (save-excursion
    (goto-char (ediary-boe))

    ;; find date string
    (unless
	(re-search-forward
	 (if european-calendar-style
	     "[0-9]+ [A-ZÄÖÜ][a-zäöü][a-zäöü] [0-9]+"
	   "[A-Z][a-z][a-z] [0-9]+, [0-9]+")
	 (ediary-eoe)			; boundary of search
	 t)				; no error
      (error "Can't find diary entry date in this entry"))
	 
    (let* ((date-string (match-string 0))
	   (old-date
	    (ediary-parse-entry-date date-string))
	   (new-date
	    (ediary-format-entry-date
	     (calendar-gregorian-from-absolute
	      (+ (calendar-absolute-from-gregorian old-date)
		 (or no-of-days 1))))))

      ;; Need to search again, so we can use replace-match
      (goto-char (ediary-boe))
      (re-search-forward date-string)
      (replace-match new-date
		     t			; don't change case
		     t))))		; insert literally


(defun ediary-entry-earlier (&optional no-of-days)
  "Move the date of the entry under point to a date that is NO-OF-DAYS earlier.

NO-OF-DAYS defaults to 1.
Works only with the date format created by `insert-diary-entry'.
Support for other formats may or may not be added later."
  (interactive "p")
  (ediary-entry-later (or (- no-of-days)
			  (- 1))))


;; core function for diary-blocks
(defun ediary-change-block (delta-1 delta-2)
  "Change the dates of the `diary-block' point is.

Move the first date by DELTA-1 days.  Move the second date by DELTA-2 days.
DELTA-1 and DELTA-2 default to 1."
    (save-excursion
    (goto-char (ediary-boe))

    ;; find date string
    (unless
	(re-search-forward
	 "%%(diary-block"
	 (ediary-eoe)			; boundary of search
	 t)				; no error
      (error "Can't find diary block date in this entry"))

    ;; find the date strings, parse them, increment them
    (let* ((first-date-string
	    (progn (re-search-forward "[0-9]+ [0-9]+ [0-9]+")
		   (match-string 0)))
	   (second-date-string
	    (progn (re-search-forward "[0-9]+ [0-9]+ [0-9]+")
		   (match-string 0)))
	   (first-date
	    (ediary-parse-block-date first-date-string))
	   (second-date
	    (ediary-parse-block-date second-date-string))
	   (new-dates
	    (concat
	     (ediary-format-block-date
	      (calendar-gregorian-from-absolute
	      (+ (calendar-absolute-from-gregorian first-date)
		 (or delta-1 1))))
	     " "
	     (ediary-format-block-date
	      (calendar-gregorian-from-absolute
	       (+ (calendar-absolute-from-gregorian second-date)
		  (or delta-2 1)))))))

      ;; Need to search again, so we can use replace-match
      (goto-char (ediary-boe))
      (re-search-forward (concat first-date-string " " second-date-string))
      (replace-match new-dates
		     t			; don't change case
		     t))))


(defun ediary-block-later (&optional no-of-days)
  "Move the `diary-block' under point to a date that is NO-OF-DAYS later.

NO-OF-DAYS defaults to 1.
Moves the beginning and the end date of the block.  The duration of the block
remains unchanged."
  (interactive "p")
  (ediary-change-block
   (or no-of-days 1)
   (or no-of-days 1)))


(defun ediary-block-earlier (&optional no-of-days)
  "Move the `diary-block' under point to a date that is NO-OF-DAYS earlier.

NO-OF-DAYS defaults to 1.
Moves the beginning and the end date of the block.  The duration of the block
remains unchanged."
  (interactive "p")
  (ediary-change-block
   (or (- no-of-days) (- 1))
   (or (- no-of-days) (- 1))))
   

(defun ediary-block-longer (&optional no-of-days)
  "Make the `diary-block' under point NO-OF-DAYS longer.

Do this by moving the end date.  The start date remains unchanged.  NO-OF-DAYS
defaults to 1."
  (interactive "p")
  (ediary-change-block
   0
   (or no-of-days 1)))


(defun ediary-block-shorter (&optional no-of-days)
  "Make the `diary-block' under point NO-OF-DAYS shorter.

Do this by moving the end date.  The start date remains unchanged.  NO-OF-DAYS
defaults to 1."
  (interactive "p")
  (ediary-change-block
   0
   (or (- no-of-days) (- 1))))


(defun ediary-time-later (&optional mins)
  "Make the time of the diary entry under point MINS minutes later.

MINS defaults to 1."
  (interactive "p")
  
  (save-excursion
    (goto-char (ediary-boe))

    ;; find time string
    (unless
	(re-search-forward
	 "[0-9][0-9]:[0-9][0-9]"
	 (ediary-eoe)			; boundary of search
	 t)				; no error
      (error "Can't find time string in this entry"))
	 
    (let* ((time-string (match-string 0))
	   (new-time
	    (ediary-increment-time
	     time-string
	     (or mins 1))))

      ;; Need to search again, so we can use replace-match
      (goto-char (ediary-boe))
      (re-search-forward time-string)
      (replace-match new-time
		     t			; don't change case
		     t))))

(defun ediary-time-earlier (&optional mins)
  "Make the time of the diary entry under point MINS minutes earlier.

MINS defaults to 1."
  (interactive "p")

  (ediary-time-later
   (or (- mins) (- 1))))


;; core function for time-blocks
(defun ediary-change-time-block (delta-1 delta-2)
  "Change the beginning and end times of the entry point is.

Move the beginning time by DELTA-1 minutes.  Move the end time by DELTA-2
minutes.  DELTA-1 and DELTA-2 default to 1."
  (save-excursion
    (goto-char (ediary-boe))

    ;; find date string
    (unless
	(re-search-forward
	 "[0-9][0-9]:[0-9][0-9]-[0-9][0-9]:[0-9][0-9]"
	 (ediary-eoe)			; boundary of search
	 t)				; no error
      (error "Can't find time block in this entry"))

    ;; find the time strings, increment them
    (let* ((first-time-string
	    (progn (goto-char (ediary-boe))
		   (re-search-forward "[0-9][0-9]:[0-9][0-9]")
		   (match-string 0)))
	   (second-time-string
	    (progn (re-search-forward "[0-9][0-9]:[0-9][0-9]")
		   (match-string 0)))
	   (new-times
	    (concat
	     (ediary-increment-time
	      first-time-string
	      (or delta-1 1))
	     "-"
	     (ediary-increment-time
	      second-time-string
	      (or delta-2 1)))))
      
      ;; Need to search again, so we can use replace-match
      (goto-char (ediary-boe))
      (re-search-forward (concat first-time-string "-" second-time-string))
      (replace-match new-times
		     t			; don't change case
		     t))))


(defun ediary-time-block-longer (&optional mins)
  "Make the diary-time-block under point MINS minutes longer.

Do this by moving the end time.  The start time remains unchanged.  MINS
defaults to 1."
  (interactive "p")
  (ediary-change-time-block
   0
   (or mins 1)))


(defun ediary-time-block-shorter (&optional mins)
  "Make the diary-time-block under point MINS minutes shorter.

Do this by moving the end time.  The start time remains unchanged.  MINS
defaults to 1."
  (interactive "p")
  (ediary-change-time-block
   0
   (or (- mins) (- 1))))


(defun ediary-time-block-later (&optional mins)
  "Make the time-block of the entry under point MINS minutes later.

MINS defaults to 1.  Moves the start and the end time of the block.  The
duration of the block remains unchanged."
  (interactive "p")
  (ediary-change-time-block
   (or mins 1)
   (or mins 1)))


(defun ediary-time-block-earlier (&optional mins)
  "Make the time-block of the entry under point MINS minutes earlier.

MINS defaults to 1.  Moves the start and the end time of the block.  The
duration of the block remains unchanged."
  (interactive "p")
  (ediary-change-time-block
   (or (- mins) (- 1))
   (or (- mins) (- 1))))
   


;;; moving in the diary-file
(defun ediary-next-entry ()
  "Move to the beginning of the next entry."
  (interactive)
  (beginning-of-line)
  (forward-line 1)
  (while
      (looking-at " ")
    (forward-line 1)))


(defun ediary-previous-entry ()
  "Move to the beginning of the previous entry.

When point is in an entry, move to the beginning of this entry.  When point is
on the beginning of an entry, move to the beginning of the previous entry.
This is not really consistent with the name of the defun, but it feels more
\"natural\"."
  (interactive)
  (forward-line -1)
  (while
      (looking-at " ")
    (forward-line -1)))


;;; mode definitions

;;(see derived.el)

;; (define-derived-mode
;;   ediary-mode				; name of new mode
;;   text-mode				; derived from parent mode
;;   "ediary"				; string for mode line
;;   "Major for editing diary files")	; docstring

(defun ediary-mode ()
  "Major mode for editing Emacs diary files.

Special commands:
\\{ediary-mode-map}

Entering ediary mode calls the value of `text-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ediary-mode-map)
  (setq mode-name "Ediary"
	major-mode 'ediary-mode)
  (set (make-local-variable 'font-lock-defaults)
       '(ediary-font-lock-keywords t))
  ;; Every function to select a new major mode starts by calling this
  ;; function.  (From the doc-string):
  (run-hooks 'text-mode-hook))


;; stolen from latex.el
;; function and key bindings:

;; e = entry
;; b = block
;; t = time
;; < = earlier
;; > = later
;; + = longer
;; - = shorter





;; o ediary-under-point-later (find out what is under point and increment it.)
;; o ediary-under-point-earlier (C-c+ and C-c-)
(defvar ediary-mode-map
  (let ((map (copy-keymap text-mode-map)))
    
    (define-key map "\C-c\C-d" 'ediary-kill-entry)
    (define-key map "\M-k"     'ediary-kill-entry)
    (define-key map "\C-c\C-x" 'ediary-modify-entry)
    (define-key map "\C-c\C-u" 'ediary-toggle-marking) ; u = unmark, can't use t
    (define-key map "\C-c\C-c" 'ediary-copy-entry)

    ;; move in file
    (define-key map "\C-c\C-p" 'ediary-previous-entry)
    (define-key map "\M-a"     'ediary-previous-entry)
    (define-key map "\C-c\C-n" 'ediary-next-entry)
    (define-key map "\M-e"     'ediary-next-entry)

    ;; move entries
    (define-key map "\C-c\C-e+" 'ediary-entry-longer) ; entry -> block
    (define-key map "\C-c\C-e>" 'ediary-entry-later)
    (define-key map "\C-c\C-e<" 'ediary-entry-earlier)

    ;; move diary-block
    (define-key map "\C-c\C-b>" 'ediary-block-later)
    (define-key map "\C-c\C-b<" 'ediary-block-earlier)
    (define-key map "\C-c\C-b+" 'ediary-block-longer)
    (define-key map "\C-c\C-b-" 'ediary-block-shorter)

    ;; move time
    (define-key map "\C-c\C-t>" 'ediary-time-later)
    (define-key map "\C-c\C-t<" 'ediary-time-earlier)
    (define-key map "\C-c\C-t\C-b+" 'ediary-time-block-longer)
    (define-key map "\C-c\C-t\C-b-" 'ediary-time-block-shorter)
    (define-key map "\C-c\C-t\C-b>" 'ediary-time-block-later)
    (define-key map "\C-c\C-t\C-b<" 'ediary-time-block-earlier)

    map)
  "Keymap used in `ediary-mode'.")


;;; font-locking
;; keywords "diary-block" und so.
(defvar ediary-font-lock-keywords
  nil
  "Additional expressions to highlight in Ediary mode.")

(setq ediary-font-lock-keywords
      (list
       ;; FIXME: Clean this regexp mess.  Observe order of regexps!
       (list "&\%%\(diary-block [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+\)"
	     '(0 font-lock-function-name-face))
       (list "%%\(diary-block [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+\)"
	     '(0 font-lock-function-name-face))

       ;; diary daily entries (only European format)
       (list "&[0-9]+ [A-ZÄÖÜ][a-zäöü][a-zäöü] [0-9]+" '(0
						font-lock-function-name-face))
       (list "[0-9]+ [A-ZÄÖÜ][a-zäöü][a-zäöü] [0-9]+" '(0 font-lock-function-name-face))

       ;; diary daily entries (non European)
       (list "&[A-Z][a-z][a-z] [0-9]+, [0-9]+" '(0 font-lock-function-name-face))
       (list "[A-Z][a-z][a-z] [0-9]+, [0-9]+" '(0
						font-lock-function-name-face))

       ;; keywords
       (list "MODIFIED" '(0 font-lock-keyword-face))
       (list "DELETED" '(0 font-lock-keyword-face))
       
       ;; FIXME: add more keywords; add other diary-*-somethings
       ))


(provide 'ediary)

;;; ediary.el ends here

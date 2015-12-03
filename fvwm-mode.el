;;; FVWM-MODE.EL --- mode for editing fvwm's config files

;; Copyright (C) 1996 Linh Dang

;; Author: Linh Dang <linhd@nortel.ca>
;; Maintainer: Linh Dang <linhd@nortel.ca>
;; Created: 04 Mar 1996
;; Version: 1.0
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <linhd@nortel.ca>) or
;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;; MA 02139, USA.

;; LCD Archive Entry:
;; fvwm-mode|Linh Dang|<linhd@nortel.ca>|
;; mode for editing fvwm's (version 1.XX) config files|
;; $Date: 1996/05/29 14:41:14 $|$Revision: 1.10 $|~/packages/fvwm-mode.el.Z|

;;; Commentary:
;;;
;;; My first attempt  to create a major mode  for Emacs !!!
;;;
;;; This mode has been tested with GNU Emacs  19.30. It might or might
;;; not work with XEmacs. I don't use XEmacs  since my 486 is too slow
;;; and doesn't have enough memory to run XEmacs.
;;;
;;; Usage:
;;;
;;; in your .emacs :
;;;
;;; (autoload 'fvwm-mode "fvwm-mode"
;;;   "Mode fore editing fvwm's config files" t)
;;;
;;; (setq auto-mode-alist (append auto-mode-alist
;;;			   '(("\\.fvwmrc$" . fvwm-mode))))

;;; BUGS:
;;;
;;; Font-lock treats #rrggbb color directives as comments !
;;; 

;;; Change log:
;; $Log: fvwm-mode.el,v $
;; Revision 1.10  1996/05/29 14:41:14  linhd
;; add comments about XEmacs
;;
;; Revision 1.9  1996/05/29 14:37:18  linhd
;; fix indent when at eob
;; patch from Alastair Burt <burt@dfki.uni-kl.de> for XEmacs
;;
;; Revision 1.8  1996/05/06 20:53:57  linhd
;; fix bug in indent again
;;
;; Revision 1.7  1996/05/06 17:55:37  linhd
;; fix empty-line indentation
;;
;; Revision 1.6  1996/04/01 16:39:20  linhd
;; fix fvwm-mark-function
;;
;; Revision 1.5  1996/04/01 16:27:03  linhd
;; simpilified fvwm-indent-line
;;
;; Revision 1.4  1996/03/07 19:00:04  linhd
;; spell-checked
;;
;; Revision 1.3  1996/03/07  16:14:40  linhd
;; complete now
;;
;; Revision 1.2  1996/03/05  14:45:14  linhd
;; working version
;;
;; Revision 1.1  1996/03/04  15:14:48  linhd
;; Initial revision
;;

;;; Code:

(defconst fvwm-mode-version (substring "$Revision: 1.10 $" 11 -2)
  "$Id: fvwm-mode.el,v 1.10 1996/05/29 14:41:14 linhd Exp $

Report bugs to: Linh Dang <linhd@nortel.ca>")

(provide 'fvwm-mode)

;;
;; This is contributed by Alastair Burt <burt@dfki.uni-kl.de>
;; If you use XEmacs, uncomment it.
;; 
;(and window-system
;     (unless (find-face 'font-lock-reference-face)
;       (make-face 'font-lock-reference-face)
;       (unless (face-differs-from-default-p 'font-lock-reference-face)
;	 (copy-face 'italic 'font-lock-reference-face))))



(defvar fvwm-function-start-word "\\(Popup\\|Function\\)")
(defvar fvwm-function-start (concat "^" fvwm-function-start-word))
(defvar fvwm-function-end-word (concat "End" fvwm-function-start-word))
(defvar fvwm-function-end (concat "^" fvwm-function-end-word  "\\([ \t]*\n\\)?"))
(defvar fvwm-line-anchor (concat "\\(#\\|*\\)"
				   "\\|"
				   "\\(" fvwm-function-end-word "\\)"))

(defconst fvwm-command "[A-Z][a-z]+")

(defvar fvwm-mode-basic-indent 4
  "number of spaces for tab")

(defvar fvwm-mode-hook nil
  "Hook to be run when mode is entered")

(defvar fvwm-syntax-table
  (let ((table (copy-syntax-table)))
    (modify-syntax-entry ?\#	"<"	table)
    (modify-syntax-entry ?\n	">#"	table)
    (modify-syntax-entry ?\"	"\"\""	table)
    (modify-syntax-entry ?\-	"_"	table)
    (modify-syntax-entry ?/	"_"	table)
    table)
  "Syntax table in use in Fvwm mode")

(defvar fvwm-abbrev-table nil
  "Abbrev table used in Fvwm mode")
(define-abbrev-table 'fvwm-abbrev-table nil)


(defvar fvwm-font-lock-keywords
  '(
    ("^\\(Popup\\|Function\\)[ \t]+\\\"\\([^\"]+\\)\\\"[ \t]*$"
     (1 font-lock-type-face nil t)
     (2 font-lock-function-name-face t t))
    ("\\(^End\\(Popup\\|Function\\)\\)[ \t]*$" 1 font-lock-type-face)
    ("^\\*\\(\\sw+\\)" 1 font-lock-reference-face)
    ("^[A-Z][A-Za-z]+" . font-lock-keyword-face)
    )
  "default font-lock keywords")

(defvar fvwm-imenu-generic-expression
  '(("Sections"
     "^#+ \\(\\(\\sw\\|\\s_\\|\\s-\\)+\\) #+" 1)
    ("Popups"
     "^Popup[ \t]+\\\"\\([^\"]+\\)\\\"" 1)
    ("Functions"
     "^Function[ \t]+\\\"\\([^\"]+\\)\\\"" 1)
    ))

(defvar fvwm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t"		'fvwm-indent-line)
    (define-key map "\e\C-h"		'fvwm-mark-function)
    ;; (define-key map [C-kp-enter]	'fvwm-mark-function)
    (define-key map "\C-c\C-f"		'fvwm-insert-function)
    (define-key map "\C-c\C-p"		'fvwm-insert-popup)
    map))


(defun fvwm-find-indent-col ()
  "Find the indent column in Fvwm mode"
  (save-excursion
    (line-move -1)
    (beginning-of-line)
    (cond ((eq 0 (point)) 0)
	  ((looking-at "#") (fvwm-find-indent-col))
	  ((looking-at fvwm-function-start)
	   fvwm-mode-basic-indent)
	  (t (back-to-indentation) (current-column)))))

;(defun fvwm-indent-line ()
;  "Indent a line in Fvwm mode"
;  (interactive)
;  (let ((offset (- (point) (save-excursion
;			     (back-to-indentation)
;			     (point))))
;	previous)
;    (save-excursion
;      (beginning-of-line)
;      (delete-region (point)
;		     (progn (skip-chars-forward " \t")
;			    (point)))
;      (setq previous (if (looking-at fvwm-line-anchor)
;			 0
;		       (fvwm-find-indent-col)))
;      (indent-to previous))
;    (back-to-indentation)
;    (goto-char (+ (point) offset))))
(defun fvwm-indent-line ()
  "Indent a line in Fvwm mode"
  (interactive)
  (let ((orig-pos (point-marker))
	new-line previous)
    (save-excursion
      (back-to-indentation)
      (setq new-line (looking-at "[ \t]*\\(\n\\)?$")
	    previous (if (looking-at fvwm-line-anchor) 0
		       (fvwm-find-indent-col)))
      (beginning-of-line)
      (indent-to previous)
      (delete-region (point)
		     (progn (skip-chars-forward " \t")
			    (point))))
    (if new-line (end-of-line)
      (goto-char orig-pos))))


(defun fvwm-indent-comment ()
  "Indent a comment in Fvwm mode"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-region (point)
		   (progn (skip-chars-forward " \t")
			  (point)))))

(defun fvwm-mark-function ()
  "Mark a Function or a Popup, leave mark at the end and point at
the beginning of the block"
  (interactive)
  (let ((beg 0) (end 0) (orig (point)))
    (save-excursion
      (and (re-search-backward fvwm-function-start (point-min) t)
	   (setq beg (point)))
      (and (re-search-forward fvwm-function-end (point-max) t)
	   (> (point) orig)
	   (setq end (point))))
    (or
     (and (> beg 0)
	  (> end beg)
	  (prog1 t
	    (push-mark end nil t)
	    (goto-char beg)))
     (error "not in a valid function"))))

(defun fvwm-insert-block (type)
  (beginning-of-line)
  (push-mark)
  (insert type " \"")
  (let ((name-point (point)))
    (insert "\"\nNop\t\"\"\nEnd" type)
    (indent-region (region-beginning) (region-end) nil)
    name-point))

(defun fvwm-insert-function ()
  "Insert a Function skeleton"
  (interactive)
  (let ((name-point (fvwm-insert-block "Function")))
    (goto-char name-point)))


(defun fvwm-insert-popup ()
  "Insert a Popup skeleton"
  (interactive)
  (let ((name-point (fvwm-insert-block "Popup")))
    (goto-char name-point)))

	  

(defun fvwm-mode ()
  "Major mode for editing fvwm config files (.fvwmrc).
The mode correctly (I hope) indents `Function's and `Popup's. Support
for Font-lock and Imenu is also available.

\\[fvwm-indent-line] correctly indent the current line.
\\[fvwm-mark-function] mark a `Function' or a `Popup'
\\[fvwm-insert-function] insert a `Function' skeleton
\\[fvwm-insert-popup] insert a `Popup' skeleton"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table fvwm-syntax-table)
  (use-local-map fvwm-mode-map)
  (setq local-abbrev-table fvwm-abbrev-table)

  (make-local-variable	'indent-line-function)
  (make-local-variable	'comment-indent-function)
  (make-local-variable	'comment-start)
  (make-local-variable	'comment-start-skip)
  (make-local-variable	'font-lock-defaults)
  (make-local-variable	'require-final-newline)
  (make-local-variable	'imenu-generic-expression)
  (setq major-mode 'fvwm-mode
	mode-name "Fvwm"
	indent-line-function 'fvwm-indent-line
	comment-indent-function 'fvwm-indent-comment
	comment-start "# "
	comment-end ""
	comment-start-skip "#[ \t]*"
	font-lock-defaults '(fvwm-font-lock-keywords nil)
	require-final-newline t
	imenu-generic-expression fvwm-imenu-generic-expression
	)
  (run-hooks 'fvwm-mode-hook))


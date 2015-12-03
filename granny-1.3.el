;;; granny.el --- Colorful display of the `cvs annotate' command

;; Copyright (C) 1997 Martin Lorentzson.

;; Author:  Martin Lorentzson <Martin.Lorentzson@emw.ericsson.se>
;; Version: $Revision: 1.3 $
;; Date: $Date: 1997/02/26 17:41:59 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to kyle@cs.odu.edu) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.

;;; Commentary:

;; Running `granny' will display the result of the cvs command `cvs
;; annotate' using colors to enhance the age of the line. Red is new
;; (hot) and blue is old (cold).

;; This was inspired by Granny version 1.0 by J. Gabriel Foster
;; (gabe@sgrail.com)

;;; Installation:

;; (cond (window-system
;; 	  (autoload 'granny "granny"
;;     "Display the result of the cvs annotate command using colors.
;;   New lines are displayed in red, old in blue. To use granny, press
;;   \\[granny] when placed in a file controlled by CVS."
;; 	    t)))

;;; Usage:

;; When in a CVS controlled file, use M-x granny.

;;; Code:

(defvar granny-color-map '(( 26.3672 . "#FF0000")
			   ( 52.7344 . "#FF3800")
			   ( 79.1016 . "#FF7000")
			   (105.4688 . "#FFA800")
			   (131.8359 . "#FFE000")
			   (158.2031 . "#E7FF00")
			   (184.5703 . "#AFFF00")
			   (210.9375 . "#77FF00")
			   (237.3047 . "#3FFF00")
			   (263.6719 . "#07FF00")
			   (290.0391 . "#00FF31")
			   (316.4063 . "#00FF69")
			   (342.7734 . "#00FFA1")
			   (369.1406 . "#00FFD9")
			   (395.5078 . "#00EEFF")
			   (421.8750 . "#00B6FF")
			   (448.2422 . "#007EFF"))
  "*Association list for time and color. Time is given in 2**-16 secs.
Default is eighteen steps using a twenty day increment. ")

(defvar granny-very-old-color "#0046FF"
  "*Color for lines older than CAR of last cons in `granny-color-map'.")

(defvar granny-background "black"
  "*Background color.")

;;;###autoload
(defun granny()
  "Display the result of the cvs annotate command using colors.
New lines are displayed in red, old in blue. To use granny, press
\\[granny] when placed in a file controlled by CVS."
  (interactive)
  (message "granny...")
  (let ((temp-buffer-name (concat "*cvs annotate " (buffer-name) "*"))
	(temp-buffer-show-function 'granny-display))
    (with-output-to-temp-buffer temp-buffer-name
      (call-process "cvs" nil (get-buffer temp-buffer-name) nil
		    "annotate" (buffer-file-name))))
  (message "granny... done"))


(defun grannycar (threshold &rest args)
  "Test successive cars of ARGS against THRESHOLD.
Return the first cons which CAR is not less than THRESHOLD, nil otherwise"
  ;; If no list is exhausted,
  (if (and (not (memq 'nil args)) (< (car (car (car args))) threshold))
      ;; apply to CARs.
      (apply 'grannycar threshold
	     ;; Recurse for rest of elements.
	     (mapcar 'cdr args))
    ;; Return the proper result
    (car (car args))))

(defun granny-display (buffer)
  "Do the granny display."

  ;; We need a list of months and their corresponding numbers.
  (let* ((local-month-numbers 
	  '(("Jan" . 1) ("Feb" .  2) ("Mar" .  3) ("Apr" .  4)
	    ("May" . 5) ("Jun" .  6) ("Jul" .  7) ("Aug" .  8) 
	    ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))
	 ;; XEmacs use extents, GNU Emacs overlays.
	 (overlay-or-extent (if (string-match "XEmacs" emacs-version)
				(cons 'make-extent 'set-extent-property)
			      (cons 'make-overlay 'overlay-put)))
	 (make-overlay-or-extent (car overlay-or-extent))
	 (set-property-overlay-or-extent (cdr overlay-or-extent)))

    (set-buffer buffer)
    (display-buffer buffer)
    (while (re-search-forward 
	    "^[0-9]+\\(\.[0-9]+\\)*\\s-+(\\sw+\\s-+\\([0-9]+\\)-\\(\\sw+\\)-\\([0-9]+\\)): "
	    nil t)

      (let* ((point (point))
	     (foo (forward-line 1))
	     (overlay (apply make-overlay-or-extent point (point) nil))
	     (day (match-string 2))
             (month (cdr (assoc (match-string 3) local-month-numbers)))
             (year (concat "19" (match-string 4))) ; The millenium problem!!
	     (high (- (car (current-time))
		      (car (encode-time 0 0 0 
					(string-to-number day)
					month
					(string-to-number year)))))
	     (color (cond ((grannycar high granny-color-map))
			  ((cons nil granny-very-old-color))))
	     ;; substring from index 1 to remove any leading `#' in the name
	     (face-name (concat "granny-face-" (substring (cdr color) 1)))
	     ;; Make the face if not done.
	     (face (cond ((intern-soft face-name))
			 ((make-face (intern face-name))))))

	(set-face-background face granny-background)
	(set-face-foreground face (cdr color))
;	(set-face-foreground face granny-background)
;	(set-face-background face (cdr color))
	(apply set-property-overlay-or-extent overlay
	       'face face nil)))))

(provide 'granny)
;; granny ends here.

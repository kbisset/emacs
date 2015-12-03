;;; zone.el --- Emacs zones out

;;; Author: Victor Zandy <zandy@cs.wisc.edu>
;;; Created: June 6, 1998

;;; Commentary:

;;  Don't zone out in front of Emacs!  Try M-x zone.
;;  If it eventually irritates you, try M-x zone-leave-me-alone.

;;  Bored by the zone pyrotechnics?  Write your own!  Add it to
;;  `zone-programs'.

;;  WARNING: Not appropriate for Emacs sessions over modems or
;;  computers as slow as mine.

;;; Code:

(require 'timer)

(defvar zone-idle 600
  "*Seconds to idle before zoning out.")

;;; Vector of functions that zone out.  `zone' will execute one of
;;; these functions, randomly chosen.  The chosen function is invoked
;;; in the *zone* buffer, which contains the text of the selected
;;; window.  If the function loops, it *must* periodically check and
;;; halt if `input-pending-p' is t (because quitting is disabled when
;;; Emacs idle timers are run).
(defvar zone-programs
  (apply 'vector
	 '(
	   zone-pgm-jitter
	   zone-pgm-putz-with-case
	   zone-pgm-dissolve
	   zone-pgm-explode
	   zone-pgm-whack-chars
	   )))


(defun zone ()
  "Zone out, completely."
  (interactive)
  (and (timerp zone-timer) (cancel-timer zone-timer))
  (let ((outbuf (get-buffer-create "*zone*"))
	(text (buffer-substring (window-start) (window-end)))
	(wp (1+ (- (window-point (selected-window))
		   (window-start)))))
    (set-buffer outbuf)
    (setq mode-name "Zone")
    (erase-buffer)
    (insert text)
    (switch-to-buffer outbuf)
    (set-window-start (selected-window) (point-min))
    (set-window-point (selected-window) wp)
    (message "Zoning...")
    (sit-for 0 500)
    (let ((pgm (elt zone-programs (random (length zone-programs)))))
      (condition-case nil
	  (progn
	    (funcall pgm)
	    (message "Zoning...sorry"))
	(error 
	 (while (not (input-pending-p))
	   (message (format "Vic was zoning when he wrote %s..." pgm))
	   (sit-for 3)
	   (message "...here's hoping he didn't hose your buffer!")
	   (sit-for 3)))
	(quit (ding) (message "Zoning...sorry"))))
    (kill-buffer outbuf)
    (zone-when-idle zone-idle)))


;;;; Zone when idle, or not.

(defvar zone-timer nil
  "Timer that zone sets to triggle idle zoning out.  If t, zone won't
zone out.")

(defun zone-when-idle (secs)
  "Zone out when Emacs has been idle for SECS seconds."
  (interactive "nHow long before I start zoning (seconds): ")
  (or (<= secs 0)
      (eq zone-timer t)
      (timerp zone-timer)
      (setq zone-timer (run-with-idle-timer secs nil 'zone))))

(defun zone-leave-me-alone ()
  "Don't zone out when Emacs is idle."
  (interactive)
  (and (timerp zone-timer) (cancel-timer zone-timer))
  (setq zone-timer t)
  (message "I won't zone out any more"))


;;;; zone-pgm-jitter
	  
(defun zone-shift-up ()
  (let* ((b (point))
	 (e (progn
	      (end-of-line)
	      (if (looking-at "\n") (1+ (point)) (point))))
	 (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-max))
    (insert s)))

(defun zone-shift-down ()
  (goto-char (point-max))
  (forward-line -1)
  (beginning-of-line)
  (let* ((b (point))
	 (e (progn
	      (end-of-line)
	      (if (looking-at "\n") (1+ (point)) (point))))
	 (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-min))
    (insert s)))

(defun zone-shift-left ()
  (while (not (eobp))
    (or (eolp)
	(let ((c (following-char)))
	  (delete-char 1)
	  (end-of-line)
	  (insert c)))
    (forward-line 1)))

(defun zone-shift-right ()
  (while (not (eobp))
    (end-of-line)
    (or (bolp)
	(let ((c (preceding-char)))
	  (delete-backward-char 1)
	  (beginning-of-line)
	  (insert c)))
    (forward-line 1)))

(defun zone-pgm-jitter ()
  (let ((ops (apply 'vector
		    '(
		      zone-shift-left
		      zone-shift-left
		      zone-shift-left
		      zone-shift-left
		      zone-shift-right
		      zone-shift-down
		      zone-shift-down
		      zone-shift-down
		      zone-shift-down
		      zone-shift-down
		      zone-shift-up
		      ))))
    (goto-char (point-min))
    (while (not (input-pending-p))
      (funcall (elt ops (random (length ops))))
      (goto-char (point-min))
      (sit-for 0 10))))
  

;;;; zone-pgm-whack-chars

(defvar zone-wc-tbl
  (let ((tbl (make-string 128 ?x))
	(i 0))
    (while (< i 128)
      (aset tbl i i)
      (setq i (1+ i)))
    tbl))

(defun zone-pgm-whack-chars ()
  (let ((tbl (copy-sequence zone-wc-tbl)))
    (while (not (input-pending-p))
      (let ((i 48))
	(while (< i 122)
	  (aset tbl i (+ 48 (random (- 123 48))))
	  (setq i (1+ i)))
	(translate-region (point-min) (point-max) tbl)
	(sit-for 0 2)))))


;;;; zone-pgm-dissolve

(defun zone-remove-text ()
  (let ((working t))
    (while working
      (setq working nil)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "[^(){}\n\t ]")
	      (let ((n (random 5)))
		(if (not (= n 0))
		    (progn 
		      (setq working t)
		      (forward-char 1))
		  (delete-char 1)
		  (insert " ")))
	    (forward-char 1))))
      (sit-for 0 2))))

(defun zone-pgm-dissolve ()
  (zone-remove-text)
  (zone-pgm-jitter))


;;;; zone-pgm-explode

(defun zone-exploding-remove ()
  (let ((i 0))
    (while (< i 20)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "[^*\n\t ]")
	      (let ((n (random 5)))
		(if (not (= n 0))
		    (forward-char 1))
		  (insert " ")))
	    (forward-char 1)))
      (setq i (1+ i))
      (sit-for 0 2)))
  (zone-pgm-jitter))

(defun zone-pgm-explode ()
  (zone-exploding-remove)
  (zone-pgm-jitter))

;;; zone-pgm-putz-with-case

;;; Faster than `zone-pgm-putz-with-case', but not as good: all
;;; instances of the same letter have the same case, which produces a
;;; less interesting effect than you might imagine.
(defun zone-pgm-2nd-putz-with-case ()
  (let ((tbl (make-string 128 ?x))
	(i 0))
    (while (< i 128)
      (aset tbl i i)
      (setq i (1+ i)))
    (while (not (input-pending-p))
      (setq i ?a)
      (while (<= i ?z)
	(aset tbl i
	      (if (zerop (random 5))
		  (upcase i)
		(downcase i)))
	(setq i (+ i (1+ (random 5)))))
      (setq i ?A)
      (while (<= i ?z)
	(aset tbl i
	      (if (zerop (random 5))
		  (downcase i)
		(upcase i)))
	(setq i (+ i (1+ (random 5)))))
      (translate-region (point-min) (point-max) tbl)
      (sit-for 0 2))))
    
(defun zone-pgm-putz-with-case ()
  (goto-char (point-min))
  (while (not (input-pending-p))
    (let ((np (+ 2 (random 5)))
	  (pm (point-max)))
      (while (< np pm)
	(goto-char np)
	(insert (if (zerop (random 2))
		    (upcase (preceding-char))
		  (downcase (preceding-char))))
	(backward-char 2)
	(delete-char 1)
	(setq np (+ np (1+ (random 5))))))
    (goto-char (point-min))
    (sit-for 0 2)))


;;; zone.el ends here

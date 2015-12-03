;so I slapped together the thing below tonight.  It provides one
;command, `archie', that'll prompt for something to search for.  It'll
;connect to an archie server and then do the search itself
;asynchronously so as to not lock up Emacs while waiting for output
;from the server.  When the result arrives, it'll pop up a buffer with
;the results.  Kinda like `man'.

;I wouldn't be at all surprised if there are bugs in the code.  Drop me
;a note if you find any.

;I'll keep updated versions of this file as
;<URL:http://www.ifi.uio.no/~larsi/other/archie.el>.  

;It would be nice if the login process was asynchronous as well, but I
;couldn't be bothered.

;;; archie.el --- a mode for handling archie searches
;; Copyright (C) 1997 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: util, network

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The `M-x archie' command will connect to an archie server and
;; send a search request.  It'll then return control to the user, 
;; and a window with the search results will be popped up when 
;; the results have arrived.
;;
;; You can have many searches going to different servers, but only
;; one request to each server at a time.
;;
;; Put the following in your .emacs:
;;
;;  (autoload 'archie "archie" nil t)

;;; Code:

(defvar archie-default-server "archie.rutgers.edu"
  "Default server to query.")

(defvar archie-default-query-method 'substring
  "Default query method.
Legal values are `exact', `substring', `case-substring' and `regexp'.")

(defvar archie-server-alist 
  '(("archie.ans.net" "USA [NY]")
    ("archie.rutgers.edu" "USA [NJ]")
    ("archie.sura.net" "USA [MD]")
    ("archie.unl.edu" "USA [NE]")
    ("archie.mcgill.ca" "Canada")
    ("archie.funet.fi" "Finland/Mainland Europe")
    ("archie.luth.se" "Sweden")
    ("archie.au" "Australia")
    ("archie.doc.ic.ac.uk" "Great Britain/Ireland")
    ("archie.wide.ad.jp" "Japan")
    ("archie.ncu.edu.tw" "Taiwan"))
  "Alist of known archie servers.")

(defvar archie-date-format "%Y-%m-%d %H:%M:%S"
  "Format of date strings.")

;;; Internal variables

(defvar archie-buffer-alist nil)
(defvar archie-current-server nil)
(defvar archie-default-method nil)
(defvar archie-doing-search nil)
(defvar archie-previous-server nil)
(defvar archie-previous-method nil)

;;; Functions

(defun archie (string &optional server method)
  (interactive
   (list
    (read-string "Search string: ")
    (if current-prefix-arg
	(setq archie-previous-server
	      (completing-read
	       "Server: " archie-server-alist nil nil 
	       (cons (or archie-previous-server "") 0)))
      archie-default-server)
    (if current-prefix-arg
	(setq archie-previous-method
	      (completing-read "Match type: "
			       '(("regexp") ("substring") ("exact")
				 ("case-substring"))
			       nil nil (cons (or archie-previous-method "")
					     0)))
      archie-default-query-method)))
  (let ((server (or server archie-default-server))
	(method (or method archie-default-query-method)))
    (when (stringp method)
      (setq method (intern method)))
    (unless (archie-connection-opened-p server)
      (unless (archie-open-connection server)
	(error "Couldn't open a connection to %s" server)))
    (archie-send-query string server method)))

(defun archie-open-connection (server)
  (archie-delete-process-buffer server)
  (let (buffer process)
    (save-excursion
      (set-buffer (setq buffer (get-buffer-create
				(format " *archie %s*" server))))
      (set (make-local-variable 'after-change-functions) 
	   '(archie-after-change-function))
      (set (make-local-variable 'archie-current-server) server)
      (set (make-local-variable 'archie-doing-search) nil)
      (erase-buffer)
      (message "Logging in on %s..." server)
      (setq process (start-process "nntpd" buffer "telnet" "-8"))
      (when (and process
		 (memq (process-status process) '(run open)))
	(push (list server process buffer) archie-buffer-alist)
	(archie-wait-for ">")
	(erase-buffer)
	(process-send-string process (format "open %s\n" server))
	(archie-wait-for "ogin:")
	(erase-buffer)
	(process-send-string process "archie\n")
	(archie-send-command "unset pager")
	(archie-send-command "autologout 1")
	(archie-send-command "unset pager")
	(archie-send-command "set output_format machine")
	t))))

(defun archie-send-query (string server method)
  (save-excursion
    (set-buffer (archie-process-buffer server))
    (if archie-doing-search
	(error "Search already in progress")
      (archie-send-command
       (format "set search %s"
	       (cond ((eq method 'regexp)
		      "regex")
		     ((eq method 'substring)
		      "sub")
		     ((eq method 'case-substring)
		      "subcase")
		     (t
		      "exact"))))
      (archie-send-command (format "find %s" string))
      (message "Querying %s about %s (%s)..." server string method)
      (setq archie-doing-search t))))

(defun archie-send-command (string)
  (archie-wait-for "archie>")
  (erase-buffer)
  (process-send-string (current-buffer) (concat string "\n")))

(defmacro archie-delete-line (&optional n)
  `(delete-region (progn (beginning-of-line) (point))
		  (progn (forward-line ,(or n 1)) (point))))

(defmacro archie-gethash (string hashtable)
  `(symbol-value (intern-soft ,string ,hashtable)))

(defmacro archie-sethash (string value hashtable)
  `(set (intern ,string ,hashtable) ,value))

(defun archie-after-change-function (beg end len &optional force)
  (when (or force (and archie-doing-search
		       (> (- end 7) beg)
		       (equal "archie> " (buffer-substring (- end 8) end))))
    (save-excursion
      (setq archie-doing-search nil)
      (message "Making archie buffer...")
      (let ((hashtb (make-vector 1023 0))
	    host dir entry date info type file)
	(goto-char (point-min))
	(while (re-search-forward "\\([0-9A-Z]+\\) \\([^ ]+\\) \\([0-9]+\\) bytes \\(..........\\) \\(.*\\)$"
				  nil t)
	  (setq host (match-string 2)
		entry (archie-gethash host hashtb)
		info (list nil (match-string 1)
			   (match-string 3)
			   (match-string 4) (match-string 5)))
	  (setcar info (file-name-directory (nth 4 info)))
	  (archie-sethash host (cons info entry) hashtb))
	;; Now we have the information.
	(pop-to-buffer (format "*archie %s search results*" 
			       archie-current-server))
	(archie-mode)
	(let (buffer-read-only)
	  (erase-buffer)
	  (mapatoms
	   (lambda (sym)
	     (let ((host (symbol-name sym))
		   (data (sort (symbol-value sym)
			       (lambda (s1 s2)
				 (string< (car s1) (car s2))))))
	       (archie-put 
		(point)
		(prog2
		    (insert host)
		    (point)
		  (insert "\n"))
		(archie-file host))
	       (while (setq entry (pop data))
		 (unless (equal (car entry) dir)
		   (insert "\n    Location: ")
		   (archie-put
		    (point)
		    (prog2
			(insert (setq dir (car entry)))
			(point)
		      (insert "\n"))
		    (archie-file host dir)))
		 (setq type (aref (nth 2 entry) 0))
		 (insert "      " (cond 
				   ((= type ?-)
				    "FILE      ")
				   ((= type ?l)
				    "LINK      ")
				   (t
				    "DIRECTORY "))
			 (nth 3 entry) " "
			 (format "%7s bytes" (nth 2 entry)) " "
			 (format
			  "%20s  "
			  (format-time-string archie-date-format
					      (archie-date (nth 1 entry)))))
		 (setq file (file-name-nondirectory (nth 4 entry)))
		 (archie-put
		  (point)
		  (prog2
		      (insert file)
		      (point)
		    (insert "\n"))
		  (archie-file host dir file)))
	       (setq dir nil)
	       (insert "\n")))
	   hashtb))
	(goto-char (point-min))
	(message "Making archie buffer...done")))))

(defun archie-date (date)
  (encode-time
   (string-to-number (substring date 12 14))
   (string-to-number (substring date 10 12))
   (string-to-number (substring date 8 10))
   (string-to-number (substring date 6 8))
   (string-to-number (substring date 4 6))
   (string-to-number (substring date 0 4))
   (string-to-number (substring date 14))))

(defun archie-file (host &optional dir file)
  (concat "/ftp@" host ":" (or dir "") (or file "")))
	
(defun archie-connection-opened-p (server)
  (let ((process (archie-process server)))
    (and process
	 (memq (process-status process) '(run open)))))

(defun archie-process (server)
  (nth 1 (assoc server archie-buffer-alist)))

(defun archie-process-buffer (server)
  (nth 2 (assoc server archie-buffer-alist)))

(defun archie-delete-process-buffer (server)
  (let ((entry (assoc server archie-buffer-alist)))
    (when entry
      (when (buffer-live-p (nth 2 entry))
	(kill-buffer (nth 2 entry)))
      (setq archie-buffer-alist (delq entry archie-buffer-alist)))))

(defun archie-put (beg end file)
  (put-text-property beg end 'archie-file file)
  (put-text-property beg end 'mouse-face 'highlight))

(defun archie-wait-for (regexp)
  "Wait until string arrives in the buffer."
  (let ((buf (current-buffer)))
    (goto-char (point-min))
    (while (not (re-search-forward regexp nil t))
      (accept-process-output (archie-process archie-current-server))
      (set-buffer buf)
      (goto-char (point-min)))))

;;;
;;; Archie mode
;;;

(defvar archie-mode-hook nil
  "Hook run in Archie mode buffers.")

(defvar archie-mode-map nil)
(unless archie-mode-map
  (setq archie-mode-map (make-sparse-keymap))
  (define-key archie-mode-map "\r" 'archie-fetch)
  (define-key archie-mode-map [mouse-2] 'archie-push))
  
(defun archie-make-menu-bar ()
  (unless (boundp 'archie-menu)
    (easy-menu-define
     archie-menu archie-mode-map ""
     '("Archie"
       ["Fetch" archie-fetch t]))))

(defun archie-mode ()
  "Major mode for displaying results from archie searches."
  (interactive)
  (buffer-disable-undo)
  (archie-make-menu-bar)
  (kill-all-local-variables)
  (setq major-mode 'archie-mode)
  (setq mode-name "Archie")
  (setq mode-line-process nil)
  (use-local-map archie-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'archie-mode-hook))

(defun archie-fetch (file)
  "Fetch the file under point."
  (interactive (list (get-text-property (point) 'archie-file)))
  (when file
    (find-file file)))

(defun archie-push (e)
  "Fetch the file under the mouse pointer."
  (interactive "e")
  (mouse-set-point e)
  (archie-fetch (get-text-property (point) 'archie-file)))

(provide 'archie)

;;; archie.el ends here

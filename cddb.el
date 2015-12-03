;;; cddb.el --- CD DataBase interface
;; Author: William M. Perry <wmperry@aventail.com>
;; Created: October 29, 1998
;; Version: 0.1
;; Keywords: multimedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1998 Free Software Foundation, Inc.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'custom)

(defconst cddb-version "0.2"
  "Version # of CDDB package.")

(defgroup cddb nil
  "CDDB - Compact Disc DataBase interface."
  :group 'hypermedia
  :prefix "cddb-")

(defcustom cddb-cdparanoia-program "cdparanoia"
  "*Where to find the cdparanoia program."
  :type 'string
  :group 'cddb)

(defcustom cddb-galette-program "galette"
  "*Where to find the galette program"
  :type 'string
  :group 'cddb)

(defcustom cddb-cdda2wav-program "cdda2wav"
  "*Where to find the cdda2wav program."
  :type 'string
  :group 'cddb)

(defcustom cddb-cdda2wav-arguments '("-N" "-t" "1" "-d" "1")
  "*Arguments to pass to cdda2wav to get CD track information."
  :type '(repeat string)
  :group 'cddb)

(defcustom cddb-cd-query-function nil
  "*Function to call to get CD track information for a device."
  :type '(radio
	  (const :tag "Guess" :value nil)
	  (const :tag "CD Paranoia" :value cddb-cdparanoia-get-info)
	  (const :tag "CDDA2WAV" :value cddb-cdda2wav-get-info)
	  (const :tag "Galette" :value cddb-galette-get-info)
	  (function :tag "Other"))
  :group 'cddb)
  
(defcustom cddb-hosts '((local "/opt/kde/share/apps/kscd/cddb/")
			(local "/usr/local/share/apps/kscd/cddb/")
			(local "/usr/share/apps/kscd/cddb/")
			(cddb "cddb.moonsoft.com" 8880)
			(cddb "cddb.sonic.net" 888)
			(cddb "sunsite.unc.edu" 8880)
			(cddb "www.cddb.com" 8880)
			(cddb "cddb.netads.com" 8880)
			(cddb "cddb.kagomi.com" 8880)
			(cddb "cddb.celestial.com" 888)
			(http "cddb.sonic.net" 80 "~cddb/cddb.cgi"))
  "*A list of CDDB database locations to search.
Entries in this list are themselves lists that specify the server
location and access method.  The lists can be:

 (local \"path\")              ;; Local database
 (cddb \"host\" port)          ;; Direct socket connection to remote server
 (http \"host\" \"path\" port) ;; Server accessible via HTTP
"
  :type '(repeat (choice
		  (list :tag "Local database"
			(const :format "" :value local)
			(directory :tag "Directory"))
		  (list :tag "CDDB Server"
			(const :format "" :value cddb)
			(string :tag "Host")
			(integer :tag "Port"))
		  (list :tag "HTTP Server"
			(const :format "" :value http)
			(string :tag "Host")
			(integer :tag "Port")
			(string :tag "URL Path"))
		  (sexp :tag "Other")))
  :group 'cddb)

(defvar cddb-possible-probe-programs
  '((cddb-cdparanoia-program . cddb-cdparanoia-get-info)
    (cddb-cdda2wav-program . cddb-cdda2wav-get-info)
    (cddb-galette-program . cddb-galette-get-info))
  "*Possible programs to probe for CD characteristics.
This is used if you have not explicitly set `cddb-cd-query-function'.

It is an assoc list.  The car of each entry is a symbol name that
holds the program name.  The cdr of each entry is a function name that
should be used as the value of `cddb-cd-query-function' if the program
is found.")

(defconst cddb-discinfo-response-regexp
  "\\(\\w+\\)\\s-+\\(........\\)\\s-\\(.*\\)"

  "Regular expression to split apart disc information responses.
Must have three match items:
#1 - The category
#2 - The discid
#3 - The title of the CD
")

(defsubst cddb-sum-digit (i)
  (let ((sum 0))
    (mapc (lambda (char)
	    (setq sum (+ sum (string-to-int (char-to-string char)))))
	  (int-to-string i))
    sum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various ways to extract the information we need to do CDDB queries using
;;; external programs.
;;;
;;; Please feel free to add more!  Just please email them to the maintainer
;;; so other people can enjoy them as well.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cddb-cdparanoia-get-info (&optional device)
  "Extractinformation about a cd using cdparanoia.
Returns a list of (DISCID TRACKS LENGTH TRACKINFO)

DISCID - the CDDB discid of the cd
TRACKS - the # of tracks on the cd
LENGTH - the total running time of the cd
TRACKINFO - a list of the frame offsets of each track
"
  (save-excursion
    (set-buffer (get-buffer-create " *cdparanoia*"))
    (erase-buffer)
    (let ((exit-status (if device
			   (call-process cddb-cdparanoia-program nil t nil "-d" device "-Q")
			 (call-process cddb-cdparanoia-program nil t nil "-Q")))
	  (track-info nil)
	  (track-time-info nil)
	  (sums 0)
	  (tracks 0)
	  (length 0)
	  (last-offset 0))
      (if (/= exit-status 0)
	  (error "Could not get execute `%s'" cddb-cdparanoia-program))
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]+\\)[^0-9]+[0-9][0-9]:[0-9][0-9]\\.[0-9][0-9].\\s-+\\([0-9]+\\)" nil t)
	(push (string-to-int (match-string 2)) track-info)
	(setq last-offset (+ (string-to-int (match-string 1)) (string-to-int (match-string 2)))))
      (setq track-info (reverse track-info)
	    tracks (length track-info)
	    track-time-info (mapcar (lambda (offset) (+ 2 (/ offset 75))) track-info)
	    length (- (+ 2 (/ last-offset 75)) (car track-time-info))
	    sums (apply (quote +) (mapcar (quote cddb-sum-digit) track-time-info)))
      (list (format "%08x" (logior (lsh (% sums 255) 24)
				   (lsh length 8)
				   tracks))
	    tracks
	    length
	    track-info))))    

(defun cddb-cdda2wav-get-info (&optional device)
  "Extractinformation about a cd using cdda2wav.
Returns a list of (DISCID TRACKS LENGTH TRACKINFO)

DISCID - the CDDB discid of the cd
TRACKS - the # of tracks on the cd
LENGTH - the total running time of the cd
TRACKINFO - a list of the frame offsets of each track
"

  (save-excursion
    (set-buffer (get-buffer-create " *cdda2wav*"))
    (erase-buffer)
    (let* ((exit-status (if device
			    (apply 'call-process
				   cddb-cdda2wav-program nil t nil 
				   "-D" device
				   cddb-cdda2wav-arguments)
			  (apply 'call-process
				 cddb-cdda2wav-program nil t nil
				 cddb-cdda2wav-arguments)))
	  (track-info nil)
	  (track-time-info nil)
	  (sums 0)
	  (tracks 0)
	  (length 0)
	  (last-offset 0))
      (if (/= exit-status 0)
	  (error "Could not get execute `%s'" cddb-cdda2wav-program))
      (goto-char (point-min))
      (if (not (re-search-forward "starting sectors.*" nil t))
	  (error "Could not understand output of `%s'" cddb-cdda2wav-program))
      (delete-region (point-min) (match-end 0))
      (while (re-search-forward "\\s-*\\([^\(]+\\)\(\\s-*\\([0-9]+\\)\),?" nil t)
	(push (string-to-int (match-string 2)) track-info))
      (setq last-offset (pop track-info))
      (setq track-info (reverse track-info)
	    tracks (length track-info)
	    track-time-info (mapcar (lambda (offset) (+ 2 (/ offset 75))) track-info)
	    length (- (+ 2 (/ last-offset 75)) (car track-time-info))
	    sums (apply (quote +) (mapcar (quote cddb-sum-digit) track-time-info)))
      (list (format "%08x" (logior (lsh (% sums 255) 24)
				   (lsh length 8)
				   tracks))
	    tracks
	    length
	    track-info))))    

(defun cddb-galette-get-info (&optional device)
  "Extract information about a CD using galette.
Returns a list (DISKCD TRACKS LENGTH TRACKINFO)

DISCID - the CDDB discid of the cd
TRACKS - the # of tracks on the cd
LENGTH - the total running time of the cd (in seconds)
TRACKINFO - a list of the frame offsets of each track
"

  (save-excursion
    (set-buffer (get-buffer-create " *galette*"))
    (erase-buffer)
    (let ((exit-status (if device
			   (call-process cddb-galette-program nil t nil "-d" device "-i")
			 (call-process cddb-galette-program nil t nil "-i")))
	  (track-info nil)
	  (length 0)
	  (num-tracks 0)
	  (discid nil))
      (if (/= exit-status 0)
	  (error "Could not execute `%s'" cddb-galette-program))
      (goto-char (point-min))
      
      ;; Get the frame offsets
      (while (re-search-forward "^CDROM: |\\s *[0-9]+ | AUDIO |\\s *\\([0-9]+\\) |" nil t)
	(push (string-to-int (match-string 1)) track-info)
	(setq num-tracks (1+ num-tracks)))
      (setq track-info (reverse track-info))
      
      ;; Get the disk length.  Easier to divide frames by 75 than to parse
      ;; mm:ss, so grab the frames
      (re-search-forward "^CDROM: | TOTAL |\\s +|\\s *\\([0-9]+\\) |" nil t)
      (setq length (/ (string-to-int (match-string 1)) 75))

      (message (match-string 1))
      
      ;; Get the disk ID
      (re-search-forward "^CDROM: disk id = \\([a-f0-9]+\\)" nil t)
      (setq discid (match-string 1))

      (list discid num-tracks length track-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XMCD file parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst cddb-parse-valid-p ()
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \r\n\t")
    (looking-at "# xmcd")))

(defun cddb-parse-get-item (name)
  (goto-char (point-min))
  (let ((regexp (format "^\\s-*%s=\\(.*\\)" name))
	(value nil))
    (while (re-search-forward regexp nil t)
      (setq value (concat value (match-string 1))))
    (and (/= 0 (length value)) value)))

(defun cddb-parse-info (&optional buffer)
  "Parses a XMCD format database record.
Returns a list of (TITLE ARTIST TRACKS TRACKALIST).

TITLE is the title of the disc
ARTIST is the artist
TRACKS is the # of tracks on the disc
TRACKALIST is an assoc list of (TITLE . EXTENDEDINFO) for each track.
"
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (if (cddb-parse-valid-p)
	(let ((track 0)
	      (done nil)
	      (tracks nil)
	      (cur-title nil)
	      (cur-extended nil)
	      (title (cddb-parse-get-item "DTITLE"))
	      (artist nil))
	  (while (not done)
	    (setq cur-title (cddb-parse-get-item (format "TTITLE%d" track))
		  cur-extended (cddb-parse-get-item (format "EXTT%d" track)))
	    (if (not (or cur-title cur-extended))
		(setq done t)
	      (if (and (not cur-extended)
		       (string-match "\\(.*\\) / \\(.*\\)" cur-title))
		  (setq cur-extended (match-string 1 cur-title)
			cur-title (match-string 2 cur-title)))
	      (push (cons cur-title cur-extended) tracks)
	      (setq track (1+ track))))
	  (if (string-match "\\(.*\\) / \\(.*\\)" title)
	      (setq artist (match-string 1 title)
		    title (match-string 2 title)))
	  (list title artist
		(length tracks)
		(reverse tracks))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cddb-local-search-internal (discid dir)
  ;; We can't use the 'files-only' argument to directory-files, since
  ;; that is a XEmacs-ism.
  (let ((subdirs (mapcar (lambda (d)
			   (and (file-directory-p d)
				(not (string-match "/\\.$" d))
				(not (string-match "/\\.\\.$" d))
				d))
			 (directory-files dir t)))
	(subdir nil)
	(matches nil)
	(entry nil))
    (setq subdirs (delete nil subdirs))
    (if (file-exists-p (expand-file-name discid dir))
	(push (expand-file-name discid dir) matches))
    (while subdirs
      (setq subdir (pop subdirs)
	    entry (cddb-local-search-internal discid subdir))
      (if entry
	  (setq matches (append matches entry))))
    matches))

(defun cddb-local-search (info dir)
  (if (not (file-exists-p dir))
      nil
    (message "Searching locally: %s" dir)
    (let ((matches (cddb-local-search-internal (car info) dir))
	  (discid nil)
	  (filename nil)
	  (title nil))
      (if (not matches)
	  nil
	(save-excursion
	  (set-buffer (get-buffer-create " *cddb*"))
	  (while matches
	    (erase-buffer)
	    (insert-file-contents (car matches))
	    (push (car matches) filename)
	    (push (cddb-parse-get-item "DISCID") discid)
	    (push (cddb-parse-get-item "DTITLE") title)
	    (pop matches)))

	(if (= (length discid) 1)
	    (setq discid (car discid)
		  title (car title)
		  filename (car filename)))

	(if (listp discid)
	    (let ((choices nil)
		  (completion-ignore-case t))
	      (while title
		(push (cons (pop title) (cons (pop filename) (pop discid))) choices))
	      (setq title (completing-read "Multiple matches, choose: " choices nil t))
	      (if (not title)
		  (error "Must choose an item from the list."))
	      (setq filename (car (car (assoc title choices)))
		    discid (cdr (car (assoc title choices))))))
	(save-excursion
	  (set-buffer (get-buffer-create " *cddb*"))
	  (erase-buffer)
	  (insert-file-contents filename)
	  (buffer-string))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remote searching via the CDDB protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cddb-build-query-string (info)
  (format "cddb query %s %d %s %d\r\n"
	  (nth 0 info)
	  (nth 1 info)
	  (mapconcat 'int-to-string (nth 3 info) " ")
	  (nth 2 info)))

(defsubst cddb-process-live-p (process)
  (if process (memq (process-status process) '(run open)) t))
	
(defsubst cddb-get-multiple-data (process)
  (goto-char (point-min))
  (while (not (re-search-forward "^\\.\r?$" nil t))
    (if (not (cddb-process-live-p process))
	(error "CDDB Connection unexpectedly closed."))
    (if process
	(accept-process-output process)))
  (replace-match "")
  (prog1
      (buffer-substring (point-min) (match-beginning 0))
    (delete-region (point-min) (match-beginning 0))))

(defun cddb-get-response (process &optional error-string)
  "Handles getting a response from a CDDB server.
Return value is a list of (CODE DATA EXTENDEDDATA)

CODE is the numerical response code
DATA is the single-line textual response from the server
EXTENDEDDATA is the extra data from the server (for x1x responses)
"
  (let ((response-code nil)
	(response-data nil)
	(extended-data nil)
	(major-code nil)
	(minor-code nil))
    (goto-char (point-min))
    (while (not (re-search-forward "^\\([0-9]+\\)\\s-+\\([^\r\n]*\\)\r*\n" nil t))
      (if (not (cddb-process-live-p process))
	  (error (or error-string "Error: %s") "Connection unexpectedly closed."))
      (if process
	  (accept-process-output process))
      (goto-char (point-min)))

    (setq response-code (string-to-int (match-string 1))
	  response-data (match-string 2)
	  major-code (/ response-code 100)
	  minor-code (/ (- (% response-code 100) (% (% response-code 100) 10)) 10))
    (replace-match "")
    (case major-code
      (1 nil)				; Informative message
      (2 nil)				; Command OK
      (3 nil)				; Command OK so far, continue
      (4				; Command Ok, but cannot be performed
       (error (or error-string "Cannot perform command: %s") response-data))
      (5				; Command unimplemented, incorrect, or program error
       (error (or error-string "Error: %s") response-data))
      (otherwise			; Unknown major response code!
       (error "Unknown major response code: %d" response-code)))

    (case minor-code
      (0 nil)				; Ready for further commands
      (1				; More server-to-client output follows
       (setq extended-data (cddb-get-multiple-data process)))
      (2 nil)				; More client-to-server input follows
      (3 nil)				; Connection will close
      (otherwise nil))			; Unknown minor code is ok?

    (list response-code response-data extended-data)))

(defun cddb-response-parse (response)
  "Parses a CDDB response.
Returns a list of (CATEGORY DISCID TITLE)."
  (case (car response)
    (200
     ;; Found exact match
     (if (string-match cddb-discinfo-response-regexp (nth 1 response))
	 (list (match-string 1 (nth 1 response))
	       (match-string 2 (nth 1 response))
	       (match-string 3 (nth 1 response)))))
    (202
     ;; No matches found
     nil
     )
    ((210 211)
     ;; 210 = Found multiple exact matches, list follows
     ;; 211 = Found inexact matches, list follows
     (let (categories discids titles matches)
       (setq matches (split-string (nth 2 response) "\r?\n")
	     matches (delete "" matches))
       (while matches
	 (if (string-match cddb-discinfo-response-regexp (car matches))
	     (progn
	       (push (match-string 1 (car matches)) categories)
	       (push (match-string 2 (car matches)) discids)
	       (push (match-string 3 (car matches)) titles))
	   (error "Can not understand query response: %s" (car matches)))
	 (pop matches))

       (if categories
	   (let ((choices nil)
		 (choice nil)
		 (completion-ignore-case t))
	     (while titles
	       (push (list (pop titles) (pop categories) (pop discids)) choices))

	     (setq choice (completing-read
			   (if (= (car response) 211)
			       "Inexact matches, choose (default: ignore): "
			     "Multiple matches, choose (default: ignore): ")
			   choices nil t))
	     (and (not (string= "" choice))
		  (assoc choice choices)
		  (list (nth 1 (assoc choice choices))
			(nth 2 (assoc choice choices))
			choice))))))))
	 
(defun cddb-cddb-search (info host port)
  "Perform a search on a remote HOST for a given cddb information INFO"
  (message "Searching remotely: %s:%d" host port)
  (save-excursion
    (set-buffer (get-buffer-create " *cddb*"))
    (erase-buffer)
    (let ((process (open-network-stream "cddb" (current-buffer) host port))
	  (query (cddb-build-query-string info))
	  (response nil)
	  (category nil)
	  (discid nil)
	  (entry nil))

      ;; This is just so we don't see the annoying (to me)
      ;; 'Process foo exited abnormally with code ###'
      ;; at the end of every connection.
      (set-process-sentinel process 'ignore)

      ;; Slurp in the opening banner
      (cddb-get-response process "Cannot connect to server: %s")

      ;; Send/receive the hello
      (process-send-string process
			   (format "cddb hello %s %s cddb.el %s\r\n"
				   (user-real-login-name)
				   (system-name)
				   cddb-version))
      (cddb-get-response process)

      ;; Actually send the query
      (process-send-string process query)
      (setq response (cddb-get-response process "Could not send query to server: %s")
	    response (cddb-response-parse response)
	    category (nth 0 response)
	    discid (nth 1 response))

      (if (not category)
	  ;; Didn't find _any_ matches, or the user didn't like what they found
	  ;; let's move on to the next server.
	  nil
	(process-send-string process (format "cddb read %s %s\r\n" category discid))
	(setq response (cddb-get-response process "Error during `cddb read': %s")
	      entry (nth 2 response)))

      (process-send-string process "quit\r\n")
      (cddb-get-response process "Problem disconnecting from server: %s")
      (if (and entry (string-match "\r\n" entry))
	  (progn
	    ;; Has \r\n crap, clean it up
	    (erase-buffer)
	    (insert entry)
	    (goto-char (point-min))
	    (while (re-search-forward "\r\n" nil t)
	      (replace-match "\n"))
	    (setq entry (buffer-string))
	    (erase-buffer)))
      entry)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remote searching via HTTP, requires the URL package to work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is a copy of what is in Emacs/W3's w3-forms.el, but I don't
;; want to require all of Emacs/W3 just for one encoding function.
(defun cddb-form-encode-xwfu (chunk)
  "Escape characters in a string for application/x-www-form-urlencoded.
Blasphemous crap because someone didn't think %20 was good enough for encoding
spaces.  Die Die Die."
  (declare (special url-unreserved-chars))
  (mapconcat
   (function
    (lambda (char)
      (cond
       ((= char ?  ) "+")
       ((memq char url-unreserved-chars) (char-to-string char))
       (t (upcase (format "%%%02x" char))))))
   chunk ""))

(defun cddb-http-search (info host port path)
  (message "Searching via HTTP: %s:%d" host port)
  (declare (special url-be-asynchronous))
  (save-excursion
    (let ((old-asynch (default-value url-be-asynchronous))
	  (category nil)
	  (discid nil)
	  (search-attributes nil)
	  (url-buffer nil)
	  (url (format "http://%s:%d/%s" host port path))
	  (url-request-method "POST")
	  (url-request-extra-headers '(("Content-type" . "application/x-www-form-urlencoded")))
	  (url-request-data nil)
	  (response nil)
	  (entry nil))
      (setq-default url-be-asynchronous nil)
      (push (cons "proto" "1") search-attributes)
      (push (cons "hello" (format "%s %s cddb.el %s"
				  (user-real-login-name)
				  (system-name)
				  cddb-version)) search-attributes)
      (push (cons "cmd" (cddb-build-query-string info)) search-attributes)
      (setq url-request-data (mapconcat (lambda (e)
					  (concat (car e) "="
						  (cddb-form-encode-xwfu (cdr e))))
					search-attributes "&")
	    url-buffer (cdr (url-retrieve url)))
      (if (not url-buffer)
	  nil
	(set-buffer url-buffer)
	(setq response (cddb-get-response nil "Problem getting query result")
	      response (cddb-response-parse response)
	      category (nth 0 response)
	      discid (nth 1 response))
	(kill-buffer url-buffer)
	(if (not category)
	    nil
	  (pop search-attributes)
	  (push (cons "cmd" (format "cddb read %s %s" category discid)) search-attributes)
	  (setq url-request-data (mapconcat (lambda (e)
					      (concat (car e) "="
						      (cddb-form-encode-xwfu (cdr e))))
					    search-attributes "&")
		url-buffer (cdr (url-retrieve url)))
	  (if (not url-buffer)
	      nil
	    (set-buffer url-buffer)
	    (setq response (cddb-get-response nil "Problem getting query result")
		  entry (nth 2 response)))))
      (setq-default url-be-asynchronous old-asynch)
      (kill-buffer url-buffer)
      (set-buffer (get-buffer-create " *cddb*"))
      (if (and entry (string-match "\r\n" entry))
	  (progn
	    ;; Has \r\n crap, clean it up
	    (erase-buffer)
	    (insert entry)
	    (goto-char (point-min))
	    (while (re-search-forward "\r\n" nil t)
	      (replace-match "\n"))
	    (setq entry (buffer-string))
	    (erase-buffer)))
      entry)))		  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main entry points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cddb-init ()
  ;; First, if the user has not specified what cd query function to
  ;; use, we look through the possible ones and use the first one we
  ;; find that is in our exec-path.
  (if (not cddb-cd-query-function)
      (let ((paths nil)
	    (possible-probes cddb-possible-probe-programs)
	    (probe nil)
	    (program-name nil))

	(while possible-probes
	  (setq probe (car possible-probes)
		possible-probes (cdr possible-probes)
		paths exec-path
		program-name (symbol-value (car probe)))
	  (while paths
	    (if (and (file-exists-p (expand-file-name program-name (car paths)))
		     (file-executable-p (expand-file-name program-name (car paths))))
		(setq paths nil
		      possible-probes nil
		      cddb-cd-query-function (cdr probe)))
	    (setq paths (cdr paths))))))

  ;; If there is _still_ no query function, then we cannot proceed,
  ;; and should not let the user proceed.
  (if (not cddb-cd-query-function)
      (save-window-excursion
	(set-buffer (get-buffer-create "*CDDB Error*"))
	(erase-buffer)
	(insert "*** ERROR ***\n\n"
		"An error occured while trying to determine how to get CD track "
		"information on your system.\n\n"
		"None of the following programs was found on your system.  "
		"Please customize one of the variables and try again.")
	(fill-region (point-min) (point-max))
	(insert "\n")
	(mapcar (lambda (v)
		  (insert (symbol-name (car v)) " -- " (symbol-value (car v)) "\n"))
		cddb-possible-probe-programs)
	(display-buffer (current-buffer))
	(read-string "An error occured - press RETURN to continue...")
	(error "Aborting")))

  ;; Next, if the user does not have the `url' package installed, they
  ;; will not be able to query remote HTTP servers, so let's just
  ;; clobber the HTTP function.
  (condition-case ()
      (require 'url)
    (error
     (message "Disabling HTTP searching due to missing `url' package")
     (defalias 'ignore 'cddb-http-search))))

(defun cddb-get-entry (&optional device)
  (cddb-init)
  (let* ((current-host nil)
	 (hosts cddb-hosts)
	 (info (funcall cddb-cd-query-function device))
	 (entry nil)
	 (query-function nil))
    (while (and hosts (not entry))
      (setq current-host (car hosts)
	    hosts (cdr hosts)
	    query-function (intern (format "cddb-%S-search" (car current-host))))
      (if (and (not (fboundp query-function))
	       (not (y-or-n-p (format "Unkown search method `%S', continue (y/n)? " (car current-host)))))
	  (error "Aborting search."))
      (setq entry (apply query-function info (cdr current-host))))
    entry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting CDDB to other formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cddb-to-xam (cddb-spec)
  (save-excursion
    (set-buffer (get-buffer-create " *cddb*"))
    (erase-buffer)
    (insert cddb-spec)
    (let* ((info (cddb-parse-info))
	   (i 1)
	   (tracks (nth 3 info)))
      (erase-buffer)
      (insert "ALBUM {\n"
	      "  TITLE \"" (or (nth 0 info) "Unknown") "\"\n"
	      "  ARTIST \"" (or (nth 1 info) "Unknown") "\"\n")
      (while tracks
	(insert "  TRACK {\n"
		"    TITLE \"" (car (car tracks)) "\"\n"
		(if (cdr (car tracks))
		    (concat "    ARTIST \"" (cdr (car tracks)) "\"\n")
		  "")
		(format "    CONTENT \"track-%d.mp3\"\n" i)
		"  }\n")
	(setq i (1+ i))
	(pop tracks))
      (insert "}\n"))
    (write-region (point-min) (point-max) (read-file-name "Save as: "))))

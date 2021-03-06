;;; socks.el --- A Socks v5 Client for Emacs
;; Author: $Author: wmperry $
;; Created: $Date: 1996/09/20 22:06:37 $
;; Version: $Revision: 1.7 $
;; Keywords: comm, firewalls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1996 by William M. Perry (wmperry@cs.indiana.edu)
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
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)

(defconst socks-version 5)

;; Common socks v5 commands
(defconst socks-connect-command 1)
(defconst socks-bind-command 2)
(defconst socks-udp-associate-command 3)

;; Miscellaneous other socks constants
(defconst socks-authentication-null 0)
(defconst socks-authentication-failure 255)

;; Response codes
(defconst socks-response-success               0)
(defconst socks-response-general-failure       1)
(defconst socks-response-access-denied         2)
(defconst socks-response-network-unreachable   3)
(defconst socks-response-host-unreachable      4)
(defconst socks-response-connection-refused    5)
(defconst socks-response-ttl-expired           6)
(defconst socks-response-cmd-not-supported     7)
(defconst socks-response-address-not-supported 8)

(defvar socks-errors
  '("Succeeded"
    "General SOCKS server failure"
    "Connection not allowed by ruleset"
    "Network unreachable"
    "Host unreachable"
    "Connection refused"
    "Time-to-live expired"
    "Command not supported"
    "Address type not supported"))

;; The socks v5 address types
(defconst socks-address-type-v4   1)
(defconst socks-address-type-name 3)
(defconst socks-address-type-v6   4)

;; Base variables
(defvar socks-debug (/= (length (getenv "SOCKS5_DEBUG")) 0)
  "*Whether debugging information should be shown during SOCKS negotiation.")

(defvar socks-host (or (getenv "SOCKS5_SERVER") "socks")
  "*The default SOCKS server to connect to.
Initialized by the SOCKS5_SERVER environment variable.")

(defvar socks-port (or (getenv "SOCKS5_PORT")   1080)
  "*The default SOCKS port to connect to.
Initialized by the SOCKS5_PORT environment variable, or a fallback of 1080.")

(defvar socks-timeout 5
  "*How long to wait for data from the SOCKS server.
This is used internally as the timeout value for `accept-process-output',
so that C-g, etc, can still work to interrupt our connection in case the
SOCKS server is down or confused.")

(defvar socks-connections (make-hash-table :size 13)
  "A hashtable containing all the active socks connections.
This is keyed on the actual process object, and the data is in turn another
hashtable that stores various information about the process (state information,
where it is really connecting to, etc).")

;; Miscellaneous stuff for authentication
(defvar socks-authentication-methods nil
  "Storage for information about the recognized authentication types.
DO NOT USE THIS VARIABLE.  See the functions
`socks-register-authentication-method' and
`socks-unregister-authentication-method' instead.")

(defvar socks-username (user-login-name)
  "*The username you would like to use to authenticate to the SOCKS server.")

(defvar socks-password nil
  "*The password you would like to use to authenticate to the SOCKS server.")

(defun socks-register-authentication-method (id desc callback)
  ;; DOCUMENT ME
  (let ((old (assq id socks-authentication-methods)))
    (if old
	(setcdr old (cons desc callback))
      (setq socks-authentication-methods
	    (cons (cons id (cons desc callback))
		  socks-authentication-methods)))))

(defun socks-unregister-authentication-method (id)
  ;; DOCUMENT ME
  (let ((old (assq id socks-authentication-methods)))
    (if old
	(setq socks-authentication-methods
	      (delq old socks-authentication-methods)))))

(socks-register-authentication-method 0 "No authentication" 'identity)

(defun socks-build-auth-list ()
  (let ((num 0)
	(retval ""))
    (mapcar
     (function
      (lambda (x)
	(if (fboundp (cdr (cdr x)))
	    (setq retval (format "%s%c" retval (car x))
		  num (1+ num)))))
     socks-authentication-methods)
    (format "%c%s" num retval)))

(defconst socks-state-waiting-for-auth 0)
(defconst socks-state-submethod-negotiation 1)
(defconst socks-state-authenticated 2)
(defconst socks-state-waiting 3)
(defconst socks-state-connected 4)

(defun socks-filter (proc string)
  ;; DOCUMENT ME
  (let ((info (cl-gethash proc socks-connections))
	state desired-len)
    (or info (error "socks-filter called on non-SOCKS connection %S" proc))
    (setq state (cl-gethash 'state info))
    (cond
     ((equal state socks-state-waiting-for-auth)
      (cl-puthash 'scratch (concat string (cl-gethash 'scratch info)) info)
      (setq string (cl-gethash 'scratch info))
      (if (< (length string) 2)
	  nil				; We need to spin some more
	(cl-puthash 'authtype (aref string 1) info)
	(cl-puthash 'scratch (substring string 2 nil) info)
	(cl-puthash 'state socks-state-submethod-negotiation info)))
     ((equal state socks-state-submethod-negotiation)
      )
     ((equal state socks-state-authenticated)
      )
     ((equal state socks-state-waiting)
      (cl-puthash 'scratch (concat string (cl-gethash 'scratch info)) info)
      (setq string (cl-gethash 'scratch info))
      (if (< (length string) 4)
	  nil
	(setq desired-len
	      (+ 6			; Standard socks header
		 (cond
		  ((= (aref string 3) socks-address-type-v4) 4)
		  ((= (aref string 3) socks-address-type-v6) 16)
		  ((= (aref string 3) socks-address-type-name)
		   (if (< (length string) 5)
		       255
		     (+ 1 (aref string 4)))))))
	(if (< (length string) desired-len)
	    nil				; Need to spin some more
	  (cl-puthash 'state socks-state-connected info)
	  (cl-puthash 'reply (aref string 1) info)
	  (cl-puthash 'response string info))))
     ((equal state socks-state-connected)
      )
     )
    )
  )

(defun socks-open-connection (&optional host port)
  ;; DOCUMENT ME
  (interactive)
  (setq host (or host socks-host)
	port (or port socks-port))
  (save-excursion
    (let ((proc (socks-original-open-network-stream "socks"
				     (if socks-debug
					 (generate-new-buffer
					  (format "Trace of SOCKS v%d to %s:%d"
						  socks-version host port)))
				     host port))
	  (info (make-hash-table :size 13))
	  (authtype nil))

      ;; Initialize process and info about the process
      (set-process-filter proc 'socks-filter)
      (process-kill-without-query proc)
      (cl-puthash proc info socks-connections)
      (cl-puthash 'state socks-state-waiting-for-auth info)
      (cl-puthash 'authtype socks-authentication-failure info)

      ;; Send what we think we can handle for authentication types
      (process-send-string proc (format "%c%s" socks-version
					(socks-build-auth-list)))

      ;; Basically just do a select() until we change states.
      (while (and (equal (cl-gethash 'state info) socks-state-waiting-for-auth)
		  (memq (process-status proc) '(run open)))
	(accept-process-output proc socks-timeout))

      (setq authtype (cl-gethash 'authtype info))
      (cond
       ((equal authtype socks-authentication-null)
	(and socks-debug (message "No authentication necessary")))
       ((equal authtype socks-authentication-failure)
	(error "No acceptable authentication methods found."))
       (t
	(let* ((auth-type (cl-gethash 'authtype info))
	       (auth-handler (assq auth-type socks-authentication-methods))
	       (auth-func (and auth-handler (cdr (cdr auth-handler))))
	       (auth-desc (and auth-handler (car (cdr auth-handler)))))
	  (set-process-filter proc nil)
	  (if (and auth-func (fboundp auth-func)
		   (progn
		     (funcall auth-func proc)
		     (= (cl-gethash 'state info) socks-state-authenticated)))
	      (message "Successfully authenticated using: %s" auth-desc)
	    (delete-process proc)
	    (error "Failed to use auth method: %d" (or auth-desc "Unknown")))
	  )
	)
       )
      (cl-puthash 'state socks-state-authenticated info)
      (set-process-filter proc 'socks-filter)
      proc)))

(defun socks-send-command (proc command atype address port)
  ;; DOCUMENT ME
  (let ((addr (case atype
		(socks-address-type-v4 address)
		(socks-address-type-v6 address)
		(t
		  (format "%c%s" (length address) address))))
	(info (cl-gethash proc socks-connections)))
    (or info (error "socks-send-command called on non-SOCKS connection %S"
		    proc))

    (cl-puthash 'state socks-state-waiting info)
    (process-send-string proc
			 (format 
			  "%c%c%c%c%s%c%c"
			  socks-version	; version 
			  command	; command
			  0		; reserved
			  atype		; address type
			  addr		; address
			  (lsh port -8)	; port, high byte
			  (- port (lsh (lsh port -8) 8))
			  ))
    (while (and (equal (cl-gethash 'state info) socks-state-waiting)
		(memq (process-status proc) '(run open)))
       (accept-process-output proc socks-timeout))

    (if (equal (cl-gethash 'reply info) socks-response-success)
	 nil				; Sweet sweet success!
       (delete-process proc)
       (if (cl-gethash 'reply info)
	   (error "%s" (nth (cl-gethash 'reply info) socks-errors))
	 (error "Unknown error sending command to SOCKS server.")))
    proc))


;; Replacement functions for open-network-stream, etc.
(defvar socks-no-proxy nil)
(defun socks-find-route (host service)
  ;; DOCUMENT ME
  (if (or (null socks-no-proxy)
	  (not (string-match socks-no-proxy host)))
      (cons socks-host socks-port)
    nil))

(if (fboundp 'socks-original-open-network-stream)
    nil					; Do nothing, we've been here already
  (fset 'socks-original-open-network-stream
	(symbol-function 'open-network-stream))
  (fset 'open-network-stream 'socks-open-network-stream))

(defvar socks-services-file "/etc/services"
  "*Where your system stores the mapping of services-to-port-numbers file.")

(defvar socks-tcp-services (make-hash-table 13 'equal)
  "A hashtable of TCP service names to port numbers.")

(defvar socks-udp-services (make-hash-table 13 'equal)
  "A hashtable of UDP service names to port numbers.")

(defun socks-parse-services ()
  ;; DOCUMENT ME
  (if (not (and (file-exists-p socks-services-file)
		(file-readable-p socks-services-file)))
      (error "Could not find services file: %s" socks-services-file))
  (save-excursion
    (clrhash socks-tcp-services)
    (clrhash socks-udp-services)
    (set-buffer (get-buffer-create " *socks-tmp*"))
    (erase-buffer)
    (insert-file-contents socks-services-file)
    ;; Nuke comments
    (goto-char (point-min))
    (while (re-search-forward "#.*" nil t)
      (replace-match ""))
    ;; Nuke empty lines
    (goto-char (point-min))
    (while (re-search-forward "^[ \t\n]+" nil t)
      (replace-match ""))
    ;; Now find all the lines
    (goto-char (point-min))
    (let (name port type)
      (while (re-search-forward "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)/\\([a-z]+\\)"
				nil t)
	(setq name (downcase (match-string 1))
	      port (string-to-int (match-string 2))
	      type (downcase (match-string 3)))
	(cl-puthash name port (if (equal type "udp")
			       socks-udp-services
			     socks-tcp-services))))))

(defun socks-find-services-entry (service &optional udp)
  "Return the port # associated with SERVICE"
  (if (equal (hashtable-fullness socks-tcp-services) 0)
      (socks-parse-services))
  (cl-gethash (downcase service) (if udp socks-udp-services socks-tcp-services)))

(defun socks-gc-hashtable ()
  (let ((dead nil))
    (cl-maphash
     (function
      (lambda (key val)
	(if (and (processp key) (memq (process-status key) '(run open)))
	    nil
	  (setq dead (cons key dead))))) socks-connections)
    (while dead
      (cl-remhash (car dead) socks-connections)
      (setq dead (cdr dead)))))

(defun socks-open-network-stream (name buffer host service)
  ;; DOCUMENT ME
  (let* ((route (socks-find-route host service))
	 proc info)
    (if (not route)
	(socks-original-open-network-stream name buffer host service)
      (setq proc (socks-open-connection (car route) (cdr route))
	    info (cl-gethash proc socks-connections))
      (socks-send-command proc socks-connect-command
			  socks-address-type-name
			  host
			  (if (stringp service)
			      (socks-find-services-entry service)
			    service))
      (cl-puthash 'buffer buffer info)
      (cl-puthash 'host host info)
      (cl-puthash 'service host info)
      (set-process-filter proc nil)
      (set-process-buffer proc (if buffer (get-buffer-create buffer)))
      proc)))


;; Authentication modules go here
;; 
;; Basic username/password authentication, ala RFC 1929
;; To enable username/password authentication, uncomment the following
;; lines:
;;
;; (socks-register-authentication-method 2 "Username/Password"
;;			      'socks-username/password-auth)

(defconst socks-username/password-auth-version 1)

(defun socks-username/password-auth-filter (proc string)
  ;; IMPLEMENT ME!!!
  )

(defun socks-username/password-auth (proc)
  ;; Does Username/password authentication ala RFC 1929
  (if (not socks-password)
      (setq socks-password (read-passwd
			    (format "Password for %s@%s: "
				    socks-username socks-host))))
  (let ((info (cl-gethash proc socks-connections)))
    (or info (error
	      "socks-username/password-auth called on non-SOCKS connection %S"
	      proc))
    (set-process-filter proc 'socks-username/password-auth-filter)
    (process-send-string proc
			 (format "%c%c%s%c%s"
				 socks-username/password-auth-version
				 (length socks-username)
				 socks-username
				 (length socks-password)
				 socks-password))
    (while (and (equal (cl-gethash 'state info) socks-state-waiting-for-auth)
		(memq (process-status proc) '(run open)))
      (accept-process-output proc socks-timeout))))


;; More advanced GSS/API stuff, not yet implemented - volunteers?
;; (socks-register-authentication-method 1 "GSS/API" 'socks-gssapi-auth)

(defun socks-gssapi-auth (proc)
  ;; This should eventually do GSS/Kerberos authentication ala RFC 1961
  nil)


(provide 'socks)

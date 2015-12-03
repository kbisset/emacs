;; mozmail.el --- Open mailto links from Mozilla in an (X)Emacs mailer.   -*- Emacs-Lisp -*-

;; Copyright (C) 2003 Steve Youngs

;; RCS: $Id: mozmail.el,v 1.4 2004/03/19 12:14:49 youngs Exp $
;; Author:        Steve Youngs <sryoungs@bigpond.net.au>
;; Maintainer:    Steve Youngs <sryoungs@bigpond.net.au>
;; Created:       <2003-12-22>
;; Last-Modified: <2004-03-19 21:53:09 (steve)>
;; Homepage:      None, contact maintainer for the latest version.
;;                Or get it from the XEmacs "net-utils" package.
;; Keywords:      mail

;; This file is part of mozmail.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; 
;;   Mozilla is a terrific web browser, but for mail and news I much
;;   prefer XEmacs & Gnus.  Once this is set up, clicking on a mailto
;;   link in Mozilla will fire up an XEmacs MUA with all the appropriate
;;   fields filled in.  At this time, the only MUA that is supported is
;;   Gnus (mainly because that is what I use).  Other MUAs (VM, MH-E,
;;   MEW, RMAIL) will be added in due course.
;;
;; Setup (Mozilla):
;;
;;   To get this to work you will need a very recent version of Mozilla,
;;   I was using 1.6b when I wrote this.  If you get `mozmail.el' to
;;   work with older versions of Mozilla, please let me know.
;;
;;   The first thing you must do is tweak your Mozilla settings so
;;   mailto links will invoke an external process.  Fire up Mozilla,
;;   and in the location bar type: about:config
;;
;;   That will give you a list of all of your settings.  There are
;;   litterally hundreds of them so prune them down by typing
;;   "protocol-handler" in the filter bar.  Now right-click on one of
;;   the items in the list and choose "New -> Boolean".  In the
;;   resulting dialog, type:
;;   "network.protocol-handler.external.mailto" (sans quotes).
;;   Another dialog will appear prompting for a value for this new
;;   variable, enter "true" (sans quotes).
;;
;;   Next, add another variable: right-click on a list item and choose "New
;;   -> String", in the dialog put: "network.protocol-handler.app.mailto".
;;   In the value dialog for this variable, put: "mozmail.sh".
;;
;;   That's all you need to do on the Mozilla side of things.  Restarting
;;   Mozilla probably wouldn't be a bad idea.
;;
;; Setup (Shell Script):
;;
;;   You will also need a very small (2 line) wrapper script.  Copy
;;   the following text to `mozmail.sh', put it somewhere in your
;;   $PATH and make it executable.
;;
;;      #!/bin/bash
;;      gnuclient -eval "(mozmail \"$1\")"
;;
;; Setup ((X)Emacs):
;;
;;   Put this file in your `load-path' and add...
;;
;;   (gnuserv-start)
;;   (autoload 'mozmail "mozmail")
;;
;;   ...to one of your `user-init-file'.

;;; Todo:
;;
;;   o Add support for VM
;;   o Add support for MH-E
;;   o Add support for MEW
;;   o Add support for RMAIL
;;   o Can this be done without using gnuserv/gnuclient?

;;; ChangeLog:
;;
;;  From this point on, `mozmail.el' is in the XEmacs packages CVS
;;  repository.  For further changes please consult
;;  ./xemacs-packages/net-utils/ChangeLog.
;;
;;  Revision 1.1  2003-12-23 14:49:47+10  steve
;;  Initial revision
;;

;;; Code:
;;;###autoload
(defun mozmail-version (&optional arg)
  "Return the current version info for mozmail.

With optional argument ARG, insert version info at point in the current
buffer."
  (interactive "P")
  (let (ver)
    (with-temp-buffer
      (erase-buffer)
      (insert-file (locate-library "mozmail.el"))
      (goto-char (point-min))
      (re-search-forward 
       "mozmail\\.el,v\\s-\\([0-9]+[0-9\\.]*[0-9]+\\)" nil t)
      (setq ver (match-string 1)))
    (if (interactive-p)
	(if arg
	    (insert (format "mozmail v%s" ver))
	  (message "mozmail v%s" ver))
      ver)))

(eval-and-compile
  (if (featurep 'url-util)
      (autoload 'url-unhex-string "url-util")
    (autoload 'url-unhex-string "url"))
  (autoload 'with-electric-help "ehelp")
  (autoload 'gnus-alive-p "gnus-util")
  (autoload 'gnus-group-mail "gnus-msg" nil t)
  (autoload 'message-goto-to "message" nil t)
  (autoload 'message-goto-subject "message" nil t)
  (autoload 'message-goto-cc "message" nil t)
  (autoload 'message-goto-bcc "message" nil t)
  (autoload 'message-goto-body "message" nil t)
  (autoload 'gnus "gnus" nil t))

;;;###autoload
(defun mozmail-commentary ()
  "*Display the commentary section of mozmail.el."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert (lm-commentary (locate-library "mozmail.el")))
	 (goto-char (point-min))
	 (while (re-search-forward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*Mozmail Commentary*"))

;;;###autoload
(defun mozmail-copyright ()
  "*Display the copyright notice for mozmail."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert-file-contents (locate-library "mozmail.el"))
	 (goto-char (point-min))
	 (re-search-forward ";;; Commentary" nil t)
	 (beginning-of-line)
	 (narrow-to-region (point-min) (point))
	 (while (re-search-backward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*Mozmail Copyright Notice*"))



(defun mozmail-compose-gnus (to &optional subject cc bcc body)
  "Compose a mail in Gnus from a Mozilla mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (unless (gnus-alive-p)
    (gnus))
  (gnus-group-mail)
  (message-goto-to)
  (insert (url-unhex-string to))
  (when subject
    (message-goto-subject)
    (insert (url-unhex-string subject)))
  (when cc
    (message-goto-cc)
    (insert (url-unhex-string cc)))
  (when bcc
    (message-goto-bcc)
    (insert (url-unhex-string bcc)))
  (when body
    (message-goto-body)
    (insert (url-unhex-string body 'allow-newlines))))

(defun mozmail-split-string (string char)
  "Does `split-string-by-char' in XEmacs and `split-string' in GNU/Emacs."
  (if (featurep 'xemacs)
      ;; XEmacs
      (split-string-by-char string char)
    ;; GNU/Emacs
    (split-string string (char-to-string char))))

(defun mozmail-split-url (url sym)
  "Split a mailto URL into its various components.

Argument URL is a mailto URL.
Argument SYM is a symbol representing the field name that you
want a value for.  Valid symbols are: `to', `subject', `cc', `bcc',
and `body'."
  (let ((value nil))
    (cond ((eq sym 'to)
	   (setq value (substring (car (mozmail-split-string url ?\?)) 7)))
	  ((eq sym 'subject)
	   (setq url (cdr (mozmail-split-string url ?\?)))
	   (when url
	     (setq url (mozmail-split-string (car url) ?&))
	     (while url
	       (when (string= "subject=" (substring (car url) 0 8))
		 (setq value (substring (car url) 8)))
	       (setq url (cdr url)))))
	  ((eq sym 'cc)
	   (setq url (cdr (mozmail-split-string url ?\?)))
	   (when url
	     (setq url (mozmail-split-string (car url) ?&))
	     (while url
	       (when (string= "cc=" (substring (car url) 0 3))
		 (setq value (substring (car url) 3)))
	       (setq url (cdr url)))))
	  ((eq sym 'bcc)
	   (setq url (cdr (mozmail-split-string url ?\?)))
	   (when url
	     (setq url (mozmail-split-string (car url) ?&))
	     (while url
	       (when (string= "bcc=" (substring (car url) 0 4))
		 (setq value (substring (car url) 4)))
	       (setq url (cdr url)))))
	  ((eq sym 'body)
	   (setq url (cdr (mozmail-split-string url ?\?)))
	   (when url
	     (setq url (mozmail-split-string (car url) ?&))
	     (while url
	       (when (string= "body=" (substring (car url) 0 5))
		 (setq value (substring (car url) 5)))
	       (setq url (cdr url)))))
	  (t
	   (error 'invalid-argument sym)))
    value))

;;;###autoload
(defun mozmail (url)
  "Use an (X)Emacs MUA as the target of a Mozilla mailto link.

See `mozmail-commentary' for instructions on how to set this up in
Mozilla."
  ;; A URL that consists of just "mailto:" and nothing else is obviously
  ;; wrong.
  (when (string= (substring url 7) "")
    (error 'invalid-argument url))
  (let ((to (mozmail-split-url url 'to))
	(subject (mozmail-split-url url 'subject))
	(cc (mozmail-split-url url 'cc))
	(bcc (mozmail-split-url url 'bcc))
	(body (mozmail-split-url url 'body)))
    (mozmail-compose-gnus to subject cc bcc body)))

(provide 'mozmail)
;;; mozmail.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End:

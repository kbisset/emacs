;; rm-tspaces.el: Automatically clean trailing whitespace from buffers.
;;
;; Copyright (C) 1995  Paul D. Smith <psmith@BayNetworks.com>
;;
;; This file requires Emacs 19 or above.  It should work with both Emacs
;; and XEmacs.  Use (require 'rm-tspaces) to load this file (don't
;; autoload it).
;;
;; For user-invoked whitespace cleanup, the function rm-trailing-spaces
;; can be bound to a key or invoked via M-x.
;;
;; The variable rm-trailing-spaces controls behavior when the buffer is
;; saved.  By default nothing is done.  See the documentation of this
;; variable for details.
;;
;; The variable rm-trailing-spaces is buffer-local.  Use setq-default to
;; set the default value for all buffers.  Use setq to override the
;; default value for a particular buffer.  If you're going to put the
;; setq in a hook, use the handy hook functions at the end of this file
;; to avoid having to write your own.
;;
;; For example, to be asked about removal for all buffers, except
;; automatically remove spaces in C files, try something like this:
;;
;;  (require 'rm-tspaces)
;;  (setq-default rm-trailing-spaces 1) ; any non-nil, non-t value
;;
;;  (add-hook 'c-mode-hook 'rm-trailing-spaces-always)
;;
;; Note the user is only queried when there is actually trailing
;; whitespace in the buffer (e.g., the buffer would be modified).
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2, or (at your option)
;;  any later version.
;;
;; Changes:
;;  1.2 - Add version info.
;;        Add simple functions to make installing in hooks simpler.


(defvar rm-trailing-spaces nil
  "*Value of t says silently remove all trailing spaces when a file is saved.
Non-nil but not t says ask user whether to remove trailing spaces or not.
nil means don't remove trailing spaces.")
(make-variable-buffer-local 'rm-trailing-spaces)


;; What are we?
;;
(defconst rm-tspaces-version (substring "$Revision: 1.2 $" 11 -2)
  "$Id: rm-tspaces.el,v 1.2 1996/05/29 19:16:38 psmith Exp $")


(defun rm-trailing-spaces-internal ()
  "Deletes trailing whitespace from all lines in the current buffer."
  (if (and (not buffer-read-only) rm-trailing-spaces)
      (save-excursion
	(goto-char (point-min))
	(if (or (eq rm-trailing-spaces t)
		(and (re-search-forward "[ \t]$" nil 1)
		     (y-or-n-p
		      (format "Remove trailing spaces in buffer %s? "
			      (buffer-name)))))
	    (while (< (point) (point-max))
	      (end-of-line nil)
	      (delete-horizontal-space)
	      (forward-line 1)))))
  nil) ; indicates buffer-not-saved for write-file-hooks

(defun rm-trailing-spaces ()
  "Deletes trailing whitespace from all lines in the current buffer."
  (interactive "*")
  (message "Deleting trailing spaces... ")
  (let ((rm-trailing-spaces t))
    (rm-trailing-spaces-internal))
  (message "Deleting trailing spaces... done"))


;; Apply this function automatically to all buffers before they're saved.
;;
(add-hook 'write-file-hooks 'rm-trailing-spaces-internal)


;; Provide some simple functions for inclusion in hooks variables
;;
(defun rm-trailing-spaces-never ()
  "Don't automatically delete trailing whitespace in this buffer."
  (setq rm-trailing-spaces nil))

(defun rm-trailing-spaces-ask ()
  "Ask before deleting trailing whitespace in this buffer."
  (setq rm-trailing-spaces 1))

(defun rm-trailing-spaces-always ()
  "Always automatically delete trailing whitespace in this buffer."
  (setq rm-trailing-spaces t))


(provide 'rm-tspaces)

;; rm-tspaces.el ends here

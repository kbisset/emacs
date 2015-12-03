;;; doh.el

;; Copyright (C) 1996 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Created: 1996-06-24

;; $Id: doh.el,v 1.2 1997/10/24 21:13:39 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

(require 'advice)

(defvar doh-file-name "~/etc/audio/simpsons/doh.au"
  "File name containing audio data.
This data is read into memory, but if you change the value of this
variable, the data is re-read.")

(defvar doh-device "/dev/audio"
  "Name of audio device.")

(defvar doh-hook '(doh!))

;; Don't change these.
(defvar doh-data nil)
(defvar doh-data-file-name "")

(defun doh! ()
  (interactive)
  (or (string= doh-file-name doh-data-file-name)
      (doh-initialize doh-file-name))
  ;; This makes use of a kludgy hack in Emacs 19's write-region to use a
  ;; string as the data instead an actual buffer region.
  (write-region doh-data nil doh-device nil 'quiet))

(defun doh-initialize (file-name)
  (interactive)
  (let ((buf (generate-new-buffer " *doh!*")))
    (save-excursion
      (set-buffer buf)
      (insert-file-contents file-name)
      (setq doh-data (buffer-substring (point-min) (point-max))))
    (kill-buffer buf))
  (setq doh-data-file-name file-name))

(defadvice ding (before run-doh-hook activate)
  "Doh!"
  (run-hooks 'doh-hook))

(defadvice keyboard-quit (around run-doh-hook activate)
  "Doh!"
  (unwind-protect
      ad-do-it
    (run-hooks 'doh-hook)))

(provide 'doh)

;;; doh.el ends here

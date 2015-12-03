;;;; compile-highlight.el -- Highlight error messages and `grep' matching lines.

;;; Description:
;;; 
;;; In compilation-mode buffers, you can click on the error messages to
;;; visit the corresponding source file line.  It would be helpful if
;;; each `compile' error message or `grep' matching line was highlighted
;;; as the mouse passed over it.  compile-highlight.el does so,
;;; depending on the value of the compile-highlight-display-limit
;;; option.

;;; Comments:
;;; 
;;; Thanks to Jari Aalto <jari.aalto@ntc.nokia.com> for his suggestions,
;;; and to Richard Stallman <rms@gnu.ai.mit.edu> for his interest.
;;; 
;;; According to David Masterson <davidm@prism.kla.com>, XEmacs already
;;; provides this feature.

;;; Usage:
;;; 
;;; 	(require 'compile-highlight)
;;; 
;;; If it takes too long to parse and highlight your compilation-mode
;;; buffers, try setting compile-highlight-display-limit to a smaller value:
;;; 
;;; 	(setq compile-highlight-display-limit 1024)

;;; LCD Archive Entry:
;;; 
;;; compile-highlight|Kevin Rodgers|kevinr@ihs.com|
;;; Highlight error messages and `grep' matching lines.|
;;; $Date: 1996/11/01 23:51:37 $|$Revision: 1.7 $||

;;; Copyright:
;;; 
;;; Copyright © 1996 Kevin Rodgers
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; 
;;; Information Handling Services has not disclaimed any copyright
;;; interest in compile-highlight.el.
;;; 
;;; Kevin Rodgers <kevin.rodgers@ihs.com>   Project Engineer
;;; Information Handling Services           Electronic Systems Development
;;; 15 Inverness Way East, M/S A201         GO BUFFS!
;;; Englewood CO 80112-5776 USA             1+ (303) 397-2807[voice]/754-3975[fax]

(provide 'compile-highlight)
(require 'compile)

(defadvice compilation-parse-errors (after highlight activate)
  "Highlight each error message as the mouse moves over it."
  (let ((inhibit-read-only t)
	(buffer-read-only nil)
	(error-list compilation-error-list))
    (while error-list
      (save-excursion
	(put-text-property (goto-char (car (car error-list)))
			   (progn (end-of-line) (point))
			   'mouse-face 'highlight))
      (setq error-list (cdr error-list)))))

(defadvice compilation-forget-errors (after unhighlight activate)
  "Don't highlight the error messages as the mouse moves over them."
  (let ((inhibit-read-only t)
	(buffer-read-only nil))
    (remove-text-properties (point-min) (point-max) '(mouse-face highlight))))

(defvar compile-highlight-display-limit
  (if window-system
      line-number-display-limit
    t)
  "*If nil, all *compilation* error messages are automatically highlighted.
If a number, only that many lines of the *compilation* buffer are parsed.
Otherwise, no *compilation* error messages are automatically highlighted.")

(defun compilation-auto-highlight (buffer message)
  "Automatically parse and highlight *compilation* error messages.
This is intended to be used as a value for compilation-finish-function (q.v.);
see also the compile-highlight-display-limit variable."
  (cond ((not compile-highlight-display-limit)
	 (compile-reinitialize-errors nil (point-max)))
	((numberp compile-highlight-display-limit)
	 (compile-reinitialize-errors nil (save-excursion
					    (goto-line
					     compile-highlight-display-limit)
					    (point))))
;;;	(compile-highlight-display-limit
;;;	 nil)
	)
  )

(if (boundp 'compilation-finish-functions)
    (add-hook 'compilation-finish-functions 'compilation-auto-highlight)
  (setq compilation-finish-function 'compilation-auto-highlight))

;;;; compile-highlight.el ends here

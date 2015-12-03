;;; codeline.el
;;;
;;; Copyright (C) 1993, 1994, 1998, 2004, 2007, 2008 Thien-Thi Nguyen
;;;
;;; This file is part of ttn's personal elisp library, released under
;;; the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3, or (at your option) any
;;; later version.  There is NO WARRANTY.  See file COPYING for details.

;;; Description: Move to beginning/end of code on a line.
;;; Keywords: editing
;;; Number-of-Guitars-the-Author-Lives-With: 3 (was 4)

;;; Commentary:

;; Got tired of hitting `C-a' and `C-e' only to go to the absolute edge of a
;; line, where either whitespace (beginning) or a comment (end) necessitated
;; extra keystrokes to get to the code.  The following two commands move the
;; point to the beginning and end of the code part of the line, respectively,
;; essentially ignoring leading whitespace and trailing comments.  Subsequent
;; invocations just call their less-lengthly-named cousins.
;;
;; There are two special cases:
;; (a) Line consists of all whitespace (including just newline).  `C-a' moves
;;     point to the proper mode-specific column.  `C-e' moves to end of line.
;; (b) Line has no code, only comment.  `C-a' and `C-e' move point to the
;;     edge of the comment.
;;
;; Code enhancement opportunities (aka bugs/caveats):
;; - Cannot handle comments at the beginning of the line (before code).
;; - Cannot handle `comment-start' if in an "incomplete" string.
;; - Depends on mode-provided `comment-start' variable, which means that
;;   if you have
;;
;;      int a;   /*comment with no space after slash-star */
;;
;;   then `C-e' will do the wrong thing.  (just use `M-;' eh?)

;;; Suggested-usage:

;; In your ~/.emacs you will want to load in codeline.el by:
;;
;;   (load-file "$SOMEPATH/codeline.el")
;;
;; as well as bind `C-a' and `C-e' to the commands by:
;;
;;   (add-hook 'c-mode-hook
;;             (lambda ()
;;               (define-key c-mode-map "\C-a" 'beginning-of-code-line)
;;               (define-key c-mode-map "\C-e" 'end-of-code-line)))
;;
;; for c-mode, for example.  Similar add-hook calls need to be made for any
;; other modes you wish to use the commands in.  The adventurous may wish to
;; bind these globally with:
;;
;;   (global-set-key "\C-a" 'beginning-of-code-line)
;;   (global-set-key "\C-e" 'end-of-code-line)
;;
;; Feedback welcome!

;;; Code:

;;;---------------------------------------------------------------------------
;;; internal variables

(defvar codeline-last-arg nil
  "Enables passing through of args to subsequent command invocations.")

(defvar comment-start-regexp)

;;;---------------------------------------------------------------------------
;;; entry points

;;;###autoload
(defun beginning-of-code-line (arg)
  "Move point to first non-whitespace char on line, or indent.
On second invocation, move point to beginning of line."
  (interactive "p")
  (if (eq this-command last-command)
      (beginning-of-line codeline-last-arg)
    (beginning-of-line)
    (if (looking-at "\\s-*\n")
        (indent-according-to-mode)
      (back-to-indentation)))
  (setq codeline-last-arg arg))

;;;###autoload
(defun end-of-code-line (arg)
  "Move to end of code line or end of line if comment.
On second invocation, move point to end of line."
  (interactive "p")
  (if (eq this-command last-command)
      (end-of-line codeline-last-arg)
    (or (and (boundp 'comment-start-regexp)
             comment-start-regexp)
        (progn
          (make-local-variable 'comment-start-regexp)
          (setq comment-start-regexp (regexp-quote (or comment-start "")))))
    (let ((eol (progn (end-of-line) (point)))
          (stop (progn (beginning-of-line) (point))))
      (if (looking-at (concat "\\s-*\n\\|\\s-*" comment-start-regexp ".*\n"))
          (end-of-line)
        (while (if (re-search-forward "\"[^\"]*\"" eol t)
                   (setq stop (1- (point)))))
        (end-of-line)
        (while (re-search-backward comment-start-regexp stop t))
        (re-search-backward "\\S-" stop t)
        (forward-char 1))))
  (setq codeline-last-arg arg))

;;;---------------------------------------------------------------------------
;;; that's it

(provide 'codeline)

;;; codeline.el ends here

;;;
;;; ch-minor-mode (comment hide minor mode)
;;;
;;; Programmer: Phillip C. Brisco 9/27/96
;;;----------------------------------------------------------------------
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;----------------------------------------------------------------------
;;; Apologia:
;;;
;;; This was written for the cases where there is legacy code that is
;;; commented out in applications.  When debugging such applications,
;;; seeing the commented out code is normally the last thing programmers
;;; want to see.  I personally find this useful, and I hope you do too,
;;; but as the GNU general license basically says, Caveat Emptor.
;;;
;;; Comments:
;;;
;;; This routine will hide (or show) all comments in a file.  Currently
;;; we assume comments start with '/*' and end with '*/' if they are
;;; multi-line comments or '#' if single-line comments.  These can be
;;; changed in the appropriate variables to suit personal taste.  When the
;;; local-ch-setup procedure is run, the <CTRL>ch key is bound to
;;; the hide function, and the <CTRL>cs is bound to the show function.
;;;
;;; The default is to access the multi-line part of the code.  To access
;;; the single-line functionality, a prefix argument will have to be
;;; tacked onto the call to hide any comments.  This is currently done
;;; by using a prefix number before typing in the requisite command
;;; (i.e. <ESC>1<CTRL>ci where <ESC>1 is the prefix command to <CTRL>ci).
;;; This default can be changed by setting ch-primary-pattern to t.
;;;
;;; We also have the ability to hide and show individual comment blocks.
;;; These functions are currently bound to the <CTRL>cv (make a block
;;; of comments visible) and <CTRL>ci (make a block of comments
;;; invisible.
;;;
;;; To toggle the single-line comment delimiter between what is currently
;;; at the point and the default value (found in ch-singleton-default),
;;; use the <CTRL>ctd command.  The mode line is updated so that the user
;;; can see the single-line comment and the beginning multi-line comment.
;;;
;;; An exceptions handler has been written so that single-line comment
;;; characters which have no significant characters (blank space) preceding
;;; them on a line can be shown if so desired.  For example, in UN*X shell
;;; scripts the '#' is a comment character, but the '#!' is not a comment
;;; delimiter.  The exceptions handler can be toggled on and off by using
;;; the <CTRL>cts key.
;;;
;;; A list has been implemented so that multiple comments in a single
;;; document can be toggled on and off (this cannot be done for a single
;;; block of comments yet) at will.  The comments are embedded in a list
;;; which gives the type of pattern (1 = primary, 2 = alternate), the
;;; type of comment (S = single-line, M = multi-line), the begin and end
;;; of the pattern and the begin and the end of the pattern's regular
;;; expression form.  This can be toggled by using the <CTRL>cth key.
;;;
;;; To show what toggle modes are currently active, use the <CTRL>ctm key.
;;;
;;; A lot of low-level code snarfing was done of hideshow and outline
;;; to get this thing to work.
;;;
;;; GOTCHAS & ETC.
;;;
;;; This has been tested for GNU Emacs 19.34 on DEC UN*X and ULTRIX boxes.
;;;
;;; It would probably be a good idea to implement the ch-hide-list for
;;; individual blocks of comments as well as for the entire document.  I
;;; may even do this if I feel like it and I have the time (a non-existent
;;; commodity right now) to do so.
;;;
;;; It would be nice to put in a facility that skips searches of hidden lines,
;;; but as far as irritation factors go, this is fairly minor patooties,
;;; so I have no intention of writing such a facility.  If anyone out there
;;; has a thing for pain, go ahead and write the durn thing (good luck).
;;;
;;; I would recommend that you put something like the following in
;;; your .emacs file (or it's equivalent) so that the comment mode
;;; will automatically be set up as a minor mode to the selected
;;; major mode (in the following example, I use text mode as the
;;; major mode).
;;;
;;;(autoload (quote ch-minor-mode) "ch-minor-mode" nil t)
;;;(defun local-ch-setup ()
;;;  (ch-minor-mode 1)
;;;  (define-key ch-minor-mode-map "\C-ch" 'ch-hide-all)
;;;  (define-key ch-minor-mode-map "\C-cs" 'ch-show-all)
;;;  (define-key ch-minor-mode-map "\C-ci" 'ch-hide-block)
;;;  (define-key ch-minor-mode-map "\C-cv" 'ch-show-block)
;;;  (define-key ch-minor-mode-map "\C-ctd" 'ch-delim-toggle)
;;;  (define-key ch-minor-mode-map "\C-cts" 'ch-skipper-toggle)
;;;  (define-key ch-minor-mode-map "\C-cth" 'ch-hider-toggle)
;;;  (define-key ch-minor-mode-map "\C-ctm" 'ch-show-toggle-modes)
;;;  (define-key ch-minor-mode-map "\C-co" 'ch-minor-mode))
;;;(add-hook 'text-mode-hook 'local-ch-setup)

;;;----------------------------------------------------------------------------
;;; internal variables

(defvar ch-minor-mode nil
  "Non-nil if using comment mode as a minor mode of some other mode.
Use the command `ch-minor-mode' to toggle this variable.")

(defvar ch-minor-mode-map nil
  "Mode map for comment minor mode.")

(defvar ch-start-multiline-regexp-default "\\/\\*"
  "Multiline start comment default.  Buffer-local.")

(defvar ch-start-multiline-regexp ch-start-multiline-regexp-default
  "Multiline start comment.  Buffer-local.")

(defvar ch-start-multiline-default "/*"
  "Multiline delimiter for start of comments.  Buffer-local.")

(defvar ch-start-multiline ch-start-multiline-default
  "Multiline delimiter for start of comments.  Buffer-local.")

(defvar ch-end-multiline-regexp-default "\\*\\/"
  "Multiline end comment.  Buffer-local.")

(defvar ch-end-multiline-regexp ch-end-multiline-regexp-default
  "Multiline end comment.  Buffer-local.")

(defvar ch-end-multiline-default "*/"
  "Multiline delimiter for end of comments.  Buffer-local.")

(defvar ch-end-multiline ch-end-multiline-default
  "Multiline delimiter for end of comments.  Buffer-local.")

(defvar ch-singleton-regexp-default "#"
  "Default delimiter for single line comments.  Buffer-local.")

(defvar ch-singleton-regexp ch-singleton-regexp-default
  "Single line comment. Buffer-local.")

(defvar ch-singleton-default "#"
  "Default delimiter for single line comments.  Buffer-local.")

(defvar ch-singleton ch-singleton-default
  "Delimiter for single line comments.  Buffer-local.")

(defvar ch-primary-pattern nil
  "nil = multiline pattern, t = single line pattern.  Buffer-local.")

(defvar ch-commenter nil
  "Holds comment patterns to update the mode line with.  Buffer-local.")

(defvar ch-skip-switch t
  "nil = Don't check skip list.  t = check skip list.")

(defvar ch-skip-list
  '(("#" . "#\!"))
  "Holds the list of characters which are not to be hidden.  The first
part of the list tells what the comment character is, and the second part
of the list tells what line is to be skipped.")

(defvar ch-hider-switch t
  "t = use ch-hide-list, nil = use ch-hide-singleton or ch-hide-multiline")

(defvar ch-hide-list
  '(("1M" . (("/*" . "*/") . ("\\/\\*" . "\\*\\/")))
    ("1S" . (("#" . t) . ("#" . t)))
    ("2S" . (("#" . t) . ("#" . t))))
  "Holds the list of comment delimiters which are to be hidden.  The
first part of the list tells whether or not the primary pattern is
to be used (1 = use primary pattern, 2 = use secondary pattern) and
if this is a single-line (S) or multi-line comment (M).  The middle
part of the list gives the pattern begin and pattern end characters.
The last part of the list gives the regular expression form of the
pattern begin and end characters.")

;;;----------------------------------------------------------------------------

;;; keymap setup
(if (not ch-minor-mode-map)
    (setq ch-minor-mode-map (make-sparse-keymap))
  (let ()
    (define-key ch-minor-mode-map [menu-bar comment]
      (cons "comment" (make-sparse-keymap "comment")))
    (define-key ch-minor-mode-map [menu-bar comment ch-show-all]
      '("Show All" . ch-show-all))
    (define-key ch-minor-mode-map [menu-bar comment ch-hide-all]
      '("Hide All" . ch-hide-all))
    (define-key ch-minor-mode-map [menu-bar comment ch-hide-block]
      '("Hide Block" . ch-hide-block))
    (define-key ch-minor-mode-map [menu-bar comment ch-show-block]
      '("Show Block" . ch-show-block))
    (define-key ch-minor-mode-map [menu-bar comment ch-delim-toggle]
      '("Delimiter Toggle" . ch-delim-toggle))
    (define-key ch-minor-mode-map [menu-bar comment ch-skipper-toggle]
      '("Skip Toggle" . ch-skipper-toggle))
    (define-key ch-minor-mode-map [menu-bar comment ch-hider-toggle]
      '("Hide Toggle" . ch-hider-toggle))
    (define-key ch-minor-mode-map [menu-bar comment ch-show-toggle-modes]
      '("Show Toggle Modes" . ch-show-toggle-modes))
    (define-key ch-minor-mode-map [menu-bar comment ch-off]
      '("Comment Off" . ch-minor-mode))))

;;; Register the minor mode keymap
(or (assq 'ch-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'ch-minor-mode ch-minor-mode-map)
               minor-mode-map-alist)))
(or (assq 'ch-minor-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(ch-minor-mode " comment")))))

;;; Make these variables buffer specific
(make-variable-buffer-local 'ch-minor-mode)
(make-variable-buffer-local 'ch-start-multiline-regexp-default)
(make-variable-buffer-local 'ch-start-multiline-regex)
(make-variable-buffer-local 'ch-end-multiline-regexp-default)
(make-variable-buffer-local 'ch-end-multiline-regexp)
(make-variable-buffer-local 'ch-start-multiline-default)
(make-variable-buffer-local 'ch-start-multiline)
(make-variable-buffer-local 'ch-end-multiline-default)
(make-variable-buffer-local 'ch-end-multiline)
(make-variable-buffer-local 'ch-singleton-regexp-default)
(make-variable-buffer-local 'ch-singleton-regexp)
(make-variable-buffer-local 'ch-singleton-default)
(make-variable-buffer-local 'ch-singleton)
(make-variable-buffer-local 'ch-primary-pattern)
(make-variable-buffer-local 'ch-commenter)
(make-variable-buffer-local 'ch-skip-switch)
(make-variable-buffer-local 'ch-skip-list)
(make-variable-buffer-local 'ch-hider-switch)
(make-variable-buffer-local 'ch-hide-list)
(put 'ch-minor-mode 'permanent-local t)

;;;
;;;----------------------------------------------------------------------
;;; O.K., here we go...

;;;
;;; ch-flag-region (snarfed from hideshow).
;;; This does the actual work of hiding or showing a region of comments.
;;;
(defun ch-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.
If FLAG is `?\\n' (the newline character) then show the text;
if FLAG is `?\\^M' \(control-M) then hide the text."
  (let ((modp (buffer-modified-p))
   buffer-read-only)    ; nothing is immune
    (unwind-protect (progn
            (subst-char-in-region
             from to
             (if (= flag ?\n) ?\C-m ?\n)
             flag t))
      (set-buffer-modified-p modp))))
;;;
;;; ch-minor-mode.
;;; Toggles the ch-minor-mode.
;;;
(defun ch-minor-mode (&optional arg)
  "Toggle comment minor mode.
With ARG, turn comment minor mode on if ARG is positive, off otherwise.
When comment minor mode is on, the menu bar is augmented with comment
commands and the comment commands are enabled.  The variables
`selective-display' and `selective-display-ellipses' are set to t.
Last, the normal hook `ch-minor-mode-hook' is run; see the doc for
`run-hooks'.

Turning comment minor mode off reverts the menu bar and the
variables to default values and disables the comment commands."
  (interactive "P")
  (setq ch-minor-mode
        (if (null arg)
       (not ch-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if ch-minor-mode
      (progn
   (ch-mode-line)
   (setq selective-display t
         selective-display-ellipses t)
   (setq minor-mode-alist (list '(ch-minor-mode " Comment")))
   (run-hooks 'ch-minor-mode-hook))
    (kill-local-variable 'selective-display)
    (setq minor-mode-alist "")
    (kill-local-variable 'selective-display-ellipses)
    (ch-mode-line t)))

;;;
;;; ch-hide-all.  Hide all comments.
;;;
(defun ch-hide-all (&optional arg)
  "Hides all delimited comments.  With no prefix argument, the default
mode (currently the default mode is multi-line, but this can be changed
to single-line by the user setting the value of ch-primary-pattern to t)
is used."
  (interactive "P")
  (setq msg-txt "All comment lines collapsed.")
  (if (or
       (and (null arg) (null ch-primary-pattern))
       (and (not (null arg)) (not (null ch-primary-pattern))))
      (if ch-hider-switch
     (setq num-lines (ch-hider nil))
   (setq num-lines (ch-hide-all-multiline)))
    (if ch-hider-switch
   (setq num-lines (ch-hider t))
      (setq num-lines (ch-hide-all-singleton))))
  (if (> num-lines 0)
      (setq msg-txt (concat (int-to-string num-lines)
             " comment lines collapsed.")))
  (message "%s" msg-txt))

;;;
;;; ch-hide-all-singleton.  Hides all contiguous comment lines that
;;; begin with the ch-singleton pattern.
;;;
(defun ch-hide-all-singleton ()
  "Hides all contiguous comment lines that begin with the ch-singleton
pattern and that don't meet the exceptions criteria described in ch-skipper
if the ch-skip-switch nil."
  (save-excursion
    (let ((found t)
     (num-lines 0))
      (beginning-of-buffer)
      (while (and (not (eobp)) found)
   (setq found (re-search-forward ch-singleton-regexp (point-max) t))
   (if (and found (or (not (ch-skipper)) (not ch-skip-switch)))
       (setq num-lines (+ num-lines (ch-hide-block-singleton))
        found t)))
      num-lines)))

;;;
;;; ch-hide-all-multiline.
;;; Hides all multi-line comments.
;;;
(defun ch-hide-all-multiline ()
  "Hides all comments delimited by ch-start-multiline and ch-end-multiline."
  (save-excursion
    (let ((found t)
     (num-lines 0))
      (beginning-of-buffer)
      (while (re-search-forward ch-start-multiline-regexp (point-max) t)
   (setq num-lines (+ num-lines (ch-hide-block-multiline))))
      num-lines)))

;;;
;;; ch-hide-block.  Hides contiguous single line comment lines.
;;;
(defun ch-hide-block (&optional arg)
  "Entry function for hiding a block of comments in either single-line
or multi-line format.

If the optional prefix argument is nil then the default pattern (normally
multi-line) is used."
  (interactive "P")
  (setq msg-txt "All comment lines collapsed.")
  (let ((num-lines 0))
    (if (or
    (and (null arg) (null ch-primary-pattern))
    (and (not (null arg)) (not (null ch-primary-pattern))))
   (setq num-lines (ch-hide-block-multiline))
      (setq num-lines (ch-hide-block-singleton)))
    (if (> num-lines 0)
   (message "%d %s" num-lines "comment lines collapsed.")
      (message "%s" msg-txt))))

;;;
;;; ch-hide-block-singleton.  Hide all contiguous comments to the
;;; current comment line.
;;;
(defun ch-hide-block-singleton ()
  "Hides all contiguous comments to the current comment line."
  (setq msg-txt "There must be more than one comment line.")
  (save-excursion
    (setq found (if (ch-ignore-whitespace) t nil))
    (if found
   (let ((opoint nil))
     
     ; Find the first comment line.
     (while (and (and (not (bobp)) found)
            (or (not (ch-skipper)) (not ch-skip-switch)))
       (forward-line -1)
       (setq found (if (ch-ignore-whitespace) t nil)))
     (if (or (and (ch-skipper) ch-skip-switch) (not (bobp)))
         (forward-line 1))
     (setq found (if (ch-ignore-whitespace) t nil)
      opoint (let () (end-of-line) (point)))
     
     ; Find the line beyond the last contiguous comment line.
     (while (and (and (not (eobp)) found)
            (or (not (ch-skipper)) (not ch-skip-switch)))
       (forward-line 1)
       (setq found (if (ch-ignore-whitespace) t nil))
       (end-of-line))
     
     ; Find the last comment line.
     (if (or (not (eobp)) (and (ch-skipper) ch-skip-switch))
         (forward-line -1))
     (end-of-line)

     ; Collapse that sucker!
     (ch-flag-region opoint (point) ?\C-m)
     (count-lines opoint (point)))
      (setq msg-txt "Point not in a comment block.")
      0)))
;;;
;;;
;;; ch-hide-block-multiline.
;;;
(defun ch-hide-block-multiline ()
  "Hides a single set of comments delimited by ch-start-multiline and
ch-end-multiline."
  (save-excursion
    (let ((num-lines 0)
     (opoint (point))
     (start-point nil)
     (end-point nil))
      (end-of-line)
      (setq start-point
       (re-search-backward ch-start-multiline-regexp (point-min) t))
      (if start-point
     (setq end-point
      (re-search-forward ch-end-multiline-regexp (point-max) t)))
      (if (and end-point (and (>= opoint start-point) (<= opoint end-point)))
     (let ()
       (setq num-lines (count-lines start-point end-point))
       (ch-flag-region start-point end-point ?\C-m))
   (setq msg-txt "Not in comment block."))
      num-lines)))

;;;
;;; ch-show-all.  Shows all comments.
;;;
(defun ch-show-all ()
  "Makes all invisible text visible."
  (interactive)
  (setq msg-txt "All comments restored.")
  (let ((num-lines 0))
    (setq num-lines (ch-count-hidden-lines))
    (save-excursion
      (ch-flag-region (point-min) (point-max) ?\n))
    (setq msg-txt (concat (int-to-string num-lines)
           " comment lines restored.")))
  (message "%s" msg-txt))

;;;
;;; ch-show-block.  Restores all comment lines wherein the cursor
;;; lies upon.
;;;
(defun ch-show-block ()
  "Entry point for restoring a region of comments."
  (interactive)
  (end-of-line)
  (let ((opoint (point))
   (num-lines 0))
    (if (and (not (let () (beginning-of-line)
             (re-search-forward ch-start-multiline-regexp opoint t)))
        (not (let () (beginning-of-line)
             (re-search-forward ch-singleton-regexp opoint t))))
   (setq msg-txt "The cursor is not on a hidden comment section.")
      (setq opoint (point))
      (setq num-lines
       (ch-count-hidden-lines opoint (let () (forward-line 1) (point))))
      (ch-flag-region opoint (point) ?\n)
      (if (< num-lines 1)
     (setq msg-txt "Comment section not hidden.")
   (setq msg-txt (concat num-lines " Comment lines restored.")))
      (goto-char opoint))
    (message "%s" msg-txt)))

;;;
;;; ch-ignore-whitespace
;;;
(defun ch-ignore-whitespace (&optional arg)
  "Returns nil if a non-whitespace character other than the value defined
in ch-singleton is found prior to the single-line comment character in
a line.  Otherwise, t is returned.

This allows for the correct checking of comment delimiters that start in
other than the first column."
  (if (null arg)
      (setq arg ch-singleton-regexp))
  (save-excursion
    (let ((q (let () (end-of-line) (point)))
     (ret-val t))
      (beginning-of-line)
      (setq ret-val (re-search-forward (concat "[ \t]*" arg) q t))
      (beginning-of-line)
      (if ret-val
     (if (= (1+ (re-search-forward "[ \t]*" q t)) ret-val)
         (setq ret-val t)
       (setq ret-val nil)))
      ret-val)))

;;;
;;; ch-count-hidden-lines.
;;;
(defun ch-count-hidden-lines (&optional begin end)
  "Counts the comment lines in a given range.  The default range is from
1 to (point-max)."
  (let ((count 0)
   epoint
   (begin-range (point-min))
   (end-range (point-max))
   (found t))
    (save-excursion
      (if (not (null begin))
     (setq begin-range begin))
      (if (not (null end))
     (setq end-range end))
      (goto-char begin-range)
      (while (and (and (not (eobp)) found) (<= (point) end-range))
   (setq found (re-search-forward "\r" end-range t))
   (if found
       (let ()
         (setq epoint (let () (end-of-line) (point)))
         (beginning-of-line)
         (while (and found (not (eobp)))
      (setq count (1+ count))
      (setq found (re-search-forward "\r" epoint t)))
         (setq found t)))))
    count))

;;;
;;; ch-delim-toggle.
;;;
(defun ch-delim-toggle ()
  "If the character under the point is different than the one in the
ch-singleton variable, then copy the character under the point into
ch-singleton.  Otherwise, copy the ch-singleton-default character into
ch-singlteon."
  (interactive)
  (if (/= (char-after (point)) (string-to-char ch-singleton))
      (setq ch-singleton (char-to-string (char-after (point)))
       ch-singleton-regexp)
    (if (string-equal ch-singleton ch-singleton-default)
   (setq ch-singleton (char-to-string (char-after (point)))
         ch-singleton-regexp)
      (setq ch-singleton ch-singleton-default)
      (setq ch-singleton-regexp ch-singleton)))
  (ch-mode-line))

;;;
;;; ch-mode-line
;;;
(defun ch-mode-line (&optional arg)
  "Updates the mode-line with the ch-singleton and
ch-start-multiline strings respectively."
  (if (null arg)
      (setq ch-commenter
       (concat(concat (concat " \"" ch-singleton)
            (concat " " ch-start-multiline))
         "\""))
    (setq ch-commenter ""))
    (setq-default mode-line-format
        (list (purecopy "")
         'mode-line-modified
         'mode-line-buffer-identification
         (purecopy "   ")
         'global-mode-string
         (purecopy "   %[(")
         'mode-name 'mode-line-process 'minor-mode-alist
         'ch-commenter
         (purecopy "%n")
         (purecopy ")%]--")
         (purecopy '(line-number-mode "L%l--"))
         (purecopy '(column-number-mode "C%c--"))
         (purecopy '(-3 . "%p"))
         (purecopy "-%-")
         (set-buffer-modified-p
          (buffer-modified-p)))))
;;;
;;; ch-skipper.
;;;
(defun ch-skipper (&optional arg)
  "Uses the ch-skip-list to determine if a line is to be hidden or not.
If the special skip character is found, then return t, else return nil."
  (let ((i 0)
   (found nil)
   (current-delim (if (null arg) ch-singleton arg)))
    (while (and (not found) (nth i ch-skip-list))
      (if (string-equal (car (nth i ch-skip-list)) current-delim)
     (let ((p nil))
       (save-excursion
         (end-of-line)
         (setq p (point))
         (beginning-of-line)
         (setq found (re-search-forward
            (cdr (nth i ch-skip-list)) p t)))))
      (setq i (1+ i)))
  found))

;;;
;;; ch-skipper-toggle
;;; Toggles the ch-skip-switch key.
;;;
(defun ch-skipper-toggle ()
  (interactive)
  (if ch-skip-switch (setq ch-skip-switch nil) (setq ch-skip-switch t))
  (message (concat "Skip list " (if ch-skip-switch "enabled." "disabled."))))
    
;;;
;;; ch-hider
;;; Hides all comments denoted by the pattern rules in the ch-hide-list.
;;;
(defun ch-hider (arg)
  "Hides all comments denoted by the pattern rules in the ch-hide-list.
The pattern rules are used to allow multiple types of comment delimiters
to be hidden in a single file.  While this has somewhat limited utility,
in the cases where a program calls itself to run in a totally different mode
than it was originally invoked (such as a shell script with SQL code) this
may be useful."
  (setq num-lines 0)
  (let ((i 0))
    (while (nth i ch-hide-list)
      (if (or (and arg (string-equal (car (nth i ch-hide-list)) "2S"))
         (and (null arg) (string-equal (car (nth i ch-hide-list)) "1S")))
     (let ()
       (setq ch-singleton (car (car (cdr (nth i ch-hide-list)))))
       (setq ch-singleton-regexp (car (cdr (cdr (nth i ch-hide-list)))))
       (setq num-lines (+ num-lines (ch-hide-all-singleton)))))
      (if (or (and arg (string-equal (car (nth i ch-hide-list)) "2M"))
         (and (null arg) (string-equal (car (nth i ch-hide-list)) "1M")))
     (let ()
       (setq ch-start-multiline (car (car (cdr (nth i ch-hide-list)))))
       (setq ch-start-multiline-regexp
        (car (cdr (cdr (nth i ch-hide-list)))))
       (setq ch-end-multiline (cdr (car (cdr (nth i ch-hide-list)))))
       (setq ch-end-multiline-regexp
        (cdr (cdr (cdr (nth i ch-hide-list)))))
       (setq num-lines (+ num-lines (ch-hide-all-multiline)))))
      (setq i (1+ i))))
  num-lines)

;;;
;;; ch-hider-toggle
;;; Toggles the ch-hide-toggle key.
;;;
(defun ch-hider-toggle ()
  (interactive)
  (setq ch-singleton ch-singleton-default
   ch-singleton-regexp ch-singleton-regexp
   ch-start-multiline ch-start-multiline-default
   ch-start-multiline-regexp ch-start-multiline-regexp-default
   ch-end-multiline ch-end-multiline-default
   ch-end-multiline-regexp ch-end-multiline-regexp-default)
  (if ch-hider-switch (setq ch-hider-switch nil) (setq ch-hider-switch t))
  (message (concat "Hide list " (if ch-hider-switch "enabled." "disabled."))))

;;;
;;; ch-show-toggle-modes
;;;
(defun ch-show-toggle-modes ()
  (interactive)
  (message (concat
       (concat "Hide list " (if ch-hider-switch "enabled. " "disabled. "))
       (concat "Skip list " (if ch-skip-switch "enabled." "disabled.")))))
(provide 'comment-hide)
;;; End of philo-minor-mode.el

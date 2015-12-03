;;; hideshow.el --- minor mode cmds to selectively display blocks of code

;; Copyright (C) 1994, 95, 96, 97, 98 Free Software Foundation

;; Author: Thien-Thi Nguyen <ttn@netcom.com>
;;	Dan Nicolaescu <dann@ics.uci.edu>
;; Maintainer: Thien-Thi Nguyen <ttn@netcom.com>
;; Maintainer-Version: 4.32
;; Keywords: C C++ java lisp tools editing comments blocks hiding outlines
;; Time-of-Day-Author-Most-Likely-to-be-Recalcitrant: early morning

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; - Commands provided
;;
;; This file provides `hs-minor-mode'.  When active, nine commands:
;;
;;   hs-{hide,show}-{all,block}, hs-show-region, hs-hide-level,
;;   hs-{save,restore}-state and hs-minor-mode
;;
;; are available, implementing block hiding and showing.  Blocks are
;; defined per mode.  In c-mode or c++-mode, they are simply curly braces,
;; while in Lisp-ish modes they are parens.  Multi-line comment blocks can
;; also be hidden.  The command `M-x hs-minor-mode' toggles the minor mode
;; or sets it (similar to outline minor mode).

;; - Customization
;;
;; With the exception of `hs-special-modes-alist', the following
;; variables and hooks can be edited using `M-x customize-variable':
;;
;;   hs-hide-comments-when-hiding-all -- self-explanatory!
;;   hs-show-hidden-short-form   -- whether or not the last line in a form
;;                                  is omitted (saving screen space)
;;   hs-isearch-open             -- what kind of hidden blocks to open when
;;                                  doing isearch
;;   hs-special-modes-alist      -- keeps at bay hideshow's heuristics with
;;                                  respect to block definitions
;;   hs-buffer-state-saving      -- if/when to keep track of hidden blocks
;;                                  across editing sessions (buffer-local)
;;   hs-state-file               -- where state is saved
;;
;; Hooks are run after some commands:
;;
;;   hs-hide-hook     in      hs-hide-block, hs-hide-all, hs-hide-level
;;   hs-show-hook             hs-show-block, hs-show-all, hs-show-region
;;
;; See docs for each variable or hook for more info.

;; - Suggested usage
;;
;; (load-library "hideshow")
;; (add-hook 'X-mode-hook 'hs-minor-mode)   ; other modes similarly
;;
;; where X = {emacs-lisp,c,c++,perl,...}.  See the doc for the variable
;; `hs-special-modes-alist' if you'd like to use hideshow w/ other modes.

;; - Bugs / caveats
;;
;; 1.  Hideshow does not work w/ emacs 18 because emacs 18 lacks the
;; function `forward-comment' (among other things).  If someone writes
;; this, please send me a copy.
;;
;; 2.  Users of cc-mode.el should not hook hideshow into
;; c-mode-common-hook since at that stage of the call sequence, the
;; variables `comment-start' and `comment-end' are not yet provided.
;; Instead, use c-mode-hook and c++-mode-hook as suggested above.

;; - Thanks and feedback
;;
;; Thanks go to the following people for valuable ideas, code and
;; bug reports.
;;
;;     adahome@ix.netcom.com                 Dean Andrews
;;     alfh@ifi.uio.no                       Alf-Ivar Holm
;;     bauer@itsm.uni-stuttgart.de           Holger Bauer
;;     christoph.conrad@post.rwth-aachen.de  Christoph Conrad
;;     d.love@dl.ac.uk                       Dave Love
;;     gael@gnlab030.grenoble.hp.com         Gael Marziou
;;     jan.djarv@sa.erisoft.se               Jan Djarv
;;     leray@dev-lme.pcc.philips.com         Guillaume Leray
;;     moody@mwt.net                         Moody Ahmad
;;     preston.f.crow@dartmouth.edu          Preston F. Crow
;;     qhslali@aom.ericsson.se               Lars Lindberg
;;     smes@post1.com                        Chew Meng Kuan
;;     sheff@edcsgw2.cr.usgs.gov             Keith Sheffield
;;     tonyl@eng.sun.com                     Tony Lam
;;     ware@cis.ohio-state.edu               Pete Ware
;;
;; Special thanks go to Dan Nicolaescu <dann@ics.uci.edu>, who reimplemented
;; hideshow using overlays (rather than selective display), added isearch
;; magic, folded in custom.el compatibility, generalized comment handling,
;; incorporated mouse support, and maintained the code in general.  Version
;; 4.0 is largely due to his efforts.
;;
;; Correspondance welcome; please indicate version number.

;;; History:

;; Hideshow was inspired when I learned about selective display.  It was
;; reimplemented to use overlays for 4.0 (see above).  WRT past history,
;; entries in the masterfile corresponding to versions 1.x and 2.x have
;; been lost.  XEmacs support is reliable as of 4.29.  State save and
;; restore was added in 3.5 (not widely distributed), and reliable as of
;; 4.30.  Otherwise, the code seems stable.  Passes checkdoc as of 4.32.

;;; Code:

(require 'easymenu)

;;---------------------------------------------------------------------------
;; user-configurable variables

(defgroup hideshow nil
  "Minor mode for hiding and showing program and comment blocks."
  :prefix "hs-"
  :group 'languages)

;;;###autoload
(defcustom hs-hide-comments-when-hiding-all t
  "*Hide the comments too when you do an `hs-hide-all'."
  :type 'boolean
  :group 'hideshow)

;;;###autoload
(defcustom hs-show-hidden-short-form t
  "*Leave only the first line visible in a hidden block.
If non-nil only the first line is visible when a block is in the
hidden state, else both the first line and the last line are shown.
A nil value disables `hs-adjust-block-beginning', which see.

An example of how this works: (in C mode)
original:

  /* My function main
     some more stuff about main
  */
  int
  main(void)
  {
    int x=0;
    return 0;
  }


hidden and `hs-show-hidden-short-form' is nil
  /* My function main...
  */
  int
  main(void)
  {...
  }

hidden and `hs-show-hidden-short-form' is t
  /* My function main...
  int
  main(void)...

For the last case you have to be on the line containing the
ellipsis when you do `hs-show-block'."
  :type 'boolean
  :group 'hideshow)

(defcustom hs-minor-mode-hook 'hs-hide-initial-comment-block
  "*Hook called when function `hs-minor-mode' is installed.
A good value for this would be `hs-hide-initial-comment-block' to
hide all the comments at the beginning of the file."
  :type 'hook
  :group 'hideshow)

(defcustom hs-isearch-open 'block
  "*What kind of hidden blocks to open when doing `isearch'.
One of the following values:

  block   -- open only blocks
  comment -- open only comments
  t       -- open both blocks and comments
  nil     -- open neither blocks nor comments

This has effect iff `search-invisible' is set to `open'."
  :type '(choice (const :tag "open only  blocks" block)
	         (const :tag "open only comments" comment)
	         (const :tag "open both blocks and comments" t)
	         (const :tag "don't open any of them" nil))
  :group 'hideshow)

;;;###autoload
(defvar hs-special-modes-alist
  '((c-mode "{" "}" nil nil hs-c-like-adjust-block-beginning)
    (c++-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning)
    (java-mode "\\(\\(\\([ \t]*\\(\\(abstract\\|final\\|native\\|p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|s\\(tatic\\|ynchronized\\)\\)[ \t\n]+\\)*[.a-zA-Z0-9_:]+[ \t\n]*\\(\\[[ \t\n]*\\][ \t\n]*\\)?\\([a-zA-Z0-9_:]+[ \t\n]*\\)([^)]*)\\([ \n\t]+throws[ \t\n][^{]+\\)?\\)\\|\\([ \t]*static[^{]*\\)\\)[ \t\n]*{\\)" "}" "/[*/]" java-hs-forward-sexp hs-c-like-adjust-block-beginning))
; I (Dan) tested the java regexp using the following:
;(defvar hsj-public)
;(defvar hsj-type)
;(defvar hsj-fname)
;(defvar hsj-par)
;(defvar hsj-throws)
;(defvar hsj-static)

;(setq hsj-public
;      (concat "[ \t]*\\("
;	      (regexp-opt '("public" "private" "protected" "abstract"
;			    "synchronized" "static" "final" "native") 1)
;	      "[ \t\n]+\\)*"))

;(setq hsj-type "[.a-zA-Z0-9_:]+[ \t\n]*\\(\\[[ \t\n]*\\][ \t\n]*\\)?")
;(setq hsj-fname "\\([a-zA-Z0-9_:]+[ \t\n]*\\)")
;(setq hsj-par "([^)]*)")
;(setq hsj-throws "\\([ \n\t]+throws[ \t\n][^{]+\\)?")

;(setq hsj-static "[ \t]*static[^{]*")


;(setq hs-block-start-regexp (concat
;			     "\\("
;			         "\\("
;				      "\\("
;				         hsj-public
;					 hsj-type
;					 hsj-fname
;					 hsj-par
;					 hsj-throws
;				      "\\)"
;				      "\\|"
;				      "\\("
;				         hsj-static
;				      "\\)"
;				 "\\)"
;				    "[ \t\n]*{"
;			      "\\)"
;				    ))

  "*Alist for initializing the hideshow variables for different modes.
It has the form
  (MODE START END COMMENT-START FORWARD-SEXP-FUNC ADJUST-BEG-FUNC).
If present, hideshow will use these values as regexps for start, end
and `comment-start', respectively.  Since Algol-ish languages do not have
single-character block delimiters, the function `forward-sexp' used
by hideshow doesn't work.  In this case, if a similar function is
available, you can register it and have hideshow use it instead of
`forward-sexp'.  See the documentation for `hs-adjust-block-beginning'
to see what is the use of ADJUST-BEG-FUNC.

If any of those is left nil, hideshow will try to guess some values
using function `hs-grok-mode-type'.

Note that the regexps should not contain leading or trailing whitespace.")

;;;###autoload
(defcustom hs-buffer-state-saving nil
  "Per buffer, controls if and when hideshow should save state.
Nil means don't save state.  Otherwise, recognized symbols are:

  on-kill  -- Save state on buffer-kill.
  on-write -- Save state on buffer write (includes `save-buffer').

Any other value is set to nil.  This variable is buffer-local and should
be set before turning on hideshow.  In the case where a file is read-only,
hideshow will interpret `on-write' as `on-kill'.

Hideshow state information is saved in the `hs-state-file', which see.
Upon a later opening of the file, hideshow will automatically attempt
to restore the state of the file.

You may wish to save and restore state manually with the commands
`hs-save-state' and `hs-restore-state', respectively.  In that case,
it is best to set `hs-buffer-state-saving' to nil."
  :type '(choice (const :tag "on buffer kill" on-kill)
		 (const :tag "on buffer write or save" on-write)
		 (const :tag "do not save state" nil))
  :group 'hideshow)

(defcustom hs-state-file "~/.hideshow-state"
  "File where hideshow saves state.
Only relevant if variable `hs-buffer-state-saving' non-nil."
  :type 'file
  :group 'hideshow)

(defvar hs-hide-hook nil
  "*Hooks called at the end of commands to hide text.
These commands include `hs-hide-all', `hs-hide-block' and `hs-hide-level'.")

(defvar hs-show-hook nil
  "*Hooks called at the end of commands to show text.
These commands include `hs-show-all', `hs-show-block' and `hs-show-region'.")

;;---------------------------------------------------------------------------
;; internal variables

(defvar hs-minor-mode nil
  "Non-nil if using hideshow mode as a minor mode of some other mode.
Use the command `hs-minor-mode' to toggle this variable.")

(defvar hs-minor-mode-map nil
  "Mode map for hideshow minor mode.")

(defvar hs-c-start-regexp nil
  "Regexp for beginning of comments.
Differs from mode-specific comment regexps in that
surrounding whitespace is stripped.")

(defvar hs-block-start-regexp nil
  "Regexp for beginning of block.")

(defvar hs-block-end-regexp nil
  "Regexp for end of block.")

(defvar hs-forward-sexp-func 'forward-sexp
  "Function used to do a `forward-sexp'.
Should change for Algol-ish modes.  For single-character block
delimiters -- ie, the syntax table regexp for the character is
either `(' or `)' -- `hs-forward-sexp-func' would just be
`forward-sexp'.  For other modes such as simula, a more specialized
function is necessary.")

(defvar hs-good-points nil
  "Alist mapping files to data interesting only to hideshow.
This entire data structure is the \"state\" that `hs-save-state' and
`hs-restore-state' work with.")

(defvar hs-adjust-block-beginning nil
  "Function used to tweak the block beginning.
It has effect only if `hs-show-hidden-short-form' is non-nil.
The block it is hidden from the point returned by this function,
as opposed to hiding it from the point returned when searching
`hs-block-start-regexp'.  In c-like modes, if we wish to also hide the
curly braces (if you think they occupy too much space on the screen),
this function should return the starting point (at the end of line) of
the hidden region.

It is called with a single argument ARG which is the the position in
buffer after the block beginning.

It should return the position from where we should start hiding.

It should not move the point.

See `hs-c-like-adjust-block-beginning' for an example of using this.")

;;---------------------------------------------------------------------------
;; support funcs

;; xemacs compatibility
(when (string-match "xemacs\\|lucid" emacs-version)
  ;; use pre-packaged compatiblity layer
  (require 'overlay)
  ;; `invisibility-spec' mutators snarfed from Emacs 20.3 lisp/subr.el
  (defun add-to-invisibility-spec (arg)
    (cond
     ((or (null buffer-invisibility-spec) (eq buffer-invisibility-spec t))
      (setq buffer-invisibility-spec (list arg)))
     (t
      (setq buffer-invisibility-spec
	    (cons arg buffer-invisibility-spec)))))
  (defun remove-from-invisibility-spec (arg)
    (if buffer-invisibility-spec
	(setq buffer-invisibility-spec
	      (delete arg buffer-invisibility-spec)))))

(defun hs-flag-region (from to flag)
  "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is non-nil the text is
hidden.  Actually flag is really either `comment' or `block' depending
on what kind of block it is suppose to hide."
    (save-excursion
      (goto-char from)
      (end-of-line)
      (hs-discard-overlays (point) to 'invisible 'hs)
      (if flag
	  (let ((overlay (make-overlay (point) to)))
	    ;; make overlay hidden and intangible
	    (overlay-put overlay 'invisible 'hs)
	    (overlay-put overlay 'hs t)
	    (if (eq flag 'comment)			; remember comments
		(overlay-put overlay 'hs-comment t))	;  (like a PO'ed SO!)
	    (when (or (eq hs-isearch-open t) (eq hs-isearch-open flag))
		(overlay-put overlay 'isearch-open-invisible
			     'hs-isearch-open-invisible))
	    (overlay-put overlay 'intangible t)))))

(defun hs-isearch-open-invisible (ov)
  "Show block at the beginning of overlay OV.
This function is set as an `isearch-open-invisible'
property to hidden overlays."
  (save-excursion
    (goto-char (overlay-start ov))
    (hs-show-block)))

(defun hs-discard-overlays (beg end prop value)
  "Remove from region (BEG END) all overlays with PROP equal to VALUE.
Overlays with a PROP property different from VALUE are not touched."
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (goto-char beg)
    (let ((overlays (overlays-in beg end))
	  o)
      (while overlays
	(setq o (car overlays))
	(if (eq (overlay-get o prop) value)
	    (delete-overlay o))
	  (setq overlays (cdr overlays))))))

(defun hs-hide-block-at-point (&optional end comment-reg)
  "Hide block iff on block beginning.
Optional arg END means reposition at end.
Optional arg COMMENT-REG is a list of the form (BEGIN . END) and
specifies the limits of the comment, or nil if the block is not
a comment."
  (if comment-reg
      (progn
	;; goto the end of line at the end of the comment
	(goto-char (nth 1 comment-reg))
	(unless hs-show-hidden-short-form (forward-line -1))
	(end-of-line)
	(hs-flag-region (car comment-reg)  (point) 'comment)
	(goto-char (if end (nth 1 comment-reg) (car comment-reg))))
      (if (looking-at hs-block-start-regexp)
	  (let* ((p ;; `p' is the point at the end of the block beginning
		  (if (and  hs-show-hidden-short-form
			    hs-adjust-block-beginning)
		      ;; we need to adjust the block beginning
		      (funcall hs-adjust-block-beginning (match-end 0))
		    (match-end 0)))
		 ;; `q' is the point at the end of the block
		 (q (progn (funcall hs-forward-sexp-func 1) (point))))
	    ;; position the point so we can call `hs-flag-region'
	    (unless hs-show-hidden-short-form (forward-line -1))
	    (end-of-line)
	    (if (and (< p (point)) (> (count-lines p q)
				      (if hs-show-hidden-short-form 1 2)))
		(hs-flag-region p (point) 'block))
	    (goto-char (if end q p))))))

(defun hs-show-block-at-point (&optional end comment-reg)
  "Show block iff on block beginning.
Optional arg END means reposition at end.  Optional arg COMMENT-REG
is a list of the forme (BEGIN . END) and specifies the limits of the
comment.  It should be nil when hiding a block."
  (if comment-reg
      (when (car comment-reg)
	(hs-flag-region (car comment-reg) (nth 1 comment-reg) nil)
	(goto-char (if end (nth 1 comment-reg) (car comment-reg))))
    (if (looking-at hs-block-start-regexp)
	(let* ((p (point))
	       (q
		(condition-case error	; probably unbalanced paren
		    (progn
		      (funcall hs-forward-sexp-func 1)
		      (point))
		  (error
		   ;; try to get out of rat's nest and expose the whole func
		   (if (/= (current-column) 0) (beginning-of-defun))
		   (setq p (point))
		   (re-search-forward (concat "^" hs-block-start-regexp)
				      (point-max) t 2)
		   (point)))))
	  (hs-flag-region p q nil)
	  (goto-char (if end (1+ (point)) p))))))

(defun hs-safety-is-job-n ()
  "Warn if `buffer-invisibility-spec' does not contain hs."
    (unless (and (listp buffer-invisibility-spec)
		 (assq 'hs buffer-invisibility-spec))
      (message "Warning: `buffer-invisibility-spec' does not contain hs!!")
      (sit-for 2)))

(defun hs-hide-initial-comment-block ()
  "Hide the first block of comments in a file.
The best usage is in `hs-minor-mode-hook', it hides all the comments at the
file beginning, so if you have huge RCS logs you won't see them!"
  (interactive)
  (let ((p (point))
	c-reg)
    (goto-char (point-min))
    (skip-chars-forward " \t\n^L")
    (setq c-reg (hs-inside-comment-p))
    ;; see if we have enough comment lines to hide
    (if (and c-reg (> (count-lines (car c-reg) (nth 1 c-reg))
		      (if hs-show-hidden-short-form 1 2)))
	(hs-hide-block)
      (goto-char p))))

(defun hs-inside-comment-p ()
  "Return non-nil if point is inside a comment, otherwise nil.
Actually, return a list containing the buffer position of the start
and the end of the comment.  A comment block can be hidden only if on
its starting line there is only whitespace preceding the actual comment
beginning.  If we are inside of a comment but this condition is not met,
we return a list having a nil as its car and the end of comment position
as cdr."
  (save-excursion
    ;; the idea is to look backwards for a comment start regexp, do a
    ;; forward comment, and see if we are inside, then extend extend
    ;; forward and backward as long as we have comments
    (let ((q (point)))
      (when (or (looking-at hs-c-start-regexp)
	      (re-search-backward hs-c-start-regexp (point-min) t))
	(forward-comment (- (buffer-size)))
	(skip-chars-forward " \t\n")
	(let ((p (point))
	      (not-hidable nil))
	  (beginning-of-line)
	  (unless (looking-at (concat "[ \t]*" hs-c-start-regexp))
	    ;; we are in this situation: (example)
	    ;; (defun bar ()
	    ;;      (foo)
	    ;;                ) ; comment
	    ;;                 ^
	    ;;   the point was here before doing (beginning-of-line)
	    ;; here we should advance till the next comment which
	    ;; eventually has only white spaces preceding it on the same
	    ;; line
	    (goto-char p)
	    (forward-comment 1)
	    (skip-chars-forward " \t\n")
	    (setq p (point))
	    (while (and (< (point) q)
			(> (point) p)
			(not (looking-at hs-c-start-regexp)))
	      (setq p (point))  ;; use this to avoid an infinite cycle
	      (forward-comment 1)
	      (skip-chars-forward " \t\n"))
	    (if (or (not (looking-at hs-c-start-regexp))
		    (> (point) q))
		;; we cannot hide this comment block
		(setq not-hidable t)))
	  ;; goto the end of the comment
	  (forward-comment (buffer-size))
	  (skip-chars-backward " \t\n")
	  (end-of-line)
	  (if (>= (point) q)
	      (list (if not-hidable nil p) (point))))))))

(defun hs-grok-mode-type ()
  "Set up hideshow variables for new buffers.
If `hs-special-modes-alist' has information associated with the
current buffer's major mode, use that.
Otherwise, guess start, end and `comment-start' regexps; `forward-sexp'
function; and adjust-block-beginning function."
  (if (and (boundp 'comment-start)
	   (boundp 'comment-end)
	   comment-start comment-end)
      (let ((lookup (assoc major-mode hs-special-modes-alist)))
	(setq hs-block-start-regexp (or (nth 1 lookup) "\\s\(")
	      hs-block-end-regexp (or (nth 2 lookup) "\\s\)")
	      hs-c-start-regexp (or (nth 3 lookup)
				    (let ((c-start-regexp
					   (regexp-quote comment-start)))
				      (if (string-match " +$" c-start-regexp)
					  (substring c-start-regexp
						     0 (1- (match-end 0)))
					c-start-regexp)))
	      hs-forward-sexp-func (or (nth 4 lookup) 'forward-sexp)
	      hs-adjust-block-beginning (nth 5 lookup)))
    (error "%s Mode doesn't support Hideshow Minor Mode" mode-name)))

(defun hs-find-block-beginning ()
  "Reposition point at block-start.
Return point, or nil if at top level."
  (let (done
	(try-again t)
	(here (point))
	(both-regexps (concat "\\(" hs-block-start-regexp "\\)\\|\\("
			      hs-block-end-regexp "\\)"))
	(buf-size (buffer-size)))
    (beginning-of-line)
    ;; A block beginning can span on multiple lines, if the point
    ;; is on one of those lines, trying a regexp search from
    ;; that point would fail to find the block beginning, so we look
    ;; backwards for the block beginning, or a block end.
    (while try-again
      (setq try-again nil)
      (if (and (re-search-backward both-regexps (point-min) t)
	       (match-beginning 1)) ; found a block beginning
	  (if (save-match-data (hs-inside-comment-p))
	      ;; but it was inside a comment, so we must look for it again
	      (setq try-again t)
	    ;; that's what we were looking for
	    (setq done (match-beginning 0)))
	;; we found a block end, or we reached the beginning of the
	;; buffer look to see if we were on a block beginning when we
	;; started
	(if (and
	     (re-search-forward hs-block-start-regexp (point-max) t)
	     (or
	      (and (>= here (match-beginning 0)) (< here (match-end 0)))
	      (and hs-show-hidden-short-form hs-adjust-block-beginning
		   (save-match-data
		     (= 1 (count-lines
			   (funcall hs-adjust-block-beginning
				    (match-end 0)) here))))))
	      (setq done (match-beginning 0)))))
    (goto-char here)
    (while (and (not done)
		;; This had problems because the regexp can match something
		;; inside of a comment!  Since inside a comment we can have
		;; incomplete sexps this would have signaled an error.
		(or (forward-comment (- buf-size)) t)	; `or' is a hack to
							; make it return t
		(re-search-backward both-regexps (point-min) t))
      (if (match-beginning 1)		; start of start-regexp
	  (setq done (match-beginning 0))
	(goto-char (match-end 0))	; end of end-regexp
	(funcall hs-forward-sexp-func -1)))
    (goto-char (or done here))
    done))

(defun hs-hide-level-recursive (arg minp maxp)
  "Recursively hide blocks ARG levels below point in region (MINP MAXP)."
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (forward-sexp)
    (setq maxp (1- (point))))
  (hs-flag-region minp maxp ?\n)	; eliminate weirdness
  (goto-char minp)
  (while (progn
	   (forward-comment (buffer-size))
	   (re-search-forward hs-block-start-regexp maxp t))
    (if (> arg 1)
	(hs-hide-level-recursive (1- arg) minp maxp)
      (goto-char (match-beginning 0))
      (hs-hide-block-at-point t)))
    (hs-safety-is-job-n)
  (goto-char maxp))

(defmacro hs-life-goes-on (&rest body)
  "Execute optional BODY iff variable `hs-minor-mode' is non-nil."
  `(let ((inhibit-point-motion-hooks t))
     (when hs-minor-mode
       ,@body)))

(put 'hs-life-goes-on 'edebug-form-spec '(&rest form))

(defun hs-already-hidden-p ()
  "Return non-nil if point is in an already-hidden block, otherwise nil."
  (save-excursion
    (let ((c-reg (hs-inside-comment-p)))
      (if (and c-reg (nth 0 c-reg))
	  ;; point is inside a comment, and that comment is hidable
	  (goto-char (nth 0 c-reg))
	(if (and (not c-reg) (hs-find-block-beginning)
		 (looking-at hs-block-start-regexp))
	    ;; point is inside a block
	    (goto-char (match-end 0)))))
    (end-of-line)
    (let ((overlays (overlays-at (point)))
	  (found nil))
      (while (and (not found) (overlayp (car overlays)))
	   (setq found (overlay-get (car overlays) 'hs)
		 overlays (cdr overlays)))
      found)))

(defun java-hs-forward-sexp (arg)
  "Function used by hideshow minor-mode for `forward-sexp' in Java mode.
ARG is interpreted as `forward-sexp' would."
  (if (< arg 0)
      (backward-sexp 1)
    (if (looking-at hs-block-start-regexp)
	(progn
	  (goto-char (match-end 0))
	  (forward-char -1)
	  (forward-sexp 1))
      (forward-sexp 1))))

(defun hs-c-like-adjust-block-beginning (arg)
  "Function to be assigned to `hs-adjust-block-beginning' for C-like modes.
ARG is a position in buffer just after {.  This goes back to the end of
the function header.  The purpose is to save some space on the screen
when displaying hidden blocks."
  (save-excursion
    (goto-char arg)
    (forward-char -1)
    (forward-comment (- (buffer-size)))
    (point)))

(defun hs-state-sync ()
  "Write hideshow state to `hs-state-file'."
  (save-excursion
    (set-buffer (find-file hs-state-file))
    (widen)
    (delete-region (point-min) (point-max))
    (insert "(setq hs-good-points\n '")
    (let ((standard-output (current-buffer)))
      (prin1 hs-good-points))
    (insert "\n)\n")
    (save-buffer 0)
    (kill-buffer (current-buffer))))

(defun hs-state-init ()
  "Read `hs-state-file' if `hs-buffer-state-saving' non-nil."
  (when hs-buffer-state-saving
    (unless (file-exists-p hs-state-file)
      (hs-state-sync))
    (load hs-state-file)))

(defun hs-state-set (file data)
  "Remember entry (FILE DATA), and then do `hs-state-sync'."
  (let ((try (assoc file hs-good-points)))
    (if try
	(setcdr try data)
      (setq hs-good-points (cons (cons file data) hs-good-points))))
  (hs-state-sync))

(defun hs-state-get (file)
  "Read `hs-state-file', then search for FILE in state alist."
  (load hs-state-file)
  (assoc file hs-good-points))

(defun hs-state-remove (file)
  "Remove FILE from state alist, then do `hs-state-sync'."
  (when (assoc file hs-good-points)
    (if (and hs-good-points (string= (caar hs-good-points) file))
	(setq hs-good-points (cdr hs-good-points))
      (let ((walker hs-good-points))
	(while walker
	  (if (string= (caadr walker) file)
	      (setcdr walker (cddr walker)))
	  (setq walker (cdr walker)))))
    (hs-state-sync)))


;;---------------------------------------------------------------------------
;; commands

;;;###autoload
(defun hs-hide-all ()
  "Hide all top level blocks, displaying only first and last lines.
Move point to the beginning of the line, and it run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'.
If `hs-hide-comments-when-hiding-all' is t, also hide the comments."
  (interactive)
  (hs-life-goes-on
   (message "Hiding all blocks ...")
   (save-excursion
     (hs-flag-region (point-min) (point-max) nil) ; eliminate weirdness
     (goto-char (point-min))
     (if hs-hide-comments-when-hiding-all
	 (let (c-reg
	       (count 0)
	       (block-and-comment-re  ;; this should match
		(concat "\\(^"        ;; the block beginning and comment start
			hs-block-start-regexp
			"\\)\\|\\(" hs-c-start-regexp "\\)")))
	   (while (re-search-forward block-and-comment-re (point-max) t)
	     (if (match-beginning 1) ;; we have found a block beginning
		 (progn
		   (goto-char (match-beginning 1))
		   (hs-hide-block-at-point t)
		   (message "Hiding ... %d" (setq count (1+ count))))
	       ;;found a comment
	       (setq c-reg (hs-inside-comment-p))
	       (if (and c-reg (car c-reg))
		   (if (> (count-lines (car c-reg) (nth 1 c-reg))
			  (if hs-show-hidden-short-form 1 2))
		       (progn
			 (hs-hide-block-at-point t c-reg)
			 (message "Hiding ... %d" (setq count (1+ count))))
		     (goto-char (nth 1 c-reg)))))))
       (let ((count 0)
	     (top-level-re (concat "^" hs-block-start-regexp))
	     (buf-size (buffer-size)))
	 (while
	     (progn
	       (forward-comment buf-size)
	       (re-search-forward top-level-re (point-max) t))
	   (goto-char (match-beginning 0))
	   (hs-hide-block-at-point t)
	   (message "Hiding ... %d" (setq count (1+ count))))))
   (hs-safety-is-job-n))
   (beginning-of-line)
   (message "Hiding all blocks ... done")
   (run-hooks 'hs-hide-hook)))

(defun hs-show-all ()
  "Show all top level blocks.
Point is unchanged; run the normal hook `hs-show-hook'.
See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "Showing all blocks ...")
   (hs-flag-region (point-min) (point-max) nil)
   (message "Showing all blocks ... done")
   (run-hooks 'hs-show-hook)))

(defun hs-hide-block (&optional end)
  "Select a block and hide it.
With prefix arg, reposition at END.  Block is defined as a sexp for
Lispish modes, mode-specific otherwise.  Comments are blocks, too.
Upon completion, point is repositioned and the normal hook
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (cond
      ((and c-reg (or (null (nth 0 c-reg))
		      (<= (count-lines (car c-reg) (nth 1 c-reg))
			  (if hs-show-hidden-short-form 1 2))))
	   (message "Not enough comment lines to hide!"))
      ((or c-reg (looking-at hs-block-start-regexp)
	       (hs-find-block-beginning))
       (hs-hide-block-at-point end c-reg)
       (hs-safety-is-job-n)
       (run-hooks 'hs-hide-hook))))))

(defun hs-show-block (&optional end)
  "Select a block and show it.
With prefix arg, reposition at END.  Upon completion, point is
repositioned and the normal hook `hs-show-hook' is run.
See documentation for `hs-hide-block' and `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (if (or c-reg
	     (looking-at hs-block-start-regexp)
	     (hs-find-block-beginning))
	   (progn
	     (hs-show-block-at-point end c-reg)
	     (hs-safety-is-job-n)
	     (run-hooks 'hs-show-hook))))))

(defun hs-show-region (beg end)
  "Show all lines from BEG to END, without doing any block analysis.
Note: `hs-show-region' is intended for use when `hs-show-block' signals
\"unbalanced parentheses\" and so is an emergency measure only.  You may
become very confused if you use this command indiscriminately."
  (interactive "r")
  (hs-life-goes-on
   (hs-flag-region beg end nil)
   (hs-safety-is-job-n)
   (run-hooks 'hs-show-hook)))

(defun hs-hide-level (arg)
  "Hide all blocks ARG levels below this block."
  (interactive "p")
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (hs-hide-level-recursive arg (point-min) (point-max))
     (message "Hiding blocks ... done"))
   (hs-safety-is-job-n)
   (run-hooks 'hs-hide-hook)))

;; Hideshow state consists of a list of relative offsets, with subsequent
;; pairs denoting non-overlapping overlay boundaries.  By using offsets rather
;; than buffer positions, we save space but require a symmetric decode when
;; doing state restore.  Lastly, hidden comments need to retain their comment
;; property (see `hs-flag-region') so the symbol `c' is used as a leading tag.

(defun hs-save-state ()
  "Save hideshow state.  See variable `hs-buffer-state-saving'."
  (interactive)
  (hs-life-goes-on
   (let ((pmax (point-max))
	 ;; Init `p' to 1 rather than `(point-min)' to disregard narrowing.
	 ;; When the buffer is not narrowed, (point-min) => 1.  We don't use
	 ;; `(save-restriction (widen) ...)' because Emacs can get confused.
	 (p 1)
	 slist opos o)
     (save-excursion
       (goto-char (point-min))
       (while (/= (setq opos (next-overlay-change (point))) pmax)
	 (if (setq o (let ((look (overlays-at opos)))
		       (and look
			    (catch 'done
			      (or look (throw 'done nil))
			      (if (overlay-get (car look) 'hs)
				  (throw 'done (car look))
				(setq look (cdr look)))))))
	     (let* ((o (if (listp o) (car o) o))		; kludge!
		    (beg (overlay-start o))
		    (end (overlay-end   o)))
	       ;; push then reverse later
	       (if (overlay-get o 'hs-comment)			; comment?
		   (setq slist (cons 'c slist)))
	       (setq slist (cons (- beg p) slist))		; beginning
	       (setq slist (cons (- end beg) slist))		; end
	       (goto-char (setq p (1+ end))))
	   (goto-char (1+ opos)))))
     (if (null slist)
	 (progn
	   (message "")
	   (hs-state-remove buffer-file-name))
       (message "Saving hideshow state ...")
       (hs-state-set buffer-file-name (nreverse slist))
       (message "Saving hideshow state ... done")))))

(defun hs-restore-state ()
  "Restore hideshow state.  See variable `hs-buffer-state-saving'."
  (interactive)
  (hs-life-goes-on
   (let ((slist (cdr (hs-state-get buffer-file-name)))
	 (p 1)			; see "`p' init" comment in `hs-save-state'
	 flag)
     (when slist
       (message "Restoring hideshow state ...")
       (save-excursion
	 (hs-discard-overlays (point-min) (point-max) 'hs t)
	 (while slist
	   (setq flag (if (eq (car slist) 'c)			; comment?
			  (progn
			    (setq slist (cdr slist))
			    'comment)
			'block))
	   (let* ((beg (+ p (car slist)))			; beginning
		  (end (+ beg (cadr slist))))			; end
	     (hs-flag-region beg end flag)
	     (setq p (1+ end)))
	   (setq slist (cddr slist))))
       (message "Restoring hideshow state ... done")))))

;;;###autoload
(defun hs-mouse-toggle-hiding (e)
  "Toggle hiding/showing of a block.
Should be bound to a mouse key.
Argument E is a mouse event used by `mouse-set-point'."
  (interactive "@e")
  (mouse-set-point e)
  (if (hs-already-hidden-p)
      (hs-show-block)
    (hs-hide-block)))

;;;###autoload
(defun hs-minor-mode (&optional arg)
  "Toggle hideshow minor mode.
With ARG, turn hideshow minor mode on if ARG is positive, off otherwise.
When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.
The variable `kill-buffer-hook' is made buffer-local and `hs-save-state'
is added to it.
The value '(hs . t) is added to `buffer-invisibility-spec'.
Last, the normal hook `hs-minor-mode-hook' is run; see the doc
for `run-hooks'.

The main commands are: `hs-hide-all', `hs-show-all', `hs-hide-block',
`hs-show-block', `hs-hide-level' and `hs-show-region'.
Also see the documentation for the variable `hs-show-hidden-short-form'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands.

Key bindings:
\\{hs-minor-mode-map}"

  (interactive "P")
  (setq hs-minor-mode
        (if (null arg)
	    (not hs-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if hs-minor-mode
      (progn
	(easy-menu-add hs-minor-mode-menu)
	(make-variable-buffer-local 'line-move-ignore-invisible)
	(setq line-move-ignore-invisible t)
	(add-to-invisibility-spec '(hs . t))		; hs invisible
	;; state stuff
	(cond ((null hs-buffer-state-saving))
	      ((eq 'on-kill hs-buffer-state-saving)
	       (add-hook 'kill-buffer-hook 'hs-save-state))
	      ((eq 'on-write hs-buffer-state-saving)
	       (add-hook (if buffer-read-only
			     'kill-buffer-hook
			   'local-write-file-hooks)
			 'hs-save-state))
	      (t
	       (setq hs-buffer-state-saving nil))) ; jam it -- why not?
	(hs-grok-mode-type)
	(run-hooks 'hs-minor-mode-hook))
    (easy-menu-remove hs-minor-mode-menu)
    (remove-from-invisibility-spec '(hs . t))))

;;---------------------------------------------------------------------------
;; load-time actions

;; keymaps and menus
(if hs-minor-mode-map
    nil
  (setq hs-minor-mode-map (make-sparse-keymap))
  (easy-menu-define hs-minor-mode-menu
    hs-minor-mode-map
    "Menu used when hideshow minor mode is active."
    (cons "Hide/Show"
	  (mapcar
	   ;; Interpret each table entry as follows: first, populate keymap
	   ;; with elements 2 and 1; then, for easymenu, use entry directly
	   ;; unless element 0 is nil, whereby the entry is "omitted".
	   (lambda (ent)
	     (define-key hs-minor-mode-map (aref ent 2) (aref ent 1))
	     (if (aref ent 0) ent "-----"))
	   ;; I believe there is nothing bound on these keys.
	   ;; menu entry      command                key
	   '(["Hide Block"    hs-hide-block          "\C-ch"]
	     ["Show Block"    hs-show-block          "\C-cs"]
	     ["Hide All"      hs-hide-all            "\C-cH"]
	     ["Show All"      hs-show-all            "\C-cS"]
	     ["Hide Level"    hs-hide-level          "\C-cL"]
	     ["Save State"    hs-save-state          "\C-co"]
	     ["Restore State" hs-restore-state       "\C-ci"]
	     ["Show Region"   hs-show-region         "\C-cR"]
	     [nil             hs-mouse-toggle-hiding [(shift button2)]]
	     )))))

;; some housekeeping
(or (assq 'hs-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'hs-minor-mode hs-minor-mode-map)
                minor-mode-map-alist)))
(or (assq 'hs-minor-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
                                   (list '(hs-minor-mode " hs")))))

;; make some variables permanently buffer-local
(mapcar (lambda (var)
	  (make-variable-buffer-local var)
	  (put var 'permanent-local t))
	'(hs-minor-mode
	  hs-c-start-regexp
	  hs-block-start-regexp
	  hs-block-end-regexp
	  hs-forward-sexp-func
	  hs-adjust-block-beginning
	  hs-buffer-state-saving))

;; state save/restore support
(or hs-good-points
    (hs-state-init))

;;---------------------------------------------------------------------------
;; that's it

(provide 'hideshow)

;;; hideshow.el ends here

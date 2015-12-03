;; LCD-ENTRY:    id-select.el|Bob Weiner|weiner@infodock.com|Syntactical region selecting|10/27/96|1.3|
;;
;; FILE:         id-select.el
;; SUMMARY:      Select larger and larger syntax-driven regions in a buffer.
;; USAGE:        XEmacs and Emacs Lisp Library
;; KEYWORDS:     matching, mouse
;;
;; AUTHOR:       Bob Weiner
;;                 derived in minute part from Martin Boyer's imouse package
;;                 and from Heinz Schmidt's thing.el within the sky-mouse package
;; ORG:          InfoDock Associates
;;
;; ORIG-DATE:    19-Oct-96 at 02:25:27
;; LAST-MOD:     27-Oct-96 at 02:15:38 by Bob Weiner
;;
;; Copyright (C) 1996  Free Software Foundation, Inc.
;; Copyright (C) 1991  International Computer Science Institute
;;
;; This file is part of InfoDock.
;; It is available for use and distribution under the terms of the GNU Public
;; License.
;;
;; DESCRIPTION:  
;;
;;   This is a radically cool, drop in mouse and keyboard-based library for
;;   selecting successively bigger syntactical regions  within a buffer.  Simply
;;   load this library and you are ready to try it out by double-clicking
;;   on various kinds of characters in different buffer major modes.  You'll
;;   quickly get the hang of it.
;;   
;;   A great deal of smarts are built-in so that it does the right thing almost
;;   all of the time; many other attempts at similar behavior such as thing.el
;;   fail to deal with many file format complexities.
;;   
;;   Double clicks of the Selection Key (left mouse key) at the same point will
;;   select bigger and bigger regions with each successive use.  The first double
;;   click selects a region based upon the character at the point of the click.
;;   For example, with the point over an opening or closing grouping character,
;;   such as { or }, the whole grouping is selected, e.g. a C function.  When on
;;   an _ or - within a programming language variable name, the whole name is
;;   selected.  The type of selection is displayed in the minibuffer as feedback.
;;   When using a language based mainly on indenting, like Bourne shell, a double
;;   click on the first alpha character of a line, such as an if statement,
;;   selects the whole statement.
;;
;;   ---------------
;;
;;   This whole package is driven by a single function, available in mouse
;;   and keyboard forms, that first marks a region based on the syntax
;;   category of the character following point.  Successive invocations mark
;;   larger and larger regions until the whole buffer is marked.  See 
;;   the documentation for the function, id-select-syntactical-region, for
;;   the kinds of syntax categories handled.
;;
;;   Loading this package automatically installs its functionalty on
;;   double-clicks (or higher) of the left mouse key.  (See the documentation
;;   for the variable, mouse-track-click-hook, for how this is done.)  A single
;;   click of the left button will remove the region and reset point.
;;
;;   The function, id-select-thing, may be bound to a key, {C-c C-m} seems to
;;   be a reasonable choice, to provide the same syntax-driven region
;;   selection functionality.  Use {C-g} to unmark the region when done.
;;   Use, id-select-thing-with-mouse, if you want to bind this to a mouse key
;;   and thereby use single clicks instead of double clicks.
;;
;;   Two other related commands are also provided:
;;    id-select-and-copy-thing - mark and copy the syntactical unit to the kill ring
;;    id-select-and-kill-thing - kill the syntactical unit at point
;;
;;   ---------------
;;
;;   To autoload this package under XEmacs or InfoDock via mouse usage, add
;;   the following line to one of your initialization files.  (Don't do this
;;   for GNU Emacs.)
;;
;;      (add-hook 'mouse-track-click-hook 'id-select-double-click-hook)
;;
;;   For any version of Emacs you should add the following autoload entries
;;   at your site:
;;
;;      (autoload 'id-select-and-kill-thing    "id-select" "Kill syntactical region selection" t)
;;      (autoload 'id-select-and-copy-thing    "id-select" "Select and copy syntactical region" t)
;;      (autoload 'id-select-double-click-hook "id-select" "Double mouse click syntactical region selection" nil)
;;      (autoload 'id-select-thing             "id-select" "Keyboard-driven syntactical region selection" t)
;;      (autoload 'id-select-thing-with-mouse  "id-select" "Single mouse click syntactical region selection" t)
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar id-select-brace-modes
  '(c++-mode c-mode java-mode objc-mode perl-mode tcl-mode)
  "*List of language major modes which define things with brace delimiters.")

(defvar id-select-text-modes
  '(fundamental-mode kotl-mode indented-text-mode Info-mode outline-mode text-mode)
  "*List of textual modes where paragraphs may be outdented or indented.")

(defvar id-select-indent-modes
  (append '(asm-mode csh-mode eiffel-mode ksh-mode python-mode pascal-mode
	    sather-mode)
	  id-select-text-modes)
  "*List of language major modes which use mostly indentation to define syntactic structure.")

(defvar id-select-indent-non-end-regexp-alist
  '((csh-mode    "\\(\\|then\\|elsif\\|else\\)[ \t]*$")
    (eiffel-mode "\\(\\|then\\|else if\\|else\\)[ \t]*$")
    (ksh-mode    "\\(\\|then\\|elif\\|else\\)[ \t]*$")
    (pascal-mode "\\(\\|then\\|else\\)[ \t]*$")
    (python-mode "[ \t]*$")
    (sather-mode "\\(\\|then\\|else if\\|else\\)[ \t]*$")
    ;;
    (fundamental-mode "[^ \t\n]")
    (kotl-mode "[^ \t\n]")
    (indented-text-mode "[^ \t\n]")
    (Info-mode "[^ \t\n]")
    (outline-mode "[^\\*]")
    (text-mode  "[^ \t\n]")
    )
  "List of (major-mode . non-terminator-line-regexp) elements used to avoid early dropoff when marking indented code.")

(defvar id-select-indent-end-regexp-alist
  '((csh-mode "end\\|while")
    (eiffel-mode "end")
    (ksh-mode "\\(fi\\|esac\\|until\\|done\\)[ \t\n]")
    (pascal-mode "end")
    (sather-mode "end")
    ;;
    (fundamental-mode "[ \t]*$")
    (indented-text-mode "[ \t]*$")
    (Info-mode "[ \t]*$")
    (text-mode  "[ \t]*$")
    )
  "List of (major-mode . terminator-line-regexp) elements used to include a final line when marking indented code.")

(defvar id-select-char-p t
  "*If t, return single character boundaries when all else fails.")

(defvar id-select-display-type t
  "*If t, display the thing selected with each mouse click.")

(defvar id-select-whitespace t
  "*If t, groups of whitespace are considered as things.")

(if (string-match "XEmacs" emacs-version)
    (add-hook 'mouse-track-click-hook 'id-select-double-click-hook)
  (if (string-match "^19\\." emacs-version)
      (progn (transient-mark-mode 1)
	     (global-set-key [mouse-1] 'mouse-set-point)
	     (global-set-key [double-mouse-1] 'id-select-thing-with-mouse)
	     (global-set-key [triple-mouse-1] 'id-select-thing-with-mouse))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;
;; Commands
;;

;;;###autoload
(defun id-select-thing ()
  "Mark the region selected by the syntax of the thing at point.
If invoked repeatedly, selects bigger and bigger things.
If 'id-select-display-type' is non-nil, the type of selection is displayed in
the minibuffer."
  (interactive
   (cond ((and (fboundp 'region-active-p) (region-active-p))
	  nil)
	 ((and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
	  nil)
	 (t
	  ;; Reset selection based on the syntax of character at point.
	  (id-select-reset)
	  nil)))
  (let ((region (id-select-boundaries (point))))
    (if region
	(progn (goto-char (car region))
	       (set-mark (cdr region))
	       (if (fboundp 'activate-region) (activate-region))
	       (if (and (boundp 'transient-mark-mode)
			transient-mark-mode)
		   (setq mark-active t))
	       (and (interactive-p) id-select-display-type
		    (message "%s" id-select-previous))
	       (run-hooks 'id-select-thing-hook)
	       t))))

;;;###autoload
(defun id-select-thing-with-mouse (event)
  "Select a region based on the syntax of the character from a mouse click.
If the click occurs at the same point as the last click, select
the next larger syntactic structure.  If 'id-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer."
  (interactive "@e")
  (cond ((and (eq id-select-prior-point (point))
	      (eq id-select-prior-buffer (current-buffer)))
	 ;; Prior click was at the same point as before, so enlarge
	 ;; selection to the next bigger item.
	 (if (and (id-select-bigger-thing) id-select-display-type)
	     (message "%s" id-select-previous))
	 t)
	(t (setq this-command 'mouse-start-selection)
	   (id-select-reset)
	   (id-select-thing-with-mouse event))))


;;;###autoload
(defun id-select-and-copy-thing ()
  "Copy the region surrounding the syntactical unit at point."
  (interactive)
  (let ((bounds (id-select-boundaries (point))))
    (if bounds (copy-region-as-kill (car bounds) (cdr bounds)))))

;;;###autoload
(defun id-select-and-kill-thing ()
  "Kill the region surrounding the syntactical unit at point."
  (interactive "*")
  (let ((bounds (id-select-boundaries (point))))
    (if bounds (kill-region (car bounds) (cdr bounds)))))


;;
;; Functions
;;

(defun id-select-boundaries (pos)
  "Return the (start . end) of a syntactically defined region based upon the last region selected or on position POS.
The character at POS is selected if no other thing is matched."
  (interactive)
  (setq zmacs-region-stays t)
  (setcar id-select-old-region (car id-select-region))
  (setcdr id-select-old-region (cdr id-select-region))
  (let ((prior-type id-select-previous))
    (cond ((eq id-select-previous 'char)
	   (id-select-syntactical-region pos))
	  ((and (car id-select-old-region)
		(memq id-select-previous
		      '(sexp sexp-start sexp-end sexp-up))
		(id-select-sexp-up pos)
		(id-select-region-bigger-p id-select-old-region id-select-region))
	   id-select-region)
	  ;;
	  ;; In the general case, we can't know ahead of time what the next
	  ;; biggest type of thing to select is, so we test them all and choose
	  ;; the best fit.  This means that dynamically, the order of type
	  ;; selection will change based on the buffer context.
	  (t (let ((min-region (1+ (- (point-max) (point-min))))
		   (result)
		   region region-size)
	       (mapcar
		(function
		 (lambda (sym-func)
		   (setq region
			 (if (car (cdr sym-func))
			     (funcall (car (cdr sym-func)) pos)))
		   (if (and region (car region)
			    (id-select-region-bigger-p
			     id-select-old-region region)
			    (setq region-size
				  (- (cdr region) (car region)))
			    (< region-size min-region))
		       (setq min-region region-size
			     result 
			     (list ;; The actual selection type is
				   ;; sometimes different than the one we
				   ;; originally tried, so recompute it here.
				   (car (assq id-select-previous
					      id-select-bigger-alist))
				   (car region) (cdr region))))))
		id-select-bigger-alist)
	       (if result
		   ;; Returns id-select-region
		   (progn (setq id-select-previous (car result))
			  (id-select-set-region (nth 1 result) (nth 2 result)))
		 ;;
		 ;; Restore prior selection type since we failed to find a
		 ;; new one.
		 (setq id-select-previous prior-type)
		 (beep)
		 (message
		  "(id-select-boundaries): `%s' is the largest selectable region"
		  id-select-previous)
		 nil))))))

;;;###autoload
(defun id-select-double-click-hook (event click-count)
  "Select a region based on the syntax of the character wherever the mouse is multi-clicked.
If the multi-click occurs at the same point as the last multi-click, select
the next larger syntactic structure.  If id-select-display-type is non-nil,
the type of selection is displayed in the minibuffer."
  (cond ((= click-count 1)
	 ;; Return nil so any other hooks are performed.
	 nil)
	(t (id-select-thing-with-mouse event))))

(defun id-select-syntactical-region (pos)
  "Return the (start . end) of a syntactically defined region based upon the buffer position POS.
Uses 'id-select-syntax-alist' and the current buffer's syntax table to
determine syntax groups.

Typically:
 Open or close grouping character syntax marks an s-expression.
 The end of a line marks the line, including its trailing newline.
 Word syntax marks the current word.
 Symbol syntax (such as _ or - ) marks a symbol.
 The fallback default is to mark the character at POS.

If an error occurs during syntax scanning, it returns nil."
  (interactive "d")
  (setq id-select-previous 'char)
  (if (save-excursion (goto-char pos) (eolp))
      (id-select-line pos)
    (let* ((syntax (char-syntax (if (eobp) (preceding-char) (char-after pos))))
           (pair (assq syntax id-select-syntax-alist)))
      (cond ((and pair
		  (or id-select-whitespace
		      (not (eq (car (cdr pair)) 'thing-whitespace))))
             (funcall (car (cdr pair)) pos))
            (id-select-char-p
             (setq id-select-previous 'char)
             (id-select-set-region pos (1+ pos)))
            (t
             nil)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun id-select-region-bigger-p (old-region new-region)
  "Return t if OLD-REGION is smaller than NEW-REGION and NEW-REGION partially overlaps OLD-REGION, or if OLD-REGION is uninitialized."
  (if (null (car old-region))
      t
    (and (> (abs (- (cdr new-region) (car new-region)))
       (abs (- (cdr old-region) (car old-region))))
	 ;; Ensure the two regions intersect.
	 (or (and (<= (min (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region)))
		  (>  (max (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region))))
	     (and (>  (min (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region)))
		  (<= (min (cdr new-region) (car new-region))
		      (max (cdr old-region) (car old-region))))))))

(defun id-select-bigger-thing ()
  "Select a bigger object where point is."
  (prog1
      (id-select-thing)
    (setq this-command 'select-thing)))

(defun id-select-reset ()
  ;; Reset syntactic selection.
  (setq id-select-prior-point (point)
	id-select-prior-buffer (current-buffer)
	id-select-previous 'char)
  (id-select-set-region nil nil))

(defun id-select-set-region (beginning end)
  "Make BEGINNING the car and END the cdr of the cons cell in the
variable 'id-select-region'.  Return the updated cons cell."
  (setcar id-select-region beginning)
  (setcdr id-select-region end)
  (if (and (null beginning) (null end))
      (progn (setcar id-select-old-region nil)
	     (setcdr id-select-old-region nil)))
  (if (and (not (eq id-select-previous 'buffer))
	   (integerp beginning) (integerp end)
	   (= beginning (point-min)) (= end (point-max)))
      ;; If we selected the whole buffer, make sure that 'thing' type is 'buffer'.
      nil
    id-select-region))

(defun id-select-string-p (&optional start-delim end-delim)
  "Returns (start . end) of string whose first line point is within or immediately before.
Positions include delimiters.  String is delimited by double quotes unless
optional START-DELIM and END-DELIM (strings) are given.
Returns nil if not within a string."
  (let ((opoint (point))
	(count 0)
	bol start start-regexp end-regexp)
    (or start-delim (setq start-delim "\""))
    (or end-delim (setq end-delim "\""))
    ;; Special case for the empty string.
    (if (looking-at (concat (regexp-quote start-delim) (regexp-quote end-delim)))
	(id-select-set-region (point) (match-end 0))
      (setq start-regexp (concat "\\(^\\|[^\\]\\)\\("
				 (regexp-quote start-delim) "\\)")
	    end-regexp   (concat "[^\\]\\(" (regexp-quote end-delim) "\\)")
	    delim-regexp (concat start-regexp "\\|" end-regexp))
      (save-excursion
	(beginning-of-line)
	(setq bol (point))
	(while (re-search-forward delim-regexp opoint t)
	  (setq count (1+ count))
	  ;; This is so we don't miss the closing delimiter of an empty string.
	  (if (and (= (point) (1+ bol))
		   (looking-at (regexp-quote end-delim)))
	      (setq count (1+ count))
	    (if (bobp) nil (backward-char 1))))
	(goto-char opoint)
	;; If found an even # of starting and ending delimiters before opoint,
	;; then opoint is at the start of a string, where we want it.
	(if (zerop (mod count 2))
	    (if (bobp) nil (backward-char 1))
	  (re-search-backward start-regexp nil t))
	;; Point is now before the start of the string.
	(if (re-search-forward start-regexp nil t)
	    (progn
	      (setq start (match-beginning 2))
	      (if (re-search-forward end-regexp nil t)
		  (id-select-set-region start (point)))))))))

;;;
;;; Code selections
;;;

(defun id-select-brace-def (pos)
  "If POS is at the first character, opening brace or closing brace of a brace delimited language definition, return (start . end) region, else nil.
The major mode for each supported brace language must be included in the
list, id-select-brace-modes."
  (interactive)
  (save-excursion
    (goto-char pos)
    (if (and (featurep 'cc-mode) (memq major-mode id-select-brace-modes))
	(let ((at-def-brace (or (looking-at "^{") (looking-at "^}")))
	      (opoint (point))
	      ;; there should be a c-point position for 'eod
	      (eod  (save-excursion (end-of-defun) (point)))
	      (state (c-parse-state))
	      brace)
	  (while state
	    (setq brace (car state))
	    (if (consp brace)
		(goto-char (cdr brace))
	      (goto-char brace))
	    (setq state (cdr state)))
	  (if (= (following-char) ?{)
	      (progn
		(forward-line -1)
		(while (not (or (bobp)
				(looking-at "[ \t\f]*$")))
		  (forward-line -1)))
	    (forward-line 1)
	    (skip-chars-forward " \t\f\n\r"))
	  (if (or at-def-brace (= (point) opoint))
	      ;; Mark the whole definition
	      (progn
		(setq id-select-previous 'brace-def)
		(id-select-set-region (point) eod))
	    (goto-char opoint)
	    nil)))))

(defun id-select-indent-def (pos)
  "If POS is at the first alpha character on a line, return (start . end) region,

The major mode for each supported indented language must be included in the
list, id-select-indent-modes."
  (interactive)
  (save-excursion
    (if (and (memq major-mode id-select-indent-modes)
	     ;; Use this function only if point is on the first non-blank
	     ;; character of a block, whatever a block is for the current mode.
	     (cond ((eq major-mode 'kotl-mode)
		    (and (looking-at "[1-9*]") (not (kview:valid-position-p))))
		   ((or (eq major-mode 'outline-mode) selective-display)
		    (save-excursion (beginning-of-line) (looking-at outline-regexp)))
		   ;; After indent in any other mode, must be on an alpha 
		   ;; or symbol-constituent character.
		   (t (looking-at "[a-zA-z]\\|\\s_")))
	     ;; Must be at the first non-whitespace character in the line.
	     (= (point) (save-excursion (back-to-indentation) (point))))
	(let* ((start-col (current-column))
	       (opoint (if (eq major-mode 'kotl-mode)
			   (progn (kotl-mode:to-valid-position) (point))
			 (beginning-of-line) (point))))
	  (while (and (zerop (forward-line 1))
		      (bolp)
		      (or (progn (back-to-indentation)
				 (> (current-column) start-col))
			  ;; If in a text mode, allow outdenting, otherwise
			  ;; only include special lines here indented to the
			  ;; same point as the original line.
			  (and (or (memq major-mode id-select-text-modes)
				   (= (current-column) start-col))
			       (looking-at
				(or (car (cdr (assq
					       major-mode
					       id-select-indent-non-end-regexp-alist)))
				    "\\'"))))))
	  (if (and (looking-at
		    (or (car (cdr (assq major-mode
					id-select-indent-end-regexp-alist)))
			"\\'"))
		   (or (memq major-mode id-select-text-modes)
		       (= (current-column) start-col)))
	      (forward-line 1))
	  (beginning-of-line)
	  ;; Mark the whole definition
	  (setq id-select-previous 'indent-def)
	  (id-select-set-region opoint (point))))))

(defun id-select-symbol (pos)
  "Return the start and end of a symbol at POS."
  ;; Test for indented def here since might be on an '*' representing
  ;; an outline entry, in which case we mark entries as indented blocks.
  (or (id-select-indent-def pos)
      (save-excursion
	(if (memq (char-syntax (if (eobp) (preceding-char) (char-after pos)))
		  '(?w ?_))
	    (progn (setq id-select-previous 'symbol)
		   (condition-case ()
		       (let ((end (scan-sexps pos 1)))
			 (id-select-set-region (min pos (scan-sexps end -1)) end))
		     (error nil)))))))

(defun id-select-sexp-start (pos)
  "Return start and end of sexp starting at POS."
  (or (id-select-brace-def pos)
      (save-excursion
	(setq id-select-previous 'sexp-start)
	(condition-case ()
	    (id-select-set-region pos (scan-sexps pos 1))
	  (error nil)))))

(defun id-select-sexp-end (pos)
  "Return start and end of sexp ending at POS."
  (or (id-select-brace-def pos)
      (save-excursion
	(setq id-select-previous 'sexp-end)
	(condition-case ()
	    (id-select-set-region (scan-sexps (1+ pos) -1) (1+ pos))
	  (error nil)))))

(defun id-select-sexp (pos)
  "Return start and end of the sexp that POS is within."
  (setq id-select-previous 'sexp)
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(id-select-set-region (progn (backward-up-list 1) (point))
			      (progn (forward-list 1) (point)))
      (error nil))))

(defun id-select-sexp-up (pos)
  "Return start and end of the sexp enclosing the selected area or nil."
  (setq id-select-previous 'sexp-up)
  ;; Keep going up and backward in sexps.  This means that id-select-sexp-up
  ;; can only be called after id-select-sexp or after itself.
  (setq pos (or (car id-select-region) pos))
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(id-select-set-region (progn (backward-up-list 1) (point))
			      (progn (forward-list 1) (point)))
      (error nil))))

;; Allow punctuation marks not followed by white-space to include
;; the previous and subsequent sexpression.  Useful in contexts such as
;; 'foo.bar'.
(defun id-select-punctuation (pos)
  "Return (start . end) region including sexpressions before and after POS, when at a punctuation character."
  (or (id-select-comment pos)
      (save-excursion
	(setq id-select-previous 'punctuation)
	(goto-char (min (1+ pos) (point-max)))
	(if (= (char-syntax (if (eobp) (preceding-char) (char-after (point))))
	       ?\ )
	    (id-select-set-region pos (1+ pos))
	  (goto-char pos)
	  (id-select-set-region
	   (save-excursion (backward-sexp) (point))
	   (progn (forward-sexp) (point)))))))

(defun id-select-comment (pos)
  "Return rest of line from POS to newline."
  (setq id-select-previous 'comment)
  (save-excursion
    (goto-char pos)
    (let ((start-regexp  (if (stringp comment-start) (regexp-quote comment-start)))
	  (end-regexp    (if (stringp comment-end)   (regexp-quote comment-end)))
	  bolp)
      (cond
       ;; Beginning of a comment
       ((and (stringp comment-start)
	     (or (looking-at start-regexp)
		 (and (skip-chars-backward comment-start)
		      (looking-at start-regexp))))
	(skip-chars-backward " \t")
	(setq bolp (bolp)
	      pos (point))
	(if (equal comment-end "")
	    (progn (end-of-line)
		   (id-select-set-region pos (point)))
	  (if (stringp comment-end)
	      ;; Skip over nested comments.
	      (let ((count 0)
		    (regexp (concat start-regexp "\\|" end-regexp)))
		(catch 'done
		  (while (re-search-forward regexp nil t)
		    (if (string-equal
			 (buffer-substring (match-beginning 0) (match-end 0))
			 comment-start)
			(setq count (1+ count))
		      ;; End comment
		      (setq count (1- count))
		      (if (= count 0)
			  (progn
			    (if (looking-at "[ \t]*[\n\r]")
				;; Don't include final newline unless the comment is
				;; first thing on its line.
				(goto-char (if bolp (match-end 0)
					     (1- (match-end 0)))))
			    (throw 'done (id-select-set-region pos (point))))))))))))
       ;; End of a comment
       ((and (stringp comment-end)
	     (not (string-equal comment-end ""))
	     (or (looking-at end-regexp)
		 (and (skip-chars-backward comment-end)
		      (looking-at end-regexp))))
	(goto-char (match-end 0))
	(if (looking-at "[ \t]*[\n\r]")
	    (goto-char (match-end 0)))
	(setq pos (point))
	(skip-chars-forward " \t")
	;; Skip over nested comments.
	(let ((count 0)
	      (regexp (concat start-regexp "\\|" end-regexp)))
	  (catch 'done
	    (while (re-search-backward regexp nil t)
	      (if (string-equal
		   (buffer-substring (match-beginning 0) (match-end 0))
		   comment-end)
		  (setq count (1+ count))
		;; Begin comment
		(setq count (1- count))
		(if (= count 0)
		    (progn
		      (skip-chars-backward " \t")
		      ;; Don't include final newline unless the comment is
		      ;; first thing on its line.
		      (if (bolp) nil (setq pos (1- pos)))
		      (throw 'done (id-select-set-region (point) pos)))))))))))))

;;;
;;; Textual selections
;;;

(defun id-select-word (pos)
  "Return start and end of word at POS."
  (or (id-select-brace-def pos)
      (id-select-indent-def pos)
      (progn (setq id-select-previous 'word)
	     (save-excursion
	       (goto-char pos)
	       (forward-word 1)
	       (let ((end (point)))
		 (forward-word -1)
		 (id-select-set-region (point) end))))))

(defun id-select-string (pos)
  "Returns (start . end) of string at POS or nil.  Pos include delimiters.
Delimiters may be single, double or open and close quotes."
  (setq id-select-previous 'string)
  (save-excursion
    (goto-char pos)
    (or (id-select-string-p) (id-select-string-p "'" "'")
	(id-select-string-p "`" "'"))))

(defun id-select-sentence (pos)
  "Return start and end of the sentence at POS."
  (setq id-select-previous 'sentence)
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(id-select-set-region (progn (backward-sentence) (point))
			      (progn (forward-sentence) (point)))
      (error nil))))

(defun id-select-whitespace (pos)
  "Return start to end of all but one char of whitespace POS, unless 
there is only one character of whitespace or this is leading whitespace on
the line.  Then return all of it."
  (setq id-select-previous 'whitespace)
  (save-excursion
    (let ((end (progn (skip-chars-forward " \t") (point)))
	  (start (progn (skip-chars-backward " \t") (point))))
      (if (looking-at "[ \t]")
	  (if (or (bolp) (= (1+ start) end))
	      (id-select-set-region start end)
	    (id-select-set-region (1+ start) end))))))

;;;
;;; Document selections
;;;

(defun id-select-line (pos)
  "Return whole of line POS is in, with newline unless at eob."
  (setq id-select-previous 'line)
  (save-excursion
    (goto-char pos)
    (let* ((start (progn (beginning-of-line 1) (point)))
	   (end (progn (forward-line 1) (point))))
      (id-select-set-region start end))))

(defun id-select-paragraph (pos)
  "Return start and end of the paragraph at POS."
  (setq id-select-previous 'paragraph)
  (save-excursion 
    (goto-char pos)
    (id-select-set-region (progn (backward-paragraph) (point))
			  (progn (forward-paragraph) (point)))))

(defun id-select-page (pos)
  "Return start and end of the page at POS."
  (setq id-select-previous 'page)
  (save-excursion 
    (goto-char pos)
    (id-select-set-region (progn (backward-page) (point))
			  (progn (forward-page) (point)))))

(defun id-select-buffer (pos)
  "Return start and end of the buffer at POS."
  (setq id-select-previous 'buffer)
  (id-select-set-region (point-min) (point-max)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar id-select-bigger-alist
  '((char nil)
    (whitespace id-select-whitespace)
    (word id-select-word)
    (symbol id-select-symbol)
    (punctuation nil)
    (string id-select-string)
    (comment id-select-comment)
    (sexp id-select-sexp)
    (sexp-start nil)
    (sexp-end nil)
    (sexp-up id-select-sexp-up)
    (line id-select-line)
    (sentence id-select-sentence)
    (brace-def id-select-brace-def)
    (indent-def id-select-indent-def)
    (paragraph id-select-paragraph)
    (page id-select-page)
    (buffer id-select-buffer)
    )
  "List of (REGION-TYPE-SYMBOL REGION-SELECTION-FUNCTION) pairs.
Used to go from one thing to a bigger thing.  See id-select-bigger-thing.
Nil value for REGION-SELECTION-FUNCTION means that region type is skipped
over when trying to grow the region.  Ordering of entries is largely
irrelevant to any code that uses this list.")


(defvar id-select-prior-buffer nil)
(defvar id-select-prior-point nil)

(defvar id-select-previous 'char
  "Most recent type of selection.  Must be set by all id-select functions.")

(defvar id-select-region (cons 'nil 'nil)
  "Cons cell that contains a region (<beginning> . <end>).
The function 'id-select-set-region' updates and returns it.")

(defvar id-select-old-region (cons 'nil 'nil)
  "Cons cell that contains a region (<beginning> . <end>).")

(defvar id-select-syntax-alist
  '((?w  id-select-word)
    (?_  id-select-symbol)
    (?\" id-select-string)
    (?\( id-select-sexp-start)
    (?\$ id-select-sexp-start)
    (?'  id-select-sexp-start)
    (?\) id-select-sexp-end)
    (?   id-select-whitespace)
    (?<  id-select-comment)
    (?.  id-select-punctuation))
  "*List of pairs of the form (SYNTAX-CHAR FUNCTION) used by the function 'id-select-syntactical-region'.
Each FUNCTION takes a single position argument and returns a region
(start . end) delineating the boundaries of the thing at that position.
Ordering of entries is largely irrelevant to any code that uses this list.")


(provide 'id-select)

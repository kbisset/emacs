;; Following code is based on Nelson Minar's HTML Helper mode.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;; INSTALLATION:
;; Add the following four lines to your .emacs file.
;; (autoload 'dml-helper-mode "/usr/local/contrib/setup/pub/sun/lib/emacs/dml-helper-mode.el" "Cool DML" t)
;; (or (assoc "\\.dml$" auto-mode-alist)
;;    (setq auto-mode-alist (cons '("\\.dml$" . dml-helper-mode)
;;                                auto-mode-alist)))


(defvar dml-helper-do-write-file-hooks nil
  "*If not nil, then dml-helper-mode will modify the local-write-file-hooks
to do timestamps.")

(defvar dml-helper-build-new-buffer nil
  "*If not nil, then dml-helper will insert dml-helper-new-buffer-strings
when new buffers are generated")

;; (see also tempo.el)

;; variables to configure

(defvar dml-helper-basic-offset 2
  "*basic indentation size used for list indentation")

(defvar dml-helper-item-continue-indent 2
  "*Indentation of lines that follow a <li> item. Default is 2, the length
of things like \"<li> \" and \"<dd> \".")

(defvar dml-helper-never-indent nil
  "*If t, the indentation code for dml-helper is turned off.")


;; hooks (see also tempo.el)

(defvar dml-helper-mode-hook nil
  "*Hook run when dml-helper-mode is started.")

(defvar dml-helper-load-hook nil
  "*Hook run when dml-helper-mode is loaded.")

(defvar dml-helper-timestamp-hook 'dml-helper-default-insert-timestamp
  "*Hook called for timestamp insertion. Override this for your own
timestamp styles.")


(defvar dml-helper-timestamp-start "<!-- hhmts start -->\n"
  "*Delimiter for timestamps. Everything between dml-helper-timestamp-start
and dml-helper-timestamp-end will be deleted and replaced with the output of
the function dml-helper-insert-timestamp if dml-helper-do-write-file-hooks
is t")

(defvar dml-helper-timestamp-end "<!-- hhmts end -->"
  "*Delimiter for timestamps. Everything between dml-helper-timestamp-start
and dml-helper-timestamp-end will be deleted and replaced with the output of
the function dml-helper-insert-timestamp if dml-helper-do-write-file-hooks
is t")

;; this is what the byte compiler does to see if its emacs18. You probably
;; don't need to change this.

(defvar dml-helper-emacs18
  (and (boundp 'emacs-version)
       (or (and (boundp 'epoch::version) epoch::version)
           (string-lessp emacs-version "19")))
  "I'll do minimal emacs18 support, grumble.")

;;}}}

(require 'tempo)

;;{{{ dml-helper-mode-syntax-table

;; emacs doesn't really seem to be general enough to handle SGML like
;; syntax. In particular, comments are a loss. We do try this, though:
;;   give < and > matching semantics

(defvar dml-helper-mode-syntax-table nil
  "Syntax table for dml-helper.")

(if dml-helper-mode-syntax-table
    ()
  (setq dml-helper-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?<  "(>  " dml-helper-mode-syntax-table)
  (modify-syntax-entry ?>  ")<  " dml-helper-mode-syntax-table)
  (modify-syntax-entry ?\" ".   " dml-helper-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " dml-helper-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " dml-helper-mode-syntax-table))

;;}}}
;;{{{ keymap variable and function setup

(defvar dml-helper-keymap-list
  '(
    dml-helper-logical-map dml-helper-emph-map dml-helper-list-map
    dml-helper-note-map dml-helper-textel-map)
  "list of all the subkeymaps dml-helper uses")

(defvar dml-helper-keymap-alist
  '(
    (logical . dml-helper-logical-map)
    (emph . dml-helper-emph-map)
    (list . dml-helper-list-map)
    (note . dml-helper-note-map))
  "alist associating cookie types with keymaps")

;; basic keymap variables (not easy to mapcar a macro)
(defvar dml-helper-mode-map (make-sparse-keymap)
  "Keymap for dml-helper")
(defvar dml-helper-logical-map nil
  "Keymap used for logical styles.")
(defvar dml-helper-emph-map nil
  "Keymap used for emphasis styles.")
(defvar dml-helper-list-map nil
  "Keymap used for lists.")
(defvar dml-helper-note-map nil
  "Keymap used for notes.")
(defvar dml-helper-textel-map nil
  "Keymap used for text elements.")

;; make keymaps into prefix commands (does this do anything useful in 18?)
(mapcar 'define-prefix-command dml-helper-keymap-list)

;; if we're emacs18, we have to build the prefix maps by hand
(if dml-helper-emacs18
    (mapcar (function (lambda (v) (set v (make-sparse-keymap))))
            dml-helper-keymap-list))

;; now build the mode keymap.
;; special mode keys
(mapcar
 (function (lambda (l) (define-key dml-helper-mode-map (car l) (nth 1 l))))
 '(("\M-\C-f" tempo-forward-mark)
   ("\M-\C-b" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)
   
   ("\M-\C-t" dml-helper-insert-timestamp-delimiter-at-point)))
 
;; indentation keys - only rebind these if the user wants indentation
(if dml-helper-never-indent
    ()
  (define-key dml-helper-mode-map "\t" 'dml-helper-indent-command)
  (define-key dml-helper-mode-map "\C-m" 'newline-and-indent)
  (define-key dml-helper-mode-map "\C-c\C-v" 'dml-validate))

;; special keybindings in the prefix maps (not in the list of cookies)
(define-key dml-helper-list-map "i" 'dml-helper-smart-insert-item)

;; install the prefix maps themselves into the mode map
;; eval the keymap in 18 so we get the value, not the symbol
(defun dml-helper-install-prefix (l)
  "Install a prefix key into the map. Special code for emacs18"
  (if dml-helper-emacs18
      (define-key dml-helper-mode-map (car l) (eval (nth 1 l)))
    (define-key dml-helper-mode-map (car l) (nth 1 l))))

(mapcar
 'dml-helper-install-prefix
 '(
   ("\C-c\C-s" dml-helper-logical-map)
   ("\C-c\C-e" dml-helper-emph-map)
   ("\C-c\C-l" dml-helper-list-map)
   ("\C-c\C-n" dml-helper-note-map)))

;;}}}
;;{{{ dml-helper-mode-abbrev-table

(defvar dml-helper-mode-abbrev-table nil
  "Abbrev table used while in dml-helper-mode.")
(define-abbrev-table 'dml-helper-mode-abbrev-table ())

;;}}}

;;{{{ dml-helper-add-cookie function for building basic cookies

(defvar dml-helper-tempo-tags nil
  "List of tags used in completion.")

(defun dml-helper-add-cookie (l)
  "Add a new cookie to dml-helper-mode. Builds a tempo-template for the
cookie and puts it into the appropriate keymap if a key is
requested."
  (let* ((type (car l))
         (keymap (cdr-safe (assq type dml-helper-keymap-alist)))
         (key (nth 1 l))
         (tag (nth 2 l))
         (name (nth 3 l))
         (cookie (nth 4 l))
         (doc (nth 5 l))
         (command (tempo-define-template name cookie tag doc 'dml-helper-tempo-tags)))

    (if (stringp key)                                     ;bind at all?
        (if keymap                                        ;special keymap?
            (define-key (eval keymap) key command)        ;bind to prefix
          (define-key dml-helper-mode-map key command))  ;bind to global
      )))

;;}}}

;;{{{ dml-helper-smart-insert-item
;; there are two different kinds of items in HTML - those in regular
;; lists <li> and those in dictionaries <dt>..<dd>
;; This command will insert the appropriate one depending on context.

(defun html-helper-smart-insert-item (&optional arg)
  "Insert a new item, either in a regular list or a dictionary."
  (interactive "*P")
  (let ((case-fold-search t))
    (if
        (save-excursion
          (re-search-backward "<item\\|<para>\\|<section\\|<chapter\\|<abstract\\|<dir>\\|<dl>" nil t)
          (looking-at "<item>\\|<para>\\|<section\\|<chapter"))
        (tempo-template-html-definition-item arg)
      (tempo-template-html-item arg))))

;;}}}

;;{{{ most of the DML keybindings

(mapcar
 'dml-helper-add-cookie
 '(
   ;; logical styles
   (logical "c" "<command>"  "dml-command"   ("<command name=\"" >(r . "Command name: ") "\">\n<description>">" </description>\n</command>">))
   (logical "g" "<arg>"      "dml-cmd-arg"   ("<arg name=\"" >(r . "Argument name: ") "\" default=\"FALSE\">\n<description>">" </description>\n</arg>">))
   (logical "e" "<equation>" "dml-equation"  ("<equation>" (r . "Text: ") "</equation>"))
   (logical "v" "<verbatim>" "dml-verbatim"  ("<verbatim>" (r . "Text: ") "</verbatim>"))
   (logical "f"  "<figure>"  "dml-figure"    ("<figure caption=\"\" link=\"" (r . "Link: ") "\" />"))

   ;;emphasis styles
   (emph    "b"       "<emphasis>"       "dml-bold"               ("<emphasis type=\"bold\">" (r . "Text: ") "</emphasis>"))
   (emph    "i"       "<emphasis>"       "dml-italic"             ("<emphasis type=\"italic\">" (r . "Text: ") "</emphasis>"))

   ;;note elements
   (note  "f" "<footnote>"  "dml-footnote"  ("<footnote id=\"\">" > "\n<foot_para> " > (r . "Footnote: ") "</foot_para>\n</footnote>">))
   (note  "o" "<foot_para>" "dml-foot-para" ("<foot_para>" (r . "Footnote: ") "</foot_para>"))
   (note  "r" "<reference>" "dml-reference" ("<reference id=\"\">" > "\n<ref_para> " > (r . "Reference: ") "</ref_para>\n</reference>">))
   (note  "p" "<ref_para>"  "dml-ref-para" ("<ref_para>" (r . "Reference: ") "</ref_para>"))


   ;;lists
   (list    "u"   "<list>"    "dml-unordered-list"  (& "<list type=\"unordered\">" > "\n<item> " > (r . "Item: ")"</item>\n</list>" >))
   (list    "n"   "<list>"    "dml-numbered-list"   (& "<list type=\"numbered\">" > "\n<item> " > (r . "Item: ")"</item>\n</list>" >))
   (list    "b"   "<list>"    "dml-bulleted-list"   (& "<list type=\"bulleted\">" > "\n<item> " > (r . "Item: ")"</item>\n</list>" >))
   (list    "i"   "<item>"    "dml-item"            (& "<item> " > (r . "Item: ") "</item>" >))
   (list    "h"   "<header>"  "dml-header"          (& "<header> " > (p . "Header: ") "\n</header>" >))
   (list    "t"   "<table>"   "dml-table"           (& "<table align=\"center\" border=\"1\" cell_size=\"1.5\"> " > "\n<tr>\n<td>" > (p . "Table data: ") "\n</td>\n</tr>\n</table>" >))

   ;;text elements
   (textel  "\e\C-d"  nil     "dml-document"         (& "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE document SYSTEM \"/usr/local/contrib/setup/pub/sun/lib/dml/dml.dtd\">\n<document title=\"\" authors=\"\" classification=\"\">" > (r . "Text: ") "\n</document>\n">))
   (textel  "\e\C-a"  nil     "dml-abstract"         (& "<abstract title=\"\">" > (r . "Text: ") "\n</abstract>">))
   (textel  "\e\C-c"  nil     "dml-chapter"          (& "<chapter title=\"\" id=\"\">" > (r . "Text: ") "\n</chapter>">))
   (textel  "\e\C-s"  nil     "dml-section"          (& "<section title=\"\" id=\"\">" > (r . "Text: ") "\n</section>">))
   (textel  "\e\C-u"  nil     "dml-subsection"       (& "<subsection title=\"\" id=\"\">" > (r . "Text: ") "\n</subsection>">))
   (textel  "\e\C-b"  nil     "dml-subsubsection"    (& "<subsubsection title=\"\" id=\"\">\n<para>" > (r . "Text: ") "\n</para>\n</subsubsection>">))
   (textel  "\e\C-p"  nil     "dml-paragraph"        (& "<para>" > (r . "Text: ") "\n</para>">))
   ))
;;}}}

(require 'easymenu)
(defconst dml-menus
  '(" DML Elements "
    "---"
    ["Document" tempo-template-dml-document t]
    ["Abstract" tempo-template-dml-abstract t]
    ["Chapter" tempo-template-dml-chapter t]
    ["Section" tempo-template-dml-section t]
    ["SubSection" tempo-template-dml-subsection t]
    ["SubSubSection" tempo-template-dml-subsubsection t]
    ["Paragraph" tempo-template-dml-paragraph t]
    "---"
    ["Equation" tempo-template-dml-equation t]
    ["Command" tempo-template-dml-command t]
    ["Command Args" tempo-template-dml-cmd-arg t]
    ["Verbatim" tempo-template-dml-verbatim t]
    ["Figure" tempo-template-dml-figure t]
    "---"
    ["Bold" tempo-template-dml-bold t]
    ["Italic" tempo-template-dml-italic t]
    "---"
    ["Unordered List" tempo-template-dml-unordered-list t]
    ["Numbered List" tempo-template-dml-numbered-list t]
    ["Bulleted List" tempo-template-dml-bulleted-list t]
    ["List Item" tempo-template-dml-item t]
    ["List Header" tempo-template-dml-header t]
    ["Table" tempo-template-dml-table t]
    "---"
    ["Footnote" tempo-template-dml-footnote t]
    ["Footnote Paragraph" tempo-template-dml-foot-para t]
    ["Reference" tempo-template-dml-reference t]
    ["Reference Paragraph" tempo-template-dml-ref-para t]
    "---"
    "---"
    ["Validate" dml-validate t]
  ))
(easy-menu-define dml-menu dml-helper-mode-map "DML Menus" dml-menus)

(defconst dml-validate-command
  "/usr/local/contrib/setup/pub/sun/bin/validate_dml"
  "*The command to validate a DML document.
The file name of current buffer file name will be appended to this,
separated by a space.")

(defvar dml-saved-validate-command nil
  "The command last used to validate in this buffer.")


;;{{{ context guessing

;; guess where we are in indented lists based on the last list token.
;; it would be much better to try to match </ul> to <ul>, and </ol> to <ol>
;; etc, but that is pretty unwieldy and slow.

(defvar dml-helper-any-list-item "<item>\\|<foot_para>\\|<ref_para>")
(defvar dml-helper-any-list-start "<list\\|<para\\|<footnote\\|<reference\\|<command\\|<arg\\|<subsubsection\\|<subsection\\|<section\\|<chapter\\|<abstract\\|<document")
(defvar dml-helper-any-list-end "</list>\\|</para>\\|</footnote>\\|</reference>\\|</command>\\|</arg>\\|</subsubsection>\\|</subsection>\\|</section>\\|</chapter>\\|</abstract>\\|</document>")
(defvar dml-helper-any-list (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
                                     dml-helper-any-list-item
                                     dml-helper-any-list-start
                                     dml-helper-any-list-end))
(defvar dml-helper-search-limit 2000 "limit on how far back we search")

(defun dml-helper-guess-context ()
  "figure out what the last list type thing before point is."
  (save-excursion
    (let* ((lim (max (point-min) (- (point) dml-helper-search-limit)))
           (context (if (re-search-backward dml-helper-any-list lim t)
                        (cond ((match-beginning 1) 'item)
                              ((match-beginning 2) 'start)
                              ((match-beginning 3) 'end)
                              (t 'error))
                      nil)))
      (cons context (current-indentation)))))

(defun dml-helper-print-context ()
  (interactive)
  (message "%s" (dml-helper-guess-context)))

;;}}}
;;{{{ indentation

(defvar dml-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

(defun dml-helper-indent-command ()
  "Command for indenting text. Just calls dml-helper-indent."
  (interactive)
  (dml-helper-indent))

;; some of the ideas are borrowed from cc-mode.el.

;; lots of special cases because we're not doing true parsing, we're
;; trying to guess what to do based on what the last item cookie was.

;; this code works best if the cookies that are the beginnings of menus
;; are on the left end of the line, and are already indented.

(defun dml-helper-indent ()
  "indentation workhorse function."
  ( if dml-helper-never-indent
      ()
    (let ((m (point-marker))
          (bol (progn (beginning-of-line) (point))))

      ;; unindent the line
      (delete-region (point) (progn (back-to-indentation) (point)))

      (let* ((where (dml-helper-guess-context))
             (context (car where))
             (previ (cdr where))
             (newi (cond 
                    ((eq context 'end) previ)
                    ((eq context 'item) previ)
                    ((eq context 'start) (+ previ dml-helper-basic-offset))
                    (t previ))))

        ;; newi is set to the basic indentation, now adjust indentation
        ;; based on what the current line is.
        (if (looking-at dml-helper-any-list)
            (cond

             ;; list token and last line was an end?
             ;; Probably inside a continued item - go backwards.
             ((and (match-beginning 1) (eq context 'end))
              (setq newi (- newi dml-helper-item-continue-indent)))

             ;; end of list and last line was an end?
             ;; Probably inside a continued item - go backwards twice
             ((and (match-beginning 3) (eq context 'end))
              (setq newi (- newi dml-helper-item-continue-indent dml-helper-basic-offset)))

             ;; Any other end of list?
             ;; Indent negative
             ((match-beginning 3)
              (setq newi (- newi dml-helper-basic-offset)))

             ;; start of list and last line
             ;; Beginning of continued item - go forwards
             ((and (match-beginning 2) (eq context 'item))
              (setq newi (+ newi dml-helper-item-continue-indent))))

          ;; we're not any sort of item, must be text.
          (cond
           ;; last line an item?
           ;; Beginning of continued item - go forward
           ((eq context 'item)
            (setq newi (+ newi dml-helper-item-continue-indent)))))

        (if dml-helper-print-indent-info
            (message "Context: %s, Previous: %s New: %s" context previ newi))

        ;; just in case
        (if (< newi 0)
            (setq newi 0))
        (indent-to newi newi)

        ;; adjust point to where it was before, or at start of indentation
        (goto-char (marker-position m))
        (if (< (current-column) (current-indentation))
            (back-to-indentation))))))

;;}}}
;;{{{ completion finder for tempo

(defvar dml-helper-completion-finder
  (if dml-helper-emacs18
      'dml-helper-emacs18-completion-finder
    "\\(\\(<\\|&\\).*\\)\\=")
  "Passed to tempo-use-tag-list, used to find tags to complete.")

;; The regexp finds everything between the last < or & and point,
;; which is good enough to match the tags HTML might complete.
;; emacs18 doesn't have the \= for regexps, though, so we do something
;; more hackish.

(defun dml-helper-emacs18-completion-finder ()
  "Unfortunately emacs18 doesn't support \\= in regexps, so we do this hack.
If you have problems with it, maybe you should upgrade to emacs19 :-)"
  (let* ((where nil)
         (s (buffer-substring
             (point)
             (setq where (save-excursion
                           (re-search-backward "<\\|&" (min (point-min) 100) t)
                           (point))))))
    (cons s where)))

;;}}}

;;{{{ timestamps

(defun dml-helper-update-timestamp ()
  "Basic function for updating timestamps. It finds the timestamp in
the buffer by looking for dml-helper-timestamp-start, deletes all text
up to dml-helper-timestamp-end, and runs dml-helper-timestamp-hook
which will presumably insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward dml-helper-timestamp-start nil t))
        (message "timestamp delimiter start was not found")
      (let ((ts-start (+ (point) (length dml-helper-timestamp-start)))
            (ts-end (if (search-forward dml-helper-timestamp-end nil t)
                        (- (point) (length dml-helper-timestamp-end))
                      nil)))
        (if (not ts-end)
            (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
          (delete-region ts-start ts-end)
          (goto-char ts-start)
          (run-hooks 'dml-helper-timestamp-hook)))))
  nil)

(defun dml-helper-default-insert-timestamp ()
  "Default timestamp insertion function"
  (insert "Last modified: "
          (current-time-string)
          "\n"))

(defun dml-helper-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point, useful
for adding timestamps to existing buffers."
  (interactive)
  (insert dml-helper-timestamp-start)
  (insert dml-helper-timestamp-end))

;;}}}
;;{{{ dml-helper-insert-new-buffer-strings

;;(tempo-define-template "dml-skeleton" dml-helper-new-buffer-template
;;                       nil
;;                       "Insert a skeleton for a HTML document")

(defun dml-helper-insert-new-buffer-strings ()
  "Insert dml-helper-new-buffer-strings."
  (tempo-template-dml-skeleton))

;;}}}

;;{{{ dml-helper-mode



(defun dml-helper-mode ()
  "
Mode for editing DML.

The main function dml-helper-mode provides is a bunch of keybindings
for the DML elements one inserts when writing DML. Typing
the key sequence for a command inserts the corresponding element and
places point in the right place. If a prefix argument is supplied, the
element is instead wrapped around the region. Alternately, one can type
in part of the element and complete it.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

\\{dml-helper-mode-map}
"
  (interactive)
  (kill-all-local-variables)

  (use-local-map dml-helper-mode-map)
  (setq local-abbrev-table dml-helper-mode-abbrev-table)
  (set-syntax-table dml-helper-mode-syntax-table)

  (setq mode-name "DML helper")
  (setq major-mode 'dml-helper-mode)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'indent-line-function)

  (setq comment-start "<!-- "
        comment-end " -->"
        comment-start-skip "<!--[ \t]*"
        comment-column 0
        indent-line-function 'dml-helper-indent)

  (tempo-use-tag-list 'dml-helper-tempo-tags dml-helper-completion-finder)
  
  (if dml-helper-do-write-file-hooks
      (add-hook 'local-write-file-hooks 'dml-helper-update-timestamp))

  (if (and dml-helper-build-new-buffer (zerop (buffer-size)))
      (dml-helper-insert-new-buffer-strings))
  (easy-menu-add dml-menu)
  (run-hooks 'dml-helper-mode-hook))

;;}}}

(provide 'dml-helper-mode)
(run-hooks 'dml-helper-load-hook)

(eval-and-compile
  (autoload 'compile-internal "compile" ""))

(defun dml-validate (command)
  "Validate a DML document.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *compilation*.
You can then use the command \\[next-error] to find the next error message
and move to the line in the SGML document that caused it."
  (interactive
   (list (read-string "Validate command: "
		      (or dml-saved-validate-command
			  (concat dml-validate-command
				  " "
				  (let ((name (buffer-file-name)))
				    (and name
					 (file-name-nondirectory name))))))))
  (setq dml-saved-validate-command command)
  (compile-internal command "No more errors"))

;;; dml-helper-mode.el ends here

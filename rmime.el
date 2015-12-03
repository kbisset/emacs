;Here is a re-implementation of my MIME reader, mime.el.  This
;implementation tries to be smart about manipulating the buffer the
;message comes from rather than allocating a parallel buffer.

;I have renamed this mime reading package "rmime" to avoid a name
;conflict with a mime composing package known as "mime".

;This implementation was designed for rmail and vm, but really ought to
;work in conjunction with any major mode.  To enable this package with
;rmail, you need to put three lines in your .emacs file:

;	(add-hook 'rmail-show-message-hook 'rmime)
;	(add-hook 'rmail-edit-mode-hook    'rmime-cancel)
;	(autoload 'rmime "rmime" "" t)

;This package is far from complete --- the purpose of this early post
;is to collect opinions from potential users.

;-- 
;								Ray
;;; rmime.el --- read MIME messages

;; Author: Ray Moody <moody@cray.com>
;; Version: $Id: rmime.el,v 0.1 1995/06/06 14:26:58 moody Exp $
;; Keywords: MIME, mail

;; Path to metamail.  Metamail is called when there is no emacs lisp
;; function to handle a MIME type.
;;
;; Metamail can be found in ftp://thumper.bellcore.com/pub/nsb.
(defvar rmime-metamail-program "metamail")

;; Path to mimencode.  Mimencode will decode MIME messages with base64
;; or quoted-printable encodings.  Mimencode comes with metamail.
(defvar rmime-mimencode-program "mimencode")

;; Path to the uufilter program, or nil if uufilter is not available.
;; The uufilter program is an enhanced version of uudecode that will
;; write to stdout if it is given -s or to a named file if given -f.
;; Uufilter comes with xloadimage.  If you don't have uufilter, set
;; this variable to nil and don't worry about it.  This style of
;; encoding is deprecated.
(defvar rmime-uufilter-program nil)

;; Function to call to extract MIME information for the current
;; message in the current buffer.
;;
;; Buffers that don't hold an RFC 822 message (or that may mess around
;; with message headers) must assign a local value to this variable.
(defvar rmime-mimeinfo-method 'rmime-obsolete-mimeinfo)

;; Lists of keystrokes to get at the play and save functions for MIME
;; bodies.
(defvar rmime-play-keystrokes '("\C-c\C-c"))
(defvar rmime-save-keystrokes '("s"))

(defvar rmime-text-string "Push C-c C-c here to display attached text")
(defvar rmime-text-face nil)
(if (not rmime-text-face)
    (set-face-foreground (setq rmime-text-face (make-face 'text)) "Deep Sky Blue"))

(defvar rmime-multipart-string "Push C-c C-c here to display attached parts")
(defvar rmime-multipart-face nil)
(if (not rmime-multipart-face)
    (set-face-foreground (setq rmime-multipart-face (make-face 'multipart)) "Cornflower Blue"))

(defvar rmime-audio-string "Push C-c C-c here to hear audio")
(defvar rmime-audio-face nil)
(if (not rmime-audio-face)
    (set-face-foreground (setq rmime-audio-face (make-face 'audio)) "Sienna"))

(defvar rmime-image-string "Push C-c C-c here to see image")
(defvar rmime-image-face nil)
(if (not rmime-image-face)
    (set-face-foreground (setq rmime-image-face (make-face 'image)) "Lime Green"))

(defvar rmime-video-string "Push C-c C-c here to start video")
(defvar rmime-video-face nil)
(if (not rmime-video-face)
    (set-face-foreground (setq rmime-video-face (make-face 'video)) "Yellow Green"))

(defvar rmime-message-face nil)
(if (not rmime-message-face)
    (set-face-foreground (setq rmime-message-face (make-face 'message)) "Dark Orange"))

(defvar rmime-application-face nil)
(if (not rmime-application-face)
    (set-face-foreground (setq rmime-application-face (make-face 'aplication)) "Red"))

(defvar rmime-transparent-charsets  '("" "us-ascii" "iso-8859-1"))
(defvar rmime-transparent-encodings '("" "7bit" "8bit" "binary"))

(defvar rmime-magic-string "\037 -=- MIME -=- \037\014\n")

(defvar rmime-tmp-buffer-name " *rmime-tmp*")
(defvar rmime-tmp-string nil)

(defvar rmime-separator-regexp "^[ \t]*\n")

;; Default type for bodies that have no Content-Type header.
;;
;; Inside of rmime-digest-action, this variable has a temporary
;; binding of 'rmime-message/rfc822.
(defvar rmime-default-type 'rmime-text/*)

(defvar rmime-overlay-list nil)
(make-variable-buffer-local 'rmime-overlay-list)

;; We use this handy macro whenever we are going to modify a buffer.
;; This macro evaluates its body after clearing read-only status.
;; Read-only status and modification status are restored to their
;; original values after the body has finished or in the event of an
;; error.
;;
;; We would like to temporarily disable undo too (saves space and
;; time), but this isn't possible --- the data structure is such that
;; if we don't keep our own undo records around, all previous undo
;; records become worthless.
(defmacro rmime-modify-buffer (&rest body)
  (let ((symbol (make-symbol "rmime-buffer-modified-flag")))
    (list 'let (cons (cons symbol '((buffer-modified-p))) '(buffer-read-only))
	  (list 'unwind-protect (cons 'progn body)
		                (list 'set-buffer-modified-p symbol)))))
(put 'rmime-modify-buffer 'lisp-indent-function 0)

(defun rmime ()
  (interactive)
  (save-excursion
    (let* ((buffer    (current-buffer))
	   (arguments (funcall rmime-mimeinfo-method))
	   (handler   (rmime-handler (car arguments)))
	   (cache     (rmime-cache)))
      (rmime-modify-buffer
	(if (identity cache)
	    (if (not (overlay-get cache 'rmime-cache))
		(progn (apply 'rmime-dispatch (overlay-get cache 'rmime-raw) arguments)
		       (delete-region (point) (point-max))
		       (goto-char (point-min))))
	  (let ((overlay (make-overlay (point-min) (point-max))))

	    ;; We need to put the overlay in the list right away.  If
	    ;; we catch an interrupt or error after we create the
	    ;; overlay and before we put it in the list, we may leak
	    ;; memory.  Yes, we could play with inhibit-quit, but it
	    ;; isn't really worth it.
	    (setq rmime-overlay-list (cons overlay rmime-overlay-list))

	    ;; This crap is more important than it seems!  See
	    ;; rmime-cancel-all for the ugly details.
	    (make-local-variable 'before-revert-hook)
	    (add-hook 'before-revert-hook 'rmime-cancel-all)

	    (make-local-variable 'write-region-annotate-functions)
	    (add-hook 'write-region-annotate-functions 'rmime-annotate)
	   
	    (save-restriction

	      ;; First order of business is cleaning up.
	      (narrow-to-region (point) (point-max))
	      (while (search-forward rmime-magic-string nil t)
		(delete-region (point-min) (point)))

	      ;; Now go do the formatting.
	      ;;
	      ;; We try to take a shortcut if the message is plain
	      ;; text, the data is in the current buffer, the encoding
	      ;; is transparent, and the message is to be presented
	      ;; inline.  This is the most common case.
	      (if (and (eq handler 'rmime-text/*)
		       (eq (current-buffer) buffer)
		       (member (downcase (or (rmime-uncomment (car (cdr arguments))) "")) rmime-transparent-encodings)
		       (rmime-inline-p (rmime-uncomment (car (cdr (cdr arguments))))))

		  ;; The shortcut applies.  All we have to do to
		  ;; present the message is to select a face from the
		  ;; charset parameter.
		  (let ((charset (cdr (assoc "charset" (rmime-parameters (rmime-unquote (car arguments)))))))
		    (if (not (member (downcase (or charset "")) rmime-transparent-charsets))
			(put-text-property (point) (point-max) 'face (intern charset))))

		;; The shortcut does not apply.  Dispatch to the
		;; proper handler the hard way.  The order of
		;; operations in this let clause is intended to reduce
		;; exposure to errors and interrupts.  Exposure cannot
		;; be eliminated.
		(let ((content (save-excursion (set-buffer buffer) (list (buffer-substring (point-min) (point-max)) 1 (- (point-max) (point-min) -1)))))
		  (overlay-put overlay 'rmime-raw content)
		  (apply 'rmime-dispatch content arguments)
		  (delete-region (point) (point-max))))
	      (overlay-put overlay 'rmime-marker (point-min-marker))
	      (overlay-put overlay 'rmime-cache t)
	      
	      ;; This only has an effect if the formatted message is
	      ;; being kept in a different buffer than the raw
	      ;; message.  Mail-mode-mimeinfo does this.
	      (goto-char (point-min)))))))))

(defun rmime-dispatch (content type encoding disposition description)
  (let ((type                               (rmime-uncomment type))
        (encoding             (downcase (or (rmime-uncomment encoding) "")))
        (disposition                        (rmime-uncomment disposition)))
    (funcall (rmime-handler type) content type encoding disposition description)
    (if (not (bolp)) (insert "\n"))))	; Force a newline if none provided.

;; It is tempting to make this function recursive.  Unfortunately,
;; this can blow away max-lisp-eval-depth.
;;
;; It is important that annotations be sorted.  We assume that that
;; the number of annotations will be small compared to the total
;; number of messages so we sort the output list rather than the input
;; list.
;;
;; It is important that no annotation begin before start.  See
;; a_write() in fileio.c.
(defun rmime-annotate (start end)
  (let (new result (list rmime-overlay-list))
    (while (identity list)
      (if (eq (overlay-start (car list))
	      (overlay-end   (car list)))
	  (delete-overlay (car list))
	(let ((raw (overlay-get (car list) 'rmime-raw)))
	  (if (identity raw)
	      (let ((point (overlay-end (car list))))
		(if (and (>= point start) (<= point end))
		    (setq result (cons (cons point rmime-magic-string) (cons (cons point (car raw)) result)))))))
	(setq new (cons (car list) new)))
      (setq list (cdr list)))
    (setq rmime-overlay-list new)
    (sort result 'car-less-than-car)))

;; This is more important than it seems.  Yes, it does clean up
;; overlays that would otherwise leak, but that is not its most
;; important function!
;;
;; revert-buffer calls insert-file-contents with REPLACE=t.  This
;; function tries, best it can, to preserve text properties.  If a
;; message body begins with the same character as the highlighted
;; label, and this message is the first message that changed since
;; last being saved, then insert-file-contents will determine that a
;; big wad of text was inserted in a highlighted label.  This results
;; in large numbers of messages being highlighted.  The only
;; reasonable way of dealing with this is to nuke all highlighting
;; before insert-file-contents gets to see it.
(defun rmime-cancel-all ()
  (interactive)
  (save-excursion
   (funcall rmime-mimeinfo-method)
   (let ((cache (rmime-cache)))
     (let ((start (if cache (set-marker (make-marker) (overlay-start cache)) (point-min-marker)))
	   (end   (if cache (set-marker (make-marker) (overlay-end   cache)) (point-max-marker))))
       (widen)
       (rmime-modify-buffer
	 (save-excursion
	   (mapcar 'rmime-cancel-1 rmime-overlay-list)))
       (setq rmime-overlay-list nil)
       (narrow-to-region start end)))))

(defun rmime-cancel ()
  (interactive)
  (save-excursion
    (funcall rmime-mimeinfo-method)
    (let ((cache (rmime-cache)))
      (if (identity cache)
	  (rmime-modify-buffer
	    (widen)
	    (narrow-to-region (overlay-start cache) (overlay-end cache))
	    (rmime-cancel-1 cache)
	    ;; This is so rmime-cancel-all doesn't cancel us a second time.
	    ;; This saves us space, too.
	    (overlay-put cache 'rmime-raw nil))))))

(defun rmime-cancel-1 (cache)
  (if (overlay-get cache 'rmime-raw)
      (progn (goto-char (overlay-get cache 'rmime-marker))
	     (insert (car (overlay-get cache 'rmime-raw)))
	     (delete-region (point) (overlay-end cache))
	     (delete-overlay cache))))

;; We always move point to the beginning of the effected text and
;; leave the mark at the end of the effected text.  We are only called
;; for actions that effect text.
(defun rmime-action (begin end action &rest arguments)
  (if (not (eq (current-buffer) (marker-buffer begin)))
      (error "This MIME body doesn't belong in this buffer!"))
  (rmime-modify-buffer
    (save-restriction
      (widen)
      (goto-char begin)
      (rmime-invalidate-cache)
      (apply action arguments)
      (delete-region (point) end)
      (push-mark)
      (goto-char begin))))

;; Notice that we find the cache overlay even if the cache was
;; flushed.  This is important for (rmime-cancel).  Without it, it is
;; impossible to cancel MIME formatting if the cache was flushed.
(defun rmime-cache ()
  (let ((overlays (overlays-at (point))))
    (while (and (identity overlays)
		(not (overlay-get (car overlays) 'rmime-marker)))
      (setq overlays (cdr overlays)))
    (car overlays)))

(defun rmime-invalidate-cache ()
  (let ((cache (rmime-cache)))
    (if (identity cache)
	(overlay-put cache 'rmime-cache nil))))

;;; Text type

(defun rmime-text/* (content type encoding disposition description)
  (if (rmime-inline-p disposition)
      (rmime-text-action content type encoding)
    (let ((here (point-marker))
	  (keymap (copy-keymap (or (current-local-map) '(keymap)))))
      (insert (or description rmime-text-string) "\n")
      (rmime-interact keymap rmime-play-keystrokes 'rmime-action here (point-marker) 'rmime-text-action content type encoding)
      (put-text-property here (point-marker) 'face rmime-text-face)
      (put-text-property here (point-marker) 'local-map keymap))))

(defun rmime-text-action (content type encoding)
  (let ((here (point))
	(charset (cdr (assoc "charset" (rmime-parameters type)))))
    (apply 'rmime-insert-content nil encoding content)
    (if (not (member (downcase (or charset "")) rmime-transparent-charsets))
	(put-text-property here (point) 'face (intern charset)))))

;;; multipart type

(defun rmime-multipart/mixed (content type encoding disposition description)
  (if (not (member encoding rmime-transparent-encodings))
      (error "A MIME multipart message has an illegal Content-Transfer-Encoding"))
  (if (rmime-inline-p disposition)
      (rmime-multipart-action content type)
    (let ((here (point-marker))
	  (keymap (copy-keymap (or (current-local-map) '(keymap)))))
      (insert (or description rmime-multipart-string) "\n")
      (rmime-interact keymap rmime-play-keystrokes 'rmime-action here (point-marker) 'rmime-multipart-action content type)
      (put-text-property here (point-marker) 'face rmime-multipart-face)
      (put-text-property here (point-marker) 'local-map keymap))))

(defun rmime-multipart-action (content type)
  ;; rmime-multipart-alist is a "temporary global" that can be
  ;; referenced by any body part that needs to know about any other
  ;; body part.
  (let ((rmime-multipart-alist (rmime-multipart-split content (rmime-parameters type))))
    (mapcar (function (lambda (x) (apply 'rmime-dispatch (cdr x)))) rmime-multipart-alist)))

;; This recursive function splits a multipart MIME message.  A list of
;; the subparts is returned.  The car of each element is the
;; Content-Id.  A body part that needs to know about other body parts
;; can use assoc.  The cdr of each element is suitable to be applied
;; to rmime-dispatch.
(defun rmime-multipart-split (content parameters)
  (let ((boundary (cdr (assoc "boundary" parameters))))
    (if (or (not (stringp boundary)) (string= boundary ""))
	(error "A multipart MIME message has no boundary string"))
    (let ((separator (concat "^--" (regexp-quote boundary)))
	  (case-fold-search))
      (save-excursion
	(apply 'rmime-tmp-buffer content)
	(goto-char (point-min))
	(if (and (re-search-forward separator nil t)
		 (eq (forward-line) 0))
	    (rmime-multipart-split-1 separator))))))

(defun rmime-multipart-split-1 (separator)
  (let ((start (point)))
    (if (re-search-forward separator nil t)
	(cons (save-excursion
		(beginning-of-line)
		(save-restriction
		  (if (> (point) start)
		      (narrow-to-region start (1- (point)))
		    (narrow-to-region (point) (point)))
		  (goto-char (point-min))
		  (re-search-forward "^[ \t]*\n" nil 'move)
		  (let ((case-fold-search t)
			(content (list rmime-tmp-string (point) (point-max))))
		    (forward-line -1)
		    (narrow-to-region (point-min) (point))
		    (cons (rmime-fetch-header "Content-Id")
			  (cons content (rmime-standard-headers))))))
	      (if (and (not (looking-at "--"))
		       (eq (forward-line) 0))
		  (rmime-multipart-split-1 separator))))))

;; mime default

(defun rmime-*/* (content type encoding disposition description)
  (let ((here (point-marker))
	(keymap (copy-keymap (or (current-local-map) '(keymap)))))
    (insert (or description (format "Press C-c C-c here for \"%s\" data" type)) "\n")
    (rmime-interact keymap rmime-play-keystrokes 'rmime-application-action content type encoding)
    (put-text-property here (point-marker) 'face rmime-application-face)
    (put-text-property here (point-marker) 'local-map keymap)))

(defun rmime-application-action (content type encoding)
  (let ((filename (make-temp-name "/tmp/metamail")))
    (apply 'rmime-write-string-content filename content)
    (call-process rmime-metamail-program nil 0 nil "-b" "-x" "-z" "-c" type "-E" (or encoding "binary") "-m" "emacs" filename)))

;;;

(defun rmime-inline-p (disposition)
  (or (not disposition)
      (string-match "\\`[ \t]*inline[ \t]*\\(;\\|\\'\\)" disposition)))

(defun rmime-interact (keymap keystrokes function &rest arguments)
  (let ((action (make-symbol "rmime-interaction")))
    (fset action (list 'lambda nil '(interactive) (list 'apply (list 'quote function) (list 'quote arguments))))
    (mapcar (function (lambda (keystroke) (define-key keymap keystroke action))) keystrokes)))

(defun rmime-insert-content (binaryp encoding string start end)
  (cond ((member encoding rmime-transparent-encodings)
	 (rmime-insert-unencoded-content string start end))
	((string= encoding "base64")
	 (rmime-decode-content rmime-mimencode-program (if binaryp '("-u") '("-u" "-p")) string start end))
	((string= encoding "quoted-printable")
	 (rmime-decode-content rmime-mimencode-program '("-u" "-q")                      string start end))
	((and rmime-uufilter-program (string= encoding "x-uue"))
	 (rmime-decode-content rmime-uufilter-program  '("-s")                           string start end))
	((error "Unknown Content-Transfer-Encoding: %s" encoding))))

(defun rmime-insert-unencoded-content (string start end)
  (insert-buffer-substring (save-excursion (rmime-tmp-buffer string start end))))

(defun rmime-decode-content (program args string start end)
  (let ((filename (make-temp-name "/tmp/mime")))
    (unwind-protect
	(progn
	  (rmime-write-string-content filename string start end)
	  (apply 'call-process program filename t nil args))
      (if (file-exists-p filename) (delete-file filename)))))

(defun rmime-write-string-content (filename string start end)
  (let ((modes (default-file-modes)))
    (unwind-protect
	(save-excursion
	  (set-default-file-modes 448)	; -rwx------
	  (rmime-tmp-buffer string start end)
	  (write-region (point-min) (point-max) filename nil 'quiet))
      (set-default-file-modes modes))))

(defun rmime-tmp-buffer (string min max)
  (prog1
      (set-buffer (get-buffer-create rmime-tmp-buffer-name))
    (if (not (eq string rmime-tmp-string))
	(progn (make-local-variable 'rmime-tmp-string)
	       (setq rmime-tmp-string nil)
	       (erase-buffer)
	       (insert string)
	       (setq rmime-tmp-string string)))
    (widen)
    (narrow-to-region min max)))


;;; Routines for collecting information about a MIME entity


;; This function returns the function to call to display this
;; mime-type.
;;
;; We look at the Content-Type field and expect to find a string of
;; the form "type/subtype".  There may be whitespace around the slash
;; and case is insignificant.  If we find a Content-Type field that we
;; can't understand, we use the fallback handler (rmime-*/*).  If we
;; can't find a Content-Type field, we assume the default type.  The
;; default type is usually text/plain, but the multipart/digest mode
;; sets a local default type of message/rfc822.
(defun rmime-handler (type)
  (if (and type (string-match "\\([!#$%&'*+-.0-9A-Z^_a-z{|}~]+\\)[ \t]*/[ \t]*\\([!#$%&'*+-.0-9A-Z^_a-z{|}~]+\\)" type))
      (or (rmime-handler-1 (downcase (substring type (match-beginning 1) (match-end 1)))
			   (downcase (substring type (match-beginning 2) (match-end 2))))
	  (identity 'rmime-*/*))
    (identity rmime-default-type)))

(defun rmime-handler-1 (basetype subtype)
  (or (rmime-handler-2 basetype subtype)
      (rmime-handler-2 basetype "*")))

(defun rmime-handler-2 (basetype subtype)
  (let ((handler (intern (concat "rmime-" basetype "/" subtype))))
    (if (fboundp handler) handler)))

;; This recursive function parses a MIME parameter list.
;;
;; parameter list is *(";" parameter)
;; parameter      is attribute "=" value
;; attribute      is token
;; token          is 1*[!#$%&'*+-.0-9A-Z^_a-z{|}~]
;; value          is a token or a quoted-string
;;
;; Whitespace is permitted to appear around any token.
;;
;; Notice that this regular expression is not anchored to the
;; beginning of the string.  This means that we will ignore any
;; garbage we happen to find during parsing and continue the best we
;; can.  We depend on this when we pass in a Content-Type field
;; without first removing the basetype/subtype clause.
;;
;; The string ";[ \t]*" matches the leading semicolon and any
;; whitespace that may come after it.
;;
;; The string "[!#$%&'*+-.0-9A-Z^_a-z{|}~]+" matches the attribute.
;;
;; The string "[ \t]*=[ \t]*" matches the equals sign and any
;; whitespace that may come around it.
;;
;; The string "[!#$%&'*+-.0-9A-Z^_a-z{|}~]+" matches the (unquoted)
;; value.
;;
;;				  -- or --
;;
;; The string "\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"" matches the (quoted)
;;                  **********************
;;               ****************************
;; value.
;;
;; The string ".*" matches anything.
;;
;; Put them together to parse a parameter list.
(defun rmime-parameters (field)
  (if (and field (string-match "\\(;[ \t]*\\)\\([!#$%&'*+-.0-9A-Z^_a-z{|}~]+\\)\\([ \t]*=[ \t]*\\)\\([!#$%&'*+-.0-9A-Z^_a-z{|}~]+\\|\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"\\)\\(.*\\)" field))
      (cons (cons (downcase (substring field (match-beginning 2) (match-end 2)))
		  (if (match-beginning 5)
		      (save-match-data (rmime-unquote (substring field (match-beginning 5) (match-end 5))))
		    (substring field (match-beginning 4) (match-end 4))))
	    (rmime-parameters (substring field (match-beginning 7) (match-end 7))))))



;; This function extracts MIME information from a buffer which hasn't
;; set rmime-mimeinfo-method.  We look for a function name
;; MAJORMODE-mimeinfo.  If we find it, we call it.  If we don't, we
;; call rmime-standard-mimeinfo.
;;
;; We use this function to add MIME support to major modes that were
;; written before this package was.
;;
;; This function is obsolete before it was even written.  In the
;; future, it will be removed.  rmime-mimeinfo-method will point to
;; rmime-standard-mimeinfo and buffers that need special coding must
;; bind a local value to this variable.
(defun rmime-obsolete-mimeinfo ()
  (let ((function (intern (concat (symbol-name major-mode) "-mimeinfo"))))
    (if (fboundp function)
	(funcall function)
      (rmime-standard-mimeinfo))))

;; This function extracts MIME information from a buffer which
;; contains a header and a body separated by a specified regular
;; expression.  Most buffers fall into this category.
;;
;; The regular expression is stored in rmime-separator-regexp.  This
;; may be a buffer-local variable.  The default value matches a blank
;; line.
;;
;; If the regular expression doesn't match anything, the entire buffer
;; is the header and the body is empty.  This is important when we
;; parse phantom bodies in rmime-message/external-body.
;;
;; Someday this function will be the value of rmime-mimeinfo-method
;; and rmime-obsolete-mimeinfo will no longer exist.
;;
;; MIME infomation is represented as:
;;  	(type description encoding disposition)
;; All fields are character strings or nil if the field is not
;; provided.
(defun rmime-standard-mimeinfo ()
  (rmime-regexp-mimeinfo rmime-separator-regexp))

(defun rmime-regexp-mimeinfo (regexp)
  (goto-char (point-min))
  (let (case-fold-search)
    (let ((found (re-search-forward regexp nil 'move))
	  (case-fold-search t))
      (prog1
	  (save-restriction
	    (if found (narrow-to-region (point-min) (match-beginning 0)))
	    (rmime-standard-headers))
	(goto-char (or found (point-max)))))))

;; This function fetches four headers mentioned in RFC 1521.  The
;; fifth header mentioned in RFC 1521 (Content-Id) has already been
;; dealt with.  (The sixth header (MIME-Version) is ignored --- if it
;; looks like a MIME message then it must be one.)
;;
;; Any function which calls this function *must* set case-fold-search
;; to t and must also narrow the buffer to just the headers of a
;; message.
(defun rmime-standard-headers ()
  (mapcar 'rmime-fetch-header '("Content-Type"
				"Content-Transfer-Encoding"
				"Content-Disposition"
				"Content-Description")))


;;; Routines for RFC 822 header manipulations


;; This function extracts one header from a message.
;;
;; Any function which calls this function *must* set case-fold-search
;; to t and must also narrow the buffer to just the headers of a
;; message.
;;
;; The string "^xxx:" matches the header we are looking for.
;;
;; The string "\\(\n*[ \t]\\)*" matches any string of whitespace not
;;             ***************
;; ending with a newline.  We don't have to worry about encountering
;; two newlines in a row because we have narrowed our buffer.  In
;; fact, if we do encounter two newlines in a row, we assume that the
;; function that called us knew what it was doing and we treat the
;; blank line as part of a header.
;;
;; The string "[^ \t\n]\\(\\(\n*[ \t]\\)*[^ \t\n]\\)*" matches any
;;                        ***************
;;                     ******************************
;; sequence of characters that doesn't start or end with whitespace.
;; At least one character is required.  Any newlines must be followed
;; by a tab or a space.
(defun rmime-fetch-header (header)
  (goto-char (point-min))
  (if (re-search-forward (concat "\\(^" (regexp-quote header) ":\\)\\(\\(\n*[ \t]\\)*\\)\\([^ \t\n]\\(\\(\n*[ \t]\\)*[^ \t\n]\\)*\\)") nil t)
      (rmime-unfold (buffer-substring (match-beginning 4) (match-end 4)))))

;; This recursive function unfolds RFC 822 header lines.
;;
;; RFC 822 doesn't say anything about whitespace at the end of a line,
;; but RFC 1521 implies that they should be blown away.
;;
;; RFC 822 says to replace the CRLF LWSP sequence with just the LWSP
;; char, but if we do this, the example in RFC 1521 won't work.  We
;; choose to replace CRLF LWSP with a space.
;;
;; The string "\\(.*[^ \t]\\)?" matches a string not ending with white
;;             ***************
;; space (including the empty string).
;;
;; The string "[ \t]*\n[ \t]*" matches a line break and any white
;; space around it.
;;
;; The string "\\(.\\|\n\\)*" matches anything (including linebreaks).
;;             *************
;;
;; Put them together and get some code to unfold RFC 822 header lines.
(defun rmime-unfold (string)
  (if (string-match "^\\(\\(.*[^ \t]\\)?\\)\\([ \t]*\n[ \t]*\\)\\(\\(.\\|\n\\)*\\)$" string)
      (concat (substring string (match-beginning 1) (match-end 1))
	      " "
	      (rmime-unfold (substring string (match-beginning 4) (match-end 4))))
    (identity string)))

;; This recursive function removes RFC 822 comments.
;;
;; Comments are text in parenthesis.
;;
;; Comments can nest.  Comments can't appear inside quotes.
;;
;; Except at the beginning or end of a line, comments are replaced
;; with a space.  This is always safe because comments can't appear in
;; an atom.  At the beginning or end of a line, they are just removed
;; (along with any whitespace around them).
;;
;; The string "\\([ \t]*\\([^\\\" \t]\\|\\\\.\\|\"\\([^\\\"]\\|\\\\.\\)*\"\\)\\)*"
;;                                                **********************
;;                      *****************************************************
;;             ******************************************************************
;; matches any string with an even number of quote marks not ending
;; with white space.  Quote marks that are backquoted don't count.  We
;; won't match a string ending with a backslash.  We will match a
;; string ending with backslash space.
;;
;; The string "[ \t]*(\\([^\\()]\\|\\\\.\\)*)[ \t]*" matches a comment
;;                    **********************
;; and its surrounding white space.
;;
;; The string "\\([^ \t].*\\)?" matches any string not beginning with
;;             ***************
;; white space.
;;
;; Put them together and get some code to remove comments from headers.
(defun rmime-uncomment (string)
  (if (and string (string-match "^\\(\\([ \t]*\\([^\\\" \t]\\|\\\\.\\|\"\\([^\\\"]\\|\\\\.\\)*\"\\)\\)*\\)\\([ \t]*(\\([^\\()]\\|\\\\.\\)*)[ \t]*\\)\\(\\([^ \t].*\\)?\\)$" string))
      (if (eq (match-beginning 1) (match-end 1))
	  (if (eq (match-beginning 7) (match-end 7))
	      (identity nil)
	    (rmime-uncomment (substring string (match-beginning 7) (match-end 7))))
	(if (eq (match-beginning 7) (match-end 7))
	    (rmime-uncomment (substring string (match-beginning 1) (match-end 1)))
	  (rmime-uncomment (concat (substring string (match-beginning 1) (match-end 1))
				   " "
				   (substring string (match-beginning 7) (match-end 7))))))
    (identity string)))

;; This recursive function removes backslashes to reveal the character
;; they protect.
;;
;; Note that this function has nothing to do with "this kind of
;; quoting".
;;
;; The string "[^\\]*" matches any string not containing a backslash.
;;
;; The string "\\\\" matches a backslash.
;;
;; The string "." matches a character following a backslash.
;;
;; The string ".*" matches anything.
;;
;; Put them together and get some code to remove backslashes.
(defun rmime-unquote (string)
  (if (and string (string-match "^\\([^\\]*\\)\\(\\\\\\)\\(.\\)\\(.*\\)$" string))
      (concat (substring string (match-beginning 1) (match-end 1))
	      (substring string (match-beginning 3) (match-end 3))
	      (rmime-unquote (substring string (match-beginning 4) (match-end 4))))
    (identity string)))


;;; Temporary glue for major modes that should know about MIME


;; Get MIME information from an RMAIL buffer.
;;
;; We can't use rmime-standard-mimeinfo because an important header
;; line may be part of rmail-ignored-headers.
(defun rmail-mode-mimeinfo ()
  (goto-char (point-min))
  (re-search-forward "\\(\\`\\|\n\\)[ \t]*\n" nil 'move)
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (aref rmail-message-vector rmail-current-message))
	(narrow-to-region (point) (point-max))
	(re-search-forward "^[ \t]*\n" nil 'move)
	(narrow-to-region (point-min) (match-beginning 0))
	(rmime-standard-headers)))))

;; Get MIME information from a *mail* buffer or *post-news* buffer.
;;
;; It is wrong to terminate the regular expression with "$" instead of
;; "\n".  Using "$" will cause the newline at the end of the separator
;; to be interpreted as the first character of the body.
(defun mail-mode-mimeinfo ()
  (prog1
      (rmime-regexp-mimeinfo (concat "^" (regexp-quote mail-header-separator) "\n"))
    (let ((window (selected-window)))
      (pop-to-buffer (concat (buffer-name) "-mime"))
      (erase-buffer)
      (save-excursion
	(select-window window)))))
(defalias 'news-reply-mode-mimeinfo 'mail-mode-mimeinfo)

;; Get MIME information from a +inbox buffer
(defun mh-folder-mode-mimeinfo ()
  (let* ((num (mh-get-msg-num t))
	 (filename (mh-msg-filename num mh-current-folder)))
    (if (not (file-exists-p filename))
	(error "Message %d does not exist" num))
    (set-buffer (get-buffer-create mh-show-buffer))
    ;; Unfortunatly, we need to reload the buffer even if it looks up
    ;; to date.  An important header field may be invisible.
    (clear-visited-file-modtime)
    (unlock-buffer)
    (setq buffer-file-name nil)
    (erase-buffer)
    (insert-file-contents filename)
    (rmime-standard-mimeinfo)))

;; Get MIME information from an INBOX buffer.
(defun vm-mode-mimeinfo ()

  ;; Check that vm is configured right.
  ;;
  ;; vm-preview-current-message sets the restriction before calling
  ;; vm-select-message-hook.  We are called inside a save-restriction
  ;; so we can't change our restriction.  We can't edit outside of our
  ;; restriction or else parts of the following message might show
  ;; after save-restriction restores the old restriction.
  ;;
  ;; Note to VM authors: bind vm-rmime-ok to a value when this
  ;; limitation is removed.
  (if (not (or (eq vm-preview-lines t) (boundp 'vm-rmime-ok)))
      (error "Sorry --- you must (setq vm-preview-lines t) to use VM with RMIME"))

  ;; We can't compile the rest of this function because some of the
  ;; subroutines we need are really macros.
  (dont-compile

    ;; Don't even think of doing this for any other major mode!
    ;;
    ;; It works only because we are called inside a save-restriction.
    ;; We would rather not be called inside a save-restriction.
    ;;
    ;; The idea is to propagate information to rmime-cancel so that
    ;; canceling the formatting doesn't cause invisible header lines
    ;; to suddenly become visible.
    (narrow-to-region (vm-vheaders-of (car vm-message-pointer)) (point-max))

    (let (case-fold-search)
      (prog1
	  (save-restriction
	    (narrow-to-region (vm-headers-of (car vm-message-pointer))
			      (vm-text-of    (car vm-message-pointer)))
	    (rmime-standard-headers))
	(goto-char (vm-text-of (car vm-message-pointer)))))))


;;; RMIME is available!

(run-hooks 'rmime-load-hook)		; For user customizations.
(provide 'rmime)

;;; rmime.el ends here

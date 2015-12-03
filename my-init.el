;;; my-init.el --- My personal emacs startup file.

;; Copyright (C) 1995 Anders Lindgren.

;; Author: Anders Lindgren <andersl@csd.uu.se>
;; Maintainer: Anders Lindgren <andersl@csd.uu.se>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;{{{ Introduction

;; This is my personal Emacs init file.  It has grown from a few lines
;; back in -91 into about 50k as of the end of -95.
;;
;; This file contains stuff from simple variable assignments into
;; complex enhancements and configurations
;;
;; `my-init' runs under Emacs 19 and XEmacs.
;;
;; Don't use this file "as-is" since it contains a lot of references to
;; personal stuff, and stuff local to Uppsala.  


;; While programming over a "couple" of years (more than half my
;; life), I have realised that a good programming environment is
;; essencial.  With Emacs, most of my demands have been fulfilled, but
;; not all.
;;
;; Luckily, Emacs is a programmable editor, and I have been able to
;; add some of the missing features to it.  I'm not going to tell you
;; to use them because they are "the best".  I just wanted you to know
;; that they exists, and that I think that they are well worth the
;; time I have invested into them:
;;
;; Follow Mode -- Takes two (or more) windows and virtually glues
;;		  them together to form a very "tall" window.
;;		  If you have access to a large graphical display,
;;		  you can work with, say, 144 lines of code at the
;;		  same time.
;;
;; Folding Mode -- Written by Jamie Lokier, But I have spent some
;;		   time enahncing it.  It allows you to make
;;		   parts of your code invisible.  You use
;;		   markers on the form `{{{' and `}}}' to
;;		   build up an hierarchy.  Folding allows you
;;		   to view only the headers (making them look
;;		   like a menu or directory listing).  It can
;;		   also narrow the text so that everything outisde
;;		   a fold is invisible.
;;
;; These, and other projects, can be found at:
;;    http://www.csd.uu.se/~andersl/emacs.shtml


;; TODO:
;;
;; Incorporate the init-file for `auc-tex'.
;;
;; Add Emacs-18 support.

;;}}}

;;; Code:

;;{{{ General

;;{{{ Emacs versions, private variables.

;;; TODO: Remove `my-emacs-type'.  Maybe add a new variable with
;;; the semantics that we are running under Emacs 19 or XEmacs, and
;;; make sure this file is runable under Emacs 18 aswell.

;;;---------------------------------------------------------------------
;;; Temporary constant, my-emacs-type
;;;

(defconst my-emacs-type
  (cond
   ((boundp 'epoch::version)		       'emacs-epoch)
   ((string-match "Lucid" emacs-version)       'emacs-lucid)
   ((string-match "^19" emacs-version)         'emacs-19)
   ((string-match "^18" emacs-version)         'emacs-18)
   (t                                          'emacs-19)))


;;;---------------------------------------------------------------------
;;; More temporary constants:
;;;
;;;  my-emacs-lucid-p  - if we run under lucid or XEmacs.  
;;;  my-emacs-xemacs-p - if we run under XEmacs.  
;;;  my-emacs-19-p     - if used under Emacs 19, not XEmacs
;;;  my-emacs-19-n-p   - if we run _at_least_ version 19.n of Emacs.
;;;

(defvar my-emacs-lucid-p (eq my-emacs-type 'emacs-lucid))
(defvar my-emacs-xemacs-p (string-match "XEmacs" emacs-version))
(defvar my-emacs-19-p (eq my-emacs-type 'emacs-19))
(defvar my-emacs-19-28-p (and (eq my-emacs-type 'emacs-19)
			      (>= emacs-minor-version 28)))
(defvar my-emacs-19-29-p (and (eq my-emacs-type 'emacs-19)
			      (>= emacs-minor-version 29)))
(defvar my-emacs-19-30-p (and (eq my-emacs-type 'emacs-19)
			      (>= emacs-minor-version 30)))

;;}}}
;;{{{ The load path

;;;---------------------------------------------------------------------
;;; Make sure we will find all elisp files we need.
;;;
;;; I have one `patch'-directory per Emacs version, hence the
;;; cryptical concat-expression below:
;;;
;;; If you are planning to steal this, don't.  Use the smaller
;;; version, and copy everything you need to `~/emacs' or
;;; `~/emacs/src'.
;;;
;;; (setq load-path 
;;;       (append load-path
;;;		  '("/usr/hacks/lib/emacs/elisp")
;;;	          (mapcar 'expand-file-name
;;;		          '("/usr/hacks/lib/emacs/elisp"
;;;			    "~/emacs"
;;;			    "~/emacs/src"
;;;			    "~andersl/emacs/src"
;;;                         ))))

(setq load-path
      (append (mapcar 'expand-file-name
		      (list "~andersl/src/follow"
			    "~andersl/src/fdb"
			    "~andersl/src/debug"
			    "~andersl/src/folding"
			    "~andersl/src/erlang"
			    "~andersl/src/dirabbrev"
			    (concat "~andersl/emacs/patch/" 
				    (symbol-name my-emacs-type)
				    (if (boundp 'emacs-minor-version)
					(concat "." (number-to-string 
						     emacs-minor-version))))))
	      load-path
	      (mapcar 'expand-file-name
		      '("~andersl/emacs/src" 
			"/usr/hacks/lib/emacs/elisp"
			"~andersl/emacs/tm"
			"~andersl/emacs/tl"
			"~andersl/emacs/mel"
			))))

;;}}}


;;;---------------------------------------------------------------------
;;; Internal emacs variables etc.
;;;

(setq gc-cons-threshold 1000000)
;; (setq ctl-arrow t)
(setq truncate-partial-width-windows nil)
(setq window-min-height 3)
;; (setq next-screen-context-lines 2)
(setq completion-auto-help t)		;; I want as much help as I can get
(setq completion-auto-exit t)		;; Don't want to always hit return
(setq require-final-newline 'ask)
(setq track-eol t)
(setq mouse-yank-at-point t)
(setq print-escape-newlines t)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq default-major-mode 'text-mode)
(setq garbage-collection-messages t)	;; Traditional
; (setq sentence-end-double-space nil)

;; No optimizations please, unless running from home...
(if window-system
    (setq baud-rate 1000000)
  (setq baud-rate 2400))

;; Unify physical and logical directories...
(setq directory-abbrev-alist '(("^/home/kobra2" . "/home/kobra")))
(setq mail-host-address "csd.uu.se")

;; (setq abbrev-file-name "~/emacs/modabbrev.defs") ; I want to use my abbrevs.
;; (if (file-readable-p abbrev-file-name)
;;    (read-abbrev-file abbrev-file-name))


(put 'eval-expression 'disabled nil)


;;;---------------------------------------------------------------------
;;; Mark handling.  The following two lines makes the highlighted
;;; region visible, but I'm still able to use all region-commands even
;;; if the region has been turned off (just like in the good ol'
;;; days!)
;;;

(if (boundp 'transient-mark-mode)
    (setq transient-mark-mode t))
(setq mark-even-if-inactive t)


;;;---------------------------------------------------------------------
;;; Search
;;;

(setq-default search-highlight t)
(define-key isearch-mode-map "\C-^" 'isearch-edit-string)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

(copy-face 'highlight 'isearch)


;;;---------------------------------------------------------------------
;;; Autosave and Backup
;;;

(setq auto-save-default t)
(setq auto-save-visited-file-name nil)
(setq make-backup-files t)
(setq backup-by-copying-when-linked t)  ; Preserve links!


;;;---------------------------------------------------------------------
;;; The mode line and the frame header.
;;;
;;; The following section adds the line number to the mode line, and
;;; the time and date to the frame header line.  The date is displayed
;;; in standard european 24 hour format, the format americans tends to
;;; refer to as "military" time...

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; `system-name' variable is not defined in XEmacs.
(defvar my-system-name (system-name)
  "The name of the system we are running on.")

(cond (window-system
       (line-number-mode t)
       (setq frame-title-format 
	     '((multiple-frames ("%b   ") 
				("" invocation-name "@" my-system-name))
	       "    " 
	       display-time-string))
       ;; Per default, the time and date goes into the mode line.
       ;; We want's it in the header line, so lets remove it.
       (remove-hook 'global-mode-string 'display-time-string)))
      

;;;---------------------------------------------------------------------
;;; Make new windows split the display into two side-by-side windows,
;;; which is useful since my Emacs use a full screen window.
;;;

(defun my-display-buffer-function (buf not-this-window)
  (if (and (not pop-up-frames)
	   (one-window-p)
	   (or not-this-window
	       (not (eq (window-buffer (selected-window)) buf)))
	   (> (frame-width) 162))
      (split-window-horizontally))
  (let ((display-buffer-function nil))
    (display-buffer buf not-this-window)))

(if (not (eq my-emacs-type 'emacs-lucid))
    (setq display-buffer-function 'my-display-buffer-function))


;;;---------------------------------------------------------------------
;;; Ignore font warnings in XEmacs
;;; (Warning: this is a hack!)
;;;

(if (eq my-emacs-type 'emacs-lucid)
    (fset 'display-warnings 'ignore))

;;}}}
;;{{{ Keyboard

;;;---------------------------------------------------------------------
;;; Redefine the keyboard
;;;

;; Goto a specific line is really needed!
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xs" 'save-buffer)


;;
;; Walk between the windows
;;

(defun my-previous-window ()
  "Previous window"
  (interactive) 
  (other-window -1))
(global-set-key "\C-xp" 'my-previous-window)

(global-set-key "\C-xn" 'other-window)


;;
;; Buffer cycling functions
;;

(defun my-unbury-buffer (&optional buf)
  "Select buffer BUF, or the last one in the buffer list.
This function is the opposite of `bury-buffer'."
  (interactive)
  (or buf (setq buf (car (reverse (buffer-list)))))
  (switch-to-buffer buf))


(cond (my-emacs-19-p
       (global-set-key '[C-tab] 'bury-buffer)
       (global-set-key '[C-backtab] 'my-unbury-buffer)))


;;
;; It's in my fingers...
;;

(global-set-key "\e\e" 'eval-expression)


;;
;; Give me back my f1 key!
;;

(and (eq my-emacs-type 'emacs-19)
     (= emacs-minor-version 29)
     (define-key key-translation-map [f1] nil))


;;
;; Make the `Print/Enter' key work as M-x again!
;;

(if (eq my-emacs-type 'emacs-lucid)
    (global-set-key [execute] 'execute-extended-command))


;;
;; Some functionality I don't want.
;;

(global-set-key "\C-x\C-u" 'ignore)	;was upcase-region
(global-set-key "\C-x\C-l" 'ignore)	;was downcase-region


;;;---------------------------------------------------------------------
;;; Smarter `read-file-name'.
;;;
;;; `minibuffer-electric-slash' stolen from XEmacs.
;;; Minor-mode:ized by Anders Lindgren.

(if (not (fboundp 'minibuffer-electric-slash))
    (progn      
      (defvar my-electric-file-mode nil)
      (defvar my-electric-file-mode-map (make-sparse-keymap))
      (make-local-variable 'my-electric-file-mode)

      (defun minibuffer-electric-slash ()
	;; by Stig@hackvan.com
	(interactive)
	(and (eq ?/ (preceding-char))
	     ;; permit `//hostname/path/to/file'
	     (not (eq (point) (1+ (point-min)))) 
	     ;; permit `http://url/goes/here'
	     (not (eq ?: (char-after (- (point) 2))))
	     (delete-region (point-min) (point)))
	(insert ?/))

      (defun minibuffer-electric-tilde ()
	(interactive)
	(and (eq ?/ (preceding-char))
	     (delete-region (point-min) (point)))
	(insert ?~))

      (defun minibuffer-electric-dollar ()
	(interactive)
	(and (eq ?/ (preceding-char))
	     (delete-region (point-min) (point)))
	(insert ?$))

      (define-key my-electric-file-mode-map "/" 'minibuffer-electric-slash)
      (define-key my-electric-file-mode-map "~" 'minibuffer-electric-tilde)
      (define-key my-electric-file-mode-map "$" 'minibuffer-electric-dollar)

      (setq minor-mode-map-alist (cons (cons 'my-electric-file-mode 
					     my-electric-file-mode-map)
				       minor-mode-map-alist))

      (defun my-electric-minibuffer-setup ()
	(setq my-electric-file-mode	      
	      (and (boundp 'minibuffer-completion-table)
		   (eq minibuffer-completion-table 'read-file-name-internal))))
      (defun my-electric-minibuffer-exit ()
	(setq my-electric-file-mode nil))
      (add-hook 'minibuffer-setup-hook 'my-electric-minibuffer-setup)
      (add-hook 'minibuffer-exit-hook  'my-electric-minibuffer-exit)))

;;}}}
;;{{{ Packages

;;{{{ Fdb

;;;---------------------------------------------------------------------
;;; I, as a developer of Emacs Lisp, have got the debugger turned on
;;; all the time.  This program makes sure some common error won't
;;; invoke the debugger.
;;;

(load "fdb.elc" 'nomessage 'noerror)

;;}}}
;;{{{ paren

;;; ------------------------------
;;; Highligth parenteses
;;;

;;; Here we have three flavours, mic-paren, stig-paren and paren...
;;;
;;; mic-paren: By mic@docs.uu.se,  Great!
;;;
;;; stig-paren: The deluxe version, normally the hackers choice.
;;; Should the matching parenthesis be outside the window, the echo
;;; buffer displays the matching line.  S-exp-mode, where everything
;;; between the parentheses is highlighted.  Dings when parenthesis
;;; are mismatched.
;;;
;;; paren: Matches both parenthesis, not just the one at the "other
;;; end".  The parenthesis is highlighted even if it is not displayed
;;; in the same window.  This one is faster since it hangs on
;;; `post-command-idle-hook', as compared to `post-command-hook'.


(cond 
;;      ((and window-system my-emacs-19-p)
;;       (require 'stig-paren)
;;       (setq paren-match-face 'bold)
;;       (make-variable-buffer-local 'blink-matching-paren)
;;       (setq-default blink-matching-paren nil)
;;       (make-variable-buffer-local 'paren-sexp-mode)
;;       (setq-default paren-sexp-mode nil)
;;       (global-set-key [?\C-\(] 'stig-paren-toggle-dingaling-mode)
;;       (global-set-key [?\C-\)] 'stig-paren-toggle-sexp-mode))

 ((and window-system my-emacs-19-p)
  (setq paren-face 'bold)
  (setq paren-highlight-offscreen t)
  ;(if (file-readable-p "~mic/emacs")
  ;    (require 'mic-paren (expand-file-name "~mic/emacs/mic-paren.el"))
    (load "mic-paren" 'noerror 'nomessage))
  ;)

;; ((and window-system my-emacs-19-p)
;;  (setq show-paren-face 'bold)
;;  (setq show-paren-mismatch-face 
;;	(if (x-display-color-p)
;;	    (let ((fn 'my-show-paren-mismatch-face))
;;	      (copy-face 'default fn)
;;	      (set-face-background fn "DeepPink")
;;	      fn)
;;	  'modeline))
;;  (require 'paren))
 
 ((and window-system my-emacs-lucid-p)
  (require 'paren))
 
 (t
  (setq blink-matching-paren t)))

;;}}}
;;{{{ imenu

;;;------------------------------------------------------------
;;; imenu
;;;

(if (eq my-emacs-type 'emacs-19)
    (progn
      (setq imenu-max-items 30)
      (autoload 'imenu-add-to-menubar "imenu" nil t)))

;;}}}
;;{{{ ark-goto-url

;;;---------------------------------------------------------------------
;;; ark-goto-url
;;;

(autoload 'ark-goto-url		"ark-goto-url" t)
(autoload 'ark-goto-url-mouse	"ark-goto-url" t)

(if (eq my-emacs-type 'emacs-19)
    (global-set-key [mouse-3]	'ark-goto-url-mouse))

(setq goto-url-command-start "netscape_beta -noraise -remote 'openURL(")

;;}}}
;;{{{ load-library/which

;;;---------------------------------------------------------------------
;;; Completion on load-library
;;;

;(if (eq my-emacs-type 'emacs-lucid)
;    nil
;  (fmakunbound 'load-library)
;  (autoload 'load-library "libcomp" nil t)
;  (autoload 'locate-file "libcomp" nil t)
;  (autoload 'library-all-completions "libcomp")
;  (autoload 'read-library "libcomp"))


;;;---------------------------------------------------------------------
;;; Like shell `which' but for elisp-libraries
;;;

;(autoload 'which "which" "Like shell-which but for elisp-libraries." t)

;;}}}
;;{{{ Time-stamp

;;;---------------------------------------------------------------------
;;; Time stamp
;;;
;;; AJn 940614
;;; AJn 950318 Changed time-stamp-format to ISO 8601
;;;
;;; If "Time-stamp: <>" or "Time-stamp " "" exist within the first
;;; 8 lines i a file when saving, it will be written back like this
;;; "Time-stamp: <'Date' 'Time' 'user'>"

(require 'time-stamp)
(if (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks (cons 'time-stamp write-file-hooks)))
(setq time-stamp-format			; Use ISO 8601 date format
      '(time-stamp-yyyy-mm-dd time-stamp-hhmm user-login-name))

;;}}}
;;{{{ framepop (OFF)

;;;---------------------------------------------------------------------
;;; framepop
;;;

;; Problems:
;; - Don't disappear when done (e.g. Completion)
;; - Appears sometime when not needed (bbdb)

;(cond (window-system (require 'framepop)))
;(define-key global-map [f5] framepop-map)

;;}}}
;;{{{ ff-paths (OFF)

;;;---------------------------------------------------------------------
;;; Find File using paths
;;;

;; Requires dired-aux.

;(if (eq my-emacs-type 'emacs-19)
;    (progn
;      (load "ff-paths.el" 'nomessage 'noerror)
;      (define-key ctl-x-map "\C-f" 'find-file-using-paths)))

;;}}}
;;{{{ dirabbrev

;;;---------------------------------------------------------------------
;;; Abbreviate user paths.
;;;

(load "dirabbrev.el" 'noerror 'nomessage)

;;}}}
;;{{{ Lispdir

;;;------------------------------------------------------------
;;; LISPDIR - The directory of elisp packages in Ohio (LCD).
;;;

(autoload 'format-lisp-code-directory "lispdir" nil t)
(autoload 'lisp-dir-apropos "lispdir" nil t)
(autoload 'lisp-dir-retrieve "lispdir" nil t)
(autoload 'lisp-dir-verify "lispdir" nil t)

;;}}}
;;{{{ dabbrev

;;;---------------------------------------------------------------------
;;; dabbrev. The behaviour changed when 19.29 was released.
;;;

(and (eq my-emacs-type 'emacs-19)
     (boundp 'emacs-minor-version)
     (>= emacs-minor-version 29)
     (setq dabbrev-case-fold-search nil))

;;}}}
;;{{{ jka-compr

;;;----------------------------------------------------------------------
;;; Handle compressed files just like normal files
;;;

(require 'jka-compr)

;; 19.30
;; (auto-compression-mode 1)

;;}}}
;;{{{ ange-ftp

;;;----------------------------------------------------------------------
;;;  Ange-Ftp
;;;

(require 'ange-ftp)
(setq ange-ftp-default-user "anonymous")
(setq ange-ftp-generate-anonymous-password t)

;;}}}
;;{{{ lispref (OFF)

;;;------------------------------------------------------------
;;; Lispref - Find references in the elisp manual.
;;;

;  (autoload 'lispref-search "lispref" nil t)

;;}}}
;;{{{ rsz-mini

(load "rsz-mini" 'noerror 'nomessage)

;;}}}
;;{{{ Horisontal resize of windows.

(cond ((eq my-emacs-type 'emacs-19)
       (autoload 'mouse-drag-scroll-bar "my-mouse"
	 "Change the width of a window by dragging on the scroll bar.")
       (global-set-key [vertical-scroll-bar S-down-mouse-1] 
		       'mouse-drag-scroll-bar)))

;;}}}
;;{{{ Hilit19 (Turing it OFF)

;; I really doesn't like hilit, but a lot of packages use it, for
;; example `webster19'.  The following code will minimize the
;; damage...
;;
;; Note: This must be set before `hilit19' is loaded.

;(setq hilit-mode-enable-list 
;      '(not emacs-lisp-mode lisp-mode latex-mode tex-mode c-mode))

(setq hilit-mode-enable-list '())

;;}}}
;;{{{ Version Control

;; I prefere to have normal backup files even though I sometimes use
;; RCS.  The problem is that I very seldom adds something to the
;; repository.

(setq vc-make-backup-files t)

;;}}}
;;{{{ ps-print

;;;---------------------------------------------------------------------
;;; ps-print
;;;


;; ps-print in XEmacs 19.12 is broken.
(if (eq my-emacs-type 'emacs-lucid)
    (defalias 'colorp 'color-specifier-p))


(cond ((or my-emacs-19-30-p my-emacs-lucid-p)
       (setq ps-italic-faces '(font-lock-comment-face
			       font-lock-string-face
			       font-lock-variable-name-face))
       (setq ps-bold-faces '(font-lock-keyword-face 
			     font-lock-variable-name-face 
			     font-lock-type-face
			     font-lock-function-name-face)))
      (t       
       (setq ps-italic-faces '(comment))
       (setq ps-bold-faces '(keyword))
       (load "ps-print" 'noerror 'nomessage)))


(setq ps-paper-type 'ps-a4)
(setq ps-print-color-p nil)


;; Two-paged output:

(defun ps2-print ()
  (interactive)
  (let ((ps-lpr-command "sh")
	(ps-lpr-switches 
	 (list "-c" 
	       (concat "eval 'psmulti -pages 2 | "
		       lpr-command))))
    (ps-print-buffer-with-faces)))


;; Abbreviate the full path with one relative the users home directory.
;; Note:  To be able to use this price of code, the package "dirabbrev",
;; written by me must be installed!
;;
;; On the other hand, that package will probably be rewritten making
;; this code obsolete.  You have been warned!

;; Note: For C, Emacs Lisp, and Erlang mode this variable is
;; later changed.
(setq-default ps-left-header '(ps-get-buffer-name my-ps-header-dirpart))

(defun my-ps-header-dirpart ()
  (let ((fname (buffer-file-name))
	(dirabbrev-command-flags (cons "--nohome" dirabbrev-command-flags)))
    (if fname
	(let ((dname (dirabbrev-file-name (file-name-directory fname))))
	  (if (string-equal (buffer-name) (file-name-nondirectory fname))
	      dname			; Just directory name
	    (concat dname (file-name-nondirectory fname))))
      "")))

;;}}}
;;{{{ Calc

;;;----------------------------------------------------------------------
;;; A little calculator
;;;

(autoload 'calc "calc.elc" "Calculator Mode" t nil)
(autoload 'quick-calc "calc.elc" "Quick Calculator" t nil)
(autoload 'full-calc "calc.elc" "Full-screen Calculator" t nil)
(autoload 'calc-eval "calc.elc" "Call Calculator" nil nil)
(autoload 'calc-grab-region "calc-ext.elc" nil t nil)
(autoload 'defmath "calc-ext.elc" nil t t)
(autoload 'calc-extensions "calc-ext.elc" nil nil nil)
(global-set-key "\e#" 'calc)

;;}}}
;;{{{ Webster 19

;;;---------------------------------------------------------------------
;;; Webster19
;;;

(setq webster-host "webster.csd.uu.se")

(autoload 'webster "webster19" nil t)
(autoload 'webster-minor-mode "webster19" nil t)

(fdb-add-signal 'error "^No cross reference$")

;;}}}
;;{{{ ISpell

(defun my-ispell-after-load ()
  (setq ispell-grep-command "/usr/local/gnu/bin/grep")
  (setq ispell-complete-word-dict "/usr/sup/share/lib/dict/english"))

(if (fboundp 'eval-after-load)
    (eval-after-load "ispell" '(my-ispell-after-load)))

;;}}}
;;{{{ Info

;;;---------------------------------------------------------------------
;;; INFO
;;;

;; Add other libraries with infos

(add-hook 'Info-mode-hook 'my-info-mode-hook)

(defun my-info-mode-hook ()
  (interactive)
  (setq Info-directory-list
	(append (mapcar 'expand-file-name
			'("/usr/hacks/doc/info/"))
		Info-directory-list))
  (setq Info-enable-edit nil))

;;}}}
;;{{{ igrep

(autoload (function igrep) "igrep"
  "*Run `grep' to match EXPRESSION in FILES..." t)
(autoload (function egrep) "igrep"
  "*Run `egrep'..." t)
(autoload (function fgrep) "igrep"
  "*Run `fgrep'..." t)
(autoload (function igrep-recursively) "igrep"
  "*Run `grep' recursively..." t)
(autoload (function egrep-recursively) "igrep"
  "*Run `egrep' recursively..." t)
(autoload (function fgrep-recursively) "igrep"
  "*Run `fgrep' recursively..." t)

;;}}}
;;{{{ elp

;; The best profiler.

(autoload 'elp-instrument-function "elp" nil t)
(autoload 'elp-restore-function "elp" nil t)
(autoload 'elp-instrument-list "elp" nil t)
(autoload 'elp-instrument-package "elp" nil t)
(autoload 'elp-results "elp" nil t)

;;}}}

;;{{{ Irc

;;;----------------------------------------------------------------------
;;; IRC
;;;

(autoload 'irc "Kiwi" "Internet Relay Chat client." t nil) ; Chatting.
(setq irc-server "irc.nada.kth.se"
      irc-msg-public "%s%9s/%s:  "
      irc-pop-on-signal 3
      irc-userinfo (concat
		    "Studying computer science at the university of"
		    " Uppsala. Call me on +46 18 247618.")
      irc-startup-hook '(lambda ()
			 (irc-execute-command "links *.se")
			 (irc-execute-command "who *.se")
			 (irc-execute-command "nickname AndersL")))

;;}}}
;;{{{ BBDB (OFF)

(autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-notes   "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-insinuate-vm       "bbdb-vm"    "Hook BBDB into VM")
(autoload 'bbdb-insinuate-rmail    "bbdb-rmail" "Hook BBDB into RMAIL")
(autoload 'bbdb-insinuate-mh       "bbdb-mhe"   "Hook BBDB into MH-E")
(autoload 'bbdb-insinuate-gnus     "bbdb-gnus"  "Hook BBDB into GNUS")
(autoload 'bbdb-insinuate-sendmail "bbdb"       "Hook BBDB into sendmail")

;;}}}
;;{{{ Gnus

;;;----------------------------------------------------------------------
;;; Gnus
;;;

;; First, remove any site specific hooks.

(setq gnus-group-mode-hook nil)


;; Add GNUS 5 to the load path.
;; GNUS 5 is a reimplementation of GNUS. (It is delivered with Emacs 19.30)

(if (and (eq my-emacs-type 'emacs-19) (>= emacs-minor-version 30))
    ()
  (setq load-path (cons (expand-file-name "~andersl/emacs/dgnus/lisp") 
			load-path)))


;;
;; Make sure the time information is correct.
;;

(load "timezone" 'noerror 'nomessage)
(setq gnus-local-timezone "GMT+1")


;;
;; Configure Gnus the way I want it.
;; (Todo: Create functions and add-hook their names!)
;;

(add-hook 'gnus-group-mode-hook 'my-gnus-group-mode-hook)

(defun my-gnus-group-mode-hook ()
  (setq
   gnus-nntp-server "news.uu.se"
   gnus-local-domain "csd.uu.se"
   gnus-local-organization 
     "Computing Science Dep., University of Uppsala, Sweden."
   gnus-large-newsgroup '100
   gnus-use-generic-from t
   gnus-auto-select-first nil
   gnus-auto-select-same t
   gnus-use-followup-to t
   gnus-use-long-file-name t
   ;; gnus-file-save-file-name 'gnus-plain-save-name
   gnus-default-article-saver 'gnus-summary-save-in-file
   gnus-subscribe-newsgroup-method 'ignore
   ;; gnus-show-mime t
   ;; gnus-show-mime-method (function metamail-buffer)
   gnus-save-killed-list nil
   gnus-check-new-newsgroups nil
   gnus-ignored-newsgroups 
     (concat "^bionet\\..*$\\|^biz\\..*$\\|^finet\\..*$\\|"
	     "^za\\..*$\\|^maus\\..*$\\|^fj\\..*$"
	     "\\|^de\\..*$\\|^no\\..*$\\|^mh\\..*$\\|^sacramento\\..*$")
   highlight-headers-follow-url-function 
     'highlight-headers-follow-url-netscape   
   gnus-button-url 'gnus-netscape-open-url
   gnus-use-adaptive-scoring t
   ;; Plain old ">  " when I quote somebody.
   mail-yank-prefix "> "

   ;; I prefere to use mew as my mailer.
   gnus-mail-reply-method 'gnus-mail-reply-using-mew
   gnus-mail-forward-method 'gnus-mail-forward-using-mew
   gnus-mail-other-window-method 'gnus-mail-other-window-using-mew)

  (require 'mew)
  (require 'mew-gnus))


(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-mode-hook)

(defun my-gnus-summary-mode-hook ()
  (interactive)

  (let ((foo gnus-default-adaptive-score-alist))
    (while foo
      (let ((bar (assq 'from (car foo))))
	(if bar (delq bar (car foo))))
      (setq foo (cdr foo)))))
  

;; I don't want adaptive scoring on authors.  This only works since a
;; 'from is never the first element in (car foo).


;; I don't want any new groups.
;(setq gnus-subscribe-newsgroup-method 'ignore)


;;
;; BBDB (off)
;;

;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;;{{{ Periodical saving of .newsrc

;; I was a annoyed that GNUS didn't save my .newsrc periodically
;; (automatically - I know I can do it manually), so I added the
;; following code to my .emacs file. All changes are made using hooks.
;;
;; Now, every time a new group is selected, if more than 20 articles
;; have been read since the last time .newsrc was saved, it's saved,
;; and the counter is reset.
;;
;; (Stolen from Jaxon, and rewritten by Anders Lindgren.)

(defvar my-gnus-read-articles 0
  "Number of reads articles since the last save.")

(defvar my-gnus-newsrc-save-frequency 20
  "When `my-gnus-read-articles' reaces this value, .newsrc is saved.")

(defun my-gnus-save-newsrc ()
  (if (> my-gnus-read-articles my-gnus-newsrc-save-frequency)
      (progn
	(gnus-save-newsrc-file)
	(setq my-gnus-read-articles 0))))

(defun my-gnus-increase-read-counter ()
  (setq my-gnus-read-articles (1+ my-gnus-read-articles)))

(add-hook 'gnus-select-group-hook 'my-gnus-save-newsrc)
(add-hook 'gnus-article-prepare-hook 'my-gnus-increase-read-counter)

;;}}}
;;{{{ A hotkey

;;
;; Set up a nice key for gnus.
;;

(defun my-goto-gnus ()
  "Go to a GNUS window, restart if needed."
  (interactive)
  (let ((buffer (or (get-buffer "*Group*")  ;; Gnus 5
		    (get-buffer "*Summary*");; Gnus 4
		    (get-buffer "*Newsgroup*"))))
    (if buffer
	(switch-to-buffer buffer)
      (gnus))))

(global-set-key '[f2] 'my-goto-gnus)

;;}}}

;;}}}
;;{{{ mh-e

;;;----------------------------------------------------------------------
;;; mh-e  (OFF)
;;;

;; (setq load-path (cons (expand-file-name "~/emacs/mh-e") load-path))

(setq mh-yank-from-start-of-msg 'body)

(defun my-mh-letter-mode-hook ()
  (auto-fill-mode)
  (mh-insert-signature)
  (open-line 1)
  (mh-to-fcc "inbox"))

(add-hook 'mh-letter-mode-hook 'my-mh-letter-mode-hook)

;; (add-hook 'mh-inc-folder-hook 'mh-rescan-folder)

;; (add-hook 'mh-folder-mode-hook 'bbdb-insinuate-mh)


;; I really would like to learn MIME,  well this is required...

(defun my-mh-inc-folder-hook ()
  (require 'tm-mh-e))

(add-hook 'mh-inc-folder-hook 'my-mh-inc-folder-hook)

(setq mh-recursive-folders t)


;; (global-set-key '[f1] 'mh-rmail)

;; (setq reporter-mailer '(mh-smail))

;;}}}
;;{{{ Mew

;;;----------------------------------------------------------------------
;;; Mew
;;;

;;
;;  Load code
;;

(setq load-path (cons (expand-file-name "~/emacs/mew") load-path))

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; A key
(global-set-key '[f1] 'mew)

;; No, I don't want to send mail using the old `send-mail' facility.
(setq reporter-mailer '(mew-send))


;;
;; A Emacs 19.30 "bug":  All files named `/draft/[0-9]+' will go into
;; mh-letter-mode, i.e. mh is loaded and some strange side-effects
;; can appear.  (For example, my mh-letter-mode-hook war runed
;; resulting in a very strange drafts.)
;;

(let ((pair (assoc "/drafts/[0-9]+\\'" auto-mode-alist)))
  (if pair
      (setq auto-mode-alist (delq pair auto-mode-alist))))


;;
;;  Set all customisation variables.
;;

;;{{{ Set some vars

;;
;; The following two variables must be set before Mew is loaded... :-(
;;

;; Have I seen it before?
(setq mew-demo nil)

;; Big Brother Database.
;(setq mew-use-bbdb t)

;;}}}
;;{{{ mew-init-hook

;;
;; This is called the first time Mew is loaded.
;;

(add-hook 'mew-init-hook 'my-mew-init-hook)

(defun my-mew-init-hook ()
  (interactive)
  ;;
  ;; Keys.
  ;;

  ; (define-key mew-summary-mode-map "i" 'my-mew-summary-scan-and-inc)
  ; (define-key mew-virtual-mode-map "i" 'my-mew-summary-scan-and-inc)  

  (setq mew-mail-domain-list '("csd.uu.se"))
  (setq mew-fcc "inbox")

  (setq mew-mail-address-list 
	'("andersl@[a-z\\.]*csd\\.uu\\.se"
	  "andersl@[a-z\\.]*docs\\.uu\\.se"))

  (setq mew-replyto-to-list '("Reply-to:"))

  ;; Yes, yes, yes, I've said it thousand times now!
  (setq mew-auto-add-content-type t)

  ;; I don't want to copy header fields when citing
  (setq mew-cite-fields '())
  (setq mew-cite-format "")
  ;; (setq mew-cite-prefix-function 'mew-cite-prefix-username)

  ;; Make ps-print generate a good-looking printout.
  (setq mew-print-function 'my-mew-ps-print2)

  ;; If I ever would like to send a CC, I can always add the line
  ;; manually.
  (setq mew-ask-cc nil)

  ;; Flush, when Emacs is terminating.
  (add-hook 'kill-emacs-hook 'mew-mark-process-all-folders)

  ;;
  ;; Quoted printable, just say NO!
  ;;

  ;; An ugly hack.  I doesn't want to `quote-printable' my national
  ;; characters since to many people use readers which can't handle
  ;; them.
  (defun mew-charset-7bit-p (charset) t))

;;}}}
;;{{{ mew-draft-mode-hook

;;
;; mew-draft-mode-hook, prepare the draft with my signature
;; and my signature... ;-)
;;

(add-hook 'mew-draft-mode-hook 'my-mew-draft-mode-hook)

(defun my-mew-draft-mode-hook ()
  (cond ((not (eq this-command 'mew-summary-reedit))
	 (set-mark (point-max))
	 (save-excursion
	   (goto-char (point-max))
	   (insert "\n\n\t-- Anders\n-- \n")
	   (insert-file-contents "~/.signature")))))

;;}}}
;;{{{ my-mew-ps-print

;;
;;  My own print function2.
;;

(defun my-mew-ps-print ()
  (require 'ps-print)
  (let ((ps-print-header nil))
    (ps-print-region-with-faces 
     (window-start (get-buffer-window (current-buffer)))
     (point-max))))


(defun my-mew-ps-print2 ()
  (require 'ps-print)
  (let ((ps-left-header '(ps-article-subject ps-article-author)))
    (ps-print-region-with-faces 
     (window-start (get-buffer-window (current-buffer)))
     (point-max))))

;;}}}
;;{{{ my-mew-scan-and-inc

;;
;;  I Fcc stuff to +inbox.  Maybe I'm the only one who does this, but
;;  I really would like to see what I've sent.
;;
;;  This is a hack, it gets the last message in the +inbox folder and
;;  executes a scan, with the update option.  The code to get the
;;  last message number was stolen from `mew-input-range'.  Unfortuantely,
;;  I can't call that function directly since it prompts the user. :-(
;;  
;;  It makes also sure that `inc' is called after the asyncronous scan
;;  process has finished.
;;

(defun my-mew-summary-scan-and-inc ()
  (interactive)
  (let ((range-erase
	 (if (get-buffer "+inbox")
	     ;; Get the last message number.  Stolen from `mew-input-range'
	     (save-excursion
	       (set-buffer "+inbox")
	       (goto-char (point-max))
	       (if (bobp)
		   (list "all" 'update) ;; buffer is empty. no need to erase
		 (forward-line -1)
		 (list 
		  (concat
		   (int-to-string (1+ (string-to-int 
				       (mew-summary-message-number))))
		   "-" 
		   "last")
		  'update))) ;; this is update!
	   (list "all" 'update))))
    (mew-summary-scan-body "+inbox" range-erase)
    (add-hook 'mew-summary-scan-sentinel-hook 'my-mew-scan-sentinel)))


(defun my-mew-scan-sentinel ()
  ;; remove ourselves!
  (remove-hook 'mew-summary-scan-sentinel-hook 'my-mew-scan-sentinel)
  (mew-summary-inc))

;;}}}
;;{{{ Mew font-lock support

;;
;; Mew font-lock support,  andersl@csd.uu.se
;;

(defun my-mew-font-lock ()
  (require 'font-lock)
  ;; Font lock must be loaded before this one is changed.
  (setq font-lock-keywords my-mew-font-lock-keywords)
  (font-lock-mode t))

(if window-system
    (progn
      (copy-face 'bold 'my-msg-subject)
      (set-face-foreground 'my-msg-subject "Blue")
      (setq my-msg-subject 'my-msg-subject)

      (copy-face 'bold 'my-msg-from)
      (set-face-foreground 'my-msg-from "Purple")
      (setq my-msg-from 'my-msg-from)

      (copy-face 'bold 'my-msg-header)
      (set-face-foreground 'my-msg-header "Firebrick")
      (setq my-msg-header 'my-msg-header)

      (copy-face 'default 'my-msg-quote)
      (set-face-foreground 'my-msg-quote "ForestGreen")
      (setq my-msg-quote 'my-msg-quote)

      (setq my-mew-font-lock-keywords
	'(("\\(^Subject:.*$\\)" 1 my-msg-subject t)
	  ("\\(^From:.*$\\)" 1 my-msg-from t)
	  ("\\(^>.*$\\)" 1 my-msg-quote t)
	  ("\\(^X-.*:.*$\\)" 1 my-msg-quote t)
	  ("\\(^[A-Za-z][A-Za-z0-9'-]+:\\)" 1 my-msg-header t)
	  ))

      (add-hook 'mew-draft-mode-hook 'my-mew-font-lock)
      (add-hook 'mew-message-mode-hook 'my-mew-font-lock)))

;;}}}

;;}}}
;;{{{ Lyskom

;;;----------------------------------------------------------------------
;;; lyskom
;;;

;; First, clear the hook, since the site default file has an annoying
;; habit of putting lambda-expressions things here...
;;
;; (Why don't they define functions with names sililar to
;; `csd-lyskom-mode-hook' so that we can call `remove-hook' to get
;; rid of them?

(setq lyskom-mode-hook nil)


;; Add my own preferences

(add-hook 'lyskom-mode-hook 'my-lyskom-mode-hook)
	      
(defun my-lyskom-mode-hook ()
  (interactive)
  (setq
   ;; Need this to keep my frame when sending ?
   kom-dont-restore-window-after-editing t 
   kom-emacs-knows-iso-8859-1 t
   kom-dashed-lines t
   kom-pop-personal-messages t
   kom-do-when-starting '(kom-list-news)
   kom-do-when-done '(kom-display-time)
   kom-show-personal-messages-in-buffer "HALLÅ!"
   ;kom-mercial "Rullar tummarna"
   kom-mercial "Bara är"
   kom-tell-phrases
   '((kom-tell-silence "Är tyst")
     (kom-tell-send "Lägger in ny text")
     (kom-tell-login "Loggar in")
     (kom-tell-read "Läser nyheter")
     (kom-tell-1st-pres "Skriver 1:a presentationen")
     (kom-tell-write-comment "Kommenterar")
     (kom-tell-write-footnote "Skriver en fotnot")
     (kom-tell-write-letter "Skriver brev")
     (kom-tell-write-reply "Skriver ett privat svar")
     (kom-tell-write-text "Skaldar")
     (kom-tell-conf-pres "Skriver presentation för ett möte")
     (kom-tell-recover "Trycker 'r' :-B")
     (kom-tell-wait "Väntar och funderar")
     (kom-tell-regret "Kastar inlägget")
     (kom-tell-review "Bättrar på minnet")
     (kom-tell-change-name "Byter namn")
     (kom-tell-change-supervisor "Ändrar organisatör")
     (kom-tell-next-lyskom "Överger detta LysKOM för ett annat")
     (kom-tell-is-back "Oh no! Not him again!")
     (kom-tell-is-idle "Leker vakum"))))


(setq lyskom-default-user-name "Anders Lindgren")
(setq lyskom-default-server "emil.csd.uu.se")
(load (expand-file-name "~/.lyskom_password") 'noerror 'nomessage 'nosuffix)


;;
;; Make the prompt more visiable.
;; TODO: Add COLOUR!
;;

(add-hook 'lyskom-after-command-hook 'lyskom-insert-blank-line-before-prompt)
(add-hook 'lyskom-before-command-hook 'lyskom-insert-blank-line-after-prompt)

(defun lyskom-insert-blank-line-before-prompt ()
  (lyskom-insert-before-prompt "\n\n"))
(defun lyskom-insert-blank-line-after-prompt ()
  (lyskom-insert ""))


;;
;; Set up a nice key for Lyskom@Emil.
;;

(global-set-key '[f3] 'goto-lyskom)
(global-set-key '[f4] 'goto-csdkom)

(defun goto-lyskom ()
  "Switch to Lyskom@Lysator buffer. If it doesn't exist, create it."
  (interactive)
  (let ((buffer (get-buffer "*kom*")))
    (if buffer
	(switch-to-buffer buffer)
      (if my-lyskom-default-password
	  (lyskom "kom.lysator.liu.se"
		  lyskom-default-user-name
		  my-lyskom-default-password)
	(lyskom "kom.lysator.liu.se"
		lyskom-default-user-name)))))


(defun goto-csdkom ()
  "Switch to Lyskom@Emil buffer. If it doesn't exist, create it."
  (interactive)
  (let ((buffer (get-buffer "*emil*")))
    (if buffer
	(switch-to-buffer buffer)
      (if my-lyskom-default-password
	  (lyskom lyskom-default-server
		  lyskom-default-user-name
		  my-lyskom-default-password)
	(lyskom lyskom-default-server
		lyskom-default-user-name)))))


;;
;; Make sure error messages doesn't activate the debugger.
;;

(fdb-add-signal 'error "^Vänta på prompten!$")

;;}}}

;;}}}
;;{{{ Major modes

;;{{{ Erlang

;;;---------------------------------------------------------------------
;;; Erlang/Xerl
;;;
;;; The file `erlang.el' in my home directory is modified to support
;;; inferior erlangs.
;;;


;; Docs ain't got it..
(let ((a '("\\.erl$" . erlang-mode))
      (b '("\\.hrl$" . erlang-mode)))
  (or (assoc (car a) auto-mode-alist)
      (setq auto-mode-alist (cons a auto-mode-alist)))
  (or (assoc (car b) auto-mode-alist)
      (setq auto-mode-alist (cons b auto-mode-alist))))


;; The source, since I do a lot of enhancements.
(autoload 'erlang-mode "erlang.el" "Mode for Erlang programs" t)
(autoload 'run-erlang "erlang.el" nil t)


;; Make the filename completer ignore ".jam"-files.
(or (member ".jam" completion-ignored-extensions)
    (setq completion-ignored-extensions 
          (append '(".jam" ".vee" ".beam") completion-ignored-extensions)))


(add-hook 'erlang-load-hook 'my-erlang-load-hook)

(defun my-erlang-load-hook ()
  (setq erlang-skel-copyright-comment
	'(& "%%% Copyright (C) 1996, Anders Lindgren." n "%%%" n))
  (setq erlang-root-dir "/usr/sup/license/share/erlang-4.3.1"))


;;
;; The hooks
;;

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

(defun my-erlang-mode-hook ()
  (interactive)

  ;; Keymap
  (local-set-key "\C-c\C-c" 'erlang-compile)

  ;; Font-Lock
  (if window-system 
      (font-lock-mode t))

  ;; IMenu
  (if (and window-system (fboundp 'imenu-add-to-menubar))
      (imenu-add-to-menubar "IMenu"))

  ;; Paren
  (cond ((eq my-emacs-type 'emacs-lucid)
	 (paren-set-mode 'paren))))


;; Generate a new header whenever I open a new source file.

(add-hook 'erlang-new-file-hook 'my-erlang-new-file-hook)

(defun my-erlang-new-file-hook ()
  (if (fboundp 'tempo-template-erlang-large-header)
      (tempo-template-erlang-large-header)))


;;;
;;; Ps-print goodies...
;;;

(add-hook 'erlang-mode-hook 'my-erlang-mode-ps-hook)

(defun my-erlang-mode-ps-hook ()
  (if (eq (car ps-left-header) 'ps-get-buffer-name)
      (set (make-local-variable 'ps-left-header)
	   (cons 'my-ps-erlang-buffer-function-name
		 (cdr ps-left-header)))))
  

(defun my-ps-erlang-buffer-function-name ()
  "Place the name of the current Erlang function in the ps-print header."
  (let ((func (save-excursion
		(erlang-end-of-clause)
		(erlang-beginning-of-clause)
		(erlang-get-function-name))))
    (if (and func (> ps-page-count 1))
	(concat (buffer-name) "  (" func ")")
      (buffer-name))))


;;;
;;; BEAM -- I would like to run the instrumented BEAM version 
;;;         on groucho.
;;;

(defun beam ()
  (interactive)
  (let ((inferior-erlang-machine "zerl"))
    (run-erlang)))

(defun beam-groucho ()
  (interactive)
  (let ((inferior-erlang-machine "remsh")
	(inferior-erlang-machine-options
	 '("groucho" "zerl" "-newshell" "-env" "TERM" "vt100"))
	(inferior-erlang-shell-type nil))
    (run-erlang)))


;;;
;;; XErl -- Extended erlang mode.  Not recomended...
;;;

;;
;; Xerl is an extension package to Erlang. However, I doesn't like all
;; features, especially not the strange erlang process always launced...
;;

;(setq load-path (cons (expand-file-name "~/emacs/xerl") load-path))
;
;(autoload 'xerl-mode "xerl" nil t)
;(autoload 'xerl-fl-mode "xerl-font-lock" nil t)
;(autoload 'xerl-imenu-create-index "xerl-imenu" nil t)


;; Nope, I think I will stick to Erlang, at least as long as `Xerl' is
;; a major mode.

; (add-hook 'erlang-mode-hook 'xerl-mode)


;; Imenu, the way I wants it!

;(defun my-xerl-imenu-add-to-menubar (&optional name)
;  (or name
;      (setq name "Imenu"))
;  (require 'imenu)
;  (if (eq window-system 'x)
;      (setq imenu-always-use-completion-buffer-p nil))
;  (setq imenu-create-index-function 'xerl-imenu-create-index)
;  (setq imenu-sort-function 'imenu--sort-by-name)
;  (imenu-add-to-menubar name))

;;}}}
;;{{{ (Emacs) Lisp

;;;---------------------------------------------------------------------
;;; Lisp and Emacs lisp.
;;;

(defun my-lisp-mode-hook ()
  (cond ((eq my-emacs-type 'emacs-19)
	 (imenu-add-to-menubar "Imenu"))
	((eq my-emacs-type 'emacs-lucid)
	 (paren-set-mode 'paren)))
  (if window-system (font-lock-mode 1)))


(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)


;;;
;;; Ps-print goodies...
;;;

(add-hook 'lisp-mode-hook 'my-lisp-mode-ps-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-ps-hook)

(defun my-lisp-mode-ps-hook ()
  (if (eq (car ps-left-header) 'ps-get-buffer-name)
      (set (make-local-variable 'ps-left-header)
	   (cons 'my-ps-lisp-buffer-function-name
		 (cdr ps-left-header)))))
  

(defun my-ps-lisp-buffer-function-name ()
  "Place the name of the current Lisp function in the ps-print header."
  (let ((func (save-excursion
		(end-of-defun)
		(beginning-of-defun)
		(if (looking-at "(def[a-z]*\\s-+\\([-A-Za-z0-9+]+\\)")
		    (buffer-substring (match-beginning 1)
				      (match-end 1))))))
    (if (and func (> ps-page-count 1))
	(concat (buffer-name) "  (" func ")")
      (buffer-name))))

;;}}}
;;{{{ Prolog

;;;----------------------------------------------------------------------
;;; prolog-mode
;;;

(defun my-prolog-mode-hook ()
  (local-set-key "\C-ch" 'insert-predicate)
  (if (eq my-emacs-type 'emacs-lucid)
      (paren-set-mode 'paren)))

(add-hook 'prolog-mode-hook 'my-prolog-mode-hook)


;; Todo: Use `tempo'.

(defun insert-predicate ()
  (interactive)
  (insert "%\n%   PREDICATE\n%\t\n%\n%   DESCRIPTION\n%\t\n%\n\n"))

;;}}}
;;{{{ REXX

;;;----------------------------------------------------------------------
;;; REXX-mode & REXX-debug
;;;

;; Rexx editing mode.
(autoload 'rexx-mode "rexx-mode.el" "REXX mode" nil t)

(or (assoc "\\.rexx$" auto-mode-alist)
    (setq auto-mode-alist
	  (cons
	   '("\\.rexx$" . rexx-mode)
	   auto-mode-alist)))

(if (boundp' interpreter-mode-alist)
    (or (assoc "rexx" interpreter-mode-alist)
	(setq interpreter-mode-alist
	      (cons
	       '("rexx" . rexx-mode)
	       interpreter-mode-alist))))

;; Rexx debug mode.
(autoload 'rxdb "rexx-debug.el" "REXX debug mode" t)
(setq rxdb-command-name "/usr/hacks/bin/rexx")

;;}}}
;;{{{ cc-mode

;;;
;;; Hook common to c-mode, c++-mode, objc-mode, and java-mode.
;;;

(defvar my-c-style
  '("my"
    (c-basic-offset . 4)
    (c-offsets-alist . ((substatement-open . 0)
			(label . 0)))
    )
  "My C Programming Style")

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c-mode-common-hook ()
  (interactive)

  ;; My personal C style
  (c-add-style (car my-c-style) (cdr my-c-style) t)

  ;; IMenu
  (cond
   ((eq my-emacs-type 'emacs-19)
    (imenu-add-to-menubar "Imenu"))
   ((eq my-emacs-type 'emacs-lucid)
    (paren-set-mode 'paren)))

  ;; Keymap
  (define-key c-mode-map "\C-c\C-c" 'compile))


;;;
;;; Extra font-lock keywords. (Useful when working with the Erlang source.)
;;;

(defvar my-c-first-time t
  "Non-nil before cc-mode is started the first time.")



(defvar my-c-extra-font-lock
  (let ((kwds (concat "sint\\(16\\|32\\)\\|uint\\(16\\|32\\)\\|byte\\|"
		      "CIO\\|Process\\|ScheduleQ\\|Index\\(Table\\|Slot\\)"
		      "Hash\\(Functions\\|Bucket\\|\\)\\|Atom")))
    (list
     (cons (concat "\\<\\(" kwds "\\)\\>") 'font-lock-type-face)
     (list (concat "\\<\\(" kwds "\\)\\>\\([ \t*&]+\\sw+\\>\\)*")
	   ;; Fontify each declaration item.
	   '(font-lock-match-c++-style-declaration-item-and-skip-to-next
	     ;; Start with point after all type specifiers.
	     (goto-char (or (match-beginning 8) (match-end 1)))
	     ;; Finish with point after first type specifier.
	     (goto-char (match-end 1))
	     ;; Fontify as a variable or function name.
	     (1 (if (match-beginning 4)
		    font-lock-function-name-face
		  font-lock-variable-name-face)))))))


(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c-mode-hook ()
  (interactive)

  ;; Font Lock
  (if (null window-system)
      ()
    (require 'font-lock)
    (if my-c-first-time
	(setq c-font-lock-keywords-3
	      (append c-font-lock-keywords-3 my-c-extra-font-lock)))
    (setq my-c-first-time nil)
    (font-lock-mode 1)))


;;;
;;; Java
;;;

(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-java-mode-hook ()
  (if (null window-system)
      ()
    (require 'font-lock)
    (require 'java-flock)
    (java-font-lock-mode)))


;;;
;;; Ps-print goodies...
;;;

(add-hook 'c-mode-common-hook 'my-c-mode-ps-hook)

(defun my-c-mode-ps-hook ()
  (if (eq (car ps-left-header) 'ps-get-buffer-name)
      (set (make-local-variable 'ps-left-header)
	   (cons 'my-ps-c-buffer-function-name
		 (cdr ps-left-header)))))


(defun my-ps-c-buffer-function-name ()
  "Place the name of the current C function in the ps-print header."
  (let ((func (save-excursion
		(end-of-defun)
		(beginning-of-defun)
		(condition-case nil
		    (progn
		      (re-search-backward
		       (concat
			"^\\([a-zA-Z0-9]+\\s-+\\)?"
			"\\([a-zA-Z0-9_*]+\\s-+\\)?"
			"\\([a-zA-Z0-9_*]+\\s-+\\)?"
			;; pointer
			"\\([*&]+\\s-*\\)?"
			;; name
			"\\([a-zA-Z0-9_*]+\\)[ \t\n]*("))
		      (buffer-substring
		       (match-beginning 5)
		       (match-end 5)))
		  (error nil)))))
    (if (and func (> ps-page-count 1))
	(concat (buffer-name) "  (" func ")")
      (buffer-name))))


;;;
;;; Tempo
;;;

(if (condition-case nil
	(progn (require 'tempo) nil)
      (error t))
    () ;; Nope, no tempo.
      

  ;; In XEmacs `user-mail-address' returns "x@y.z (Foo Bar)" ARGH!
  (defvar my-c-tempo-mail-address
    (concat (user-login-name) "@"
	    (or (and (boundp 'mail-host-address)
		     (symbol-value 'mail-host-address))
		(system-name)))
    "Mail address of the user.")
  
  
  (defun my-c-tempo-dd-mmm-yyyy ()
    "Return the current date as a string in \"DD Mon YYYY\" form.
The first character of DD is space if the value is less than 10."
    (let ((date (current-time-string)))
      (format "%2d %s %s"
	      (string-to-int (substring date 8 10))
	      (substring date 4 7)
	      (substring date -4))))
  
  
  (defun my-c-tempo-right-header-column ()
    (indent-to-column 77)
    nil)
  
  
  ;; Alexander style header.
  (defvar my-c-tempo-header
    '((beginning-of-buffer)
      "/**************************************"
      "***************************************" n
      " * File         : " (file-name-nondirectory (buffer-file-name))
      (my-c-tempo-right-header-column) "*" n
      " * Author       : " (user-full-name) " <" my-c-tempo-mail-address ">"
      (my-c-tempo-right-header-column) "*" n
      " * Organisation : Computing Science Department, Uppsala University."
      (my-c-tempo-right-header-column) "*" n
      " * Original     : " (my-c-tempo-dd-mmm-yyyy)
      (my-c-tempo-right-header-column) "*" n
      " * Revidated    : " (my-c-tempo-dd-mmm-yyyy)
      (my-c-tempo-right-header-column) "*" n
      " * Purpose      : " p
      (my-c-tempo-right-header-column) "*" n
      " *-------------------------------------"
      "--------------------------------------*" n
      " * CHANGES: "
      (my-c-tempo-right-header-column) "*" n    
      " ***************************************"
      "**************************************/" n))
  
  (tempo-define-template "c-header" my-c-tempo-header)
  

  ;; Simple separator
  
  (defvar my-c-tempo-separator
    '("/*-------------------------------------"
      "--------------------------------------*" n
      " * " p (my-c-tempo-right-header-column) "*" n
      " *-------------------------------------"
      "--------------------------------------*/" n))
  
  (tempo-define-template "c-separator" my-c-tempo-separator))
  
  
;;;
;;; Old stuff (Should be deleted)
;;;
  
;    (c-electric-pound-behavior . (alignleft))
;    (c-hanging-colons-alist . ((member-init-intro (before))
;			       (inher-intro)
;			       (case-label (after))
;			       (label (after))
;			       (access-key (after))))
;    (c-cleanup-list . ((scope-operator)
;		       (empty-defun-braces)
;		       (list-close-comma)
;		       (defun-close-semi)))
;    (c-offsets-alist . ((substatement-open . 0)
;			(member-init-intro     . 0)
;			(arglist-intro         . +)
;			(arglist-cont          . nerc-c-lineup-arglist)
;			(arglist-cont-nonempty . nerc-c-lineup-arglist)
;			(arglist-close         . nerc-c-lineup-arglist-close)))
;   (c-hanging-braces-alist . ((brace-list-open)
;			      (substatement-open after)
;			      (block-close . c-snug-do-while)
;			      (brace-list-close)
;			      (defun-close)
;			      (class-close)))
;    (c-echo-semantic-information-p . t)
;    (nerc-c-lineup-stay-within-parens . nil)
;    (c-hanging-comment-ender-p . nil)	; don't force comment enders to hang
;					; at the end of a line.
;;    (statement-open . 0)
;;    (substatement-open . 0)

  ;; (load "c-comment-mode" nil t)
  ;; (load "c-indent" nil t)

  ;(setq c-tab-always-indent t)          ;; cc
  ;; (setq c-auto-newline t)
  ;; (setq c-indent-level 4)               
  ;(setq c-continued-statement-offset 4)
  ;(setq c-continued-brace-offset 0)
  ;(setq c-brace-offset -4)
  ;(setq c-argdecl-indent 0)
  ;(setq c-label-offset -4)

;;}}}
;;{{{ Ksh

;;;----------------------------------------------------------------------
;;; Ksh mode
;;;

(add-hook 'ksh-mode-hook 'my-ksh-mode-hook)

(defun my-ksh-mode-hook ()
  (interactive)
  (if window-system (font-lock-mode 1)))

(setq font-lock-doc-string-face "normal")

;;}}}
;;{{{ Text

;;;---------------------------------------------------------------------
;;; Text mode hook
;;;

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;}}}
;;{{{ SML

;;
;; Font-lock - Make sure keywords are bold:ed etc.
;;

;; This command is normally defined in Emacs 19.29...
(if (not (fboundp 'turn-on-font-lock))
    (defun turn-on-font-lock () (font-lock-mode t)))


(defun my-sml-load-hook ()
  (if window-system
      (require 'sml-font)))

(add-hook 'sml-load-hook 'my-sml-load-hook)


;;
;;  Customise the behaviour of inferior SML.
;;
;;  - Save old commands in a file `~/.sml-history'
;;  - Make sure emacs doesn't ask the question:
;;	"Active processes active, really exit"
;;    when terminating Emacs.
;;

(defun my-inferior-sml-mode-hook ()
  ;; Save commands entered at the command prompt.
  (if (fboundp 'comint-read-input-ring)
      (progn
	(setq comint-input-ring-file-name "~/.sml-history")
	(comint-read-input-ring t)
	(make-local-variable 'kill-buffer-hook)
	(add-hook 'kill-buffer-hook 'comint-write-input-ring)))

  ;; Make sure Emacs doesn't ask stupid questions.
  ;; (Note: sml-buffer isn't defined here.)
  (process-kill-without-query (get-buffer-process (current-buffer))))

(add-hook 'inferior-sml-mode-hook 'my-inferior-sml-mode-hook)
  

;;
;; Two start command for two different flavours.
;;

(defun run-sml-moscow ()
  (interactive)
  (let ((sml-program-name "mosml-nj93"))
    (sml)
    (switch-to-buffer sml-buffer)))

(defun run-sml-nj ()
  (interactive)
  (let ((sml-program-name "sml-0.93"))
    (sml)
    (switch-to-buffer sml-buffer)))

;;}}}
;;{{{ HTML helper mode

(add-hook 'html-helper-load-hook 'my-html-helper-load-hook)

(defun my-html-helper-load-hook ()
  (cond (window-system 
	 (require 'html-font))))



(add-hook 'html-helper-mode-hook 'my-html-helper-mode-hook)

(defun my-html-helper-mode-hook ()
  (cond (window-system
	 (font-lock-mode 1))))

;;}}}
;;{{{ Help mode

;; Try to make *Help*-buffer qo away when `q' is pressed.
;;
;; The ideal would be to restore the old window configuration, but
;; that doesn't seem to be possible...

(require 'view)

(define-key view-mode-map "q" 'my-view-exit)

(defun my-view-exit ()
  (interactive)
  (if (string= (buffer-name) "*Help*")
      (kill-buffer (current-buffer))
    (view-exit)))

;;}}}
;;{{{ TeX (and AucTeX)

;;;------------------------------------------------------------
;;; TeX
;;;

;; I don't want to load ".info" files, and some more...
(if (not (member ".info" completion-ignored-extensions))
    (setq completion-ignored-extensions
	  (append completion-ignored-extensions
		  '(".info" ".aux"  ".cp" ".cps" ".dvi"
		    ".fn"   ".info" ".ky" ".log" ".pg"
		    ".pgs"  ".toc"  ".tp" ".vr"))))


;; (require 'iso-cvt)			; Convert to/from iso/TeX. 


;;;------------------------------------------------------------
;;; AucTeX
;;;

(setq TeX-lisp-directory (expand-file-name "~andersl/emacs/auctex/"))

;; The init file.
(load (concat TeX-lisp-directory "tex-site"))


;; My own hooks.
(add-hook 'TeX-mode-hook 'my-TeX-mode-hook)
(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)


(defun my-TeX-mode-hook ()
  (interactive)
  (cond (window-system
	 (require 'font-lock)
	 (let ((pair (assoc 
      "\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
                      tex-font-lock-keywords)))
	   (if pair
	       (setcar pair 
      "\\\\\\(cite\\|label\\|pageref\\|ref\\|vref\\){\\([^} \t\n]+\\)}")))
	 (font-lock-mode t)))

  ; (setq TeX-print-command "dvips %s -o '!lp -d%p -nod'")
  (setq TeX-queue-command "lpstat -d%p")

  ;; %c stands for "current directory".
  (kill-local-variable 'TeX-expand-list)
  (set (make-local-variable 'TeX-expand-list)
       (cons
	(list "%c" 'symbol-value 'default-directory)
	TeX-expand-list))


  ;; Make it possible to start a compilation on a remote machine.
  (kill-local-variable 'TeX-command-list)
  (set (make-local-variable 'TeX-command-list)
       (append
	(list
	 (list "Shelly" 
       "remsh shelly.csd.uu.se \"cd %c ; %l '\\nonstopmode\\input{%t}'\"" 
	       'TeX-run-LaTeX nil t)
	 (list "Groucho" 
       "remsh groucho.csd.uu.se \"cd %c ; %l '\\nonstopmode\\input{%t}'\""
	       'TeX-run-LaTeX nil t))
	TeX-command-list))

  ;; Make \item's look nice.
  (setq LaTeX-item-indent -6)
  
  ;; Make AUXTeX query for Master File if the current file has no
  ;; \documentclass... 
  (setq TeX-master nil)

  ;; Enable AUCTeX parsing.
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)

  ;; This needed if you have directories ending with "//" in your
  ;; TEXINPUTS.
  (setq TeX-macro-private nil)

  ;; I prefer to have my figures as close as possible
  (setq TeX-float "htb")

  ;; I want to keep my old M-g
  (local-set-key "\M-g" 'goto-line))


(defun my-LaTeX-mode-hook ()
  (interactive)
  (setq LaTeX-version "2e")
  (setq TeX-command-default "Groucho")
  (setq latex-help-info-file "(latex2e)"))

;;}}}
;;{{{ Makefile mode

(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

(defun my-makefile-mode-hook ()
  (if window-system (font-lock-mode 1)))

;;}}}

;;{{{ Comint

;; Lets make C-a avare of the prompt.
;;
;; `comint-show-maximum-output': I also like to work at the bottom of
;; the window, not in the middle.

(add-hook 'comint-mode-hook 'my-comint-mode-hook)

(defun my-comint-mode-hook ()
  (interactive)
  (setq comint-scroll-show-maximum-output t)
  (local-set-key "\C-a" 'comint-bol)
  (local-set-key "\C-c\C-a" 'beginning-of-line))


(add-hook 'comint-exec-hook 'my-comint-exec-hook)

(defun my-comint-exec-hook ()
  (interactive)
  (process-kill-without-query (get-buffer-process (current-buffer))))

;;}}}
;;{{{ Interactive Lisp

(defun my-inferior-lisp-mode-hook ()
  (setq comint-input-ring-file-name "~/.ilisp_history")
  (comint-read-input-ring t)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'comint-write-input-ring))

(add-hook 'inferior-lisp-mode-hook 'my-inferior-lisp-mode-hook)

(defadvice inferior-lisp (after my-rename act)
  "The buffer is immediately renamed to *lisp*."
  (rename-buffer "*lisp*" t))

;;}}}
;;{{{ Interactive Shell

;;;----------------------------------------------------------------------
;;; Interactive Shell
;;;

;; Make shell strip ^M:s

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(defun my-shell-mode-hook ()
  (interactive)
  (if (fboundp 'shell-strip-ctrl-m)
      (add-hook 'comint-output-filter-functions
		'shell-strip-ctrl-m )))


(setq explicit-shell-file-name "bash")

;;}}}
;;{{{ GDB

(add-hook 'gdb-mode-hook 'my-gdb-mode-hook)

(defun my-gdb-mode-hook ()
  (interactive)

  (setq comint-input-ring-file-name "~/.gdb_history")
  (comint-read-input-ring t)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'comint-write-input-ring))

;;}}}

;;}}}
;;{{{ Minor modes

;;{{{ Follow mode

;;;---------------------------------------------------------------------
;;; follow-mode
;;;
;;; My own little program making your workspace twice as big.
;;;
;;; http://www.csd.uu.se/~andersl/follow.shtml
;;;

;; The uncompiled, as I make a lot of changes...  (Don't do this
;; at home, kids.)

(autoload 'follow-mode "follow.el" nil t)
(autoload 'follow-delete-other-windows-and-split "follow.el" nil t)

;; (autoload 'follow-mode "follow" nil t)
;; (autoload 'follow-delete-other-windows-and-split "follow" nil t)


;;
;; Some global keys so that I can activate Follow Mode fast.
;;

(global-set-key [f8] 'follow-mode)
(global-set-key [f7] 'follow-delete-other-windows-and-split)


;;
;; The hook;  Set better keys.  (Follow Mode is not allowed to
;; set keys other than `C-c <punctuation character> <whatever>').
;;

(add-hook 'follow-mode-hook 'my-follow-mode-hook)

(defun my-follow-mode-hook ()
  (define-key follow-mode-map "\C-c\C-v"  'follow-scroll-up)
  (define-key follow-mode-map "\C-cv"	  'follow-scroll-down)
  (define-key follow-mode-map "\C-cb"	  'follow-switch-to-buffer)
  (define-key follow-mode-map "\C-cl"	  'follow-recenter))

;;}}}
;;{{{ Font-lock

;;;----------------------------------------------------------------------
;;;  font-lock
;;;

;;; TODO: Add lazy-lock.  But I want's to get the feeling of
;;; normal `font-lock' first.


;; Create new faces for comments and others.
;;
;; #000 is black, #FFF is (almost) white. For more precition, use #RRGGBB.

(cond ((or my-emacs-19-30-p my-emacs-lucid-p)
       ;; Set the fonts named font-lock-...
       (copy-face 'default 'font-lock-comment-face)
       (set-face-foreground 'font-lock-comment-face "#666")
       (copy-face 'bold 'font-lock-function-name-face)
       (copy-face 'bold 'font-lock-keyword-face)
       (set-face-foreground 'font-lock-keyword-face "Violet"))
      (my-emacs-19-p
       ;; Before Emacs 19.30, the varibles named `font-lock-...'
       ;; should contain the name of the font.
       (copy-face 'default 'comment)
       (set-face-foreground 'comment "#666")
       (setq font-lock-comment-face 'comment)
       (setq font-lock-function-name-face 'bold)))


;; The color of the comment font is too close to the colour of the
;; `region' face...
(cond (my-emacs-19-p
       (set-face-background 'region "#DDD"))
      (my-emacs-lucid-p
       (set-face-background 'zmacs-region "#DDD")))


;; Must be defined before font lock is loaded.
(setq font-lock-maximum-decoration t)


;;
;; General font-lock initialization.
;; This is called every time font lock is activated.
;;

(add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)

(defun my-font-lock-mode-hook ()
  ;; My mode line is already full...
  (setcar (cdr (assq 'font-lock-mode minor-mode-alist)) "")

  ;; I belive that the need for fontfication increases with
  ;; the size of the file. 
  (setq font-lock-maximum-size (* 1024 1024)))


;;
;; Used by some of my hooks to change the face of words
;; enclosed in ` ':s, inside comments.
;;
;; This is probably obsolete since there exists a `reference'
;; face.
;;

;;(defun my-replace-backquote-quote-keyword (keywords face)
;;  "Replace the face highlighting words inside `':s"
;;  (mapcar 
;;   (function
;;    (lambda (pair)
;;      ;; A regexp which should match another regexp :-/
;;      (if (string-match "\\``\\\\(.*\\+\\\\)'\\'" (car pair))
;;	  (if (consp (cdr pair))
;;	      (setcar (cdr (cdr pair)) face)
;;	    (if (not (numberp (cdr pair)))
;;		(setcdr pair face))))))
;;   keywords))

;;}}}
;;{{{ Folding

;;;------------------------------------------------------------
;;; Folding
;;;

(add-hook 'folding-mode-hook 'my-folding-mode-hook)

(defun my-folding-mode-hook ()
  (interactive)
  (setq fold-behave-table
	'((close 	fold-hide)
	  (open   	fold-enter)
	  (up		fold-exit)
	  (other	fold-mouse-call-original)))
  (cond ((eq my-emacs-type 'emacs-19)
	 (define-key folding-mode-map [mouse-3] 'fold-mouse-context-sensitive))
	((eq my-emacs-type 'lucid)
	 (define-key folding-mode-map '(mouse3) 'fold-mouse-context-sensitive))))


;;; I like the keys the way they used to be...
(setq fold-default-keys-function 'fold-bind-backward-compatible-keys)

(load "folding.el" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)

;; (load "fold-isearch.el" 'nomessage 'noerror)

;;}}}
;;{{{ Fill

;;;---------------------------------------------------------------------
;;; fill / adaptive fill
;;;

;; Untested. The old version fails when executing C-u % RET in
;; Erlang mode...

(setq default-fill-column 70)
(if (>= emacs-minor-version 29)
    (require 'filladapt))

;;}}}
;;{{{ Maniac - A REAL fill mode

(autoload 'maniac-fill-mode "maniac" nil t)

;;}}}

;;}}}
;;{{{ Extra

;;{{{ Find Library

;; By Peter Breton <pbreton@i-kinetics.com>

(defun find-library (arg library)
  "Find the file of an elisp library"
  (interactive "p\nsLibrary: ")
  (let  ((lib (locate-library library)))
    (if (not lib)
	(error "Can't find library %s" library)
      (if (string-match "elc$" lib)
	  (setq lib (substring lib 0 -1)))
      (if (> arg 1)
	  (view-file-other-window lib)
	(find-file-other-window lib)))))

(defun which (exe &optional showall)
  "Show the full path name of an executable.
This command searches the directories in `exec-path'"
  (interactive "sWhich: ")
  (catch 'answer
    (mapcar
     '(lambda (dir)
	(mapcar
	 '(lambda (suf)
	    (let ((try (expand-file-name (concat exe suf) dir)))
	      (and (file-executable-p try)
		   (null (file-directory-p try))
		   (progn
		     (message "%s is %s" exe try)
		     (throw 'answer try)))))
	 '("")))
     exec-path)
    (message "Can't find %s in search path" exe)
    nil))

;; Kevin Rodgers <kevin.rodgers@ihs.com>
(defun locate-function (function &optional NOSUFFIX)
  "Show the full path name of the loaded Emacs library that defines FUNCTION.

This command checks whether FUNCTION is autoloaded, or it searches the
elements of `load-history' to find the library; then calls `locate-
library' to find the file.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY (a la calling `load' instead of `load-library')."
  ;; `interactive' form copied from `describe-function' in help.el:
  (interactive
   (let* ((fn (function-called-at-point))
	  (enable-recursive-minibuffers t)	     
	  (val (completing-read
		(if fn
		    (format "Describe function (default %s): " fn)
		  "Describe function: ")
		obarray 'fboundp t)))
     (list (if (equal val "")
	       fn
	     (intern val))
	   current-prefix-arg)))
  (let* ((fn-object (symbol-function function))
	 (library (if (and (listp fn-object)
			   (eq (car fn-object) 'autoload))
		      (nth 1 fn-object)))
	 (history load-history))
    (while (and history (null library))
      (if (memq function (cdr (car history)))
	  (setq library (car (car history))))
      (setq history (cdr history)))
    (if library
	(locate-library library))))

;;}}}

;;}}}
;;{{{ Debug

;;;----------------------------------------------------------------------
;;; For edebug
;;;

;(setq edebug-global-prefix "...whatever you want")  ; default is C-xX
;(define-key emacs-lisp-mode-map "\^Xx" 'edebug-defun)
;(autoload 'edebug-defun "edebug")
;(autoload 'edebug-debug "edebug")
;(setq debugger 'edebug-debug)


;;;----------------------------------------------------------------------
;;; Activate the debugger whenever an error occurs.
;;;

(add-hook 'after-init-hook 'my-after-init-hook)

(defun my-after-init-hook ()
  (setq debug-on-error t))

;;}}}

;;; End of my-init.el

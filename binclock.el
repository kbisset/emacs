;;; binclock.el --- Display the current time using a binary clock.
;; Copyright 1999 by Dave Pearson <davep@hagbard.demon.co.uk>
;; $Revision: 1.1 $

;; binclock is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;; 
;; binclock displays the current time using a binary display.

;;; Revision history:
;;
;; $Log: binclock.el,v $
;; Revision 1.1  1999/02/03 13:12:16  davep
;; Initial revision
;;

;;; Code:

(require 'cl)

;; If customize isn't available just use defvar instead.

(eval-and-compile
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Customize options.

(defgroup binclock nil
  "binclock - Display the time in binary."
  :group 'games
  :prefix "binclock-")

(defcustom binclock-mode-hook nil
  "*Hooks to run when starting `binclock-mode'."
  :type  'hook
  :group 'binclock)

(defcustom binclock-display 'zero-and-one
  "*The format for display the binary values."
  :type '(choice (const zero-and-one)
                 (const lisp-list)
                 (const hash-string))
  :group 'binclock)

(defcustom binclock-clock-type 'bcd
  "*Type of clock to display."
  :type '(choice (const bcd)
                 (const hh-mm-ss))
  :group 'binclock)

(defcustom binclock-24hour t
  "*Should the time be displayed using the 24hour clock?"
  :type 'boolean
  :group 'binclock)

;; Non-customize variables.

(defvar binclock-buffer-name "*Binary Clock*"
  "Name of the binary clock buffer.")

(defvar binclock-mode-map nil
  "Local keymap for the binary clock.")

(defvar binclock-buffer nil
  "Pointer to the binary clock buffer.")

(defvar binclock-window nil
  "Pointer to the binary clock window.")

(defvar binclock-timer-handle nil
  "Handle for the update timer.")

;; Define the default keyboard map.
(unless binclock-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "q" #'binclock-quit)
    (define-key map "?" #'describe-mode)
    (setq binclock-mode-map map)))

(defmacro binclock-with-write-enabled (&rest body)
  "Turn off read-only, perform BODY and then turn it on again."
  `(let ((buffer-read-only nil))
     ,@body))

;; Mode definition.

(put 'binclock-mode 'mode-class 'special)

(defun binclock-mode ()
  "A mode for interacting with the binary clock.

The key bindings for `binclock-mode' are:

\\{binclock-mode-map}"
  (kill-all-local-variables)
  (use-local-map binclock-mode-map)
  (setq major-mode       'binclock-mode
        mode-name        "Binary Clock"
        buffer-read-only t
        truncate-lines   t)
  (buffer-disable-undo (current-buffer))
  (run-hooks 'binclock-mode-hook))

;; Main code.

;;;###autoload
(defun binclock ()
  "Display the binary clock."
  (interactive)
  (let ((window-min-height 2)
        (old-window (selected-window)))
    (setq binclock-window (select-window (split-window-vertically (- (window-height) 2)))
          binclock-buffer (switch-to-buffer binclock-buffer-name))
    (unless binclock-timer-handle
      (setq binclock-timer-handle (run-at-time nil 1 #'binclock-timer)))
    (binclock-mode)
    (select-window old-window)))

(defun binclock-timer ()
  "Update the binary clock display."
  (with-current-buffer binclock-buffer
    (binclock-with-write-enabled
      (erase-buffer)
      (let ((time-list (cond ((eq binclock-clock-type 'hh-mm-ss)
                              (binclock-time-hh-mm-ss))
                             ((eq binclock-clock-type 'bcd)
                              (binclock-time-bcd))
                             (t '()))))
        (cond ((eq binclock-display 'lisp-list)
               (loop for value in time-list
                     do (progn
                          (insert (prin1-to-string value))
                          (insert "  "))))
              ((eq binclock-display 'zero-and-one)
               (loop for value in time-list
                     do (progn
                          (insert (binclock-boollist-as-string value ?1 ?0))
                          (insert "  "))))
              ((eq binclock-display 'hash-string)
               (loop for value in time-list
                     do (progn
                          (insert (binclock-boollist-as-string value ?# ?.))
                          (insert "  "))))
              (t
               (insert "Invalid display method")))))))

;; Keyboard response functions.

(defun binclock-quit ()
  "Close the binary clock."
  (interactive)
  (when binclock-timer-handle
    (cancel-timer binclock-timer-handle))
  (setq binclock-timer-handle nil)
  (kill-buffer binclock-buffer)
  (delete-window binclock-window))

;; Support functions.

(defun* binclock-to-binary (num &optional (bits 8))
  "Convert a positive integer NUM into a binary list. Pad the list out to
BITS bits. BITS is optional and if not supplied defaults to 8."
  (loop for bit downfrom (1- bits) to 0
        collect (not (zerop (logand num (expt 2 bit))))))

(defun binclock-time-hh-mm-ss ()
  "Get the time, in HH:MM:SS format, as a list of binary lists."
  (let ((now (decode-time)))
    (mapcar #'binclock-to-binary
            (list (binclock-hour-fixup (caddr now)) (cadr now) (car now)))))

(defun binclock-time-bcd ()
  "Get the time, in BCD format, as a list of binary values."
  ;; `cl-floor' returns a list instead of multiple values, note that I make
  ;; use of this fact. Should emacs ever acquire "real" (values ...) this
  ;; code will break.
  (let* ((now (decode-time))
         (hr  (cl-floor (binclock-hour-fixup (caddr now)) 10))
         (mn  (cl-floor (cadr  now) 10))
         (sc  (cl-floor (car   now) 10)))
    (mapcar #'(lambda (n) (binclock-to-binary n 4))
            (list (car hr) (cadr hr)
                  (car mn) (cadr mn)
                  (car sc) (cadr sc)))))

(defun binclock-boollist-as-string (list on off)
  "Convert LIST (a list of logical values) to a string.
Use ON for true values and OFF for false values."
  (coerce (loop for bit in list collect (if bit on off)) 'string))

(defun binclock-hour-fixup (hour)
  "Fixup HOUR depending on the setting of `binclock-24hour'."
  (if binclock-24hour
      hour
    (- hour (if (> hour 12) 12 0))))

(provide 'binclock)

;;; Local Variables: ***
;;; eval: (put 'binclock-with-write-enabled 'lisp-indent-function 0) ***
;;; End: ***

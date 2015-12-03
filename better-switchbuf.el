;; --------------------------------------------------------------------------
;;; better-switchbuf.el -- Give `switch-to-buffer' a more convenient "history" 
;;
;; Author: Anders Holst  (aho@sans.kth.se)
;; Last change: 24 March 1994
;; Copyright (C)  1994  Anders Holst
;;
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
;; along with your copy of Emacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; -------------------------------------------------------------------------- 

;;; Commentary:
;;
;;  This code defines a variation of `read-buffer' which uses the buffer list
;;  as "history".  Already visible buffers, and the default buffer, can be
;;  found downwards in the history list, whereas the other buffers are upwards,
;;  in order of most recent use (ie. as returned by `buffer-list').
;;
;;  This function is then used in redefinitions of `switch-to-buffer', 
;;  `switch-to-buffer-other-window' and `switch-to-buffer-other-frame' to
;;  give these a more convenient history list.
;;
;;  To use it, just load this file.

;;; Code:

(defun better-read-buffer (prompt &optional def req)
  "Like `read-buffer' but with a more convenient \"history\" list.
Down in the history list is the default buffer and already visible buffers.
Up is the other buffers, in order of most recent use."
  (let* ((res ())
         (bufs (mapcar 'buffer-name (buffer-list)))
         (alist (mapcar 'list bufs))
         (hist (delq () (mapcar '(lambda (x) 
                                   (and (not (string-match "^ \\*.*\\*$" x))
                                        x))
                                bufs)))
         (def (if (bufferp def) (buffer-name def) def))
         (num (if (and def (member def bufs))
                  (- (length bufs) (length (member def bufs)))
                (let ((thist hist)
                      (i 0))
                  (while (and thist (get-buffer-window (car thist)))
                    (setq thist (cdr thist))
                    (setq i (1+ i)))
                  i))))
    (if def
        (setq prompt (format "%s(default %s) " prompt def)))
    (setq res (completing-read prompt alist () req () 
                               (cons 'hist num)))
    (if (string= res "")
        def
      res)))

(fset 'switch-to-buffer-orig 
      (symbol-function 'switch-to-buffer))

(defun switch-to-buffer (name &optional norecord)
  "Select buffer BUFFER in the current window.
BUFFER may be a buffer or a buffer name.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones.

NOTE: This function is redefined to give a more convenient \"history\" list.
The original function is in `switch-to-buffer-orig'."
  (interactive (list (better-read-buffer "Switch to buffer: " 
                                         (other-buffer)
                                         ())))
  (switch-to-buffer-orig name norecord))
      
(defun switch-to-buffer-other-window (buffer)
  "Select buffer BUFFER in another window.
NOTE: This function is redefined to give a more convenient \"history\" list."
  (interactive (list (better-read-buffer "Switch to buffer in other window: " 
                                         (other-buffer)
                                         ())))
  (let ((pop-up-windows t))
    (pop-to-buffer buffer t)))

(defun switch-to-buffer-other-frame (buffer)
  "Switch to buffer BUFFER in another frame.
NOTE: This function is redefined to give a more convenient \"history\" list."
  (interactive (list (better-read-buffer "Switch to buffer in other frame: " 
                                         (other-buffer)
                                         ())))
  (let ((pop-up-frames t))
    (pop-to-buffer buffer t)))

(provide 'better-switchbuf)
;;; better-switchbuf.el ends here

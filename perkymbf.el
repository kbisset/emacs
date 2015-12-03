;;; perkymbf.el -- dedicated minibuffer frame that highlights when prompting

;; Copyright (C) 2000 Neil W. Van Dyke

;; Author:   Neil W. Van Dyke <nwv@acm.org>
;; Created:  04-Dec-2000
;; Version:  0.1
;; Keywords: minibuffer, frames, x, window managers
;; X-URL:    http://www.neilvandyke.org/perkymbf/
;; X-RCS:    $Id: perkymbf.el,v 1.23 2000/12/05 21:19:58 nwv Exp $ GMT

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; `perkymbf' makes your Emacs use a dedicated minibuffer frame which becomes
;; visually highlighted whenever it is prompting for input.  The author finds
;; this helpful on a large X display.
;;
;; `perkymbf' was developed under FSF Emacs 20.7 on a GNU/Linux system, and
;; will probably work with other recent FSF Emacs versions.  It has not been
;; tested on non-Unixish systems, nor with XEmacs.
;;
;; To install `perkymbf', put this file somewhere in your load path, add the
;; following line to your `~/.emacs' file, restart Emacs, and optionally
;; customize any features you like under Environment->Perkymbf.
;;
;;     (when window-system (require 'perkymbf))
;;
;; If you wish to set `default-frame-alist' or `minibuffer-frame-alist' in
;; your `~/.emacs', you should do it before loading `perkymbf', like so:
;;
;;     (when window-system
;;       (setq minibuffer-frame-alist
;;             '((width                 . 181)
;;               (height                . 1)
;;               (left                  . (+ 2))
;;               (top                   . (- 6))
;;               (title                 . "Emacs Minibuffer")
;;               (internal-border-width . 4)))
;;       (require 'perkymbf))
;;
;; If you later find that you have some funky package that is pretending to
;; read from the minibuffer, but the minibuffer frame isn't becoming
;; highlighted, add something like the following to your `~/.emacs' file (in
;; this example, `funky-password-reader' is the name of the function that is
;; pretending to read from the minibuffer).
;;
;;     (perkymbf-defadvice-echoprompter funky-password-reader)
;;
;; Now that you have a perky minibuffer frame, you might wish to have your X
;; window manager do special handling of it.  The X window class of the
;; minibuffer frame is `Emacs', and the X window name is `minibuffer'.  For
;; example, if you use Larswm, your `.larswmrc' will probably have a line
;; something like:
;;
;;     larswm.toolclass.0: Emacs~minibuffer
;;
;; The exact window manager configuration is left as an exercise for the
;; reader.

;;; Change Log:

;; [Version 0.1, 05-Dec-2000, nwv@acm.org] Initial release.

;;; Code:

(defconst perkymbf-version "0.1")

(require 'advice)
(require 'cl)
(require 'custom)

;; Customization:

(defgroup perkymbf nil
  "*Make your dedicated minibuffer frame more perky."
  :group  'environment
  :prefix "perkymbf-")

(defcustom perkymbf-normal-foreground-color "black"
  "*Foreground color for minibuffer frame."
  :type  'string
  :group 'perkymbf)

(defcustom perkymbf-normal-background-color "white"
  "*Background color for minibuffer frame."
  :type  'string
  :group 'perkymbf)

(defcustom perkymbf-normal-cursor-color "white"
  "*Cursor color for minibuffer frame."
  :type  'string
  :group 'perkymbf)

(defcustom perkymbf-active-foreground-color "black"
  "*Foreground color for active minibuffer frame."
  :type  'string
  :group 'perkymbf)

(defcustom perkymbf-active-background-color "yellow"
  "*Background color for active minibuffer frame."
  :type  'string
  :group 'perkymbf)

(defcustom perkymbf-active-cursor-color "red"
  "*Cursor color for active minibuffer frame."
  :type  'string
  :group 'perkymbf)

;; Macros:

(defmacro perkymbf-defadvice-echoprompter (symbol)
  (unless (symbolp symbol)
    (signal 'wrong-type-argument (list 'symbolp symbol)))
  (let ((advice-sym  (intern (format "perkymbf-%s-advice-echoprompter" symbol)))
        (minibuf-frame-sym (gensym))
        (saved-frame-sym   (gensym)))
    `(defadvice ,symbol (around ,advice-sym activate protect)
       (let ((,minibuf-frame-sym (perkymbf-minibuffer-frame))
             (,saved-frame-sym   (selected-frame)))
         (unwind-protect
             (progn
               (when (frame-live-p ,minibuf-frame-sym)
                 (perkymbf-minibuffer-setup-hook-func ,minibuf-frame-sym))
               (unwind-protect
                   (progn
                     (when (frame-live-p ,minibuf-frame-sym)
                       (select-frame ,minibuf-frame-sym))
                     ad-do-it)
                 ;; unwind-protect cleanup
                 (when (frame-live-p ,saved-frame-sym)
                   (select-frame ,saved-frame-sym))))
           ;; unwind-protect cleanup
           (when (frame-live-p ,minibuf-frame-sym)
             (perkymbf-minibuffer-exit-hook-func ,minibuf-frame-sym)))))))

;; Functions:

(defun perkymbf-minibuffer-frame ()
  (or (window-frame (minibuffer-window))
      default-minibuffer-frame))

(defun perkymbf-minibuffer-exit-hook-func (&optional minibuffer-frame)
  (modify-frame-parameters
   (or minibuffer-frame (perkymbf-minibuffer-frame))
   (list (cons 'background-color perkymbf-normal-background-color)
         (cons 'foreground-color perkymbf-normal-foreground-color)
         (cons 'cursor-color     perkymbf-normal-cursor-color)
         (cons 'cursor-type      '(bar . 0)))))

(defun perkymbf-minibuffer-setup-hook-func (&optional minibuffer-frame)
  (modify-frame-parameters
   (or minibuffer-frame (perkymbf-minibuffer-frame))
   (list (cons 'background-color perkymbf-active-background-color)
         (cons 'foreground-color perkymbf-active-foreground-color)
         (cons 'cursor-color     perkymbf-active-cursor-color)
         (cons 'cursor-type      'box))))

(defun perkymbf-alist-add-unique (orig-alist add-alist)
  (let ((new-alist (copy-sequence add-alist))
        (probe     orig-alist))
    (while probe
      (unless (assq (car (car probe)) add-alist)
        (setq new-alist (nconc new-alist (list (car probe)))))
      (setq probe (cdr probe)))
    new-alist))

;; Init:

(if (not window-system)
    (message
     "You loaded `perkymbf', but `window-system' is nil, so not initializing.")

  (setq default-frame-alist
        (perkymbf-alist-add-unique default-frame-alist
                                   '((minibuffer . nil))))

  (setq minibuffer-frame-alist
        (perkymbf-alist-add-unique minibuffer-frame-alist
                                   '((auto-raise     . t)
                                     (menu-bar-lines . 0)
                                     (minibuffer     . only)
                                     (name           . "minibuffer"))))
  
  (add-hook 'minibuffer-exit-hook  'perkymbf-minibuffer-exit-hook-func)
  (add-hook 'minibuffer-setup-hook 'perkymbf-minibuffer-setup-hook-func)
  
  (perkymbf-defadvice-echoprompter comint-read-noecho)
  (perkymbf-defadvice-echoprompter crypt-read-string-no-echo)
  (perkymbf-defadvice-echoprompter map-y-or-n-p)
  (perkymbf-defadvice-echoprompter read-passwd)
  (perkymbf-defadvice-echoprompter y-or-n-p))

(provide 'perkymbf)

;; perkymbf.el ends here

;;; flycheck-pos-tip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "flycheck-pos-tip" "../../../../.emacs.d/elpa/flycheck-pos-tip-20170504.1504/flycheck-pos-tip.el"
;;;;;;  "ecf96d5dde0bf2da8f3aca1763665ec7")
;;; Generated autoloads from ../../../../.emacs.d/elpa/flycheck-pos-tip-20170504.1504/flycheck-pos-tip.el

(defvar flycheck-pos-tip-mode nil "\
Non-nil if Flycheck-Pos-Tip mode is enabled.
See the `flycheck-pos-tip-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `flycheck-pos-tip-mode'.")

(custom-autoload 'flycheck-pos-tip-mode "flycheck-pos-tip" nil)

(autoload 'flycheck-pos-tip-mode "flycheck-pos-tip" "\
A minor mode to show Flycheck error messages in a popup.

When called interactively, toggle `flycheck-pos-tip-mode'.  With
prefix ARG, enable `flycheck-pos-tip-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `flycheck-pos-tip-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`flycheck-pos-tip-mode'.  Otherwise behave as if called
interactively.

In `flycheck-pos-tip-mode' show Flycheck's error messages in a
GUI tooltip.  Falls back to `flycheck-display-error-messages' on
TTY frames.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/flycheck-pos-tip-20170504.1504/flycheck-pos-tip-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/flycheck-pos-tip-20170504.1504/flycheck-pos-tip.el")
;;;;;;  (22874 47632 729799 102000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-pos-tip-autoloads.el ends here

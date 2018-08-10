;;; flycheck-status-emoji-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "flycheck-status-emoji" "../../../../.emacs.d/elpa/flycheck-status-emoji-20180330.2325/flycheck-status-emoji.el"
;;;;;;  "dafae9b813dd1f63819119eb2b9357c6")
;;; Generated autoloads from ../../../../.emacs.d/elpa/flycheck-status-emoji-20180330.2325/flycheck-status-emoji.el

(defvar flycheck-status-emoji-mode nil "\
Non-nil if Flycheck-Status-Emoji mode is enabled.
See the `flycheck-status-emoji-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `flycheck-status-emoji-mode'.")

(custom-autoload 'flycheck-status-emoji-mode "flycheck-status-emoji" nil)

(autoload 'flycheck-status-emoji-mode "flycheck-status-emoji" "\
Toggle Flycheck status emoji mode.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode; any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, while `toggle' toggles the state.

When enabled, this mode replaces the standard Flycheck mode-line
status indicators with cute, compact emoji that convey the
corresponding information.  For example, a buffer shows status
“😔” while being checked, then “😱” to report errors, “😟” to report
warnings, or “😌” if no problems were found.

See <https://github.com/liblit/flycheck-status-emoji#readme> for
additional documentation.  Visit
<https://github.com/liblit/flycheck-status-emoji/issues> or use
command `flycheck-status-emoji-submit-bug-report' to report bugs
or offer suggestions for improvement.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/flycheck-status-emoji-20180330.2325/flycheck-status-emoji-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/flycheck-status-emoji-20180330.2325/flycheck-status-emoji.el")
;;;;;;  (23404 55953 821779 236000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-status-emoji-autoloads.el ends here

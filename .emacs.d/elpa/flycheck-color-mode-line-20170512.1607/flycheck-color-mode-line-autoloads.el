;;; flycheck-color-mode-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "flycheck-color-mode-line" "../../../../.emacs.d/elpa/flycheck-color-mode-line-20170512.1607/flycheck-color-mode-line.el"
;;;;;;  "6f7537001012d85151cdadbd2c8b6dd0")
;;; Generated autoloads from ../../../../.emacs.d/elpa/flycheck-color-mode-line-20170512.1607/flycheck-color-mode-line.el

(autoload 'flycheck-color-mode-line-mode "flycheck-color-mode-line" "\
Minor mode to color the mode line with the Flycheck status.

When called interactively, toggle
`flycheck-color-mode-line-mode'.  With prefix ARG, enable
`flycheck-color-mode-line-mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `flycheck-color-mode-line-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-color-mode-line-mode'.
Otherwise behave as if called interactively.

\(fn &optional ARG)" t nil)

(custom-add-frequent-value 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/flycheck-color-mode-line-20170512.1607/flycheck-color-mode-line-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/flycheck-color-mode-line-20170512.1607/flycheck-color-mode-line.el")
;;;;;;  (22874 47632 357804 316000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-color-mode-line-autoloads.el ends here

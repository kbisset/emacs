;;; ghub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ghub" "../../../../.emacs.d/elpa/ghub-20180805.1851/ghub.el"
;;;;;;  "cd449916f91d94c9a5ea9c0e3d4ed23f")
;;; Generated autoloads from ../../../../.emacs.d/elpa/ghub-20180805.1851/ghub.el

(autoload 'ghub-create-token "ghub" "\
Create, store and return a new token.

HOST is the Github instance, usually \"api.github.com\".
USERNAME is the name of a user on that instance.
PACKAGE is the package that will use the token.
SCOPES are the scopes the token is given access to.

\(fn HOST USERNAME PACKAGE SCOPES)" t nil)

(autoload 'ghub-token-scopes "ghub" "\
Return and echo the scopes of the specified token.
This is intended for debugging purposes only.  The user
has to provide several values including their password.

\(fn HOST USERNAME PACKAGE)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/ghub-20180805.1851/ghub-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/ghub-20180805.1851/ghub-graphql.el"
;;;;;;  "../../../../.emacs.d/elpa/ghub-20180805.1851/ghub-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/ghub-20180805.1851/ghub.el") (23404
;;;;;;  54436 25367 219000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ghub-autoloads.el ends here

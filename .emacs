(setq message-log-max t)
;(setq TeX-lisp-directory "~")

;;(setq TeX-lisp-directory "~")
;;(require 'cl)
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
(setq load-path (cons (expand-file-name "/opt/local/share/emacs/site-lisp") load-path))
;(setq load-path (cons (expand-file-name "/usr/share/emacs24/site-lisp/magit") load-path))
(setq load-path (cons (expand-file-name "/opt/local/share/emacs/site-lisp") load-path))
(setq load-path (cons (expand-file-name (concat "~" init-file-user "/emacs")) load-path))
(setq load-path (cons (expand-file-name (concat "~" init-file-user "/emacs/muse/lisp")) load-path))
(setq load-path (cons (expand-file-name (concat "~" init-file-user "/emacs/planner")) load-path))
(setq load-path (cons (expand-file-name (concat "~" init-file-user "/emacs/remember-2.0")) load-path))
;(setq load-path (cons (expand-file-name "~/emacs/color-theme") load-path))
;(setq load-path (cons (expand-file-name "/sw/share/emacs22-carbon/site-lisp") load-path))
;(setq load-path (cons (expand-file-name "/sw/share/emacs22/site-lisp") load-path))
;(setq load-path (cons (expand-file-name "/sw/share/emacs/site-lisp") load-path))
;(setq load-path (cons (expand-file-name "~/emacs/w3/lisp") load-path))
;(setq load-path (cons (expand-file-name "~/emacs/gnus/lisp") load-path))

;; (setq Info-default-directory-list 
;;       (append
;;        '("/usr/local/info")
;;        '("/export/home/cancun/tsa5/kbisset/info")
;;        '("/export/home/snark/tsa5/kbisset/emacs/w3")
;;        '("/export/home/snark/tsa5/kbisset/emacs/auctex")
;;        '("/export/home/snark/tsa5/kbisset/emacs/info")
;;        Info-default-directory-list
;;        ))

(setq biff-listup-when-start nil)

;
;(require 'tex-site)
(load "krb-emacs")
;(load ".gnus")
;(load-file "/usr/local/lib/gemacs/lisp/ispell.elc")
;(load "jka-load")

;(setq load-path (cons (expand-file-name "~/Emacs") load-path))
;(eval-after-load "shell" '(load "shellc"))
(setq custom-file (concat "~" init-file-user "/emacs/krb-custom.el"))
(require 'custom)
(load-file custom-file)

;;(cd "~")

(if (string= system-name "Keiths-MacBook-Pro.local")
    (set-face-font 'default "-adobe-Source Code Pro-normal-normal-normal-*-*-180-*-*-m-0-iso10646-1"))

;;(set-default-font "-adobe-Source Code Pro-normal-normal-normal-*-*-110-*-*-m-0-iso10646-1" nil nil)
;;(set-default-font "-adobe-Source Code Pro-normal-normal-normal-*-*-120-*-*-m-0-iso10646-1" nil nil)
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))
(put 'downcase-region 'disabled nil)

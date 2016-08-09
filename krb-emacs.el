;;(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/color-theme-6.6.0")
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))
(require 'color-theme)
(require 'color-theme-hober)
(color-theme-hober)

;; (eval-after-load "color-theme"
;; 	'(progn
;; 		(color-theme-initialize)
;; 		(color-theme-hober)
;;		(require 'color-theme-solarized)
;; 		(color-theme-solarized-dark)
;;    ))

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(load-library "subr")
;(setq enable-multibyte-characters nil)
;(set-default-font "-apple-monaco-medium-r-normal--12-120-72-72-m-120-mac-roman" nil)
;;(set-default-font "-fondu-monaco-medium-r-normal--14-130-75-75-p-79-macroman-1")
;;(set-default-font "-*-Lucida Sans Typewriter-normal-normal-semicondensed-*-*-*-*-*-p-0-iso10646-1" nil nil)
;(set-default-font "-apple-Lucida_Sans-medium-normal-normal-*-*-*-*-*-p-0-iso10646-1" nil nil)
(set-default-font "-adobe-Source Code Pro-normal-normal-normal-*-*-120-*-*-m-0-iso10646-1" nil nil)
(require 'jka-compr)
(display-time)
;(setq krb-default-font "-b&h-lucidatypewriter-medium-r-normal-sans-14-140-75-75-m-90-iso8859-1")

; tcl mode
;(autoload 'tcl-mode "tcl" "Tcl mode." t)
;(autoload 'inferior-tcl "tcl" "Run inferior Tcl process." t)
;(setq auto-mode-alist (append '(("\\.tcl$" . tcl-mode)) auto-mode-alist))
(setq tcl-indent-level 2)
;(setq auto-mode-alist (append '(("\\.bml$" . tcl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.kml$" . nxml-mode)) auto-mode-alist))
;(add-hook 'tcl-mode-hook 'tcl-guess-application)
;(add-hook 'tcl-mode-hook 'tcl-auto-fill-mode)

; perl mode
(setq auto-mode-alist (append '(("\\.pl$" . perl-mode)) auto-mode-alist))

; parsec
;(setq auto-mode-alist (append '(("\\.pc$" . c-mode)) auto-mode-alist))

; gcov
(setq auto-mode-alist (append '(("\\.C.gcov$" . c++-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.h.gcov$" . c++-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.c.gcov$" . c-mode)) auto-mode-alist))

; diffs
(setq auto-mode-alist (append '(("diff$" . diff-mode)
                                ("diff.out$" . diff-mode)) auto-mode-alist))
; ant files
(add-to-list 'auto-mode-alist '("\\.ant$" . ant-mode))

; xml files
(add-to-list 'auto-mode-alist '("\\.xsd$" . nxml-mode))

; protobuf files
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
(autoload 'protobuf-mode "protobuf-mode"
  "Mode for editing Google protobuf files" t)

; fvwmrc mode
;;; (autoload 'fvwm-mode "fvwm-mode"
;;;   "Mode for editing fvwm's config files" t)

;;; (setq auto-mode-alist (append auto-mode-alist
;;; 			   '(("\\.fvwm2rc" . fvwm-mode))))

;;; server stuff
;(load "gnuserv")
;(setq server-program "/local/bin/gnuserv")
;(cond ((equal emacs-major-version 20)
;       (require 'gnuserv-compat)  
;       (require 'gnuserv)
;       (catch nil (gnuserv-start))
;))
;(gnuserv-start)

(defun ispell-file (file)
  (interactive)
  (find-file file) 
  (ispell-buffer)
  (save-buffer)
)


(defun no-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(defun no-trailing-whitespace-w3m (unused) (no-trailing-whitespace))

(autoload 'w3m-goto-url "w3m" "WWW Browser" t)
;;(require 'subr)
(defun browse-url-w3m (url &optional new-window)
  "Ask the w3m WWW browser to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window.  A non-nil interactive
prefix argument reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "W3M URL: "))
  (if (browse-url-maybe-new-window new-window)
      (w3m-goto-url-new-session url)
    (w3m-goto-url url)))


(defun google-search (phrase)
  "Search google for a phrase and use w3m-el to browse the results."
  (interactive "sGoogle Search: ")
  (w3m-browse-url (format "http://www.google.com/search?q=%s" 
			  (subst-char-in-string ?  ?+ phrase))))
	(global-set-key "\C-c\C-z." 'browse-url-at-point)
	(global-set-key "\C-c\C-zb" 'browse-url-of-buffer)
	(global-set-key "\C-c\C-zr" 'browse-url-of-region)
	(global-set-key "\C-c\C-zu" 'browse-url)
	(global-set-key "\C-c\C-zv" 'browse-url-of-file)
	(add-hook 'dired-mode-hook
		  (lambda ()
	             (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)))
     (setq browse-url-browser-function '(("^mailto:" . browse-url-mail)
     				    ("." . browse-url-w3m)))
;;      (eval-after-load "w3"
;;        '(progn
;;          (fset 'w3-fetch-orig (symbol-function 'w3-fetch))
;;          (defun w3-fetch (&optional url target)
;;            (interactive (list (w3-read-url-with-default)))
;;            (if (eq major-mode 'gnus-article-mode)
;;                (browse-url url)
;;              (w3-fetch-orig url target)))))
;;; Emacs/W3 Configuration
;       (require 'w3-auto "w3-auto")
; (cond ((equal emacs-major-version 21)
;        (defun w3-load-flavors ()
;          (require 'w3-e20))
; ))

;;;; Emacs-w3 configuration options
;(autoload 'w3 "w3" "WWW Browser" t)
;(autoload 'w3-preview-this-buffer "w3" "WWW Previewer" t)
;(autoload 'w3-follow-url-at-point "w3" "Find document at pt" t)
;(autoload 'w3 "w3" "WWW Browser" t)
;(autoload 'w3-open-local "w3" "Open local file for WWW browsing" t)
;(autoload 'w3-fetch "w3" "Open remote file for WWW browsing" t)
;(autoload 'w3-use-hotlist "w3" "Use shortcuts to view WWW docs" t)
;(autoload 'w3-show-hotlist "w3" "Use shortcuts to view WWW docs" t)
;(autoload 'w3-follow-link "w3" "Follow a hypertext link." t)
;(autoload 'w3-batch-fetch "w3" "Batch retrieval of URLs" t)
;(autoload 'url-get-url-at-point "url" "Find the url under the cursor" nil)
;(autoload 'url-file-attributes  "url" "File attributes of a URL" nil)
;(autoload 'url-popup-info "url" "Get info on a URL" t)
;(autoload 'url-retrieve   "url" "Retrieve a URL" nil)
;(autoload 'url-buffer-visiting "url" "Find buffer visiting a URL." nil)
;(autoload 'gopher-dispatch-object "gopher" "Fetch gopher dir" t)
;(autoload 'widget-convert-text "wid-edit" "something gnus forgot" t)
(global-set-key "\C-cb" 'goto-bulletin)
(global-set-key "\C-cF" 'goto-forum)
(global-set-key "\C-cp" 'goto-phonebook)
(global-set-key "\C-cw" 'goto-weather)
(global-set-key "\C-cd" 'goto-dict)
(global-set-key "\C-ca" 'goto-acm-technews)
(require 'webjump)
(global-set-key "\C-cj" 'webjump)
(setq webjump-sites
      '(("Bulletin" . "http://www.lanl.gov/newsbulletin")
      ("ACM Technews" . "http://www.acm.org/technews/current/homepage.html")
      ("Forum" . "http://www.lanl.gov/orgs/pa/newsbulletin/forum.shtml")
      ("Phone Book" . "http://www.lanl.gov/internal/phone.html")
      ("Weather" . "http://weather.lanl.gov/")
      ("Dictionary" . "http://www.m-w.com/cgi-bin/dictionary")))

(defun goto-acm-technews () (interactive) 
  (w3m-goto-url "http://www.acm.org/technews/current/homepage.html"))
(defun goto-bulletin () (interactive) 
  (w3m-goto-url "http://www.lanl.gov/newsbulletin"))
(defun goto-forum () (interactive) 
  (w3m-goto-url "http://www.lanl.gov/orgs/pa/newsbulletin/forum.shtml"))
;http://www.lanl.gov/orgs/pa/newsbulletin/2002/022202/
(defun goto-metacrawler () (interactive) 
  (w3m-goto-url "http://www.metacrawler.com/"))
(defun goto-phonebook () (interactive) 
  (w3m-goto-url  "http://www.lanl.gov/internal/phone.html"))
(defun goto-weather () (interactive) 
  (w3m-goto-url "http://weather.lanl.gov/"))
(defun goto-dict () (interactive) 
  (w3m-goto-url "http://www.m-w.com/cgi-bin/dictionary"))
(defun goto-director () (interactive)
  (w3m-goto-url "http://int.lanl.gov/organization/director/ask-director.shtml"))
; (defun goto-dict () (interactive) 
;   (w3-fetch "http://www.lanl.gov/labview/search/index.html"))

;;; End of Emacs-w3 configuration options

;; cc-mode stuff
;(load-library "cc-mode-19")
(defconst krb-c-style
  '(
    (c-basic-offset . 2)
    (c-tab-always-indent . 1)
;    (c-cleanup-list . (scope-operator 
;                       brace-else-brace 
;                       empty-defun-braces
;                       defun-close-semi  
;                       list-close-comma))
    (c-offsets-alist . ((substatement-open . 0)
                        (case-label . +)
                        (innamespace . --)
                        ))
)
  "KRB Programming Style")

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun krb-c-mode-common-hook ()
  ;; load the dmacro stuff
  (require 'dmacro)
  (dmacro-load (concat "~" init-file-user "/emacs/krb.dm"))
  (auto-fill-mode t)
  (setq auto-dmacro-alist '(("\\.C$" . srcmasthead)
                            ("Test.*\\.C$" . testsrcmasthead)
                            ("Test.*\\.h$" . testhdrmasthead)
                            ("\\.H$" . hdrmasthead_short)
                            ("\\.h$" . hdrmasthead_short))
        auto-dmacro-case-fold nil)
  ;; add my personal style and set it for the current buffer
  (c-add-style "KRB" krb-c-style t)
  (setq tab-width 2
        indent-tabs-mode nil
        c-hanging-comment-ender-p nil
        c-hanging-comment-starter-p nil)
  (c-toggle-hungry-state 1)
  ;; keybindings for C, C++, and Objective-C.  We can put these in
  ;;(define-key c-mode-map "\177"     'delete-backward-char)
  (define-key c-mode-base-map "\177" 'c-electric-delete)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map "\M-s" 'sort-lines)
  ;;(global-unset-key "\C-x\C-t")
  ;;(define-key c-mode-map "\C-x\C-t\C-s" 'tags-search)
  ;;(define-key c-mode-map "\C-x\C-t%" 'tags-query-replace)
  ;;(define-key c-mode-map "\033:"    'make-new-C-banner-comment)
  ;;(define-key c-mode-map "\033s"    'insert-C-function-stub)
  ;;(define-key c-mode-map "\015"     'newline-and-indent)
  ;;(define-key c-mode-map "\033\011" 'c-indent-line)
  ;;(define-key c-mode-map "\177"     'delete-backward-char)
  ;;(define-key c-mode-map "\030F"    'auto-fill-mode)
  (define-key c-mode-base-map "\C-a"    'beginning-of-code-line)
  (define-key c-mode-base-map "\C-e"    'end-of-code-line)
  )
(add-hook 'c-mode-common-hook 'krb-c-mode-common-hook)

;; hide show minor mode
(setq hs-special-modes-alist 
  '((c-mode "{" "}")
    (c++-mode "{" "}")
    (tcl-mode "{" "}")
    ))
(defun my-hs-setup () "enables hideshow and binds some commands"
  (require 'hideshow)
  (hs-minor-mode 1)
  (define-key hs-minor-mode-map "\C-ch" 'hs-hide-block)
  (define-key hs-minor-mode-map "\C-cs" 'hs-show-block)
  (define-key hs-minor-mode-map "\C-cH" 'hs-hide-all)
  (define-key hs-minor-mode-map "\C-cS" 'hs-show-all)
  (define-key hs-minor-mode-map "\C-cR" 'hs-show-region)
  (hs-hide-all)
  )
;;(add-hook 'c-mode-common-hook 'my-hs-setup t) 
;;(add-hook 'tcl-mode-hook 'my-hs-setup t)

;; packages
(require 'uniquify)
(require 'ishl)
;(setq PC-meta-flag nil)
;(setq PC-include-file-path '("/usr/include"))
;(require 'complete)
;(require 'better-switchbuf)
;(require 'iswitchb)
;(iswitchb-default-keybindings) ; krb - removed 6/15/2012
;;(require 'pcl-cvs) ; krb - removed 6/15/2012
;; ifdef hiding
;(setq hide-ifdef-lines t)
;(require 'hide-if)

;; paren hiliting
;(make-face 'my-paren-face)
;(make-face 'my-paren-mismatch-face)
(require 'paren)
(set-face-foreground 'show-paren-mismatch-face "red")
(set-face-background 'show-paren-mismatch-face "black")
(set-face-foreground 'show-paren-match-face "green")
(set-face-background 'show-paren-match-face "black")
(show-paren-mode)
;(setq show-paren-match-face 'my-paren-face)
;(setq show-paren-mismatch-face 'my-paren-mismatch-face)

;(autoload 'cvs-update "pcl-cvs")
;(require 'w3-props)
;(require 'w3-auto)
;(load-file "~/emacs/w3/lisp/widget.elc")
;(load-file "~/emacs/w3/lisp/wid-edit.elc")
;(require 'widget)
;(require 'wid-edit)
;(Require 'crypt++)
(setq crypt-encryption-type 'des 
      crypt-encryption-file-extension "\\(\\.e\\)$")
;;(require 'biff3)
;;(load-library "context") 
;;(read-context)
(load-library "codeline")

;; function menus
;(cond (window-system 
;       (require 'func-menu)
;       (define-key global-map [S-down-mouse-3] 
;        'function-menu)))

;; imenu stuff
(cond (window-system 
       (require 'hier-imenu)
       (define-key global-map [S-down-mouse-3] 
        'imenu)))


; ediff package
(setq ediff-use-last-dir t)
(autoload 'eeep-ediff "ediff" "Init Stuff" t)
(autoload 'ediff-buffers "ediff" "Visual interface to diff(1)" t)
(autoload 'ediff  "ediff"  "Visual interface to diff(1)" t)
(autoload 'ediff-files "ediff" "Visual interface to diff(1)" t)
(autoload 'ediff-files-remote "ediff" "Visual interface to diff(1)") 
(autoload 'epatch  "ediff"  "Visual interface to patch(1)" t)
(autoload 'ediff-patch-file "ediff" "Visual interface to patch(1)" t)
(autoload 'ediff-patch-buffer "ediff" "Visual interface to patch(1)" t)

; rolo stuff
(autoload 'rolo-fgrep "rolo" "Find entries in rolodex." t)   
(autoload 'rolo-grep "rolo"  "Find entries in rolodex." t)   
(autoload 'rolo-edit "rolo"  "Edit personal rolodex file." t)
(autoload 'rolo-sort "rolo"  "Sort rolodex file." t)

(autoload 'load-database "database" "Load up database files" t)
;(autoload 'do-gnus "do-gnus" "Gnus Reader" t)

; rmail stuff
;(setq rmail-dont-reply-to-names "info-\\|kbisset\\>\\|csall\\|cscrlall\\|csgrad\\|csfaculty")
;(setq rmail-delete-after-output t)
;(add-hook 'rmail-show-message-hook 'rmime)
;(add-hook 'rmail-edit-mode-hook    'rmime-cancel)
;(autoload 'rmime "rmime" "" t)
(setq mail-yank-prefix ">")

(add-hook 'message-mode-hook 
	  '(lambda () 
	     (define-key message-mode-map "\C-c\C-a" 
	       'mail-abbrev-insert-alias)))
;; 	       'mail-interactive-insert-alias)))
(setq user-mail-address "kbisset@vbi.vt.edu")
(add-hook 'mail-setup-hook 'mail-abbrevs-setup)
(load-library "krb-mailaliases")
;(require 'mime-setup)
;; (setq gnus-secondary-select-methods '((nnml "private") 
;; ;;                                      (nntp "news.cis.dfn.de")
;; ;;                                      (nntp "news.individual.net")
;; ;;                                      (nntp "news.teranews.com")
;; ;;                                      (nntp "alt.teranews.com")
;; ;;                                      (nntp "unlimited.teranews.com")
;; ;;                                      (nntp "news.gmane.org")
;;                                       (nntp "localhost" 3119))
;;       nnmail-crosspost nil
;;       gnus-auto-expirable-newsgroups ".*"
;;       gnus-auto-select-next nil
;;       gnus-nntp-server nil
;;       gnus-select-method '(nnml "~/Mail")
;;       gnus-startup-file "~/.newsrc"
;;       )
;; (load "mail-split.el")

;;(setq-default gnus-topic-mode t)
;;  (setq gnus-background-mode 'dark
;; ;;       widget-menu-minibuffer-flag t
;;        gnus-large-newsgroup 250
;; ;;       gnus-topic-mode t
;;        gnus-topic-display-empty-topics nil
;;        gnus-interactive-exit nil
;;        gnus-save-newsrc-file nil
;;        gnus-save-killed-list nil
;;        gnus-auto-select-first nil
;; ;;       browse-url-browser-function 'browse-url-w3
;; ;;       browse-url-browser-function 'browse-url-netscape
;;        browse-url-netscape-program "mozilla"
;;        browse-url-browser-function 'browse-url-w3m
;;        browse-url-netscape-arguments "-noraise %s"
;;        browse-url-netscape-startup-arguments "-no-session-management -no-about-splash -dont-force-window-stacking -install"
;;        gnus-subscribe-newsgroup-method 'gnus-subscribe-hierarchically
;;  ;;      gnus-auto-expirable-newsgroups "nndir*"
;;        gnus-check-bogus-groups nil
;;        gnus-check-new-newsgroups t
;;        gnus-read-active-file nil
;;  ;      gnus-read-active-file t
;;        gnus-inhibit-startup-message t
;;        gnus-thread-sort-functions '(gnus-thread-sort-by-date)
;;        gnus-summary-mode-line-format "%g/%A"
;;        gnus-summary-gather-subject-limit 25
;;        gnus-article-display-hook 
;;        '(gnus-article-hide-headers 
;;          gnus-article-hide-signature
;;          gnus-article-display-x-face
;;  ;;        gnus-article-highlight
;; ;;         rmime
;;          )
;;        gnus-thread-hide-subtree t
;;        gnus-auto-center-summary t
;;        gnus-visible-headers nil
;;        gnus-ignored-headers "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:\\|^Received:\\|^Mail-from:\\|^X-.*:\\|^Return-Path:\\|^Resent-.*:\\|^Encoding:\\|^Message-Id:\\|^Precedence:\\|^Resent-Sender:\\|.*Reply-To:\\|Content-.*:\\|Mime-.*:\\|^Original-.*:\\^List-.*:"
;; )

;; (add-hook 'gnus-startup-hook
;; (add-hook 'gnus-summary-prepare-hook
;;   '(lambda () 
;;      (define-key gnus-summary-mode-map "n"    'gnus-summary-next-thread)
;;      (define-key gnus-summary-mode-map "p"    'gnus-summary-prev-thread)
;;      (define-key gnus-summary-mode-map "e"    'gnus-summary-mark-as-expirable)
;;      (define-key gnus-summary-mode-map [next] 'gnus-summary-next-page)
;;      (define-key gnus-summary-mode-map [prev] 'gnus-summary-prev-page)
;;      ))
;; (setq gnus-thread-sort-functions '(gnus-thread-sort-by-number 
;;                                    gnus-thread-sort-by-date
;;                                    gnus-thread-sort-by-subject
;; ;;                                   gnus-thread-sort-by-total
;;                                    gnus-thread-sort-by-total-score))
;; ; (setq gnus-thread-sort-functions '(
;; ;                                    gnus-thread-sort-by-total))
;; (setq gnus-simplify-subject-fuzzy-regexp "\([Rr][Ee] *:?\):? *")
;; (setq gnus-summary-gather-subject-limit 'fuzzy)
;; (setq gnus-auto-expirable-newsgroups ".*")
                                         ; face and frame stuff
(setq default-frame-alist '((visibility . t) 
                            (minibuffer . t)
                           (top . 0) 
;;                             (left . 453) 
                             (modeline . t) 
                             (width . 100) 
                             (height . 55) 
                             (cursor-type . box) 
                             (auto-lower) 
                             (auto-raise) 
                             (icon-type) 
                             (border-color . "black") 
                             (cursor-color . "red") 
                             (mouse-color . "yellow") 
                             (background-color . "black") 
                             (foreground-color . "wheat") 
                             (vertical-scroll-bars . nil) 
                             (menu-bar-lines . 0)
                             (internal-border-width . 2) 
;;                             (border-width . 2) 
                             ;;                                    (menu-bar-lines . 1) 
                                        ;                                    (font . "-etl-*-medium-r-*-*-16-*-*-*-*-*-iso8859-1")
                            ;;                                    (font . "-nucleus-gamow-medium-r-*-*-12-*-*-*-*-*-iso8859-1")
;;                            (wait-for-wm . nil)
                            ))

;; ;;  (cond (window-system 
;; ;;         (cond ((x-display-color-p)
;; ;;                (setq default-frame-alist '((visibility . t) 
;; ;;                                            (minibuffer . t)
;; ;;                                            (top . 0) 
;; ;;                                     (left . 453) 
;; ;;                                     (modeline . t) 
;; ;;                                     (width . 91) 
;; ;;                                     (height . 61) 
;; ;;                                     (cursor-type . box) 
;; ;;                                     (auto-lower) 
;; ;;                                     (auto-raise) 
;; ;;                                     (icon-type) 
;; ;;                                     (border-color . "black") 
;; ;;                                     (cursor-color . "red") 
;; ;;                                     (mouse-color . "yellow") 
;; ;;                                     (background-color . "black") 
;; ;;                                     (foreground-color . "wheat") 
;; ;;                                     (vertical-scroll-bars . nil) 
;; ;;                                     (menu-bar-lines . 0)
;; ;;                                     (internal-border-width . 2) 
;; ;;                                     (border-width . 2) 
;; ;; ;;                                    (menu-bar-lines . 1) 
;; ;; ;                                    (font . "-etl-*-medium-r-*-*-16-*-*-*-*-*-iso8859-1")
;; ;; ;;                                    (font . "-nucleus-gamow-medium-r-*-*-12-*-*-*-*-*-iso8859-1")
;; ;;                                     (wait-for-wm . nil)
;; ;;                                     ))
;; ;;               (setq initial-frame-alist '(;(visibility . t) 
;; ;;                                           (minibuffer . t)
;; ;;                                            (top . 0) 
;; ;;                                     (left . 362) 
;; ;;                                     (modeline . t) 
;; ;;                                     (width . 90) 
;; ;;                                     (height . 53) 
;; ;;                                     (cursor-type . box) 
;; ;;                                     (auto-lower) 
;; ;;                                     (auto-raise) 
;; ;;                                     (icon-type) 
;; ;;                                     (border-color . "black") 
;; ;;                                     (cursor-color . "red") 
;; ;;                                     (mouse-color . "yellow") 
;; ;;                                     (background-color . "black") 
;; ;;                                     (foreground-color . "gainsboro") 
;; ;;                                     (vertical-scroll-bars . nil) 
;; ;;                                     (menu-bar-lines . 0)
;; ;;                                     (internal-border-width . 2) 
;; ;;                                    (border-width . 2) 
;; ;;                                   ))
;; ;; ;;                (setq minibuffer-frame-alist '((top . 1001) 
;; ;; ;;                                               (left . 452) 
;; ;; ;;                                               (width . 104) ;; etl
;; ;; ;; ;;                                              (width . 121) ;; gamow
;; ;; ;;                                               (height . 1)
;; ;; ;;                                               (autoraise . t)
;; ;; ;;                                               (autolower . t)
;; ;; ;;                                               (name . "Emacs-MiniBuffer")
;; ;; ;; ;                                              (font . "-etl-*-medium-r-*-*-14-*-*-*-*-*-iso8859-1")
;; ;; ;; ;;                                              (font . "-nucleus-gamow-medium-r-semicondensed-*-10-*-*-*-*-*-iso8859-1")
;; ;; ;;                                               ))
;; ;;               (set-face-background 'highlight "cyan")
;; ;;               (set-face-foreground 'highlight "black")
;; ;;   ;;            (set-face-foreground 'secondary-selection "black")
;; ;; ;;              (set-face-foreground 'region "black")
;; ;; ;;              (set-face-background 'region "darkblue")
;; ;;               )
;; ;;              (t
;; ;;               (setq default-frame-alist '((visibility . t) 
;; ;;                                           (top . 25) 
;; ;;                                           (left . 0) 
;; ;;                                           (unsplittable) 
;; ;;                                           (modeline . t) 
;; ;;                                           (width . 90) 
;; ;;                                           (height . 55) 
;; ;;                                           (cursor-type . box) 
;; ;;                                           (auto-lower) 
;; ;;                                           (auto-raise) 
;; ;;                                           (icon-type) 
;; ;;                                           (border-color . "black") 
;; ;;                                           (cursor-color . "white") 
;; ;;                                           (mouse-color . "white") 
;; ;;                                           (background-color . "black") 
;; ;;                                           (foreground-color . "white") 
;; ;;                                           (vertical-scroll-bars . nil) 
;; ;;                                           (menu-bar-lines . 0)
;; ;;                                           (internal-border-width . 2) 
;; ;;                                           (border-width . 2) 
;; ;; ))))
;; ;; ))

;; load font-lock - now, the standard fontifying package 
(cond (window-system 
       (setq font-lock-maximum-decoration 3
             font-lock-background-mode 'dark
;; krb - removed 2012/06/15
;;             font-lock-support-mode 'fast-lock-mode
;;             font-lock-support-mode 'lazy-lock-mode
             font-lock-display-type 'color
             )
;;       (require 'lazy-lock) ; krb - removed 6/15/12
;;       (require 'font-latex)
       (require 'font-lock)
       (global-font-lock-mode 1) 
       ))

;(defun my-ctypes-load-hook ()
;  (ctypes-read-file "~/.ctypes_std_c" nil t t)
;  (ctypes-read-file ".ctypes" nil t t))
;(add-hook 'ctypes-load-hook 'my-ctypes-load-hook)


;; man stuff
;; (cond (window-system 
;;        (require 'man-frames)
;;        (setq Man-notify 'newframe)
;; ))
; html-mode
;(autoload 'html-helper-mode "combo-html-mode" "Yay HTML" t)
;(setq auto-mode-alist 
(append '(("\\.html$" . html-helper-mode)) auto-mode-alist)
;(setq auto-mode-alist 
;      (append '(("\\.html$" . html-helper-mode)) auto-mode-alist))

;(setq html-helper-do-write-file-hooks t)
;(setq html-helper-build-new-buffer t)
;(setq html-helper-address-string 
;  "<a href=\"http://www.cs.nmsu.edu/~kbisset/\">Keith Bisset &lt;kbisset@cs.nmsu.edu&gt;</a>")

;; ebd database stuff
(setq db-format-file-path 
      '("/home/grad1/kbisset/emacs/edb" 
        "/home/grad1/kbisset/emacs/edbibtex"))
(autoload 'db-find-file "database" "Emacs Database" t)

; compilation stuff
;; (require 'compile)
;; (setq compile-auto-highlight t)
;; (cond (window-system 
;;        (require 'compile-frame)
;;        (setq compilation-frame-alist
;;              '((name . nil) 
;;                (height . 10) (width . 90) 
;;                (minibuffer . t)
;; ;;               (font . "-etl-*-medium-r-*-*-16-*-*-*-*-*-iso8859-1")
;; ;               (font . "-etl-*-medium-r-*-*-14-*-*-*-*-*-iso8859-1")
;; ;;               (font . "-nucleus-gamow-medium-r-semicondensed-*-10-*-*-*-*-*-iso8859-1")
;; ;;               (font . "6x10")
;;                (top . 700)
;;                (left . 0)
;;                (modeline . nil)
;;                ))
;; ;             '((name . nil) 
;; ;               (height . 15) (width . 80) 
;; ;               (top . 590) (left . 0) 
;; ;               (minibuffer . nil)
;; ;               (modeline . nil)
;; ;               ))
;;        (add-hook 'compilation-frame-selected-hook
;;           '(lambda () 
;;              (raise-frame compilation-frame-id)
;;              ))
;; ))

;; ;(load-library "compile2")

(defvar compile-command "cd $HOME/cws/src/snitch && catkin build")
(global-set-key "\C-x~" 'find-this-error)
(global-set-key "\eM" 'remote-compile)
;;(setq remote-shell-program "/local/bin/ssh")
(global-set-key "\em" 'compile)
(global-set-key "\ek" 'kill-compilation)

;; Colorize compiler output
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; framepop stuff
;; User variables are `framepop-max-frame-size', `framepop-min-frame-size'
;; and `framepop-frame-parameters'.
;;(require 'framepop)
;; (cond (window-system (require 'advice)
;;                     (require 'framepop)
;;                     (setq special-display-function 'framepop-special-display)
;;                     (setq special-display-buffer-names '("*compilation*"))
;;                     (framepop-wrap 'remote-compile "*compilation*")
;;                     (setq framepop-max-frame-size 30
;;                           framepop-min-frame-size 30
;;                      framepop-frame-parameters 
;;                           '((name . "*compilation") 
;;                             (unsplittable . t) ; always include this
;;                             (width . 80) ; this parameter is needed
;;                             ;; (auto-raise . t)
;;                             (left . 0)
;;                             (top . -30)
;;                             ;; The following is needed for some WM's
;;                             ;; (user-position . t)
;;                             (background-color . "black") 
;;                             (foreground-color . "white") 
;;                             (Vertical-scroll-bars . nil) 
;;                             (modeline . nil)
;;                             (menu-bar-lines . 0)
;;                             (minibuffer . nil)))
;;                     ))

;(define-key global-map "\C-cz" 'framepop-toggle-frame)
;(define-key global-map "\C-cv" 'framepop-scroll-frame)
;(define-key global-map "\C-cs" 'framepop-show-frame)
;(define-key global-map "\C-cx" 'framepop-iconify-frame)
;;(define-key global-map "\C-cr" 'framepop-raise-frame)
;;(define-key global-map "\C-cl" 'framepop-lower-frame)

;(defun kf-focus-next-or-previous-frame (parg)
;  "Switch the focus to the next logical frame (and raise that frame to
;the front).  Keyboard input will go to the newly selected frame.
;Prefix arg means go to previous frame, not next frame.
;The mouse cursor will not follow you, which is kind of a weird
;feeling, but you'll get used to it."
;  (interactive "P")
;  (let* ((nowframe (selected-frame))
;        (nextframe (if parg (previous-frame) (next-frame)))
;        (visip (frame-visible-p nextframe)))
;    (and visip
;         (progn
;           (select-frame nextframe)
;           (if (eq visip 'icon) (iconify-or-deiconify-frame))
;           (set-mouse-position nextframe (- (frame-width nextframe) 1) 0)
;           (unfocus-frame)))))
;(global-set-key "\C-x5o" 'kf-focus-next-or-previous-frame)


; shell stuff
(setq shell-prompt-pattern "^[a-z]*\@[a-z]* [[0-9]*\] ")
(setq shell-mode-hook
      (function
       (lambda ()
         (set-variable 'tab-width 8)
         )))

(setq auto-mode-alist
  (append '(
      ("\\.cc$"  . c++-mode)
      ("\\.h$"  . c++-mode)
      ("\\.hpp$"  . c++-mode)
      ("\\.l$"  . c-mode)
      ("\\.nr$" . nroff-mode)
      ("\\.x$"  . c-mode)
      ("\\.otl$" . outline-mode))
    auto-mode-alist))


;;; AUC-TeX stuff
;;(require 'tex-site)
(setq TeX-parse-self t ; Enable parse on load.
      TeX-auto-save t ; Enable parse on save.
      TeX-master nil
)
;;; TeX stuff
(setq tex-dvi-view-command "xdvi")
(setq tex-dvi-print-command "dvips")
(setq bibtex-sort-ignore-string-entries t)

;;(autoload 'cite-it-mode "cite-it" "" t)
;;(if (not (boundp 'TeX-mode-hook)) (setq TeX-mode-hook nil))
;;(add-hook 'TeX-mode-hook '(lambda () (auto-fill-mode 1)))

;          '(lambda ()
;             (auto-fill-mode 1)
;             (define-key tex-mode-map
;               "\C-c\\" 'tex-insert-plain-command)
;             ;;         (load "tex-complete")
;             ))

;;(if (not (boundp 'LaTeX-mode-hook)) (setq LaTeX-mode-hook nil))
;; RefTeX Stuff
(autoload 'reftex-mode               "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex            "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-add-to-label-alist "reftex" "RefTeX Minor Mode" nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Bib-cite stuff
(autoload 'turn-on-bib-cite "bib-cite")
(add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)
(setq LaTeX-section-label "sec:"
      TeX-electric-escape t
      TeX-insert-braces nil
      TeX-parse-self t
      TeX-auto-save t
;      TeX-auto-global "/usr/local/share/emacs/site-lisp/auctex/auto"
;      TeX-macro-global "/usr/local/teTeX/texmf/tex/"
)
;(add-hook 'LaTeX-mode-hook '(lambda () 
;                              (require 'font-latex)
;                              (reftex-mode)
;;                              (require 'bib-cite)
;                              (bib-cite-minor-mode)
;;                             (cite-it-mode)
;;                              (cite-it-scan-bibliography)
;;                              ))
;;(TeX-add-style-hook "latex2e"
;;      (function (lambda () (cite-it-scan-bibliography))))


;;;        (require 'hilit-LaTeX)
;;         (define-key tex-mode-map
;;           "\C-c\C-m" 'latex-make-environment)
;;         (define-key tex-mode-map
;;           "\C-cI" 'latex-insert-begin)
;;         (define-key tex-mode-map
;;           "\C-ci" 'cite-it-insert-cite-and-citation)
;;         (define-key tex-mode-map
;;           "\C-c\\" 'tex-insert-latex-command)
;             ))

;;(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;(setq reftex-label-alist 
;      '(
;                                        ; Section label.
;        ("section"   "sec" "~\\ref{%s}" t
;         "Part" "Chapter" "Chap." "Section" "Sect." "Paragraph" "Par."
;         "\\S" "Teil" "Kapitel" "Kap." "Abschnitt" )  
;                                        ; Equation label.
;        ("equation"  "eq" "~(\\ref{%s})" 
;         "\\\\begin{\\(equation\\|eqnarray\\)}\\|\\\\\\\\"
;         "Equation" "Eq." "Eqn." "Gleichung"  "Gl.") 
;                                        ; Equationarray label
;        ("eqnarray"  "eq" "~(\\ref{%s})"
;         "\\\\begin{\\(equation\\|eqnarray\\)}\\|\\\\\\\\"
;         "Equation" "Eq." "Eqn."  "Gleichung"  "Gl.") 
;                                        ; Figure label.
;        ("figure"    "fig" "~\\ref{%s}" "\\\\caption\\(\\[[^\\]]*\\]\\)?{"
;         "Figure" "Fig." "Abbildung" "Abb.")
;                                        ; Table label.
;        ("table"     "tab" "~\\ref{%s}" "\\\\caption\\(\\[[^\\]]*\\]\\)?{"
;         "Table" "Tab." "Tabelle") 
;        ))

;(autoload 'set-help-file "word-help"
;  "Sets the set of Texinfo files used for `word-help' to HELPFILE."
;  t nil)
;(autoload 'word-help "word-help"
;  "Find documentation on the KEYWORD under the cursor (or passed)."
;  t nil)
;(autoload 'word-help-complete "word-help"
;  "Perform completion on the symbol preceding the point." t nil)
;(autoload 'word-help-add-keywords "word-help"
;  "Add user keywords to a certain Info file." nil nil)
;(define-key help-map [?\C-i] 'word-help)
;(global-set-key [\C-tab] 'word-help-complete)

;; OO-Browser stuff
;; (add-hook 'br-mode-hook 'br-setup-external)
;; (setq br-directory "/home/tsa5/kbisset/src/emacs/oobr")
;; (autoload 'oo-browser (expand-file-name "br-start" br-directory)
;;   "Invoke the OO-Browser" t)
;; (autoload 'br-env-browse (expand-file-name "br-start" br-directory)
;;   "Browse an existing OO-Browser Environment" t)
 
;; (defvar br-env-lang-avector
;;   '[("C++"     . "c++-")
;;     ("Python"  . "python-")]
;;   "Association vector of (LANGUAGE-NAME . LANGUAGE-PREFIX-STRING) elements of OO-Browser languages.")
;; You may want to change the settings of 'c++-cpp-include-dirs'
;;    and 'c++-include-dirs' at the bottom of "br-site.el".
 
;; Other Stuff
(add-hook 'text-mode-hook     'turn-on-auto-fill)
(setq-default fill-column 90)
;;(add-hook 'write-file-hooks 'time-stamp)

(setq make-backup-files t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      backup-by-copying-when-linked t
      require-final-newline     1
      visible-bell              nil
      mode-line-inverse-video nil
      frame-title-format '( "--Emacs--" system-name "--%b--")
;;      icon-title-format  '( "--Emacs--" system-name "--" )
      ispell-dicionary "~/.ispell-dict"
      ispell-enable-tex-parser t
      next-line-add-newlines nil
      mark-even-if-inactive t
      display-time-day-and-date t
      display-time-24hr-format t
      display-time-use-mail-icon t
      default-tab-width 2
      find-file-visit-truename t
      show-trailing-whitespace t
      indicate-empty-lines t
      completion-ignore-case t
)
(setq-default indent-tabs-mode nil)
(setq-default case-fold-search t)

;; id-select stuff
(autoload 'id-select-and-kill-thing    "id-select" 
  "Kill syntactical region selection" t)
(autoload 'id-select-and-copy-thing    "id-select" 
  "Select and copy syntactical region" t)
(autoload 'id-select-double-click-hook "id-select" 
  "Double mouse click syntactical region selection" nil)
(autoload 'id-select-thing             "id-select" 
  "Keyboard-driven syntactical region selection" t)
(autoload 'id-select-thing-with-mouse  "id-select" 
  "Single mouse click syntactical region selection" t)
(define-key global-map "\C-ct" 'id-select-thing)

;(define-key global-map "\C-xt"  'time-stamp)
(define-key ctl-x-map "C-xt"  'time-stamp)
(define-key global-map "\C-cg"    'goto-line)
(define-key global-map "\^H" 'delete-backward-char)
(define-key global-map "\e?" 'help-for-help)
;;(define-key "\ed" 'gdb)
;;(define-key "\er" 'replace-string)
(define-key global-map "\C-z" nil)
(define-key global-map "\C-cc" 'comment-region)

(cond (window-system 
       (global-set-key "\C-x\C-c" nil)
       (global-set-key "\C-x\C-z" nil)
       (global-set-key "\C-xCZ" 'iconify-or-deiconify-frame)
       ))
(global-set-key "\C-xCC" 'save-buffers-kill-emacs)
;;(define-key global-map "\003u"    'untabify-all)
;;(define-key global-map "\003i"    'c-indent-command)
;;(define-key global-map "\003\014" 'ind-line)
;;(define-key global-map "\003p"    'indent-all)
;;(define-key global-map "\003/"    'com-line)
 
(transient-mark-mode 1)
;;(put 'eval-expression 'disabled nil)

                    
(fset 'ind-line "i")
(fset 'untabify-all "/u<>xuntabify
ju")
(fset 'indent-all "/u<>xindent-region
ju")
;;(fset 'com-line "/u/*  */ju")

(put 'narrow-to-region 'disabled t)
(put 'auto-fill-mode  'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)


(setq default-mode-line-format
      (append nil 
              '("-%1*-<%b>--" 
                global-mode-string 
                "--" (-3 . "%p") 
                "--#%l-" 
                "-%[(" 
                mode-name 
                minor-mode-alist 
                "%n" 
                mode-line-process 
                ")%] " 
                user-login-name "@" system-name
                " -%-"
                )))
(setq frame-title-format
      (append nil 
              '(""
                user-login-name "@" system-name
                " [%b]"
                ))) 
(defun xbuffy-command ()  
  (interactive)
  (select-frame (next-frame))
  (switch-to-mail)
;;  (bury-buffer)
  )

(defun switch-to-mail ()
  (interactive)
  (cond ((not (boundp 'gnus-version)) (require 'gnus)))
  (cond ((not (gnus-alive-p)) (gnus 3 t)))
;;  (cond ((not (buffer-live-p "*Group*")) (gnus 3)))
;;   (cond ((equal (selected-frame) calendar-frame)
;;          (select-frame (previous-frame (selected-frame) nil))))
  (switch-to-buffer "*Group*")
  (gnus-group-get-new-news 3)
)

(define-key global-map "\C-cm" 'switch-to-mail)


;; stuff to look at
;; imenu 
;;; `cc-imenu-c++-generic-expression' must handle the following cases
;;; correctly:-
;;; Case #1
;;;  `const volatile int* ReadIOPort( ... )'
;;;
;;; Case #2
;;;  `const volatile X& X::X( ... )'
;;;
;;; Case #3
;;;  `template <class T> const volatile X<T>& X<T>::X( ... )'
;;;
;;; Case #4
;;;  `template <class T> const volatile X<T>& X<T>::operator <= ( ... )'

;;; WARNING: E-mailer may have wrapped some lines

(setq pp-cc-imenu-c++-generic-expression
  (` 
   ((nil
     (, 
      (concat
       "^"				; beginning of line is required
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template<...>"
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	; type specs; there can be no
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	; more than 3 tokens, right?
       "\\("				; last type spec including */&
       "[a-zA-Z0-9_:<>]+"
       "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" ; either pointer/ref sign or whitespace
       "\\)?"				; if there is a last type spec
       "\\("				; name; take that into the imenu entry
                                        ; FIRST interpret the sub-regexp for any
                                        ; `operator ... ()'. Why? see below.
       "\\([a-zA-Z0-9_:<>~]*\\)?operator[ \t]*"
       "\\("
       "[a-zA-Z0-9_:<>+-*/=!,&|^]+"	; `operator >>' or `operator <<' etc 
; C++ operators out of the set ( + - * / -> () == != < > => <= , ++ -- & | ^ )
; and also any type conversion operators too.
; operator does quite NOT work out with this old sub-regexp ...
       "[^a-zA-Z1-9_][^(]*"		; ...or operator
       "\\|"
       "\(\)"				; get function object `operator () (...)'
					; this must come before the function name 
					; sub-regexp otherwise `operator()(...)' 
					; becomes function `operator(...)' :-(

       "\\)"
       "\\|"
       "[a-zA-Z0-9_:<>~]+"		; here is the function name sub-regexp
					; member function, ctor or dtor...
 					; (may not contain * because then 
 					; "a::operator char*" would become "char*"!)
       "\\)"
       "[ \t]*([^)]*)[ \t\n]*[^		;]" ; require something other than a ; after
 					; the (...) to avoid prototypes.  Can't
 					; catch cases with () inside the parentheses
 					; surrounding the parameters
 					; (like "int foo(int a=bar()) {...}"
       )) 6)    
    ("Class" 
     (, (concat 
         "^"				; beginning of line is required
         "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
         "class[ \t]+"
         "\\([a-zA-Z0-9_]+\\)"		; this is the string we want to get
         "[ \t]*[:{]"
         )) 2)))
;  "Imenu generic expression for C++ mode.  See
;`imenu-generic-expression'."
)

(defun unbury-buffer ()
  "Unbury buffer (from Vegard@protek.unit.no (Vegard Vesterheim))"
  (interactive)
  (require 'cl)
  (switch-to-buffer (car (last (buffer-list)))))

(global-set-key [f9] 'bury-buffer)
(global-set-key [S-f9] 'unbury-buffer)

(add-hook 'message-load-hook 
          '(lambda () 
             (set-face-foreground 'message-header-xheader-face "cyan")
             (set-face-foreground 'message-separator-face "darkcyan")
             (set-face-foreground 'message-header-name-face "yellow")
             (set-face-foreground 'message-header-other-face "red")
             ))
(add-hook 'message-send-hook 'ispell-message)
(add-hook 'message-send-hook 'mml-to-mime)
; disptime.el
(defadvice switch-to-buffer (before existing-buffers-only activate)
  "Switch to existing buffers only, unless called with a prefix argument."
  (interactive (list (read-buffer "Switch to buffer: " (other-buffer)
                                  (null current-prefix-arg)))))

;;(standard-display-european 1)

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
;;
;; You may also want something like:
;;
(setq auto-mode-alist
      (append '(("\\.Xdefaults$"    . xrdb-mode)
;;                 ("\\.Xenvironment$" . xrdb-mode)
;;                 ("\\.Xresources$"   . xrdb-mode)
                ("*.\\.ad$"         . xrdb-mode)
                ("*.\\.xdb$"         . xrdb-mode)
                 )
               auto-mode-alist))
;;
;; Emacs 19 users will probably have to add the following to their
;; .emacs file:
;;
;; (defadvice char-after (before c-char-after-advice
;; 			      (&optional pos)
;; 			      activate preactivate)
;;   "POS is optional and defaults to the position of point."
;;   (if (not pos)
;;       (setq pos (point))))
;(setq minibiffer-auto-raise t)
(require 'ffap)
(setq ffap-require-prefix t)
(global-set-key "\C-x\C-f" 'find-file-at-point)
(global-set-key "\C-xm" 'message-mail)
(setq user-mail-address "kbisset@vbi.vt.edu")
;;(set-default-font "-etl-*-medium-r-*-*-18-*-*-*-*-*-iso8859-1")
;;(set-default-font "-nucleus-gamow-medium-r-semicondensed-*-12-*-*-*-*-*-iso8859-1")
(setq mime-editor/split-message nil)

(if (not (eq 'user-full-name "Super-User"))
    (progn (require 'calendar)
           (cond ((equal emacs-major-version 20)
                  (load-library "cal-weather")))
;;(require 'cal-desk-calendar)

     (set-face-background 'calendar-today-face "red")
     (set-face-foreground 'calendar-today-face "yellow")

     (setq mark-diary-entries-in-calendar t
           view-diary-entries-initially t
           diary-file "~/.diary"
           holidays-in-diary-buffer t
           diary-list-include-blanks t
           )
     (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
     (add-hook 'diary-display-hook 'fancy-diary-display)
;;    (add-hook 'diary-display-hook 'mark-holidays)
;;     (calendar-one-frame-setup)
     ))

(defun global-pop-menu (event)
  "Pop up the menu-bar menu at the mouse position (saved)"
  (interactive "e")
       (save-window-excursion
           (let ((mpp (mouse-position))    ;;save mouse starting position
                 (do-cmd (x-popup-menu t (lookup-key global-map [menu-bar])))
                 ary lng)
                                           ;; restore mouse original position
               (set-mouse-position (car mpp) (car (cdr mpp)) (cdr (cdr mpp)))
               (if do-cmd                  ;; any command requested ?
                   (progn                  ;; yes, must convert list to vector
                       (Setq do-cmd (append '(menu-bar) do-cmd))
                       (setq lng (length do-cmd))
                       (setq ary (make-vector lng nil))    ;;command vector
                       (while (> lng 0)
                           (setq lng (1- lng))
                           (aset ary lng (nth lng do-cmd)))
                       (call-interactively (lookup-key global-map ary))
                   )))))

(defun global-pop-menu-by-key ()
  "Pop up a menu of global operation by key, restore mouse position afterwards"
  (interactive)
       (let ((mpp (mouse-position)))
           (set-mouse-position (selected-frame) 0 0)
           (global-pop-menu (mouse-position))
           (set-mouse-position (car mpp) (car (cdr mpp)) (cdr (cdr mpp)))))

;; you may bind only mouse events to `global-pop-menu'
(global-set-key [C-S-mouse-3] 'global-pop-menu)

;; speedbar
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(global-set-key "\C-cs" 'speedbar-get-focus)
;; (setq speedbar-frame-parameters '((minibuffer . t)
;;                                   (width . 30)
;;                                   (height . 71)
;;                                   (border-width . 0)
;; ;;                                  (menu-bar-lines . 0)
;;                                   (unsplittable . t)
;; ;                                  (font . "-etl-*-medium-r-*-*-14-*-*-*-*-*-iso8859-1")
;; ;;                                  (font . "-nucleus-gamow-medium-r-semicondensed-*-10-*-*-*-*-*-iso8859-1")

;;                                   ))

;; (autoload 'w3-speedbar-buttons "sb-w3"
;;   "W3 specific speedbar button generator.")

; (require 'w3-toc)

(setq diary-mail-days 7 
      european-calendar-style t 
      diary-mail-addr "kbisset@vbi.vt.edu" )

; (setq compilation-error-regexp-alist
;   '(
;     ;; NOTE!  See also grep-regexp-alist, below.

;     ;; 4.3BSD grep, cc, lint pass 1:
;     ;; 	/usr/src/foo/foo.c(8): warning: w may be used before set
;     ;; or GNU utilities:
;     ;; 	foo.c:8: error message
;     ;; or HP-UX 7.0 fc:
;     ;; 	foo.f          :16    some horrible error message
;     ;; or GNU utilities with column (GNAT 1.82):
;     ;;   foo.adb:2:1: Unit name does not match file name
;     ;;
;     ;; We'll insist that the number be followed by a colon or closing
;     ;; paren, because otherwise this matches just about anything
;     ;; containing a number with spaces around it.
;     ("\n\
; \\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
; :\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)

;     ;; Microsoft C/C++:
;     ;;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
;     ;;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
;     ("\n\\(\\([a-zA-Z]:\\)?[^:( \t\n-]+\\)[:(][ \t]*\\([0-9]+\\)[:) \t]" 1 3)

;     ;; Borland C++:
;     ;;  Error ping.c 15: Unable to open include file 'sys/types.h'
;     ;;  Warning ping.c 68: Call to function 'func' with no prototype
;     ("\n\\(Error\\) \\([a-zA-Z]?:?[^:( \t\n]+\\)\
;  \\([0-9]+\\)\\([) \t]\\|:[^0-9\n]\\)" 2 3)

;     ;; 4.3BSD lint pass 2
;     ;; 	strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)
;     ("[ \t:]\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(](+[ \t]*\\([0-9]+\\))[:) \t]*$"
;      1 2)

;     ;; 4.3BSD lint pass 3
;     ;; 	bloofle defined( /users/wolfgang/foo.c(4) ), but never used
;     ;; This used to be
;     ;; ("[ \t(]+\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+" 1 2)
;     ;; which is regexp Impressionism - it matches almost anything!
;     ("([ \t]*\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))" 1 2)

;     ;; MIPS lint pass<n>; looks good for SunPro lint also
;     ;;  TrimMask (255) in solomon.c may be indistinguishable from TrimMasks (93) in solomon.c due to truncation
;     ("[^ ]+ (\\([0-9]+\\)) in \\([^ ]+\\)" 2 1)
;     ;;  name defined but never used: LinInt in cmap_calc.c(199)
;     ("in \\([^(]+\\)(\\([0-9]+\\))$" 1 2)

;     ;; Ultrix 3.0 f77:
;     ;;  fort: Severe: addstf.f, line 82: Missing operator or delimiter symbol
;     ;; Some SGI cc version:
;     ;;  cfe: Warning 835: foo.c, line 2: something
;     ("\n\\(cfe\\|fort\\): [^:\n]*: \\([^ \n]*\\), line \\([0-9]+\\):" 2 3)
;     ;;  Error on line 3 of t.f: Execution error unclassifiable statement
;     ;; Unknown who does this:
;     ;;  Line 45 of "foo.c": bloofle undefined
;     ;; Absoft FORTRAN 77 Compiler 3.1.3
;     ;;  error on line 19 of fplot.f: spelling error?
;     ;;  warning on line 17 of fplot.f: data type is undefined for variable d
;     ("\\(\n\\|on \\)[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
; of[ \t]+\"?\\([a-zA-Z]?:?[^\":\n]+\\)\"?:" 3 2)

;     ;; Apollo cc, 4.3BSD fc:
;     ;;	"foo.f", line 3: Error: syntax error near end of statement
;     ;; IBM RS6000:
;     ;;  "vvouch.c", line 19.5: 1506-046 (S) Syntax error.
;     ;; Unknown compiler:
;     ;;  File "foobar.ml", lines 5-8, characters 20-155: blah blah
;     ;; Microtec mcc68k:
;     ;;  "foo.c", line 32 pos 1; (E) syntax error; unexpected symbol: "lossage"
;     ;; GNAT (as of July 94):
;     ;;  "foo.adb", line 2(11): warning: file name does not match ...
;     ;; IBM AIX xlc compiler:
;     ;;  "src/swapping.c", line 30.34: 1506-342 (W) "/*" detected in comment.
;     ("\"\\([^,\" \n\t]+\\)\", lines? \
; \\([0-9]+\\)\\([\(.]\\([0-9]+\\)\)?\\)?[:., (-]" 1 2 4)

;     ;; MIPS RISC CC - the one distributed with Ultrix:
;     ;;	ccom: Error: foo.c, line 2: syntax error
;     ;; DEC AXP OSF/1 cc
;     ;;  /usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah
;     ("rror: \\([^,\" \n\t]+\\)[,:] \\(line \\)?\\([0-9]+\\):" 1 3)

;     ;; IBM AIX PS/2 C version 1.1:
;     ;;	****** Error number 140 in line 8 of file errors.c ******
;     ("in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)
;     ;; IBM AIX lint is too painful to do right this way.  File name
;     ;; prefixes entire sections rather than being on each line.

;     ;; Lucid Compiler, lcc 3.x
;     ;; E, file.cc(35,52) Illegal operation on pointers
;     ("\n[EW], \\([^(\n]*\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)" 1 2 3)

;     ;; GNU messages with program name and optional column number.
;     ("\n[a-zA-Z]?:?[^0-9 \n\t:]+:[ \t]*\\([^ \n\t:]+\\):\
; \\([0-9]+\\):\\(\\([0-9]+\\)[: \t]\\)?" 1 2 4)

;     ;; Cray C compiler error messages
;     ("\n\\(cc\\| cft\\)-[0-9]+ c\\(c\\|f77\\): ERROR \\([^,\n]+, \\)* File = \\([^,\n]+\\), Line = \\([0-9]+\\)" 4 5)

;     ;; IBM C/C++ Tools 2.01:
;     ;;  foo.c(2:0) : informational EDC0804: Function foo is not referenced.
;     ;;  foo.c(3:8) : warning EDC0833: Implicit return statement encountered.
;     ;;  foo.c(5:5) : error EDC0350: Syntax error.
;     ("\n\\([^( \n\t]+\\)(\\([0-9]+\\):\\([0-9]+\\)) : " 1 2 3)

;     ;; Sun ada (VADS, Solaris):
;     ;;  /home3/xdhar/rcds_rc/main.a, line 361, char 6:syntax error: "," inserted
;     ("\n\\([^, ]+\\), line \\([0-9]+\\), char \\([0-9]+\\)[:., \(-]" 1 2 3)
;     )
; )

;; (make-face 'compilation-mode-make-face)  ;Make output
;; (make-face 'compilation-mode-make-error-face)  ;Make error output
;; (make-face 'compilation-mode-error-face) ;Errors
;; (make-face 'compilation-mode-warn-face)  ;Warnings
;; (make-face 'compilation-mode-description-face)  ;description of above two
;; (make-face 'compilation-mode-location-face)  ;location of above
;; (set-face-foreground 'compilation-mode-make-face "green")
;; (set-face-foreground 'compilation-mode-make-error-face "red")
;; (set-face-foreground 'compilation-mode-error-face "red")
;; (set-face-foreground 'compilation-mode-warn-face "magenta")
;; (set-face-foreground 'compilation-mode-description-face "yellow")
;; (set-face-foreground 'compilation-mode-location-face "cyan")

(setq compilation-mode-font-lock-keywords
;      '(("^\\([a-zA-Z]?:?[^ \n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$"
;         1 font-lock-function-name-face))
      '(
        ("^\\(make.*: \\*\\*\\*\\)" 1 'compilation-mode-make-error-face)
        ("^\\(make.*:\\)" 1 'compilation-mode-make-face)
;;        ("\\(Warning:.*\\.\\)" 1 font-lock-function-name-face)
        ("\\(Error:\\)" 1 'compilation-mode-error-face)
        ("\\(Warning:\\)" 1 'compilation-mode-warn-face)
        ("\\(warning:\\)" 1 'compilation-mode-warn-face)
        ("\\(Error:.*\\.\\)" 1 'compilation-mode-description-face keep)
        ("\\(Warning:.*\\.\\)" 1 'compilation-mode-description-face keep)
        ("\\(\".*\", line [0-9]*:\\)" 1 'compilation-mode-location-face)
        )
;;;  ("^\\([^\n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$" 0 font-lock-keyword-face keep)
      )
(menu-bar-mode -1)
(blink-cursor-mode 0)
;;(fset 'char-int 'identity)

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(defun flyspell-generic-progmode-verify ()
  "Used for `flyspell-generic-check-word-p' in programming modes."
  (let ((f (get-text-property (point) 'face)))
    (memq f '(font-lock-comment-face font-lock-string-face))))

(defun flyspell-prog-mode ()
  "Turn on `flyspell-mode' for comments and strings."
  (interactive)
  (setq flyspell-generic-check-word-p 'flyspell-generic-progmode-verify)
  (flyspell-mode 1))

(add-hook 'c-mode-hook 'flyspell-prog-mode)
(add-hook 'LaTeX-mode-hook '(lambda () (flyspell-mode t)))

(defun scroll-up-one-line (n)
  "Like scroll-up, but scrolls only one line by default."
  (interactive "p")
  (scroll-up n))

(defun scroll-down-one-line (n)
  "Like scroll-down, but scrolls only one line by default."
  (interactive "p")
  (scroll-down n))

(global-set-key (kbd "S-<down>") 'scroll-up-one-line)
(global-set-key (kbd "S-<up>") 'scroll-down-one-line)
(setq save-history-max-length nil)
;;(require 'save-history)
(require 'savehist)
(autoload 'dict "dict-web" "Lookup a word or phrase in the Online Dictionary Database." t)

(defun time-stamp-insert ()
  (interactive)
  (setq ts-format (match-string 4 time-stamp-pattern))
  (let ((new-time-stamp (time-stamp-string ts-format)))
    (insert-and-inherit new-time-stamp)
))
(define-key isearch-mode-map (quote [mouse-2]) 'isearch-yank-kill)
(define-key isearch-mode-map (quote [down-mouse-2]) nil)

;(require 'follow-mouse)
;(toggle-follow-mouse 1)
;(setq follow-mouse-auto-raise nil)

;; Change ellipses in elided text
;;(setq buffer-display-table (copy-tree standard-display-table))
;;(set-display-table-slot buffer-display-table 4
;;     [122 122 122 46 122 122 46 46 122 46 46])
;;                        [46 46 46])
(make-face 'krb-ellipses-face)
(set-face-foreground 'krb-ellipses-face "yellow")
(set-face-underline-p 'krb-ellipses-face t)
(set-display-table-slot standard-display-table 4 
  (make-vector 3 (+ (lsh (face-id 'krb-ellipses-face) 19) ?.)))
(set-display-table-slot standard-display-table 1
  (make-vector 3 (+ (lsh (face-id 'krb-ellipses-face) 19) ?.)))

;; make backup files in ~/.backups/ rather than scattered around all
;; over the filesystem. 
;; adapted from http://www.linuxgazette.com/issue31/marsden.html

(require 'dired)

(defvar backup-files-into-one-directory t
  "*Variable to control if emacs backup files should go into one directory.")

(defun make-backup-file-name (file-name)                     
  "Create the non-numeric backup file name for `file-name'." 
  (if (and (file-exists-p "~/.backups")
  	   backup-files-into-one-directory)
      (concat (expand-file-name "~/.backups/")
	      (dired-replace-in-string "/" "_" file-name) "~")
    (concat file-name "~")))

(require 'screenline)
(tool-bar-mode -1)


;;(require 'perkymbf)
;; perkymbf
(setq perkymbf-normal-foreground-color "white"
      perkymbf-normal-background-color "black"
      perkymbf-normal-cursor-color "red"
      perkymbf-active-foreground-color "black"
      perkymbf-active-background-color "yellow"
      perkymbf-active-cursor-color "red")
;; (require 'browse-kill-ring)

;; (global-set-key [A-right] 'down-list)
;; (global-set-key [A-left] 'up-list)
;; (global-set-key [A-up] 'forward-sexp)
;; (global-set-key [A-down] 'backward-sexp)

(require 'from)
(setq from-highlight-regexp "^.*\.lanl\.gov"
      from-line-format "<%d> %-30,30a(%-20,20n) %-48,49s"
      from-date-format "%a %H:%M")

;;      from-line-format "%N")

(add-hook 'display-time-hook 'from-update-buffer)
(defun make-directory-if-needed ()
   "Hook into find-file-hook and query to create a directory when a file is opened."
  (let ((dir-part (file-name-directory buffer-file-name)))
    (when (and (not (file-directory-p dir-part))
	       (not noninteractive)
	       (y-or-n-p (format "The directory %s does not exist. Create it?" dir-part)))
      (make-directory dir-part t))
    nil))

(add-hook 'find-file-not-found-hooks 'make-directory-if-needed)

;; (require 'ecb)
;; (autoload 'ecb-activate "ecb" "Activate ECB" t)
;; (ecb-layout-define "leftkrb" left
;;   "This function creates the following layout:

;;    -------------------------------------------------------
;;    |              |                                      |
;;    |              |                                      |
;;    |              |                                      |
;;    |  Methods     |                 Edit                 |
;;    |              |                                      |
;;    |              |                                      |
;;    |              |                                      |
;;    |              |                                      |
;;    |              |                                      |
;;    |              |                                      |
;;    |              |                                      |
;;    -------------------------------------------------------
;;    |                                                     |
;;    |                    Compilation                      |
;;    |                                                     |
;;    -------------------------------------------------------

;; If you have not set a compilation-window in `ecb-compile-window-height' then the
;; layout contains no durable compilation window and the other windows get a little
;; more place."
;;   (ecb-set-methods-buffer)
;; ;;  (ecb-split-ver 0.75)
;; ;;  (ecb-set-history-buffer)
;;   (select-window (next-window)))

(setq semantic-load-turn-useful-things-on t
      semanticdb-project-roots
      (list "/home/kbisset/mobicom/src"))
;(require 'semantic-load)

(define-key global-map [(shift up)]
  '(lambda()(interactive)
     (setq tabbar-buffer-group-mode t)))

(define-key global-map [(shift down)]
  '(lambda()(interactive)
     (setq tabbar-buffer-group-mode nil)))

(define-key global-map [(shift right)]
  '(lambda()(interactive)
     (tabbar-select-next-tab 1)))

(define-key global-map [(shift left)]
  '(lambda()(interactive)
     (tabbar-select-next-tab -1)))

;; (defun tabbar-select-next-tab( index )
;;   "+1 for next tab on right, -1 for next tab on left; all selections
;; are circular."
;;   (let* ((set (tabbar-current-tabset))
;;          (tabs (tabbar-tabs set))
;;          (icur (position (tabbar-selected-tab set) tabs))
;;          (inext (+ index icur))
;;          (new-tab (nth (cond
;;                         ((eq inext -1) num-tabs)
;;                         ((>= inext (length tabs)) 0)
;;                         (t inext))
;;                        tabs)))
;;     (setq tabbar-last-selected-tab new-tab)
;;     (switch-to-buffer (tabbar-tab-value new-tab))
;;     (force-mode-line-update)
;;     (sit-for 0)))

(setq browse-url-new-window-flag t)
(setq browse-url-mozilla-new-window-is-tab t)

 (cond (window-system 
        (require 'mwheel)
        (mouse-wheel-mode t)))

;; flymake
;;     (add-hook 'find-file-hooks 'flymake-find-file-hook)

(setq x-mouse-click-focus-ignore-position nil)
(global-set-key (quote [f13]) (quote clipboard-yank))
(server-start nil)

;; planner mode
(setq planner-project "WikiPlanner")
(setq muse-project-alist
      '(("WikiPlanner"
         ("~/plans"   ;; Or wherever you want your planner files to be
          :default "index"
          :major-mode planner-mode
          :visit-link planner-visit-link))))
(require 'planner)
(require 'remember-planner)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)
(setq ns-antialias-text t)

;; (require 'confluence)
;; (setq confluence-url "https://collaboration.vbi.vt.edu/confluence/rpc/xmlrpc")

(defun tabbar-buffer-groups-krb ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name)
             '("*scratch*" "*Messages*"))
     "Common"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    ((memq major-mode '(c-mode c++-mode))
     "C++"
     )
    ((memq major-mode 
         '(Makefile.am Autoconf))
     "Build"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups-krb)

;; assuming confluence.el and xml-rpc.el are in your load path
(require 'confluence)

;; note, all customization must be in *one* custom-set-variables block
;;(custom-set-variables
 ;; ... other custimization

 ;; confluence customization
;;(setq '(confluence-default-space-alist (list (cons confluence-url "~kbisset"))))
(setq confluence-url "https://collaboration.vbi.vt.edu/confluence/rpc/xmlrpc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; confluence editing support (with longlines mode)

;; (autoload 'confluence-get-page "confluence" nil t)

;; (eval-after-load "confluence"
;;   '(progn
;;      (require 'longlines)
;;      (progn
;;        (add-hook 'confluence-mode-hook 'longlines-mode)
;;        (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
;;        (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
;;        (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

;; ;; LongLines mode: http://www.emacswiki.org/emacs-en/LongLines
;; (autoload 'longlines-mode "longlines" "LongLines Mode." t)

;; (eval-after-load "longlines"
;;   '(progn
;;      (defvar longlines-mode-was-active nil)
;;      (make-variable-buffer-local 'longlines-mode-was-active)

;;      (defun longlines-suspend ()
;;        (if longlines-mode
;;            (progn
;;              (setq longlines-mode-was-active t)
;;              (longlines-mode 0))))

;;      (defun longlines-restore ()
;;        (if longlines-mode-was-active
;;            (progn
;;              (setq longlines-mode-was-active nil)
;;              (longlines-mode 1))))

;;      ;; longlines doesn't play well with ediff, so suspend it during diffs
;;      (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
;;                                              activate compile preactivate)
;;        "Suspend longlines when running ediff."
;;        (with-current-buffer (ad-get-arg 0)
;;          (longlines-suspend)))

    
;;      (add-hook 'ediff-cleanup-hook 
;;                '(lambda ()
;;                   (dolist (tmp-buf (list ediff-buffer-A
;;                                          ediff-buffer-B
;;                                          ediff-buffer-C))
;;                     (if (buffer-live-p tmp-buf)
;;                         (with-current-buffer tmp-buf
;;                           (longlines-restore))))))))

;; keybindings (change to suit)

;; open confluence page
;; (global-set-key "\C-xwf" 'confluence-get-page)

;; ;; setup confluence mode
;; (add-hook 'confluence-mode-hook
;;           '(lambda ()
;;              (local-set-key "\C-xw" confluence-prefix-map)))


;; EverNote integration
;; (add-to-list 'load-path "~/emacs/evernote-mode")
;; (require 'evernote-mode)
;; (setq evernote-developer-token "S=s40:U=4332bf:E=14c80a08a4e:C=14528ef5e52:P=1cd:A=en-devtoken:V=2:H=1e146158564df74b15f1b43ee8ebf8e9")
;; (setq enh-enclient-command "/opt/local/bin/enclient.rb")

;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
;; (setq evernote-password-cache t)
;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)

;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     :height 160
;;                     :weight 'normal
;;                     :width 'normal)

;;; Host specific settings
(setq krb-default-modeline-color (cons (face-background 'mode-line)
                                       (face-foreground 'mode-line)))
(setq krb-default-background-color "black")
(setq krb-background-color
      (cond 
       ((string= system-name "Keiths-MacBook-Pro.local") "#1a042b")
       ((string= system-name "vagrant-ubuntu-trusty-64") "#001900")
       ((string= system-name "stormtrooper") "#00002f")
;       ((string= system-name "cloud") "")
       ((string= system-name "katrina") "#211700")
       ((string= system-name "deep1") "#222200")
       ((string= system-name "deep2") "#222200")
       ((string= system-name "deep3") "#222200")
       ((string= system-name "deep4") "#222200")
       ((string= system-name "deep5") "#222200")
       ((string= system-name "deep6") "#222200")
       (t krb-default-background-color)))

(set-background-color krb-background-color)

(setq krb-modeline-color
      (cond 
       ((string= system-name "Keiths-MacBook-Pro.local") '("#93a1a1" . "Purple"))
       ((string= system-name "vagrant-ubuntu-trusty-64") '("#494d54" . "#66C26C"))
       ((string= system-name "stormtrooper") '("#4B4D54" . "#90C3D4"))
       ((string= system-name "cloud") '("#4B4D54" . "#fc8d62"))
       ((string= system-name "katrina") '("#4B4D54" . "#fc8d62"))
       ((string= system-name "deep1") '("#4B4D54" . "#DEDE16"))
       ((string= system-name "deep2") '("#4B4D54" . "#DEDE16"))
       ((string= system-name "deep3") '("#4B4D54" . "#DEDE16"))
       ((string= system-name "deep4") '("#4B4D54" . "#DEDE16"))
       ((string= system-name "deep5") '("#4B4D54" . "#DEDE16"))
       ((string= system-name "deep6") '("#4B4D54" . "#DEDE16"))
       (t krb-default-modeline-color)))

(set-face-foreground 'mode-line (car krb-modeline-color))
(set-face-background 'mode-line (cdr krb-modeline-color))
(set-face-foreground 'fringe (cdr krb-modeline-color))
(set-face-background 'fringe (car krb-modeline-color))


;; Inverse for inactive
(set-face-foreground 'mode-line-inactive (cdr krb-modeline-color))
(set-face-background 'mode-line-inactive (car krb-modeline-color))

; force change

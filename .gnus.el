(setq mail-yank-prefix ">")

(add-hook 'message-mode-hook 
	  '(lambda () 
	     (define-key message-mode-map "\C-c\C-a" 
	       'mail-interactive-insert-alias)))

;; (add-hook 'mail-setup-hook 'mail-abbrevs-setup)
;; e20 (require 'mime-setup)
;; (setq nnmail-spool-file "/home/grad1/kbisset/tmpmail/listmail"
;;       nnmail-keep-last-article t
;;       nnmail-crosspost nil
;;       nnmail-split-methods '(
;;                              ("list.macgps" "^Mailing-List: list macmap@yahoogroups.com;")
;;                              ("jeep.jeep-tj" "^Subject:.*JEEP-TJ")
;;                              ("jeep.jeep-tj" "^Subject:.*jeep-tj")
;;                              ("jeep.tj" "^Subject:.*[Tt][Jj]")
;;                              ("jeep.jeep-l" "^Subject:.*JEEP-L" )
;;                              ("jeep.swb" "^Subject:.*SWB" )
;;                              ("darwin" "^Subject:.*darwin" )
;;                              ("jeep.onelist.jeepsonline" "Mailing-List:.*JeepsOnline")
;;                              ("jeep.onelist.jeep" "Mailing-List:.*jeep@onelist")
;;                              ("jeep.onelist.sandiajeep" "Mailing-List:.*SandiaJeepClub")
;;                              ("jeep.onelist" "From:.*@onelist.com")
;;                              ("jeep.egroups.jeep" "^Mailing-List:.*jeep@egroups.com")
;;                              ("jeep.egroups.jeep" "^Mailing-List:.*jeep@yahoogroups.com")
;;                              ("jeep.jeeptech" "Sender: owner-jeeptech")
;;                              ("jeep.jeeptech" "List-Id: The JeepTech List")
;;                              ("goldpan" "Mailing-List:.*Goldpan@egroups.com")
;;                              ("goldpan" "Mailing-List:.*Goldpan@yahoogroups.com")
;;                              ("other" "")))
(setq gnus-secondary-select-methods ;;'((nnml "private") (nnslashdot ""))
;;      '((nnml "private") (nntp "lugnet.com") (nntp "News.CIS.DFN.DE"))
      '((nnml "private") (nntp "news.individual.net"))
      gnus-auto-select-next nil
      gnus-auto-expirable-newsgroups "nnml.*")


(setq gnus-nntp-server "news.vt.edu"
      gnus-select-method '(nntp)
      gnus-startup-file "~/.newsrc-news"
      )
(setq gnus-background-mode 'dark
       gnus-large-newsgroup 250
       gnus-interactive-exit nil
       gnus-save-newsrc-file nil
       gnus-save-killed-list nil
       gnus-auto-select-first nil
       gnus-score-find-score-files-function 'gnus-score-find-hierarchical
       browse-url-browser-function 'browse-url-w3
       gnus-use-cache 'passive
       gnus-subscribe-newsgroup-method 'gnus-subscribe-hierarchically
 ;;      gnus-auto-expirable-newsgroups "nndir*"
       gnus-check-bogus-groups nil
       gnus-check-new-newsgroups '((nntp "news.individual.com") 
                                   )
 ;      gnus-read-active-file nil
       gnus-read-active-file t
       gnus-inhibit-startup-message t
       gnus-thread-sort-functions (quote (gnus-thread-sort-by-number
                                          gnus-thread-sort-by-date
;;                                          my-gnus-thread-sort-by-subject
                                          gnus-thread-sort-by-total-score))
;;       gnus-thread-sort-functions '(gnus-thread-sort-by-date)
       gnus-summary-mode-line-format "%g/%A"
       gnus-summary-line-format "%U%R%z%I%(%[%i:%V %-20,20n%]%) %s
"
       gnus-score-interactive-default-score 1
       gnus-summary-gather-subject-limit 25
       gnus-summary-make-false-root 'dummy
       gnus-article-display-hook 
       '(gnus-article-hide-headers 
         gnus-article-hide-signature
;;         gnus-article-hide-citation-in-followups
         gnus-article-add-buttons
         gnus-article-highlight-headers
         gnus-article-highlight-citation
;;         rmime
         )
       gnus-thread-hide-subtree t
       gnus-subscribe-newsgroup-method 'gnus-subscribe-topics
       gnus-auto-center-summary t
       gnus-visible-headers nil
       gnus-summary-line-format "%U%R%z%I%(%[%t:%i:%V %-20,20n%]%) %s
"
       gnus-ignored-headers "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Sender:\\|^Received:\\|^Mail-from:\\|^X-[^F]*:\\|^Return-Path:\\|^Resent-.*:\\|^Encoding:\\|^Message-Id:\\|^Precedence:\\|^Resent-Sender:\\|.*Reply-To:\\|Content-.*:\\|Mime-.*:"
;;       gnus-header-face-alist '(("" nil nil) 
;;				("Approved" nil gnus-header-newsgroups-face) 
;;				("Subject" nil gnus-header-subject-face) 
;;				("Newsgroups:.*," nil gnus-header-newsgroups-face)
;;				("" gnus-header-name-face gnus-header-content-face))))
)

;;\\|^Approved:
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)	
(add-hook 'gnus-article-prepare-hook 'gnus-article-de-quoted-unreadable)
(add-hook 'gnus-summary-prepare-hook
          '(lambda () 
             (define-key gnus-summary-mode-map "n"    'gnus-summary-next-thread)
             (define-key gnus-summary-mode-map "p"    'gnus-summary-prev-thread)
             (define-key gnus-summary-mode-map "e"    'gnus-summary-mark-as-expirable)
             ))

;(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number 
;                                   gnus-thread-sort-by-date
;                                   my-gnus-thread-sort-by-subject))
;(setq gnus-simplify-ignored-prefixes "\\[JEEP-L:.*\\]")
;(setq gnus-simplify-subject-fuzzy-regexp "^\\[JEEP-L:[0-9]*\\]\\|^\\[JP\\]\\|^\([Rr][Ee]: *\)*\\|^\\[TETEX [0-9]*\\]")
(setq gnus-summary-gather-subject-limit 'fuzzy)
(setq gnus-auto-expirable-newsgroups "nndir.*")

;; (defun my-gnus-simplify-subject-jeep (subject)
;;   (if (string-match "^\([Rr][Ee] *: *\)+" subject)
;;       (substring subject (match-end 0))
;;     (my-gnus-simplify-subject-jeep1 subject)))

;; (defun my-gnus-simplify-subject-jeep1 (subject)
;; ;;  (if (string-match "^ *\\[JEEP\\-L:[0-9]*\\][ \t]*" subject)
;;   (if (string-match "^ *\\[[JFS].*\\][ \t]*" subject)
;;       (my-gnus-simplify-subject-tetex (substring subject (match-end 0)))
;;     (my-gnus-simplify-subject-tetex subject)))

;; (defun my-gnus-simplify-subject-tetex (subject)
;;   (if (string-match "^ *\\[TETEX:[0-9]*\\][ \t]*" subject)
;;       (my-gnus-simplify-subject-re (substring subject (match-end 0)))
;;     (my-gnus-simplify-subject-re subject)))

;; (defun my-gnus-simplify-subject-re (subject)
;;   (if (string-match "^\([Rr][Ee] *: *\)+" subject)
;;       (substring subject (match-end 0))
;;     subject))

;; (defun my-gnus-article-sort-by-subject (h1 h2)
;;   (string-lessp
;;    (downcase (my-gnus-simplify-subject-jeep (mail-header-subject h1)))
;;    (downcase (my-gnus-simplify-subject-jeep (mail-header-subject h2)))))

(defun my-gnus-thread-sort-by-subject (h1 h2)
  (my-gnus-article-sort-by-subject
   (gnus-thread-header h1) (gnus-thread-header h2)))

(add-hook 'message-load-hook 
          '(lambda () 
             (set-face-foreground 'message-header-xheader-face "cyan")
             (set-face-foreground 'message-separator-face "darkcyan")
             (set-face-foreground 'message-header-name-face "yellow")
             (set-face-foreground 'message-header-other-face "red")
             ))
(add-hook 'message-send-hook 'ispell-message)
(global-set-key "\C-xm" 'message-mail)
(setq mime-editor/split-message nil)
;(setq nnmail-movemail-program "/local/xemacs-21.1.8/lib/xemacs-21.1.8/sparc-sun-solaris2.7/movemail")
;(setq exec-directory "/local/xemacs-21.1.8/lib/xemacs-21.1.8/sparc-sun-solaris2.7/")

; (require 'gnus-filterhist)
; (add-hook 'gnus-group-mode-hook 'gnus-filter-history)
; (add-hook 'gnus-after-getting-new-news-hook 'gnus-filter-history)
; (gnus-add-configuration
;  '(group
;    (vertical 1.0
;              (group 1.0 point)
; 		     ("*Filter History*" 0.15)
; 		     )
;    ))

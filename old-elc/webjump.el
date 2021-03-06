;;; webjump.el --- programmable Web hotlist
     
;; Copyright (C) 1996 Free Software Foundation

;; Author:    Neil W. Van Dyke <nwv@acm.org>
;; Created:   Fri 09 Aug 1996
;; Version:   1.4
;; Keywords:  webjump web www browse-url
;; X-URL:     http://www.cs.brown.edu/people/nwv/

;; This file is not yet part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Change Log:

;; [Version 1.4, Tue 17 Sep 1995, nwv] Removed the evil "defconst-TEST" that
;; slipped into 1.3.  Organized webjump-sample-sites and modified the content a
;; bit.

;; [Version 1.3, Fri 30 Aug 1996, nwv] Fixed broken `if' function in
;; `webjump-to-javaapi' (bugfix already posted).  Added `webjump-to-iwin'.
;; Added comment on purpose of `webjump-sample-sites'.  Added
;; `webjump-read-choice'.

;; [Version 1.2, Fri 16 Aug 1996, nwv] Oops, got Gamelan mixed up with Digital
;; Espresso somehow.  Added `mirrors' builtin and used it for the sample GNU
;; Archive site.  Added some other sample sites.  Split sample sites out into
;; separate constant.  Misc. small changes.  Copyright has been transferred to
;; the FSF.

;; [Version 1.1, Sat 10 Aug 1996, nwv] Added missing call to `webjump-url-fix'
;; (thanks to Istvan Marko <mi@bgytf.hu> for pointing this out).  Added
;; ``builtins'' concept in order to support `simple-query' builtin for covering
;; the majority of cases.  Added a couple more sample sites.

;; [Version 1.0, Fri 09 Aug 1996, nwv] Wrote initial version and posted to
;; gnu.emacs.sources.

;;; Commentary:

;; WebJump provides a sort of ``programmable hotlist'' of Web sites that can
;; quickly be invoked in your Web browser.  Each Web site in the hotlist has a
;; name, and you select the desired site name via a completing string prompt in
;; the minibuffer.  The URL for each Web site is defined as a static string or
;; a built-in or custom function, allowing interactive prompting for
;; site-specific queries and options.

;; Note that WebJump was originally intended to complement your conventional
;; browser-based hotlist, not replace it.  (Though there's no reason you
;; couldn't use WebJump for your entire hotlist if you were so inclined.)

;; The `webjump-sites' variable, which defines the hotlist, defaults to some
;; example sites.  You'll probably want to override it with your own favorite
;; sites.  The documentation for the variable describes the syntax.

;; You may wish to add something like the following to your `.emacs' file:
;;
;;   (load "webjump")
;;   (global-set-key "\C-c\C-j" 'webjump)
;;   (setq webjump-sites
;;         (append '(
;;                   ("My Home Page" . "www.someisp.net/users/joebobjr/")
;;                   ("Pop's Site"   . "www.joebob-and-son.com/")
;;                   )
;;                 webjump-sample-sites))
;;
;; The above loads this package, binds `C-c C-j' to invoke WebJump, and adds
;; your personal favorite sites to the hotlist.

;; The `webjump-sample-sites' constant mostly contains sites that are expected
;; to be generally useful to Emacs users or that have some sort of query which
;; can be coded in WebJump.  There are two main goals of this sample site list:
;; (1) demonstrate WebJump capabilities and usage; (2) provide definitions for
;; many popular sites so that people don't have to reinvent the wheel.  A few
;; assorted other sites have been thrown in on a whim.  No commercial sites are
;; included unless they provide a free, generally-useful service.  Inclusion of
;; a site does not represent an endorsement.  Please contact the maintainer
;; with change requests.

;; The `browse-url' package is used to submit URLs to the browser, so any
;; browser-specific configuration should be done there.

;; WebJump inherits a small amount code from my `altavista.el' package, and is
;; intended to obsolete that package.

;;; Code:

;;-------------------------------------------------------- Package Dependencies

(require 'browse-url)

;;------------------------------------------------------ Package Identification

(defconst webjump-version "1.4")
(defconst webjump-author "Neil W. Van Dyke <nwv@acm.org>")
(defconst webjump-maintainer-address "nwv@acm.org")
(defconst webjump-vc-id
  "$Id: webjump.el,v 1.31 1996/09/17 08:11:27 nwv Exp $")

;;------------------------------------------------------------------- Constants

(defconst webjump-sample-sites
  '(

    ;; FSF, not including Emacs-specific.
    ("GNU Project FTP Archive".
     [mirrors "ftp://prep.ai.mit.edu/pub/gnu/"
              ;; ASIA:
              "ftp://ftp.cs.titech.ac.jp"
              "ftp://tron.um.u-tokyo.ac.jp/pub/GNU/prep"
              "ftp://cair-archive.kaist.ac.kr/pub/gnu"
              "ftp://ftp.nectec.or.th/pub/mirrors/gnu"
              ;; AUSTRALIA:
              "ftp://archie.au/gnu"
              "ftp://archie.oz/gnu"
              "ftp://archie.oz.au/gnu"
              ;; AFRICA:
              "ftp://ftp.sun.ac.za/pub/gnu"
              ;; MIDDLE-EAST:
              "ftp://ftp.technion.ac.il/pub/unsupported/gnu"
              ;; EUROPE:
              "ftp://irisa.irisa.fr/pub/gnu"
              "ftp://ftp.univ-lyon1.fr/pub/gnu"
              "ftp://ftp.mcc.ac.uk"
              "ftp://unix.hensa.ac.uk/mirrors/uunet/systems/gnu"
              "ftp://src.doc.ic.ac.uk/gnu"
              "ftp://ftp.ieunet.ie/pub/gnu"
              "ftp://ftp.eunet.ch"
              "ftp://nic.switch.ch/mirror/gnu"
              "ftp://ftp.informatik.rwth-aachen.de/pub/gnu"
              "ftp://ftp.informatik.tu-muenchen.de"
              "ftp://ftp.win.tue.nl/pub/gnu"
              "ftp://ftp.nl.net"
              "ftp://ftp.etsimo.uniovi.es/pub/gnu"
              "ftp://ftp.funet.fi/pub/gnu"
              "ftp://ftp.denet.dk"
              "ftp://ftp.stacken.kth.se"
              "ftp://isy.liu.se"
              "ftp://ftp.luth.se/pub/unix/gnu"
              "ftp://ftp.sunet.se/pub/gnu"
              "ftp://archive.eu.net"
              ;; SOUTH AMERICA: 
              "ftp://ftp.inf.utfsm.cl/pub/gnu"
              "ftp://ftp.unicamp.br/pub/gnu"
              ;; WESTERN CANADA:
              "ftp://ftp.cs.ubc.ca/mirror2/gnu"
              ;; USA:
              "ftp://wuarchive.wustl.edu/systems/gnu"
              "ftp://labrea.stanford.edu"
              "ftp://ftp.digex.net/pub/gnu"
              "ftp://ftp.kpc.com/pub/mirror/gnu"
              "ftp://f.ms.uky.edu/pub3/gnu"
              "ftp://jaguar.utah.edu/gnustuff"
              "ftp://ftp.hawaii.edu/mirrors/gnu"
              "ftp://uiarchive.cso.uiuc.edu/pub/gnu"
              "ftp://ftp.cs.columbia.edu/archives/gnu/prep"
              "ftp://gatekeeper.dec.com/pub/GNU"
              "ftp://ftp.uu.net/systems/gnu"])
    ("GNU Project Home Page" . "www.fsf.org")
					;"www.gnu.ai.mit.edu"
					;"agnes.dida.physik.uni-essen.de/~gnu"

    ;; Emacs.
    ("Eieio" . "ftp.ultranet.com/pub/zappo/")
    ("Emacs Lisp Archive" .
     "ftp://archive.cis.ohio-state.edu/pub/gnu/emacs/elisp-archive/")
    ("Insidious Big Brother Database" . "home.netscape.com/people/jwz/bbdb/")
                                        ;"ftp.xemacs.org/pub/bbdb/"
    ("Mailcrypt" . "cag-www.lcs.mit.edu/mailcrypt/")
    ("XEmacs Home" . "www.xemacs.org")  ; Doesn't hurt to have this here. :)
    ("Yahoo: Emacs" .
     "www.yahoo.com/Computers_and_Internet/Software/Editors/Emacs/")

    ;; General interest.
    ("AltaVista" . 
     [simple-query
      "www.altavista.digital.com"
      "www.altavista.digital.com/cgi-bin/query?pg=aq&what=web&fmt=.&q="
      "&r=&d0=&d1="])
    ("Archie" .
     [simple-query "http://hoohoo.ncsa.uiuc.edu/cgi-bin/AA.pl"
		   "http://hoohoo.ncsa.uiuc.edu/cgi-bin/AA.pl?query=" ""])
    ("Interactive Weather Information Network" . webjump-to-iwin)
    ("Lycos" .
     [simple-query "www.lycos.com" "www.lycos.com/cgi-bin/pursuit?query=" ""])
    ("Usenet FAQs" . 
     [simple-query "www.cis.ohio-state.edu/hypertext/faq/usenet/FAQ-List.html"
		   "www.cis.ohio-state.edu/htbin/search-usenet-faqs/form?find="
		   ""])
    ("RTFM Usenet FAQs by Group" .
     "ftp://rtfm.mit.edu/pub/usenet-by-group/")
    ("RTFM Usenet FAQs by Hierachy" .
     "ftp://rtfm.mit.edu/pub/usenet-by-hierarchy/")
    ("Webster" . 
     [simple-query "c.gp.cs.cmu.edu:5103/prog/webster"
		   "gs213.sp.cs.cmu.edu/prog/webster?" ""])
    ("X Consortium Archive". "ftp.x.org")
    ("Yahoo" . 
     [simple-query "www.yahoo.com" "search.yahoo.com/bin/search?p=" ""])
    ("Yahoo: Reference" "www.yahoo.com/Reference/")

    ;; Computer privacy and social issues.
    ("Computer Professionals for Social Responsibility" . "www.cpsr.org/dox/")
    ("Electronic Frontier Foundation" . "www.eff.org")
    ("Pretty Good Privacy" . "web.mit.edu/network/pgp.html")
    ("Risks Digest" . webjump-to-risks)

    ;; Java.
    ("Digital Espresso" .
     [simple-query "www.io.org/~mentor/DigitalEspresso.html"
		   "www.jars.com/cgi-bin/aglimpse/01?query="
		   "&case=on&whole=on&errors=0&maxfiles=100&maxlines=30"])
    ("Java API" . webjump-to-javaapi)

    ;; Fun.
    ("Bastard Operator from Hell" . "www.replay.com/bofh/")
    ("Dilbert" . "www.unitedmedia.com/comics/dilbert/")
    ("Playboy" . (if (webjump-adult-p) "www.playboy.com" "www.whitehouse.gov"))

    ;; Author's indulgence.
    ("Brown University" .
     [simple-query "www.brown.edu" "www.brown.edu/cgi-local/bsearch?" ""])

    )
  "Sample hotlist for WebJump.")

(defconst webjump-state-to-postal-alist
  '(("Alabama" . "al") ("Alaska" . "ak") ("Arizona" . "az") ("Arkansas" . "ar")
    ("California" . "ca") ("Colorado" . "co") ("Connecticut" . "ct")
    ("Delaware" . "de") ("Florida" . "fl") ("Georgia" . "ga") ("Hawaii" . "hi")
    ("Idaho" . "id") ("Illinois" . "il") ("Indiana" . "in") ("Iowa" . "ia")
    ("Kansas" . "ks") ("Kentucky" . "ky") ("Louisiana" . "la") ("Maine" . "me")
    ("Maryland" . "md") ("Massachusetts" . "ma") ("Michigan" . "mi")
    ("Minnesota" . "mn") ("Mississippi" . "ms") ("Missouri" . "mo")
    ("Montana" . "mt") ("Nebraska" . "ne") ("Nevada" . "nv")
    ("New Hampshire" . "nh") ("New Jersey" . "nj") ("New Mexico" . "nm")
    ("New York" . "ny") ("North Carolina" . "nc") ("North Dakota" . "nd")
    ("Ohio" . "oh") ("Oklahoma" . "ok") ("Oregon" . "or")
    ("Pennsylvania" . "pa") ("Rhode Island" . "ri") ("South Carolina" . "sc")
    ("South Dakota" . "sd") ("Tennessee" . "tn") ("Texas" . "tx")
    ("Utah" . "ut") ("Vermont" . "vt") ("Virginia" . "va")
    ("Washington" . "wa") ("West Virginia" . "wv") ("Wisconsin" . "wi")
    ("Wyoming" . "wy")))

;;------------------------------------------------------------ Option Variables

(defvar webjump-sites
  webjump-sample-sites
  "*Hotlist for WebJump.

The hotlist is represented as an association list, with the CAR of each cell
being the name of the Web site, and the CDR being the definition for the URL of
that site.  The URL definition can be a string (the URL), a vector (specifying
a special \"builtin\" which returns a URL), a symbol (name of a function which
returns a URL), or a list (which when `eval'ed yields a URL).

If the URL definition is a vector, then a \"builtin\" is used.  A builtin has a
Lisp-like syntax, with the name as the first element of the vector, and any
arguments as the following elements.  The three current builtins are `name',
which returns the name of the site as the URL, `simple-query', which
returns a URL that is a function of a query entered by the user, and `mirrors',
which allows the user to select from among multiple mirror sites for the same
content.

The first argument to the `simple-query' builtin is a static URL to use if the
user enters a blank query.  The second and third arguments are the prefix and
suffix, respectively, to add to the encoded query the user enters.  This
builtin covers Web sites that have single-string searches with the query
embedded in the URL.

The arguments to the `mirrors' builtin are URLs of mirror sites.

If the symbol of a function is given, then the function will be called with the
Web site name (the one you specified in the CAR of the alist cell) as a
parameter.  This might come in handy for various kludges.

For convenience, if the `http://', `ftp://', or `file://' prefix is missing
from a URL, WebJump will make a guess at what you wanted and prepend it before
submitting the URL.")

;;------------------------------------------------------- Sample Site Functions

(defun webjump-to-iwin (name)
  (let ((prefix "http://iwin.nws.noaa.gov/")
        (state (webjump-read-choice name "state"
                                    (append '(("Puerto Rico" . "pr"))
                                            webjump-state-to-postal-alist))))
    (if state
        (concat prefix "iwin/" state "/"
                (webjump-read-choice name "option"
                                     '(("Hourly Report" . "hourly")
                                       ("State Forecast" . "state")
                                       ("Local Forecast" . "local")
                                       ("Zone Forecast" . "zone")
                                       ("Short-Term Forecast" . "shortterm")
                                       ("Weather Summary" . "summary")
                                       ("Public Information" . "public")
                                       ("Climatic Data" . "climate")
                                       ("Aviation Products" . "aviation")
                                       ("Hydro Products" . "hydro")
                                       ("Special Weather" . "special")
                                       ("Watches and Warnings" . "warnings"))
                                     "zone")
                ".html")
      prefix)))

(defun webjump-to-javaapi (name)
  (let* ((prefix "http://www.javasoft.com/products/JDK/CurrentRelease/api/")
	 (packages '(("java.applet") ("java.awt") ("java.awt.image")
		     ("java.awt.peer") ("java.io") ("java.lang") ("java.net")
		     ("java.util") ("sun.tools.debug")))
	 (completion-ignore-case t)
	 (package (completing-read (concat name " package: ") packages nil t)))
    (if (webjump-null-or-blank-string-p package)
        (concat prefix "packages.html")
      (concat prefix "Package-" package ".html"))))

(defun webjump-to-risks (name)
  (let (issue volume)
    (if (and (setq volume (webjump-read-number (concat name " volume")))
	     (setq issue  (webjump-read-number (concat name " issue"))))
	(format "catless.ncl.ac.uk/Risks/%d.%02d.html" volume issue)
      "catless.ncl.ac.uk/Risks/")))

;;-------------------------------------------------------------- Core Functions

;;;###autoload
(defun webjump ()
  "Jumps to a Web site from a programmable hotlist.

See the documentation for the `webjump-sites' variable for how to customize the
hotlist.

Feedback on WebJump can be sent to the author, Neil W. Van Dyke <nwv@acm.org>,
or submitted via `\\[webjump-submit-bug-report]'.  The latest version can be
gotten from `http://www.cs.brown.edu/people/nwv/'."
  (interactive)
  (let* ((completion-ignore-case t)
	 (item (assoc (completing-read "WebJump to site: " webjump-sites nil t)
		      webjump-sites))
	 (name (car item))
	 (expr (cdr item)))
    (funcall browse-url-browser-function
	     (webjump-url-fix
	      (cond ((not expr) "")
		    ((stringp expr) expr)
		    ((vectorp expr) (webjump-builtin expr name))
		    ((listp expr) (eval expr))
		    ((symbolp expr)
		     (if (fboundp expr)
			 (funcall expr name)
		       (error "WebJump URL function \"%s\" undefined." expr)))
		    (t (error "WebJump URL expression for \"%s\" invalid."
			      name)))))))

(defun webjump-adult-p ()
  (and (boundp 'age) (integerp age) (>= age 21)))

(defun webjump-builtin (expr name)
  (if (< (length expr) 1)
      (error "WebJump URL builtin for \"%s\" empty." name))
  (let ((builtin (aref expr 0)))
    (cond
     ((eq builtin 'mirrors)
      (if (= (length expr) 1)
          (error
           "WebJump URL builtin \"mirrors\" for \"%s\" needs at least 1 arg."))
      (webjump-choose-mirror name (cdr (append expr nil))))
     ((eq builtin 'name)
      name)
     ((eq builtin 'simple-query)
      (webjump-builtin-check-args expr name 3)
      (webjump-do-simple-query name (aref expr 1) (aref expr 2) (aref expr 3)))
     (t (error "WebJump URL builtin \"%s\" for \"%s\" invalid."
	       builtin name)))))

(defun webjump-builtin-check-args (expr name count)
  (or (= (length expr) (1+ count))
      (error "WebJump URL builtin \"%s\" for \"%s\" needs %d args."
	     (aref expr 0) name count)))

(defun webjump-choose-mirror (name urls)
  (webjump-read-url-choice (concat name " mirror")
                           urls
                           (webjump-mirror-default urls)))

(defun webjump-do-simple-query (name noquery-url query-prefix query-suffix)
  (let ((query (webjump-read-string (concat name " query"))))
    (if query
	(concat query-prefix (webjump-url-encode query) query-suffix)
      noquery-url)))

(defun webjump-mirror-default (urls)
  ;; Note: This should be modified to apply some simple kludges/heuristics to
  ;; pick a site which is likely "close".  As a tie-breaker among candidates
  ;; judged equally desirable, randomness should be used.
  (car urls))

(defun webjump-read-choice (name what choices &optional default)
  (let* ((completion-ignore-case t)
         (choice (completing-read (concat name " " what ": ") choices nil t)))
    (if (webjump-null-or-blank-string-p choice)
        default
      (cdr (assoc choice choices)))))

(defun webjump-read-number (prompt)
  ;; Note: I should make this more robust someday.
  (let ((input (webjump-read-string prompt)))
    (if input (string-to-number input))))
  
(defun webjump-read-string (prompt)
  (let ((input (read-string (concat prompt ": "))))
    (if (webjump-null-or-blank-string-p input) nil input)))
  
(defun webjump-read-url-choice (what urls &optional default)
  ;; Note: Convert this to use `webjump-read-choice' someday.
  (let* ((completions (mapcar (function (lambda (n) (cons n n)))
                              urls))
	 (input (completing-read (concat what
                                         ;;(if default " (RET for default)" "")
                                         ": ")
                                 completions
                                 nil
                                 t)))
    (if (webjump-null-or-blank-string-p input)
        default
      (car (assoc input completions)))))

(defun webjump-null-or-blank-string-p (str)
  (or (null str) (string-match "^[ \t]*$" str)))

(defun webjump-submit-bug-report ()
  "Submit via mail a bug report on WebJump."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   webjump-maintainer-address
   (concat "webjump.el " webjump-version " " webjump-vc-id)
   '(webjump-sites)
   nil
   nil
   (concat
    "[Dear bug report submitter:  Please ensure that the variable dumps\n"
    "below do not contain any information you consider private.]\n")))

(defun webjump-url-encode (str)
  (mapconcat '(lambda (c)
		(cond ((= c 32) "+")
		      ((or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (char-to-string c))
		      (t (upcase (format "%%%02x" c)))))
	     str
	     ""))

(defun webjump-url-fix (url)
  (if (webjump-null-or-blank-string-p url)
      ""
    (webjump-url-fix-trailing-slash
     (cond
      ((string-match "^[a-zA-Z]+:" url) url)
      ((string-match "^/" url) (concat "file://" url))
      ((string-match "^\\([^\\./]+\\)" url)
       (concat (if (string= (downcase (match-string 1 url)) "ftp")
		   "ftp"
		 "http")
	       "://"
	       url))
      (t url)))))

(defun webjump-url-fix-trailing-slash (url)
  (if (string-match "^[a-zA-Z]+://[^/]+$" url)
      (concat url "/")
    url))

;;-----------------------------------------------------------------------------

(provide 'webjump)

;; webjump.el ends here

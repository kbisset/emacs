;; stock-quote -- Grab a stock quote (delayed) from the Net
;; Copyright (C) 1996 John Wiegley

;; Author: John Wiegley <johnw@borland.com>
;; Keywords: stock,financial
;; $Revision:   1.0  $
;; $Date:   $
;; $Source: $

;; LCD Archive Entry:
;; stock-quote|John Wiegley|johnw@borland.com|
;; Grab a stock quote (delayed) from the Net|
;; $Date:   $|$Revision:   1.0  $|
;; |

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; This function gets stock data from the Money Quick Quotes server.

(require 'w3)

(defun get-stock-quote (ticker)
  "Get the stock price of a ticker symbol."
  (w3-fetch
   (concat "http://quote.pathfinder.com/money/quote/qc?symbols=" ticker))
  (goto-char (point-min))
  (let (price)
    (if (re-search-forward "Bid.*\\([0-9][0-9][0-9][0-9]\\)")
        (setq price (match-string 1)))
    (bury-buffer)
    price))

(defun stock-quote (ticker)
  (interactive "sTicker: ")
  (setq ticker (upcase ticker))
  (message (concat ticker ": " (get-stock-quote ticker))))

;;; end of e-lisp
(provide 'stock-quote)

(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))
  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist))) alist))

(defun my-colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

(setq default-mode-line-format
      (append nil '("-%1*-<%b>--" global-mode-string "--" (-3 . "%p") "--#%l-" 
                    "-%[(" mode-name minor-mode-alist "%n" mode-line-process 
                    ")%] " user-login-name "@" system-name " -%-")))

(setq pp-cc-imenu-c++-generic-expression
      (` ((nil (, (concat "^"	"\\(template[ \t]*<[^>]+>[ \t]*\\)?" 
       "\\([a-zA-Z0-9_:]+[ \t]+\\)?" "\\([a-zA-Z0-9_:]+[ \t]+\\)?"	
       "\\(" "[a-zA-Z0-9_:<>]+" "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" 
       "\\)?" "\\(" "\\([a-zA-Z0-9_:<>~]*\\)?operator[ \t]*" "\\("
       "[a-zA-Z0-9_:<>+-*/=!,&|^]+"	"[^a-zA-Z1-9_][^(]*"		
       "\\|" "\(\)"	"\\)" "\\|" "[a-zA-Z0-9_:<>~]+"	"\\)"
       "[ \t]*([^)]*)[ \t\n]*[^		;]")) 6)
          ("Class" (, (concat "^"	"\\(template[ \t]*<[^>]+>[ \t]*\\)?" 
                              "class[ \t]+" "\\([a-zA-Z0-9_]+\\)"		
                              "[ \t]*[:{]")) 2))))
(make-face 'krb-ellipses-face)
(set-face-foreground 'krb-ellipses-face "yellow")
(set-face-underline-p 'krb-ellipses-face t)
(set-display-table-slot standard-display-table 4 
  (make-vector 3 (+ (lsh (face-id 'krb-ellipses-face) 19) ?.)))
(set-display-table-slot standard-display-table 1
  (make-vector 3 (+ (lsh (face-id 'krb-ellipses-face) 19) ?.)))

(add-hook 'display-time-hook 'from-update-buffer)

(setq krb-modeline-color
      (cond 
       ((string= system-name "MacBook-Pro.local") '("#93a1a1" . "Purple"))
       ((string= system-name "vagrant-ubuntu-trusty-64") '("#494d54" . "#66C26C"))
       ((string= system-name "stormtrooper") '("#4B4D54" . "#90C3D4"))
       ((string= system-name "cloud") '("#4B4D54" . "#fc8d62"))
       ((string-match "deep[0-9]" system-name) '("#4B4D54" . "#DEDE16"))
       (t krb-default-modeline-color)))

(set-face-foreground 'mode-line (car krb-modeline-color))
(set-face-background 'mode-line (cdr krb-modeline-color))
(set-face-foreground 'fringe (cdr krb-modeline-color))
(set-face-background 'fringe (car krb-modeline-color))

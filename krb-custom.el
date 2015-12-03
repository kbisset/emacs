
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-mode-hook
   (quote
    ((lambda nil
       (setq hier-imenu-levels
             (quote
              (("\\part" . 0)
               ("\\part*" . 0)
               ("\\chapter" . 1)
               ("\\chapter*" . 1)
               ("\\section" . 2)
               ("\\section*" . 2)
               ("\\subsection" . 3)
               ("\\subsection*" . 3)
               ("\\subsubsection" . 4)
               ("\\subsubsection*" . 4)
               ("\\paragraph" . 5)
               ("\\subparagraph" . 6)))
             hier-imenu-header-name-function
             (quote ydi-latex-header-name)
             hier-imenu-anchor-end-of-header t hier-imenu-style
             (quote numbered)
             imenu-create-index-function
             (quote imenu-default-create-index-function)
             imenu-generic-expression
             (quote
              (("Labels" "\\\\label{\\([^}]+\\)}" 1)
               (imenu-create-hierarchical-index)))))
     (lambda nil
       (flyspell-mode)))))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "open")
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t)
 '(gnus-check-new-newsgroups (quote ask-server) t)
 '(gnus-nntp-server nil t)
 '(gnus-read-active-file (quote some) t)
 '(gnus-secondary-select-methods (quote ((nnml "private"))))
 '(gnus-select-method (quote (nntp "localhost")))
 '(ispell-program-name "/opt/local/bin/aspell")
 '(mode-line-inverse-video t t)
 '(muse-project-alist (quote (("WikiPlanner" ("~/plans" "index")))))
 '(ns-antialias-text nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa.stable" . "http://melpa.org/packages/"))))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(ps-printer-name "/Users/kbisset/tmp/emacs.ps")
 '(safe-local-variable-values
   (quote
    ((TeX-master . proposal\.tex)
     (visual-line-mode . t)
     (TeX-master . draft\.tex))))
 '(save-place t nil (saveplace))
 '(save-place-file "/Users/kbisset/.emacs.d/emacs-places")
 '(save-place-save-skipped nil)
 '(save-place-skip-check-regexp
   "\\`/\\(?:cdrom\\|floppy\\|mnt\\|Volumes\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)")
 '(savehist-additional-variables
   (quote
    (file-name-history command-history extended-command-history frame-name-history query-replace-history read-expression-history set-variable-value-history shell-command-history yes-or-no-p-history face-name-history grep-history)))
 '(savehist-mode t)
 '(sudoku-download t)
 '(sudoku-download-method "wget"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(flymake-errline ((((class color)) (:background "LightPink" :foreground "black"))))
 '(flymake-warnline ((((class color)) (:background "LightBlue2" :foreground "black"))))
 '(mode-line ((t (:background "#073642" :foreground "#93a1a1"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:background "red" :foreground "black"))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "red" :weight light))))
 '(paren-face-match-light ((t (:slant italic))))
 '(region ((t (:background "selectedTextBackgroundColor" :foreground "selectedTextColor"))))
 '(smerge-refined-change ((t (:background "dark green")))))

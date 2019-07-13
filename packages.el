(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))

;;; set up package syncing to allow for syncing between different machines

;; list of packages to sync
(setq pfl-packages
        '(
            company
            company-auctex
            company-c-headers
            company-irony
            company-quickhelp
	    powerline
	    neotree
            magit
            ))
; activate all the packages
(package-initialize)
;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

;; install packages from the list that are not yet installed
(dolist (pkg pfl-packages)
    (when (and (not (package-installed-p pkg)) (assoc pkg package-archive-contents))
        (package-install pkg)))

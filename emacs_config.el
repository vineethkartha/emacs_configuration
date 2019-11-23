(custom-set-variables
'(inhibit-startup-screen t))

(add-to-list 'package-archives
	     '("melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))

(unless package-archive-contents
  (package-refresh-contents))

(package-initialize)

;; if not yet installed, install package use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq pfl-packages
        '(
            company
            company-c-headers
            company-irony
            company-quickhelp
	    company-rtags
	    flycheck
	    flycheck-rtags
	    powerline
	    org-bullets
	    rtags
	    srefactor
	    origami
	    ace-window
            ))

	    (dolist (pkg pfl-packages)
	    (when (and (not (package-installed-p pkg)) (assoc pkg package-archive-contents))
	    (package-install pkg)))

(global-linum-mode 1)
(global-flycheck-mode)
(powerline-default-theme)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-ellipsis " â¤µ")
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t) (shell . t)))

(defun setup-flycheck-rtags ()
(interactive)
(flycheck-select-checker 'rtags)
;; RTags creates more accurate overlays.
(setq-local flycheck-highlighting-mode nil)
(setq-local flycheck-check-syntax-automatically nil))

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
;; make sure you have company-mode installed
(require 'company)
(define-key c-mode-base-map (kbd "M-.")
(function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,")
(function rtags-find-references-at-point))
;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
;;(define-key prelude-mode-map (kbd "C-c r") nil)
;; install standard rtags keybindings. Do M-. on the symbol below to
;; jump to definition and see the keybindings.
(rtags-enable-standard-keybindings)
;; comment this out if you don't have or don't use helm
;;(setq rtags-use-helm t)
;; company completion setup
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;; use rtags flycheck mode -- clang warnings shown inline
(require 'flycheck-rtags)
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'setup-flycheck-rtags))

(semantic-mode 1) ;; -> this is optional for Lisp

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

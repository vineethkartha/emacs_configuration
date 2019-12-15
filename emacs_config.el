(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))

(package-initialize)
(when (not package-archive-contents)  
  (package-refresh-contents))

(defvar my-packages
  '(
    use-package
    elpy
    flycheck
    ace-window
    org-bullets
    powerline
    which-key
    dashboard
    )
  )

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(defun enable-flycheck()
  (flycheck-mode 1)
  )

(defun set-local-key-for-hs-mode()
  (hs-minor-mode 1)
  (local-set-key (kbd "M-;") 'hs-toggle-hiding)
  )

(defun copy-line-above ()
  (interactive)
  (previous-line)
  (setq line (buffer-substring-no-properties
	      (line-beginning-position)
	      (line-end-position))
	)
  (next-line)
  (insert line)
  )

(global-linum-mode 1)
(ido-mode 1)
(electric-pair-mode 1)
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq global-company-mode 1))

(use-package company-irony
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-irony)
  )

;sudo apt-get install libclang-9-dev
;M-x irony-install-server
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  )
  (add-hook 'c++-mode-hook (
		   lambda()
			 (enable-flycheck)
			 (set-local-key-for-hs-mode)
			 ))

(elpy-enable)
(setq elpy-rpc-virtualenv-path 'current)

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-ellipsis "->")
(org-babel-do-load-languages
 'org-babel-load-languages '(
			     (C . t)
			     (shell . t)
			     (emacs-lisp . t)
			     (plantuml . t)
			     )
 )

(setq org-todo-keywords 
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("IN-PROGRESS" . "yellow")
	("WAITING" . "blue") ("DONE" . "green") ("CANCELED" . "orange")))

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

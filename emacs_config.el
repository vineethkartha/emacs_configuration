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
    company
    company-irony
    dashboard
    use-package
    flycheck
    ace-window
    org-bullets
    powerline
    which-key
    linum-relative
    plantuml-mode
    sos
    )
  )
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (message "Installing %s ..." pkg)
    (condition-case nil
        (package-install pkg)
      (error (warn "Failed to install %s ..." pkg)))
    ))

(defun enable-flycheck()
  (flycheck-mode 1)
  )

(defun set-local-key-for-hs-mode()
  (hs-minor-mode 1)                     
  (local-set-key (kbd "M-;") 'hs-toggle-hiding)
  )

(global-linum-mode 1)
(global-visual-line-mode 1)
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

;(elpy-enable)
;(setq elpy-rpc-virtualenv-path 'current)

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)

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
(setq org-hide-emphasis-markers t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))))
(setq org-image-actual-width nil)

(setq org-plantuml-jar-path "~/Tools/plantuml.jar")

(setq org-todo-keywords 
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("IN-PROGRESS" . "yellow")
        ("WAITING" . "blue") ("DONE" . "green") ("CANCELED" . "orange")))
(global-set-key (kbd "C-c 2") (lambda() (interactive)(find-file "~/orgmode/todo.org")))

(global-set-key (kbd "C-c 1") 'add-todo-date)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(load-file "~/emacs_configuration/helper-scripts.el")
(global-set-key (kbd "C-c d") 'delete-word)
(global-set-key (kbd "C-c r") 'toggle-rel-linum)

(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(setq plantuml-jar-path "~/Tools/plantuml.jar")
(setq plantuml-output-type "png")
(global-set-key (kbd "C-c s") 'plantuml-save-to-file)
(global-set-key [f7] 'delete-org-plantuml-file)

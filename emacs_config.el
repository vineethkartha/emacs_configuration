(require 'package)
    (package-initialize)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/"))
    (add-to-list 'package-archives
                 '("org" . "http://orgmode.org/elpa/"))

    (package-initialize)
    (when (not package-archive-contents)  
      (package-refresh-contents))

    (defvar my-packages
      '(
        company
        company-irony
;        dashboard
        use-package
        flycheck
        ace-window
        org-bullets
        powerline
        which-key
        linum-relative
        plantuml-mode
        sos
	htmlize
	solarized-theme
	dracula-theme
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

(defun check-spacemacs-theme()
  (if (file-exists-p "~/.emacs.d/themes/spacemacs-common.el")
      (progn
        (load-file "~/.emacs.d/themes/spacemacs-common.el")
        t
        )
    (progn 
      (message "Spacemacs theme not present")
      nil
      )
    )
  )

(defun set-my-theme()
  (if (check-spacemacs-theme)
      (load-theme 'spacemacs-dark)
    (progn
      (if (featurep 'solarized-dark-theme)
          (load-theme 'solarized-dark)
        (message "No Interesting theme so sad")
        )
      )
    )
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

;     (elpy-enable)
;     (setq elpy-rpc-virtualenv-path 'current)

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
(global-set-key "\C-cc" 'org-capture)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 2 days
(setq org-deadline-warning-days 2)
;;show me tasks scheduled or due in next 7 day
(setq org-agenda-span 7)
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
(setq org-agenda-custom-commands
      `(("W" "Weekly Status" 
         tags (concat "+TODO=\"DONE\""
                      "+CLOSED>=\"<-7d>\""
                      "+CLOSED<\"<today>\""))))
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 1)))

(setq org-plantuml-jar-path "~/Tools/plantuml.jar")

(setq org-todo-keywords 
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("IN-PROGRESS" . "yellow")
        ("WAITING" . "blue") ("DONE" . "green") ("CANCELED" . "orange")))
(global-set-key (kbd "C-c 2") (lambda() (interactive)(find-file "~/orgmode/todo.org")))
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/orgmode/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/orgmode/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))
(global-set-key (kbd "C-c 1") 'add-todo-date)
(setq org-log-done 'time)
(setq org-log-done 'note)

(setq electric-pair-pairs
      '(
        (?~ . ?~)
        (?* . ?*)
        (?/ . ?/)
        ))

;    (use-package dashboard
;      :ensure t
;      :config
;      (dashboard-setup-startup-hook))

(load-file "~/emacs_configuration/helper-scripts.el")
(global-set-key (kbd "C-c d") 'delete-word)
(global-set-key (kbd "C-c r") 'toggle-rel-linum)
(global-set-key (kbd "C-c j") 'copy-line-above)

(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(setq plantuml-jar-path "~/Tools/plantuml.jar")
(setq plantuml-output-type "png")
(global-set-key (kbd "C-c s") 'plantuml-save-to-file)
(global-set-key [f7] 'delete-org-plantuml-file)

;; New location for backups.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; Never silently delete old backups.
(setq delete-old-versions -1)
;; Use version numbers for backup files.
(setq version-control t)
;; Even version controlled files get to be backed up.
(setq vc-make-backup-files t)

(set-my-theme)
(global-linum-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode t)
(ido-mode 1)
(electric-pair-mode 1)
(setq visible-bell 1)
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )
(set-face-background hl-line-face "ivory")

(setq inhibit-startup-message t)
(add-hook 'after-init-hook '(lambda () (org-agenda-list 1)))
(switch-to-buffer "*Org Agenda*")

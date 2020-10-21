(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

	     ;(add-to-list 'package-archives
             ;'("melpa" . "http://melpa.milkbox.net/packages/"))

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
    popup-kill-ring
    htmlize
    solarized-theme
    darcula-theme
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

(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

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

(with-eval-after-load 'company
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  )

(add-hook 'c++-mode-hook (
                          lambda()
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

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)
(push '("[ ]" . "☐") prettify-symbols-alist)
(push '("[X]" . "☑" ) prettify-symbols-alist)
(push '("[-]" . "❍" ) prettify-symbols-alist)
(push '("#+begin_src" . "↦" ) prettify-symbols-alist)
(push '("#+end_src" . "⇤" ) prettify-symbols-alist)
(push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
(push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
(prettify-symbols-mode)))
(setq org-ellipsis "->")
(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (C . t)
                             (shell . t)
                             (emacs-lisp . t)
                             (plantuml . t)
                             )
 )
(setq org-src-fontify-natively t)
(setq org-hide-emphasis-markers t)

(setq org-plantuml-jar-path "~/Tools/plantuml.jar")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
;(setq org-archive-location "~/orgmode/gtd/archive/2020.org")
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
      `(("Q" . "Custom Queries")
      ("Qt" "Team Status" 	
         tags (concat "+TODO=\"DONE\""
                      "+CLOSED>=\"<-7d>\""
                      "+CLOSED<\"<today>\"")
         (org-agenda-sorting-strategy tag-up)
         )
        ("Qm" "Monthly Status" 
         tags (concat "+TODO=\"DONE\""
                      "+CLOSED>=\"<-30d>\""
                      "+CLOSED<\"<today>\"")
         (org-agenda-sorting-strategy tag-up)
         )
        ("Qy" "Yearly Status" 
         tags (concat "+TODO=\"DONE\""
                      "+CLOSED>=\"<enter start date>\""
                      "+CLOSED<\"<enter end date>\"")
         (org-agenda-sorting-strategy tag-up)
         )
        ("Qw" "Weekly review"
         agenda ""
         ((org-agenda-span 'week)
          (org-agenda-start-on-weekday 2)
          (org-agenda-start-with-log-mode t)
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'notdeadline 'todo '("TODO" "LIVE" "STALL")))
          ))))
          (setq org-refile-targets
          '((org-agenda-files :maxlevel . 1)))

(setq org-todo-keywords 
      '((sequence "TODO" "LIVE" "STALL" "|" "DONE" "KILL")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("LIVE" . "yellow")
        ("STALL" . "blue") ("DONE" . "green") ("KILL" . "orange")))
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/orgmode/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/orgmode/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))
(setq org-log-done 'time)
(setq org-log-done 'note)

;(setq electric-pair-pairs
;      '(
;        (?~ . ?~)
;        (?* . ?*)
;        (?/ . ?/)
;        ))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

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

;     (setq inhibit-startup-message t)
;     (add-hook 'after-init-hook '(lambda () (org-agenda-list 1)))
;     (switch-to-buffer "*Org Agenda*")

;(set-my-theme)
;; Set the frame width and height at startup
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 130)) 

(global-linum-mode 1) ;; turn on line numbers
(global-visual-line-mode 1) 
(global-hl-line-mode t) ;; to highlight current line
(ido-mode 1)
(electric-pair-mode 1) ;; mode to set mathching braces etc.
;; Enabling whitespace mode to detect crossing of 100 columns
(setq-default
 whitespace-line-column 100
 whitespace-style  '(face lines-tail)
 )
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-whitespace-mode)
(setq visible-bell 1)
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )
(set-face-background hl-line-face "gray13")
(global-set-key (kbd "C-c g") 'find-file-at-point)
(use-package popup-kill-ring
  :ensure t
  :config
  (global-set-key (kbd "C-y") 'popup-kill-ring))

(load-file "~/emacs_configuration/helper-scripts.el")
(global-set-key (kbd "C-c d") 'delete-word)
(global-set-key (kbd "C-c w") 'copy-word)
(global-set-key (kbd "C-c r") 'toggle-rel-linum)
(global-set-key (kbd "C-c j") 'copy-line-above)

(require 'desktop)
(setq session-save-path default-directory)
(defvar my-desktop-session-dir
  (concat (getenv "HOME") "/.emacs.d/desktop-sessions/")
  "*Directory to save desktop sessions in")

(defvar my-desktop-session-name-hist nil
  "Desktop session name history")

(defun my-desktop-save (&optional name)
  "Save desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Save session" t)))
  (when name
    (make-directory (concat my-desktop-session-dir name) t)
    (desktop-save (concat my-desktop-session-dir name) t)))

(defun my-desktop-save-and-clear ()
  "Save and clear desktop."
  (interactive)
  (call-interactively 'my-desktop-save)
  (desktop-clear)
  (setq desktop-dirname nil))

(defun my-desktop-read (&optional name)
  "Read desktop by name."
  (interactive)
  (unless name
    (setq name (my-desktop-get-session-name "Load session")))
  (when name
    (desktop-clear)
    (desktop-read (concat my-desktop-session-dir name))))

(defun my-desktop-change (&optional name)
  "Change desktops by name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (when name
      (my-desktop-save name))
    (call-interactively 'my-desktop-read)))

(defun my-desktop-name ()
  "Return the current desktop name."
  (interactive)
  (let ((name (my-desktop-get-current-name)))
    (if name
        (message (concat "Desktop name: " name))
      (message "No named desktop loaded"))))

(defun my-desktop-get-current-name ()
  "Get the current desktop name."
  (when desktop-dirname
    (let ((dirname (substring desktop-dirname 0 -1)))
      (when (string= (file-name-directory dirname) my-desktop-session-dir)
        (file-name-nondirectory dirname)))))

(defun my-desktop-get-session-name (prompt &optional use-default)
  "Get a session name."
  (let* ((default (and use-default (my-desktop-get-current-name)))
         (full-prompt (concat prompt (if default
                                         (concat " (default " default "): ")
                                       ": "))))
    (completing-read full-prompt (and (file-exists-p my-desktop-session-dir)
                                      (directory-files my-desktop-session-dir))
                     nil nil nil my-desktop-session-name-hist default)))

(defun my-desktop-kill-emacs-hook ()
  "Save desktop before killing emacs."
  (when (file-exists-p (concat my-desktop-session-dir "last-session"))
    (setq desktop-file-modtime
          (nth 5 (file-attributes (desktop-full-file-name (concat my-desktop-session-dir "last-session"))))))
  (my-desktop-save "last-session"))

(add-hook 'kill-emacs-hook 'my-desktop-kill-emacs-hook)

;;;; loads the different libraries to set up Emacs accordingly

;; load the library that sets up package repositories and syncing
(load-library "packages")

;; This contains templates for C++ file and comments
(load-library "templates")

;; Turn on the line numbers
(global-linum-mode 1)

;; KeyBindings
(global-set-key (kbd "<f8>") 'neotree-toggle) 
(powerline-default-theme)

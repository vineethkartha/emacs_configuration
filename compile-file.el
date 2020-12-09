(defun compile-file ()
  (interactive)
  (setq file (file-name-nondirectory (buffer-file-name)))
  (compile-file-helper file)
  )

(defun compile-file-helper (file)
  (setq compiler "g++")
  (setq flags "-std=c++17")
  (setq extension "out")
    (message (format "%s %s %s -o %s%s"
                   compiler
                   flags
                   file
                   (shell-quote-argument (file-name-sans-extension file))
                   (if extension
                       (format ".%s" extension)
                     "")))
  (compile (format "g++ -std=c++17 %s -o %s%s"
                   file
                   (shell-quote-argument (file-name-sans-extension file))
                   (if extension
                       (format ".%s" extension)
                     "")))
  t
  )

(defun run-file()
  (interactive)
  (setq file (file-name-nondirectory (buffer-file-name)))
  (setq executable (concat (shell-quote-argument (file-name-sans-extension file)) ".out"))
  ;(delete-file executable)
  (term (format "./%s" executable))
  )

(defun add-cpp-template()
  "Generate a simple cpp file to get started"
  (interactive )
  (setq text (format "#include<iostream>\n#include<string>\n\nint main() {\n\n\treturn 0;\n}"))
  (insert text)
  )

(defun add-abstract(funcName)
  "Comment block to add abstract for a function"
  (interactive "sEnter the Function Name: ")
  (setq text (format "// Function: %s =====================\n" funcName) )
  (setq text (concat text "// Abstract:\n/**\n * Lorem ipsum dolor sit amet, consectetur adipiscing elit,\n * sed do eiusmod tempor incididunt ut labore et dolore magna aliquam\n */"))  
  (insert text)
  )

(defun add-todo-date ()
  "To add date to a todo list in org"
  (interactive)
  (if (string-equal (format-time-string "%a") "Tue")
      (insert (format-time-string "*** _%b-%d-%Y_"))
    (insert (format-time-string "*** %b-%d-%Y"))
    )
  )

(defun kill-thingatpoint (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      )
    )
  )

(defun delete-org-plantuml-file ()
  "Delete the plant uml generated file."
  (interactive)
  (setq thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%,")
  (message "%s" (thing-at-point 'filename))
  (setq fname (thing-at-point 'filename))
  (delete-file fname)
  (kill-whole-line)
  )

(defun copy-line-above ()
  "Copy the line from above the current cursor position"
  (interactive)
  (save-excursion
    (previous-line)
    (setq line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))
          )
    (next-line)
    (insert line)
    )
  )

(defun delete-word ()
  "Delete the word under the cursor. This reduces the strokes M-b , M-d"
  (interactive)
  (kill-thingatpoint 'symbol)
  )

(defun copy-word ()
  "Copy the word under the cursor. This reduces the strokes M-b , C-spc , M-f, M-w"
  (interactive)
  (let (bounds)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (save-excursion
      (copy-region-as-kill (car bounds) (cdr bounds)))
    )
  )

(defun toggle-rel-linum()
  "Toggle the relative line numbering"
  (interactive)
  (if (bound-and-true-p linum-mode)
      (progn
        (linum-relative-toggle)
        (if (bound-and-true-p linum-mode)
            nil
          (linum-mode))
        )
    (progn
      (linum-mode))
    )
  )

(defun copy-full-file-path()
  "Copy the full path to a file in opened in the current buffer"
  (interactive)
  (setq file-name (buffer-file-name))
  (kill-new file-name)
  )


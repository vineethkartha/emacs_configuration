(defun add-cpp-template()
  (interactive )
  (setq text (format "#include<iostream>\n#include<string>\n\nint main() {\n\n\treturn 0;\n}"))
  (add-text-helper text)
  )

(defun add-abstract(funcName)
  (interactive "sEnter the Function Name: ")
  (setq text (format "// Function: %s =====================\n" funcName) )
  (setq text (concat text "// Abstract:\n/**\n * Lorem ipsum dolor sit amet, consectetur adipiscing elit,\n * sed do eiusmod tempor incididunt ut labore et dolore magna aliquam\n */"))  
  (insert text)
  )

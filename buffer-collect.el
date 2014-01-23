(defvar bc:type-list
  '(".el" ".h" ".c" ".cpp")
  "List of extensions or name pattens to collect buffers.")

(defun bc:type-list-to-string (&optional to-regexp)
  (if to-regexp
      (let ((ex-regexp "^\\.\\([a-zA-Z0-9]+\\)$"))
        (mapconcat (lambda (ex)
                     (if (string-match ex-regexp ex)
                         (replace-regexp-in-string ex-regexp "\\\\.\\1\\\\($\\\\|<.*>\\\\)" ex)
                       (regexp-opt (cons ex nil))))
                   bc:type-list
                   "\\|"))
    (mapconcat 'identity bc:type-list " ")))

(defun bc:type-list-str-prompt ()
  (let ((default-str (bc:type-list-to-string)))
    (read-string "Buffer type: " default-str nil default-str)))

(defun bc:set-type-list (type-str)
  "TYPE-STR is a string of extensions or name patterns
separated white space. (ex. \".el .h .c foo bar\")
TYPE-STR is needed non empty string.
If a pattern contains white space, escape it with backslash."
  (if (or (not type-str) (string-equal type-str ""))
      (error "Cannot set empty.")
    (setq bc:type-list (split-string type-str))))

(defun bc:collect-buffers (&optional type-str interactively)
  "Parameter TYPE-STR is same as `bc:set-type-list'."
  (bc:set-type-list (if interactively (bc:type-list-str-prompt) type-str))
  (let ((p (bc:type-list-to-string t)))
    (reduce (lambda (acc buf)
              (if (string-match p (buffer-name buf))
                  (cons buf acc)
                acc))
            (buffer-list)
            :initial-value nil)))

(provide 'buffer-collect)

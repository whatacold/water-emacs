;;;; my programming utilities

;; http://xahlee.info/emacs/emacs/elisp_read_file_content.html
(defun w/read-lines (file-path)
  "Return a list of lines of FILE-PATH."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file-path))
    (split-string (buffer-string) "\n" t)))

;;;; my programming utilities

;; http://xahlee.info/emacs/emacs/elisp_read_file_content.html
(defun w/read-lines (file-path)
  "Return a list of lines of FILE-PATH."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file-path))
    (split-string (buffer-string) "\n" t)))

;; https://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun w/print-to-file (filename data)
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun w/read-from-file (filename)
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (cl-assert (eq (point) (point-min)))
      (read (current-buffer)))))

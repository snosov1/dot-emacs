(defun raw-toc ()
  (let ((content (buffer-substring-no-properties
                  (point-min) (point-max))))
    (with-temp-buffer
      (insert content)
      (beginning-of-buffer)
      (keep-lines "^\*")
      (buffer-substring-no-properties
       (point-min) (point-max)))))

(defun hrefify-github (str)
  (let* ((spc-fix (replace-regexp-in-string " " "-" str))
         (slash-fix (replace-regexp-in-string "/" "" spc-fix)))
    slash-fix))

(defun hrefify-toc (toc hrefify)
  (with-temp-buffer
    (insert toc)
    (beginning-of-buffer)
    (skip-chars-forward "* ")

    ;; (let ((moreLines t))
    ;;   (while (= 0 (forward-line 1))
    ;;
    ;;     (setq moreLines (= 0 (forward-line 1)))
    ;;     ))

    (let* ((beg (point))
           (end (line-end-position))
           (heading (buffer-substring-no-properties
                     beg end)))
      (insert "[[")
      (insert (funcall hrefify heading))
      (insert "][")
      (end-of-line)
      (insert "]]"))
    (buffer-substring-no-properties
     (point-min) (point-max))
    )
  )

(defun insert-raw-toc ()
  (save-excursion
    (beginning-of-buffer)
    (insert (raw-toc))
    (newline-and-indent)
    )
  )

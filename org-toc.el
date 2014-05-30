(defvar toc-regexp "^*.*:toc:\\($\\|[^ ]*:$\\)")

(defun raw-toc ()
  (let ((content (buffer-substring-no-properties
                  (point-min) (point-max))))
    (with-temp-buffer
      (insert content)
      (beginning-of-buffer)
      (keep-lines "^\*")

      (beginning-of-buffer)
      (re-search-forward toc-regexp)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point)))

      (buffer-substring-no-properties
       (point-min) (point-max)))))

(defun hrefify-github (str)
  (let* ((spc-fix (replace-regexp-in-string " " "-" str))
         (slash-fix (replace-regexp-in-string "/" "" spc-fix)))
    (concat "#" slash-fix)))

(defun hrefify-toc (toc hrefify)
  (with-temp-buffer
    (insert toc)
    (beginning-of-buffer)

    (while
        (progn
          (when (looking-at "\\*")
            (insert "*")
            (skip-chars-forward "* ")
            (let* ((beg (point))
                   (end (line-end-position))
                   (heading (buffer-substring-no-properties
                             beg end)))
              (insert "[[")
              (insert (funcall hrefify heading))
              (insert "][")
              (end-of-line)
              (insert "]]"))
            (= 0 (forward-line 1)))))

    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun insert-toc ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((case-fold-search t))
      ;; find the first heading with the :TOC: tag
      (re-search-forward toc-regexp)
      (forward-line 1)

      ;; remove previous TOC
      (while (looking-at "\\*\\*")
        (delete-region (point) (progn (forward-line 1) (point))))

      (insert (hrefify-toc (raw-toc) 'hrefify-github))
      )))

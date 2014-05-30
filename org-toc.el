(defvar toc-regexp "^*.*:toc:\\($\\|[^ ]*:$\\)")
(defvar special-chars-regexp "[][~`!@#$%^&*()+={}|\:;\"'<,>.?/]")
(defvar max-depth 2)

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
         (upcase-fix (replace-regexp-in-string "[A-Z]" 'downcase spc-fix t))
         (special-chars-fix (replace-regexp-in-string special-chars-regexp "" upcase-fix t))
         )
    (concat "#" special-chars-fix)))

(defun hrefify-toc (toc hrefify)
  (with-temp-buffer
    (insert toc)
    (beginning-of-buffer)

    (while
        (progn
          (when (looking-at "\\*")
            (delete-char 1)
            (replace-string "*" "    " nil
                            (line-beginning-position)
                            (or (save-excursion
                                  (search-forward " " (line-end-position) t))
                                (line-end-position)))
            (skip-chars-forward " ")
            (insert "- ")

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

(defun flush-subheadings (toc max-stars)
  (with-temp-buffer
    (insert toc)
    (beginning-of-buffer)

    (let ((re "^"))
      (dotimes (i (1+ max-stars))
        (setq re (concat re "\\*")))
      (flush-lines re))

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
      (delete-region (point)
                     (save-excursion
                       (search-forward-regexp "^\\*")
                       (forward-line -1)
                       (end-of-line)
                       (point)))

      (insert (hrefify-toc (flush-subheadings (raw-toc) max-depth) 'hrefify-github))
      )))

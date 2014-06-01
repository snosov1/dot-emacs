;;; org-toc.el --- add table of contents to org-mode files

;; Copyright (C) 2014 Sergei Nosov

;; Author: Sergei Nosov <sergei.nosov [at] gmail.com>
;; Version: 1.0
;; Keywords: org-mode org toc table of contents
;; URL: https://github.com/snosov1/org-toc

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; org-toc is a utility to have an up-to-date table of contents in the
;; org files without exporting (primarily for readme files on GitHub).

;; To enable this functionality put into your .emacs file something
;; like

;; (add-hook 'before-save-hook 'ot-insert-toc)

;; After that, every time you'll be saving an org file the first
;; heading with a :TOC: tag will be updated with the current table of
;; contents.


;;; Code:

;; just in case, simple regexp "^*.*:toc:\\($\\|[^ ]*:$\\)"
(defconst ot-toc-regexp "^*.*:toc\\(@[0-9]\\|\\(@[0-9]@[a-zA-Z]+\\)\\)?:\\($\\|[^ ]*:$\\)"
  "Regexp to find the heading with the :toc: tag")
(defconst ot-special-chars-regexp "[][~`!@#$%^&*()+={}|\:;\"'<,>.?/]"
  "Regexp with the special characters (which are omitted in hrefs
  by GitHub)")

(defcustom ot-max-depth 2
  "Maximum depth of the headings to use in the table of
contents. The default of 2 uses only the highest level headings
and their subheadings (one and two stars)."
  :group 'org-toc)

(defun ot-raw-toc ()
  "Return the \"raw\" table of contents of the current file,
i.e. simply flush everything that's not a heading."
  (let ((content (buffer-substring-no-properties
                  (point-min) (point-max))))
    (with-temp-buffer
      (insert content)
      (beginning-of-buffer)
      (keep-lines "^\*")

      ;; don't include the TOC itself
      (beginning-of-buffer)
      (re-search-forward ot-toc-regexp)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point)))

      (buffer-substring-no-properties
       (point-min) (point-max)))))

(defun ot-hrefify-github (str)
  "Given a heading, transform it into a href using the GitHub
rules."
  (let* ((spc-fix (replace-regexp-in-string " " "-" str))
         (upcase-fix (replace-regexp-in-string "[A-Z]" 'downcase spc-fix t))
         (special-chars-fix (replace-regexp-in-string ot-special-chars-regexp "" upcase-fix t))
         )
    (concat "#" special-chars-fix)))

(defun ot-hrefify-toc (toc hrefify)
  "Format the raw `toc' using the `hrefify' function to transform
each heading into a link."
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

(defun ot-flush-subheadings (toc max-depth)
  "Flush subheadings of the raw `toc' deeper than `max-depth'."
  (with-temp-buffer
    (insert toc)
    (beginning-of-buffer)

    (let ((re "^"))
      (dotimes (i (1+ max-depth))
        (setq re (concat re "\\*")))
      (flush-lines re))

    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun ot-insert-toc ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (beginning-of-buffer)
      (let ((case-fold-search t))
        ;; find the first heading with the :TOC: tag
        (when (re-search-forward ot-toc-regexp (point-max) t)
          (forward-line 1)

          ;; insert newline if TOC is currently empty
          (when (looking-at "^\\*")
            (open-line 1))

          ;; remove previous TOC
          (delete-region (point)
                         (save-excursion
                           (search-forward-regexp "^\\*" (point-max) 0)
                           (forward-line -1)
                           (end-of-line)
                           (point)))

          (insert (ot-hrefify-toc (ot-flush-subheadings (ot-raw-toc) ot-max-depth) 'ot-hrefify-github)))))))

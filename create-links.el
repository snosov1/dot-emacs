(let ((dir (file-name-directory load-file-name))
      (dest-prefix "~/")
      (dest-names (list ".emacs"
                        "emacs-init.org"
                        ".mc-lists.el"
                        ".abbrev_defs"
                        ".yasnippets")))
  (dolist (dest dest-names)
    (when (not (file-exists-p (concat dest-prefix dest)))
      (make-symbolic-link (concat dir dest) (concat dest-prefix dest)))))

;; initialization
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;; list of required packages
(setq required-packages
      '(
        browse-kill-ring
        cmake-mode
        d-mode
        dummyparens
        expand-region
        smex
        string-edit-at-point
        window-numbering
        markdown-mode
        magit
        move-text
        multiple-cursors
        mwim
        org-pomodoro
        toc-org
        treesit-auto
        paredit
        unfill
        yaml-mode
        yasnippet
        wgrep
        ))

;; install external packages
(require 'cl-lib)
(map-y-or-n-p
 "Package %s is missing. Install? "
 '(lambda (package)
    (when (not package-archive-contents)
      (package-refresh-contents))
    (package-install package))
 (cl-remove-if 'package-installed-p required-packages)
 '("package" "packages" "install"))

(setq vc-follow-symlinks t)

(require 'ob-tangle)

(org-babel-load-file (concat (file-name-directory load-file-name) "emacs-init.org"))

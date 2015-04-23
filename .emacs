;; initialization
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
(setq package-enable-at-startup nil)
(package-initialize)

;; install external packages
(require 'cl-lib)
(map-y-or-n-p
 "Package %s is missing. Install? "
 '(lambda (package)
    ;; for some reason, package-install doesn't work well if you
    ;; won't call package-refresh-contents beforehand
    (unless (boundp '--package-contents-refreshed-on-init)
      (package-refresh-contents)
      (setq --package-contents-refreshed-on-init 1))
    (package-install package))
 (cl-remove-if 'package-installed-p
               '(
                 async
                 browse-kill-ring
                 cmake-mode
                 d-mode
                 dired-details
                 dos
                 dummyparens
                 expand-region
                 smex
                 string-edit
                 window-numbering
                 markdown-mode
                 magit
                 multiple-cursors
                 org
                 ox-reveal
                 toc-org
                 paredit
                 gitconfig-mode
                 gitignore-mode
                 unfill
                 yaml-mode
                 yasnippet
                 wgrep
                 ))
 '("package" "packages" "install"))

(setq vc-follow-symlinks t)

(require 'org-install)
(require 'ob-tangle)

(org-babel-load-file "~/emacs-init.org")

;; initialization
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
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
                 auto-complete
                 ac-dcd
                 android-mode
                 async
                 browse-kill-ring
                 cmake-mode
                 d-mode
                 dired-details
                 dos
                 dummyparens
                 expand-region
                 flycheck
                 smex
                 window-numbering
                 markdown-mode
                 magit
                 multiple-cursors
                 org
                 ox-reveal
                 org-toc
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

;; initialization
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
(setq package-enable-at-startup nil)
(package-initialize)

(setq vc-follow-symlinks t)

(require 'org-install)
(require 'ob-tangle)

(org-babel-load-file "~/emacs-init.org")

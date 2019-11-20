;; initialization
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
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
        string-edit
        window-numbering
        markdown-mode
        move-text
        multiple-cursors
        org
        org-pomodoro
        toc-org
        paredit
        gitconfig-mode
        gitignore-mode
        unfill
        yaml-mode
        yasnippet
        wgrep
        ))

(when (version-list-<= (list 24 4) (list emacs-major-version emacs-minor-version))
  (add-to-list 'required-packages
               'magit))
(when (version-list-<= (list 8 3 0) (version-to-list (org-version)))
  (add-to-list 'required-packages
               'ox-reveal))

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

(require 'org-install)
(require 'ob-tangle)

(org-babel-load-file (concat (file-name-directory load-file-name) "emacs-init.org"))

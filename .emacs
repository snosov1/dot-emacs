;; lose tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(calendar-week-start-day 1)
 '(default-input-method "russian-computer")
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message nil)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-verbosity nil)
 '(org-agenda-files (quote ("c:/workspace/org/")))
 '(tab-width 4)
 '(whitespace-style (quote (lines-tail face tabs trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; KEY BINDINGS
;; global
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x C-d") 'dired)

;; org-mode
(require 'org)
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map "\C-\M-p"
               'org-metaup)
             (define-key org-mode-map "\C-\M-n"
               'org-metadown)
             (define-key org-mode-map "\C-\M-b"
               'org-metaleft)
             (define-key org-mode-map "\C-\M-f"
               'org-metaright)))

;; setup color theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-clarity)))

;; enable whitespace mode for source editing modes
(require 'whitespace)
(add-hook 'c++-mode-hook
  (function (lambda ()
              (whitespace-mode t))))
(add-hook 'c-mode-hook
  (function (lambda ()
              (whitespace-mode t))))
(add-hook 'emacs-lisp-mode-hook
  (function (lambda ()
              (whitespace-mode t))))

;; starting emacs server
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t) ; Suppress error "directory
                                              ; ~/.emacs.d/server is unsafe"
                                              ; on windows.
(server-start)

;; disable 'confusing' functions disabling
(put 'narrow-to-region 'disabled nil)

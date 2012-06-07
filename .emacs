;; lose tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-verbosity nil)
 '(tab-width 4))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; starting the Emacs server
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t) ; Suppress error "directory
                                              ; ~/.emacs.d/server is unsafe"
                                              ; on windows.
(server-start)

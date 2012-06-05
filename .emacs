(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(ls-lisp-verbosity nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t) ; Suppress error "directory
                                              ; ~/.emacs.d/server is unsafe"
                                              ; on windows.
(server-start)
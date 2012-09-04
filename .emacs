;; paths
(add-to-list 'load-path "~/.emacs.d")
(let ((default-directory "~/Dropbox/emacs"))
  (normal-top-level-add-subdirs-to-load-path))

;; ------------------------------------------------------------
;; EXTERNAL DEPENDENCIES

;; auto-complete mode

;; NOTE: this mode doesn't play well along with yasnippet, so I've
;; commented out everything about it from ac sources
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  ;; auto-complete-clang
  (when (require 'auto-complete-clang nil t)
    (add-hook 'c-mode-common-hook
              '(lambda ()
                 (define-key c-mode-base-map (kbd "\C-c TAB") 'ac-complete-clang))))
  ;; auto-complete-python
  (unless (equal window-system 'w32) ;; somehow ac-python hangs on windows
    (require 'ac-python nil t)))

;; yasnippet
(when (require 'yasnippet nil t)
  (yas-global-mode 1))

;; cmake-mode
(when (require 'cmake-mode nil t)
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist)))

(when (require 'dos nil t)
  (setq auto-mode-alist
        (append '(("\\.cmd\\'" . dos-mode)
                  ("\\.bat\\'" . dos-mode))
                auto-mode-alist)))

;; magit
(require 'magit nil t)

;; ------------------------------------------------------------
;; DROPPED DEPENDENCIES

;; for R statstics language
;; (when (require 'ess-site nil t)
;;   (setq ess-use-auto-complete t))

;; too bad this dired-async stuff is not working really well
;; (particulary with renaming and deletions)
;; (eval-after-load "dired-aux"
;;   '(require 'dired-async nil t))

;; ------------------------------------------------------------
;; DEPENDENCIES

;; enables key bindings from dired-x (like C-x C-j)
(require 'dired-x)

;; enable org-mode
(require 'org)
;; enable python execution in org-mode
(require 'ob-python)
(require 'ob-R)

;; ------------------------------------------------------------
;; kbd DEFINITIONS

;; open explorer in current directory
(fset 'open-explorer
   [?\M-& ?e ?x ?p ?l ?o ?r ?e ?r ?  ?. return])
;; open current name in explorer
(fset 'open-in-explorer
   [?& ?e ?x ?p ?l ?o ?r ?e ?r return])
;; open nautilus in current directory
(fset 'open-nautilus
   [?\M-& ?n ?a ?u ?t ?i ?l ?u ?s ?  ?. return])
;; open current name in nautilus
(fset 'open-in-nautilus
   [?& ?n ?a ?u ?t ?i ?l ?u ?s return])

;; ------------------------------------------------------------
;; PREDEFINED REGISTERS

;; mirror address
(set-register ?M
              "//samba/nfs/inn/proj/ipp/mirror/")

;; ------------------------------------------------------------
;; DEFUNS

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; functions to save and restore window configuration for ediff-mode
(defun ediff-save-window-configuration ()
  (window-configuration-to-register ?E))
(defun ediff-restore-window-configuration ()
  (jump-to-register ?E))

(defun open-window-manager ()
  "Open default system windows manager in current directory"
  (interactive)
  (when (equal window-system 'w32)
    (save-window-excursion (execute-kbd-macro (symbol-function 'open-explorer))))
  (when (equal window-system 'x)
    (save-window-excursion (execute-kbd-macro (symbol-function 'open-nautilus)))))

(defun open-in-window-manager ()
  "Open item under cursor in default system windows manager"
  (interactive)
  (when (equal window-system 'w32)
    (save-window-excursion (execute-kbd-macro (symbol-function 'open-in-explorer))))
  (when (equal window-system 'x)
    (save-window-excursion (execute-kbd-macro (symbol-function 'open-in-nautilus)))))

(defun dired-goto-file-ido (file)
  "Use ido-read-file-name in dired-goto-file"
  (interactive
   (prog1                          ; let push-mark display its message
       (list (expand-file-name
          (ido-read-file-name "Goto file: " ; use ido-read-file-name
                  (dired-current-directory))))
     (push-mark)))
  (dired-goto-file file))

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window"
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    (select-window other)               ;; comment to stay in current window
    )
  )

(defun double-quote-word ()
  "Put word at point in double quotes"
  (interactive)
  (setq boundaries (bounds-of-thing-at-point 'word))
  (save-excursion
    (goto-char (car boundaries))
    (insert ?\")
    (goto-char (+ 1 (cdr boundaries)))
    (insert ?\")))

(defun show-file-name ()
  "Show the full path file name in the minibuffer and add it to kill ring"
  (interactive)
  (message (buffer-file-name))
  (kill-new (buffer-file-name)))

(defun open-line-indent ()
  "Use newline-and-indent in open-line command if there are
non-whitespace characters after the point"
  (interactive)
  (save-excursion
    (if (looking-at-p "\\s-*$") ;; how in earth does this work?
        (newline)
      (newline-and-indent))))

;; ------------------------------------------------------------
;; CUSTOMIZED

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(calendar-week-start-day 1)
 '(custom-enabled-themes (quote (tango-dark)))
 '(default-input-method "russian-computer")
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(ediff-before-setup-hook (quote (ediff-save-window-configuration)))
 '(ediff-highlight-all-diffs t)
 '(ediff-quit-hook (quote (ediff-cleanup-mess ediff-restore-window-configuration exit-recursive-edit)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-suspend-hook (quote (ediff-default-suspend-function ediff-restore-window-configuration)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message nil)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-verbosity nil)
 '(org-agenda-files (quote ("~/Dropbox/Private/org/")))
 '(org-capture-templates (quote (("t" "Simple TODO" entry (file+headline "~/Dropbox/Private/org/notes.org" "Tasks") "* TODO %?
  DEADLINE:%^t") ("e" "Expenses entry" table-line (file "~/Dropbox/Private/org/expenses.org") "| %u | %^{tag|misc|grocery|room|gas|car|sveta-stuff|sveta-cafe|lunch|dance|snack|condoms|phone} | %^{cost} | %^{desc} |"))))
 '(org-confirm-babel-evaluate nil)
 '(org-directory "~/Dropbox/Private/org")
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
 '(org-src-fontify-natively t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(scroll-error-top-bottom t)
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(whitespace-style (quote (face tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark newline-mark)))
 '(yas-trigger-key "M-/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "green"))))
 '(diff-file-header ((t (:background "black" :weight bold))))
 '(diff-header ((t (:background "black"))))
 '(diff-refine-change ((t (:background "dark slate gray"))))
 '(diff-removed ((t (:foreground "yellow"))))
 '(dired-async-in-process-face ((t (:background "cornflower blue"))))
 '(ediff-current-diff-A ((t (:background "white" :foreground "black"))))
 '(ediff-current-diff-Ancestor ((t (:background "white" :foreground "black"))))
 '(ediff-current-diff-B ((t (:background "white" :foreground "black"))))
 '(ediff-current-diff-C ((t (:background "white" :foreground "black"))))
 '(ediff-even-diff-A ((t (:background "antique white" :foreground "Black"))))
 '(ediff-even-diff-Ancestor ((t (:background "antique white" :foreground "black"))))
 '(ediff-even-diff-B ((t (:background "antique white" :foreground "black"))))
 '(ediff-even-diff-C ((t (:background "antique white" :foreground "Black"))))
 '(ediff-fine-diff-A ((t (:background "gainsboro" :foreground "blue"))))
 '(ediff-fine-diff-Ancestor ((t (:background "gainsboro" :foreground "red"))))
 '(ediff-fine-diff-B ((t (:background "gainsboro" :foreground "forest green"))))
 '(ediff-fine-diff-C ((t (:background "gainsboro" :foreground "purple"))))
 '(ediff-odd-diff-A ((t (:background "antique white" :foreground "black"))))
 '(ediff-odd-diff-Ancestor ((t (:background "antique white" :foreground "black"))))
 '(ediff-odd-diff-B ((t (:background "antique white" :foreground "Black"))))
 '(ediff-odd-diff-C ((t (:background "antique white" :foreground "black"))))
 '(magit-item-highlight ((t (:background "black")))))

;; ------------------------------------------------------------
;; MISCELLANEOUS CONFIGS

;; lose tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; start emacs server on first run
(require 'server)
(when (equal window-system 'w32)
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                                 ; ~/.emacs.d/server is unsafe"
                                                 ; on windows.
(unless (server-running-p) (server-start))

;; make background a little darker
(set-background-color "#1d1f21")

;; disable 'confusing' functions disabling
(put 'narrow-to-region 'disabled nil)

;; shut up the bell
(setq ring-bell-function 'ignore)

;; ediff: fine highlight by char, not words
(setq ediff-forward-word-function 'forward-char)

;; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; show matching parentheses
(show-paren-mode 1)

;; ------------------------------------------------------------
;; KEY BINDINGS

;; global
(global-set-key (kbd "C-x f")     'find-file)
(global-set-key (kbd "C-x C-d")   'dired)
(global-set-key [C-tab]           'ido-switch-buffer)
(global-set-key (kbd "C-x C-q")   'view-mode)
(global-set-key (kbd "C-M-p")     'previous-buffer)
(global-set-key (kbd "C-M-n")     'next-buffer)
(global-set-key (kbd "\C-c c")    'org-capture)
(global-set-key (kbd "\C-c a")    'org-agenda)
(global-set-key (kbd "\C-a")      'smart-beginning-of-line)
(global-set-key (kbd "\C-x \C-b") 'ibuffer)
(global-set-key (kbd "M-p")       'scroll-down-line)
(global-set-key (kbd "M-n")       'scroll-up-line)
(global-set-key (kbd "\C-c m")    'magit-status)
(global-set-key (kbd "\C-c s")    'swap-buffers-in-windows)
(global-set-key (kbd "M-\"")      'double-quote-word)
(global-set-key (kbd "\C-c w")    'show-file-name)
(global-set-key (kbd "\C-o")      'open-line-indent)
(global-set-key (kbd "\C-x v a")  'vc-annotate)
(global-set-key (kbd "\C-x v b")  'vc-annotate)

;; convinient binding for C-x C-s in org-src-mode
(add-hook 'org-src-mode-hook
          '(lambda ()
             (define-key org-src-mode-map (kbd "C-x C-s") 'org-edit-src-exit)))

(add-hook 'org-mode-hook
          '(lambda ()
             ;; don't redefine C-<TAB>
             (define-key org-mode-map [C-tab]
               nil)
             ;; swap active/inactive time-stamp bindings
             (define-key org-mode-map (kbd "C-c .")
               'org-time-stamp-inactive)
             (define-key org-mode-map (kbd "C-c !")
               'org-time-stamp)))

(add-hook 'dired-mode-hook
          '(lambda()
             ;; keep default behavior in dired
             (define-key dired-mode-map (kbd "C-x C-q")
               'dired-toggle-read-only)
             ;; use global key bindings intead
             (define-key dired-mode-map (kbd "C-M-p")
               nil)
             (define-key dired-mode-map (kbd "C-M-n")
               nil)
             ;; external window manager
             (define-key dired-mode-map (kbd "E")
               'open-window-manager)
             (define-key dired-mode-map [(shift return)]
               'open-in-window-manager)
             (define-key dired-mode-map (kbd "j")
               'dired-goto-file-ido)))


(add-hook 'view-mode-hook
          '(lambda ()
             ;; navigation
             (define-key view-mode-map "p"
               'previous-line)
             (define-key view-mode-map "n"
               'next-line)
             (define-key view-mode-map "f"
               'forward-char)
             (define-key view-mode-map "b"
               'backward-char)
             (define-key view-mode-map "l"
               'recenter-top-bottom)
             (define-key view-mode-map "e"
               'move-end-of-line)
             (define-key view-mode-map "a"
               'smart-beginning-of-line)))

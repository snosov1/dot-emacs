;; lose UI early
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; no splash screen
(setq inhibit-startup-message t)

;; ------------------------------------------------------------
;; PATHS
(add-to-list 'load-path "~/.emacs.d")

;; this complicated 'let' places every subdir of Dropbox/emacs AT THE
;; BEGINING of load-path, so custom versions of libraries are loaded
;; before bundled ones (org and term in particular)
(let ((default-directory "~/Dropbox/emacs"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; ------------------------------------------------------------
;; BUILT-IN DEPENDENCIES

;; enables key bindings from dired-x (like C-x C-j)
(require 'dired-x)

;; for zap-up-to-char
(require 'misc)

;; for git-grep command
(require 'vc-git)
(require 'grep)

;; hippie-expand
(require 'hippie-exp)

;; enable org-mode
(require 'org)
;; enable python execution in org-mode
(require 'ob-python)
(require 'ob-R)

;; webjump
(when (require 'webjump nil t)
  (add-to-list 'webjump-sites
               '("Lingvo" .
                 [simple-query
                  "lingvopro.abbyyonline.com"
                  "http://lingvopro.abbyyonline.com/en/Translate/en-ru/"
                  ""]))
  (add-to-list 'webjump-sites
               '("Urban Dictionary" .
                 [simple-query
                  "www.urbandictionary.com"
                  "http://www.urbandictionary.com/define.php?term="
                  ""])))

;; ------------------------------------------------------------
;; EXTERNAL PACKAGES

;; initialization
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

(package-initialize)

;; check if the required packages are installed; suggest installing if not
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(
   auto-complete
   smex
   elpy
   window-numbering
   ))

;; auto-complete
(eval-after-load "auto-complete-autoloads"
  '(progn
     (if (require 'auto-complete-config nil t)
         (progn
           (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
           (ac-config-default)

           ;; don't autostart ac
           (setq ac-auto-start nil)
           (define-key ac-mode-map (kbd "C-.") 'auto-complete))
       (warn "auto-complete not found"))))

;; smex
(eval-after-load "smex-autoloads"
  '(progn
     (if (require 'smex nil t)
         (progn
           (smex-initialize)
           (global-set-key (kbd "M-x") 'smex))
       (warn "smex not found"))))

;; elpy
;; to install dependencies:
;; $ pip install elpy rope pyflakes ipython
(eval-after-load "elpy-autoloads"
  '(progn
     (if (require 'elpy nil t)
         (progn
           (elpy-enable)
           (elpy-clean-modeline)
           (elpy-use-ipython)
           (add-hook 'elpy-mode-hook
                     '(lambda ()
                        (define-key elpy-mode-map (kbd "M-p")
                          nil)
                        (define-key elpy-mode-map (kbd "M-n")
                          nil))))
       (warn "elpy not found"))))

(eval-after-load "window-numbering-autoloads"
  '(progn
     (if (require 'window-numbering nil t)
         (window-numbering-mode 1)
       (warn "window-numbering-mode not found"))))

;; ------------------------------------------------------------
;; EXTERNAL DEPENDENCIES

;; NOTE: these dependencies should be eventually migrated to use
;; package.el

;; android-mode
(when (require 'android-mode nil t)
  (let (sdkdir (getenv "ANDROID_SDK"))
    (if sdkdir
        (setq android-mode-sdk-dir sdkdir)
      (setq android-mode-sdk-dir "~/Development/android-sdk-linux")))
  (setq android-mode-key-prefix "\C-c \C-a"))

;; cmake-mode
(when (require 'cmake-mode nil t)
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("CMakeCache\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist)))

;; dos-mode
;; for editing Windows .bat-files
(when (require 'dos nil t)
  (setq auto-mode-alist
        (append '(("\\.cmd\\'" . dos-mode)
                  ("\\.bat\\'" . dos-mode))
                auto-mode-alist)))

;; d-mode
;; for the D programming language
(when (require 'd-mode nil t)
  (setq auto-mode-alist
        (append '(("\\.d\\'" . d-mode))
                auto-mode-alist)))

;; magit
(when (require 'magit nil t)
  (require 'gitignore-mode nil t)
  (require 'gitconfig-mode nil t)
  (require 'gitattributes-mode nil t)

  (setq magit-revert-item-confirm nil)

  ;; Add an extra newline to separate commit message from git commentary
  (defun magit-commit-mode-init ()
    (when (looking-at "\n")
      (open-line 1)))
  (add-hook 'git-commit-mode-hook 'magit-commit-mode-init))


;; expand-region
(when (require 'expand-region nil t)
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region))

;; dired-details
(when (require 'dired-details nil t)
  (add-hook 'dired-mode-hook
            '(lambda ()
               (dired-details-install)
               (setq dired-details-hidden-string "--- ")
               (define-key dired-mode-map (kbd "h") 'dired-details-toggle))))

;; multiple cursors
(when (require 'multiple-cursors nil t)
  (defun mc/mark-all-dispatch ()
    "Calls mc/edit-lines if multiple lines are selected and
mc/mark-all-like-this otherwise"
    (interactive)
    (cond
     ((> (- (line-number-at-pos (region-end))
            (line-number-at-pos (region-beginning))) 0)
      (mc/edit-lines))
     (t
      (mc/mark-all-like-this))))

  (setq mc/list-file "~/Dropbox/dot-emacs/.mc-lists.el")
  (load mc/list-file t) ;; load, but no errors if it does not exist yet please

  (global-set-key (kbd "C->")     'mc/mark-next-like-this)
  (global-set-key (kbd "C-<")     'mc/mark-previous-like-this)

  (global-set-key (kbd "M-@") 'mc/mark-all-dispatch)

  (global-set-key (kbd "M-#") 'mc/insert-numbers))

;; markdown
(when (require 'markdown-mode nil t)
  (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
  (setq markdown-command (concat "perl " (expand-file-name "~/Dropbox/bin/Markdown.pl"))))

;; browse-kill-ring
(when (require 'browse-kill-ring nil t)
  (global-set-key (kbd "C-x C-y") 'browse-kill-ring)
  (setq browse-kill-ring-quit-action 'save-and-restore))

;; dummyparens
(when (require 'dummyparens nil t)
  (global-dummyparens-mode))

;; paredit
(when (require 'paredit nil t)
  (global-set-key (kbd "C-S-h") 'paredit-splice-sexp))

(when (require 'wgrep nil t)
  (setq wgrep-enable-key "\C-x\C-q")
  (add-hook 'grep-mode-hook
            '(lambda ()
               (define-key grep-mode-map "\C-c\C-c"
                 'wgrep-save-all-buffers))))

;; cuda-mode
(require 'cuda-mode nil t)

;; export org to s5
(when (require 'ox-s5 nil t)
  (defadvice org-s5-export-to-html (around org-s5-export-to-html-tango activate)
    "Wrap around org-s5-export-to-html to use tango theme colors for source
code fontification."
    (let (
          (old-builtin-foreground (face-attribute 'font-lock-builtin-face :foreground))
          (old-comment-foreground (face-attribute 'font-lock-comment-face :foreground))
          (old-constant-foreground (face-attribute 'font-lock-constant-face :foreground))
          (old-function-foreground (face-attribute 'font-lock-function-name-face :foreground))
          (old-keyword-foreground (face-attribute 'font-lock-keyword-face :foreground))
          (old-string-foreground (face-attribute 'font-lock-string-face :foreground))
          (old-type-foreground (face-attribute 'font-lock-type-face :foreground))
          (old-variable-foreground (face-attribute 'font-lock-variable-name-face :foreground))
          )
      (set-face-foreground 'font-lock-builtin-face       "#75507b")
      (set-face-foreground 'font-lock-comment-face       "#5f615c")
      (set-face-foreground 'font-lock-constant-face      "#204a87")
      (set-face-foreground 'font-lock-function-name-face "#a40000")
      (set-face-foreground 'font-lock-keyword-face       "#346604")
      (set-face-foreground 'font-lock-string-face        "#5c3566")
      (set-face-foreground 'font-lock-type-face          "#204a87")
      (set-face-foreground 'font-lock-variable-name-face "#b35000")

      ad-do-it

      (set-face-foreground 'font-lock-builtin-face        old-builtin-foreground)
      (set-face-foreground 'font-lock-comment-face        old-comment-foreground)
      (set-face-foreground 'font-lock-constant-face       old-constant-foreground)
      (set-face-foreground 'font-lock-function-name-face  old-function-foreground)
      (set-face-foreground 'font-lock-keyword-face        old-keyword-foreground)
      (set-face-foreground 'font-lock-string-face         old-string-foreground)
      (set-face-foreground 'font-lock-type-face           old-type-foreground)
      (set-face-foreground 'font-lock-variable-name-face  old-variable-foreground)
      )))

(when (require 'image-mode nil t)
  (defun next-image (arg)
    "Visit the next arg'th image in the same directory of the
same type."
    (interactive "P")
    (unless (and (buffer-file-name) (eq major-mode 'image-mode))
      (error "Not visiting a file in image mode"))
    (let* ((files   (directory-files
                     (file-name-directory (buffer-file-name)) nil
                     (file-name-extension (buffer-file-name)) ))
           (len     (length files))
           (this    (file-name-nondirectory (buffer-file-name)))
           (idx     0))
      (dolist (file files)
        (if (not (string= this file))
            (setq idx  (1+ idx))
          (setq idx
                (mod (+ idx (if arg arg 1)) len))
          (kill-this-buffer) ;; we don't want to have a thousand image
                             ;; buffers around
          (find-file (elt files idx))))))

  (defun previous-image (arg)
    "Visit previous image. See `next-image'"
    (interactive "P")
    (next-image (if arg (- arg) -1)))

  (add-hook 'image-mode-hook
            '(lambda ()
               (define-key image-mode-map "n" 'next-image)
               (define-key image-mode-map "p" 'previous-image)
               )))

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
;; SKELETONS

(define-skeleton printf-skeleton
  "insert printf statement"
  "value: "
  "printf(\"" str " = %" _ "d\\n\", " str ");" \n)
(define-skeleton tostring-skeleton
  "insert template to convert anything to string"
  "#include <sstream>\n"
  "template <typename T>\n"
  "std::string to_string(const T& obj)\n"
  "{\n"
  "    std::ostringstream oss;\n"
  "    oss << obj;\n"
  "    return oss.str();\n"
  "}\n")


;; ------------------------------------------------------------
;; ADVICES

(defadvice insert-for-yank-1 (after indent-region activate)
  "Indent yanked region in certain modes, C-u prefix to disable"
  (if (and (not current-prefix-arg)
           (member major-mode '(emacs-lisp-mode
                                lisp-mode
                                c-mode c++-mode objc-mode d-mode java-mode cuda-mode
                                LaTeX-mode TeX-mode
                                xml-mode html-mode css-mode)))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice ido-switch-buffer (after maintain-ansi-term activate)
  "Go to prompt when switched to ansi-term"
  (when (member major-mode '(term-mode))
      (term-line-mode)
      (end-of-buffer)
      (end-of-line)
      (term-char-mode)))

;; ------------------------------------------------------------
;; DEFUNS

(defun orgtbl-to-latex-matrix (table params)
  "Convert the Orgtbl mode TABLE to a LaTeX Matrix."
  (interactive)
  (let* ((params2
          (list
           :tstart (concat "\\[\n\\begin{pmatrix}")
           :tend "\\end{pmatrix}\n\\]"
           :lstart "" :lend " \\\\" :sep " & "
           :efmt "%s\\,(%s)" :hline "\\hline")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

(defun org-s5-init-dir ()
  "Initialize directory for S5 presentation"
  (interactive)
  (start-process-shell-command "git" nil "git clone ~/Dropbox/emacs/org-s5/ ."))

(defmacro smart-isearch (direction)
  `(defun ,(intern (format "smart-isearch-%s" direction)) (&optional regexp-p no-recursive-edit)
     "If region is active and non empty, use it for searching and
make first jump. Otherwise, behave like original function."
     (interactive "P\np")
     (let ((smart-p (and (region-active-p) (< (region-beginning) (region-end)))))
       (when smart-p
         (kill-ring-save (region-beginning) (region-end)))

       (,(intern (format "isearch-%s" direction)) regexp-p no-recursive-edit)

       (when smart-p
         (isearch-yank-kill)
         (,(intern (format "isearch-repeat-%s" direction)))))))

(smart-isearch forward)
(smart-isearch backward)

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun c-current-function-name ()
  "Returns current function name in C-like
languages ('beginning-of-defun'-based)"
  (save-excursion
    (beginning-of-defun)
    (skip-chars-forward "^(")
    (skip-chars-backward "\s\n")
    (setq defun-name-end (point))
    (skip-chars-backward "^[:space:]")
    (buffer-substring-no-properties (point) defun-name-end)))

(defun configure-theme ()
  (load-theme 'tango-dark)
  (enable-theme 'tango-dark)
  ;; make background a little darker
  (set-background-color "#1d1f21")
  ;; require term mode after theme is set
  (require 'term)
  ;; set better ansi-term colors
  (setq ansi-term-color-vector [unspecified "#1d1f21" "#cc6666" "firebrick" "#f0c674" "#81a2be" "#b294bb" "cyan3" "#c5c8c6"])
  ;; set font
  (ignore-errors
    (set-frame-font
     (car (x-list-fonts "-*-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-*-*-iso10646-1")))))

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

(defun dired-jump-to-bottom ()
  "Jumps to the last file"
  (interactive)
  (end-of-buffer)
  (dired-previous-line 1))

(defun dired-jump-to-top ()
  "Jumps to the .. entry"
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 1)
  ;; skip another line depending on hidden/shown state of dired-details
  (when (or (not (boundp 'dired-details-state))
            (equal dired-details-state 'shown))
    (dired-next-line 1))
  (if (looking-at "\\.") ;; top-level directories don't have a
                         ;; .. entry
      (dired-next-line 1)))

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

(defun toggle-window-split ()
  "Switches from a horizontal split to a vertical split and vice versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun notify-send (title msg &optional icon)
  "Show a popup; TITLE is the title of the message, MSG is the
context. ICON is the optional filename or keyword.
Portable keywords are: error, important, info."
  (interactive)
  (if (or (eq window-system 'x)
          (eq window-system 'w32))
      (save-window-excursion
        (async-shell-command (concat "notify-send "
                                     (if icon (concat "-i " icon) "-i important")
                                     " \"" title "\" \"" msg "\"")))
    ;; text only version
    (message (concat title ": " msg))))

(defcustom git-grep-switches "--extended-regexp -I -n --ignore-case "
  "Switches to pass to 'git grep'."
  :type 'string)

(defun git-grep (re)
  (interactive
   (list (let ((gg-init-value
                ;; if region is active - use its value as an init
                (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  nil)))
           (read-from-minibuffer "git grep: " gg-init-value nil nil 'grep-history))))
  (let ((grep-use-null-device nil))
    (grep (format "git --no-pager grep %s -e %s -- %s"
                  git-grep-switches
                  re
                  (expand-file-name (vc-git-root default-directory))))))

(defvar hs-hide-all-toggle-state nil "Current state of hideshow for toggling all.")
(make-variable-buffer-local 'hs-hide-all-toggle-state)
(defun hs-toggle-hideshow-all (arg)
  "Toggle hideshow all. Prefix arg is the level of hiding."
  (interactive "P")
  (if (not arg)
      (setq arg 1))
  (setq hs-hide-all-toggle-state (not hs-hide-all-toggle-state))
  (if hs-hide-all-toggle-state
      (hs-hide-level arg)
    (hs-show-all)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun upcase-dispatch (arg)
  "Use upcase word or region"
  (interactive "P")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word (if arg arg 1))))

(defun downcase-dispatch (arg)
  "Use downcase word or region"
  (interactive "P")
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word (if arg arg 1))))

(defun capitalize-dispatch (arg)
  "Use capitalize word or region"
  (interactive "P")
  (if (region-active-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word (if arg arg 1))))

(defun eval-dispatch ()
  "Evaluate previous sexp or region"
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end) t)
    (eval-and-replace)))

(defun fill-paragraph-with-set (arg)
  "Temporary sets fill-column to given prefix argument and calls
fill-paragraph"
  (interactive "P")
  (setq cfc (current-fill-column))
  (if arg
      (set-fill-column arg))
  (fill-paragraph 'nil 't)
  (set-fill-column cfc))

;; move text
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun join-following-line ()
  "Joins the following line"
  (interactive)
  (join-line -1))

(defcustom pop-predefined-register ?}
  "Register for saving window configuration before jump"
  :type 'register)

(defun jump-to-register-with-save (register &optional delete)
  "Like jump-to-register, but saves current window configuration
to predefined register"
  (interactive "cJump to register: \nP")
  ;; autosave current window configuration unless we're jumping back
  (unless (equal register pop-predefined-register)
    (window-configuration-to-register pop-predefined-register))
  (jump-to-register register delete))

;; ------------------------------------------------------------
;; CUSTOMIZED

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (d-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(calendar-week-start-day 1)
 '(compilation-scroll-output (quote first-error))
 '(default-input-method "russian-computer")
 '(diff-update-on-the-fly nil)
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(ediff-before-setup-hook (quote (ediff-save-window-configuration)))
 '(ediff-highlight-all-diffs t)
 '(ediff-quit-hook (quote (ediff-cleanup-mess ediff-restore-window-configuration exit-recursive-edit)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-suspend-hook (quote (ediff-default-suspend-function ediff-restore-window-configuration)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(frame-background-mode (quote dark))
 '(grep-find-command (quote ("find . -type f -exec grep -nHi -e  {} +" . 35)))
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill)))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message nil)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-verbosity nil)
 '(magit-diff-refine-hunk t)
 '(org-agenda-files (quote ("~/Dropbox/Private/org/")))
 '(org-capture-templates (quote (("t" "Simple TODO" entry (file+headline "~/Dropbox/Private/org/notes.org" "Tasks") "* TODO %?
DEADLINE:%^t") ("e" "Expenses entry" table-line (file "~/Dropbox/Private/org/expenses.org") "| %u | %^{tag|misc|grocery|room|gas|car|sveta-stuff|cafe|lunch|dance|snack|condoms|phone|house|bus|repo} | %^{cost} | %^{desc} |") ("l" "Link" entry (file+headline "~/Dropbox/Private/org/notes.org" "Links") "* %?
%c"))))
 '(org-clock-mode-line-total (quote current))
 '(org-confirm-babel-evaluate nil)
 '(org-directory "~/Dropbox/Private/org")
 '(org-hide-leading-stars t)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
 '(org-src-fontify-natively t)
 '(org-startup-indented t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(scroll-conservatively 1)
 '(scroll-error-top-bottom t)
 '(show-paren-delay 0)
 '(tab-width 4)
 '(tags-case-fold-search nil)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(wdired-allow-to-change-permissions t)
 '(whitespace-style (quote (face tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark newline-mark)))
 '(yas-prompt-functions (quote (yas-dropdown-prompt yas-ido-prompt yas-completing-prompt yas-x-prompt yas-no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "green"))))
 '(diff-file-header ((t (:background "black" :weight bold))))
 '(diff-header ((t (:background "black"))))
 '(diff-refine-change ((t (:background "dark slate gray"))))
 '(diff-removed ((t (:foreground "tomato"))))
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
;; KEY BINDINGS

;; global
(global-set-key (kbd "C-x f")       'find-file)
(global-set-key (kbd "C-x C-d")     'dired)
(global-set-key [C-tab]             'ido-switch-buffer)
(global-set-key (kbd "C-x C-q")     'view-mode)
(global-set-key (kbd "C-M-p")       'backward-paragraph)
(global-set-key (kbd "C-M-n")       'forward-paragraph)
(global-set-key (kbd "\C-c c")      'org-capture)
(global-set-key (kbd "\C-c a")      'org-agenda)
(global-set-key (kbd "\C-x \C-b")   'ibuffer)
(global-set-key (kbd "\C-x b")      'ibuffer)
(global-set-key (kbd "M-p")         'move-text-up)
(global-set-key (kbd "M-n")         'move-text-down)
(global-set-key (kbd "\C-c m")      'magit-status)
(global-set-key (kbd "\C-c s")      'swap-buffers-in-windows)
(global-set-key (kbd "\C-c\C-s")    'swap-buffers-in-windows)
(global-set-key (kbd "M-\"")        'double-quote-word)
(global-set-key (kbd "\C-c w")      'show-file-name)
(global-set-key (kbd "\C-x v a")    'vc-annotate)
(global-set-key (kbd "\C-x v b")    'vc-annotate)
(global-set-key (kbd "<f5>")        'revert-buffer)
(global-set-key (kbd "\C-c f")      'toggle-window-split)
(global-set-key (kbd "\C-c\C-f")    'toggle-window-split)
(global-set-key [(control shift f)] 'git-grep)
(global-set-key (kbd "\C-x\C-e")    'eval-dispatch)
(global-set-key (kbd "M-\\")        'fixup-whitespace)
(global-set-key (kbd "C-M-h")       'backward-kill-word)
(global-set-key (kbd "M-/")         'hippie-expand)
(global-set-key (kbd "\C-x k")      'kill-this-buffer)
(global-set-key (kbd "C-+")         'org-list-repair)
(global-set-key (kbd "C-x w")       'webjump)
(global-set-key (kbd "\C-x\C-r")    'rename-buffer)
(global-set-key (kbd "C-x t")       'toggle-truncate-lines)
(global-set-key (kbd "M-j")         'join-following-line)
(global-set-key (kbd "M-Z")         'zap-up-to-char)

;; remap existing commands with "smarter" versions
(define-key global-map [remap move-beginning-of-line] 'smart-beginning-of-line)
(define-key global-map [remap upcase-word]            'upcase-dispatch)
(define-key global-map [remap downcase-word]          'downcase-dispatch)
(define-key global-map [remap capitalize-word]        'capitalize-dispatch)
(define-key global-map [remap jump-to-register]       'jump-to-register-with-save)
(define-key global-map [remap fill-paragraph]         'fill-paragraph-with-set)
(define-key global-map [remap open-line]              'open-line-indent)
(define-key global-map [remap isearch-forward]        'smart-isearch-forward)
(define-key global-map [remap isearch-backward]       'smart-isearch-backward)

;; define translations
(define-key key-translation-map [?\C-h] [?\C-?]) ;; translate C-h to DEL

;; C-/ is not representable with an ASCII control code, so it cannot
;; be sent to terminals, but it is a convenient keybinding for
;; undo. So mapping it to "traditional" undo sequence C-_ is a cute
;; way around
(define-key key-translation-map [?\C-/] [?\C-_]) ;; translate C-/ to C-_

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

(define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-jump-to-top)
(define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(add-hook 'dired-mode-hook
          '(lambda()
             ;; keep default behavior in dired
             (define-key dired-mode-map (kbd "C-x C-q")
               'dired-toggle-read-only)
             (define-key dired-mode-map (kbd "E")
               'open-window-manager)
             (define-key dired-mode-map [(shift return)]
               'open-in-window-manager)
             (define-key dired-mode-map (kbd "j")
               'dired-goto-file-ido)))

(add-hook 'view-mode-hook
          '(lambda ()
             ;; simpler navigation
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
               'smart-beginning-of-line)
             (define-key view-mode-map "v"
               'scroll-up-command)))

(add-hook 'cmake-mode-hook
          '(lambda ()
             (define-key cmake-mode-map "\C-c\C-c"
               'compile)))

(add-hook 'sh-mode-hook
          '(lambda ()
             (define-key sh-mode-map "\C-c\C-c"
               'compile)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-base-map "\C-c\C-c"
               'compile)
             (define-key c-mode-base-map "\C-c\C-o"
               'ff-find-other-file)
             (define-key c-mode-base-map "\C-c\C-p"
               'printf-skeleton)
             (define-key c-mode-base-map (kbd "C-M-h")
               'backward-kill-word)
             (define-key c-mode-base-map (kbd "M-j")
               'join-following-line)
             ;; hs-mode
             (hs-minor-mode t)
             (define-key c-mode-base-map "\C-ch"
               'hs-toggle-hideshow-all)
             ;; set //-style comments for c-mode
             (setq comment-start "//" comment-end "")))

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; ------------------------------------------------------------
;; MISCELLANEOUS CONFIGS

;; write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
;; make backups of files, even when they're under version control
(setq vc-make-backup-files t)

(require 'server)
(when (equal window-system 'w32)
  ;; Suppress error "directory ~/.emacs.d/server is unsafe" on
  ;; windows.
  (defun server-ensure-safe-dir (dir) "Noop" t))

;; start emacs server on first run
(unless (server-running-p) (server-start))
;; do not disturb with "buffer still has active clients" on buffer killing
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(setq term-remote-hosts '(
                          ("argus-cv" "ssh" "sergei@argus-cv.dnsalias.org" "-p7707")
                          ("carma-1" "ssh" "ubuntu@192.168.0.106")
                          ("jetson-1" "ssh" "nvidia@192.168.0.147")
                          ("jetson-2" "ssh" "nvidia@192.168.0.166")
                          ("sergei-ws" "ssh" "sergei@192.168.0.43")
                          ("tegra-server" "ssh" "snosov1@192.168.0.14") ;; masha without spec symbols
                          ))
(setq ange-ftp-dumb-unix-host-regexp (regexp-opt '(
                                                   "files.itseez.com"
                                                   )))

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

;; replace selection with input or yank
(delete-selection-mode 1)

;; make emacs look good
(configure-theme)

;; RecognizeCamelCaseSubwording
(global-subword-mode)
;; don't remap some commands
(define-key subword-mode-map (vector 'remap 'transpose-words) nil)
(define-key subword-mode-map (vector 'remap 'upcase-word) nil)
(define-key subword-mode-map (vector 'remap 'downcase-word) nil)
(define-key subword-mode-map (vector 'remap 'capitalize-word) nil)

;; dired listing switches
(setq dired-listing-switches (concat "-alh"
                                     (when (not (equal window-system 'w32))
                                       " --group-directories-first")))

;; ibuffer groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("org"  (mode . org-mode))
               ("dired" (mode . dired-mode))
               ("D" (mode . d-mode))
               ("C/C++" (or
                         (mode . cc-mode)
                         (mode . c-mode)
                         (mode . c++-mode)))
               ("magit" (name . "^\\*magit"))
               ("emacs" (name . "^\\*Messages\\*$"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; compile customization
(make-variable-buffer-local 'compile-command)
;; those patterns are used by dmd compiler
(setq compilation-error-regexp-alist
      (append '(("^\\(.*?\\)(\\([0-9]+\\)): Warning:" 1 2 nil 1)
                ("^\\(.*?\\)(\\([0-9]+\\)): Error:" 1 2 nil 2))
              compilation-error-regexp-alist))

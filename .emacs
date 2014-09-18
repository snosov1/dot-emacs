;; lose UI early
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; no splash screen
(setq inhibit-startup-message t)
(setq mac-command-modifier 'meta)

;; ------------------------------------------------------------
;; BUILT-IN DEPENDENCIES

;; common lisp primitives
(require 'cl-lib)

;; enables key bindings from dired-x (like C-x C-j)
(require 'dired-x)

;; for zap-up-to-char
(require 'misc)

;; for git-grep command
(require 'vc-git)
(require 'grep)

;; abbrev
(when (require 'abbrev nil t)
  (add-hook 'find-file-hook
            '(lambda()
               (abbrev-mode -1)))
  (setq-default abbrev-mode nil))

;; hippie-expand
(require 'hippie-exp)

;; image-mode
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
           (idx     0)
           (dir     (file-name-directory (buffer-file-name))))
      (dolist (file files)
        (if (not (string= this file))
            (setq idx  (1+ idx))
          (setq idx
                (mod (+ idx (if arg arg 1)) len))
          (kill-this-buffer) ;; we don't want to have a thousand image
          ;; buffers around
          (find-file (concat dir (elt files idx)))))))

  (defun previous-image (arg)
    "Visit previous image. See `next-image'"
    (interactive "P")
    (next-image (if arg (- arg) -1)))

  (add-hook 'image-mode-hook
            '(lambda ()
               (define-key image-mode-map "n" 'next-image)
               (define-key image-mode-map "p" 'previous-image)
               )))

;; python
(when (require 'python nil t)
  (if (executable-find "ipython")
      (setq
       python-shell-interpreter "ipython"
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "))

  (add-hook 'python-mode-hook
            '(lambda ()
               (define-key python-mode-map (kbd "\C-c\C-c") 'compile)
               (define-key python-mode-map (kbd "\C-c\C-e") 'python-shell-send-buffer)
               )))

;; compile
(when (require 'compile nil t)
  (make-variable-buffer-local 'compile-command)

  ;; those patterns are used by dmd compiler
  (setq compilation-error-regexp-alist
        (append '(("^\\(.*?\\)(\\([0-9]+\\)): Warning:" 1 2 nil 1)
                  ("^\\(.*?\\)(\\([0-9]+\\)): Error:" 1 2 nil 2))
                compilation-error-regexp-alist)))

;; ibuffer
(when (require 'ibuffer nil t)
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
                 ("Markdown" (mode . markdown-mode))
                 ("emacs" (name . "^\\*Messages\\*$"))
                 ("shell commands" (name . "^\\*.*Shell Command\\*"))
                 ))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

(when (require 'tramp nil t)
  (defun append-tramp-host ()
    "Appends host name to the current buffer name for remote files"
    (interactive)
    (when (tramp-tramp-file-p default-directory)
      (rename-buffer
       (concat
        (replace-regexp-in-string " <.*>$" "" (or (uniquify-buffer-base-name) (buffer-name)))
        " <"
        (tramp-file-name-host
         (tramp-dissect-file-name default-directory)) ">")
       t))))

(when (require 'flyspell nil t)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (defcustom ispell-common-dictionaries
    '("en" "ru")
    "List of dictionaries for common use")

  (setq ispell-dictionary (car ispell-common-dictionaries))

  (defun ispell-next-dictionary()
    "Cycle through dictionaries in `ispell-common-dictionaries'"
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (next (cadr (member dic ispell-common-dictionaries)))
           (change (if next next (car ispell-common-dictionaries))))
      (ispell-change-dictionary change)))

  (define-key flyspell-mode-map (kbd "C-x M-$") 'flyspell-buffer)
  (define-key flyspell-mode-map (kbd "C-c M-$") 'ispell-next-dictionary))

;; ------------------------------------------------------------
;; EXTERNAL PACKAGES

;; initialization
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
(setq package-enable-at-startup nil)
(package-initialize)

;; check if the required packages are installed; suggest installing if not
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

;; PER-PACKAGE CONFIGURATION

(eval-after-load "auto-complete-autoloads"
  '(progn
     (when (require 'auto-complete nil t)
       (require 'auto-complete-config)

       (defun ac-expand-no-next ()
         "Try expand, and if expanded twice, complete."
         (interactive)
         (unless (ac-expand-common)
           (let ((string (ac-selected-candidate)))
             (when string
               (if (equal ac-prefix string)
                   (ac-complete)
                 (ac-expand-string string (eq last-command this-command))
                 ;; Do reposition if menu at long line
                 (if (and (> (popup-direction ac-menu) 0)
                          (ac-menu-at-wrapper-line-p))
                     (ac-reposition))
                 (setq ac-show-menu t)
                 string)))))

       (ac-flyspell-workaround)
       (setq-default ac-use-comphist nil)
       (define-key ac-completing-map [tab] 'ac-expand-no-next)
       ;(define-key ac-completing-map "\r" nil)

       (defun ac-yasnippet-candidates-sorted-by-length ()
         "Sorts yasnippet candidates by length."
         (sort (ac-yasnippet-candidates) '(lambda (l r) (< (length l) (length r)))))

       (ac-define-source yasnippet
         '((depends yasnippet)
           (candidates . ac-yasnippet-candidates-sorted-by-length)
           (action . yas-expand)
           (candidate-face . ac-yasnippet-candidate-face)
           (selection-face . ac-yasnippet-selection-face)
           (symbol . "a")))

       (add-hook 'emacs-lisp-mode-hook
                 '(lambda ()
                    (auto-complete-mode t)
                    (setq ac-sources '(
                                       ac-source-yasnippet
                                       ac-source-features
                                       ac-source-functions
                                       ac-source-variables
                                       ac-source-symbols
                                       ac-source-words-in-same-mode-buffers
                                       ))))

       (when (and (require 'ac-dcd nil t) (require 'd-mode nil t))
         (if (and (executable-find ac-dcd-server-executable)
                  (executable-find ac-dcd-executable))
             (progn
               (add-hook 'd-mode-hook
                         '(lambda ()
                            (auto-complete-mode t)
                            (ac-dcd-maybe-start-server)
                            (setq ac-sources '(
                                               ac-source-yasnippet
                                               ac-source-dcd
                                               ac-source-words-in-same-mode-buffers
                                               ))))
               (define-key d-mode-map [remap find-tag]     'ac-dcd-goto-definition)
               (define-key d-mode-map [remap pop-tag-mark] 'ac-dcd-goto-def-pop-marker)
               (define-key d-mode-map (kbd "M-?")          'ac-dcd-show-ddoc-with-buffer)
               (define-key d-mode-map (kbd "C-c i")        'ac-dcd-add-imports))
           (warn "dcd-server not found"))))))

(eval-after-load "org-autoloads"
  '(progn
     (when (require 'org nil t)
       ;; enable python execution in org-mode
       (require 'ob-python)
       (require 'ob-R)

       (defun conditional-org-reveal-export-to-html ()
         (save-excursion
           (beginning-of-buffer)
           (when (search-forward "#+REVEAL" nil nil)
             (org-reveal-export-to-html))))

       (add-hook 'org-ctrl-c-ctrl-c-final-hook
                 'conditional-org-reveal-export-to-html))))

(eval-after-load "org-toc-autoloads"
  '(progn
     (if (require 'org-toc nil t)
         (add-hook 'org-mode-hook 'org-toc-enable)
       (warn "org-toc not found"))))

(eval-after-load "unfill-autoloads"
  '(progn
     (if (require 'unfill nil t)
         (define-key global-map [remap fill-paragraph]
           (defun fill-paragraph-dispatch (arg)
             "Fill or unfill paragraph"
             (interactive "P")
             (if arg
                 (if (region-active-p)
                     (unfill-region (region-beginning) (region-end))
                   (unfill-paragraph))
               (fill-paragraph 'nil 't))))
       (warn "unfill not found"))))

(eval-after-load "yaml-mode-autoloads"
  '(progn
     (if (require 'yaml-mode nil t)
         (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
       (warn "yaml-mode not found"))))

(eval-after-load "async-autoloads"
  '(progn
     (unless (require 'dired-async nil t)
       (warn "dired-async not found"))))

(eval-after-load "dummyparens-autoloads"
  '(progn
     (if (require 'dummyparens nil t)
         (global-dummyparens-mode)
       (warn "dummyparens not found"))))

(eval-after-load "smex-autoloads"
  '(progn
     (if (require 'smex nil t)
         (progn
           (smex-initialize)
           (global-set-key (kbd "M-x") 'smex))
       (warn "smex not found"))))

(eval-after-load "window-numbering-autoloads"
  '(progn
     (if (require 'window-numbering nil t)
         (window-numbering-mode 1)
       (warn "window-numbering-mode not found"))))

(eval-after-load "markdown-mode-autoloads"
  '(progn
     (if (require 'markdown-mode nil t)
         (progn
           (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

           (add-hook 'markdown-mode-hook
                     '(lambda ()
                        (define-key markdown-mode-map (kbd "M-p")
                          nil)
                        (define-key markdown-mode-map (kbd "M-n")
                          nil)
                        (define-key markdown-mode-map (kbd "\C-c\C-c")
                          nil)
                        (define-key markdown-mode-map (kbd "\C-c\C-e")
                          'markdown-export))))
       (warn "markdown-mode not found"))))

(eval-after-load "magit-autoloads"
  '(progn
     (if (require 'magit nil t)
         (progn
           (require 'gitignore-mode nil t)
           (require 'gitconfig-mode nil t)
           (require 'gitattributes-mode nil t)

           (setq
            magit-revert-item-confirm nil
            magit-diff-refine-hunk t))
       (warn "magit not found"))))

;; android-mode
(eval-after-load "android-mode-autoloads"
  '(progn
     (when (require 'android-mode nil t)
       (let ((sdkdir (getenv "ANDROID_SDK")))
         (if sdkdir
             (setq android-mode-sdk-dir sdkdir)
           (setq android-mode-sdk-dir "~/Development/android-sdk-linux")))
       (setq android-mode-key-prefix "\C-c\C-a"))))

;; cmake-mode
(eval-after-load "cmake-mode-autoloads"
  '(progn
     (when (require 'cmake-mode nil t)
       (setq auto-mode-alist
             (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                       ("CMakeCache\\.txt\\'" . cmake-mode)
                       ("\\.cmake\\'" . cmake-mode))
                     auto-mode-alist)))))

;; d-mode
;; for the D programming language
(eval-after-load "d-mode-autoloads"
  '(progn
     (when (require 'd-mode nil t)
       (when (fboundp 'd-lineup-cascaded-calls)
           (add-hook 'd-mode-hook
                     '(lambda ()
                        (add-to-list 'c-offsets-alist '(arglist-cont-nonempty . d-lineup-cascaded-calls))
                        (add-to-list 'c-offsets-alist '(statement-cont . d-lineup-cascaded-calls)))))
       (setq auto-mode-alist
             (append '(("\\.d\\'" . d-mode)
                       ("\\.di\\'" . d-mode))
                     auto-mode-alist)))))

;; expand-region
(eval-after-load "expand-region-autoloads"
  '(progn
     (when (require 'expand-region nil t)
       (add-hook 'text-mode-hook
                 '(lambda ()
                    (setq-local er/try-expand-list
                          (remove 'er/mark-method-call er/try-expand-list))))

       (global-set-key (kbd "C-=") 'er/expand-region)
       (setq expand-region-fast-keys-enabled nil))))

;; dired-details
(eval-after-load "dired-details-autoloads"
  '(progn
     (when (require 'dired-details nil t)
       (add-hook 'dired-mode-hook
                 '(lambda ()
                    (dired-details-install)
                    (setq dired-details-hidden-string "--- ")
                    (define-key dired-mode-map (kbd "h") 'dired-details-toggle))))))

;; multiple cursors
(eval-after-load "multiple-cursors-autoloads"
  '(progn
     (when (require 'multiple-cursors nil t)
       (defun mc/mark-all-dispatch ()
         "- add a fake cursor at current position

- call mc/edit-lines if multiple lines are marked

- call mc/mark-all-like-this if marked region is on a single line"
         (interactive)
         (cond
          ((not (region-active-p))
           (mc/create-fake-cursor-at-point)
           (mc/maybe-multiple-cursors-mode))
          ((> (- (line-number-at-pos (region-end))
                 (line-number-at-pos (region-beginning))) 0)
           (mc/edit-lines))
          (t
           (mc/mark-all-like-this))))

       (defun mc/align ()
         "Aligns all the cursor vertically."
         (interactive)
         (let ((max-column 0)
               (cursors-column '()))
           (mc/for-each-cursor-ordered
            (mc/save-excursion
             (goto-char (overlay-start cursor))
             (let ((cur (current-column)))
               (setq cursors-column (append cursors-column (list cur)))
               (setq max-column (if (< max-column cur) cur max-column)))))

           (defun mc--align-insert-times ()
             (interactive)
             (dotimes (_ times)
               (insert " ")))

           (mc/for-each-cursor-ordered
            (let ((times (- max-column (car cursors-column))))
              (mc/execute-command-for-fake-cursor 'mc--align-insert-times cursor))
            (setq cursors-column (cdr cursors-column)))))

       (setq mc/list-file "~/.mc-lists.el")
       (load mc/list-file t) ;; load, but no errors if it does not exist yet please

       (global-set-key (kbd "C->")  'mc/mark-next-like-this)
       (global-set-key (kbd "C-<")  'mc/mark-previous-like-this)

       (global-set-key (kbd "M-@") 'mc/mark-all-dispatch)
       (global-set-key (kbd "M-#") 'mc/insert-numbers)
       (global-set-key (kbd "M-'") 'mc/align))))

;; browse-kill-ring
(eval-after-load "browse-kill-ring-autoloads"
  '(progn
     (when (require 'browse-kill-ring nil t)
       (global-set-key (kbd "C-x C-y") 'browse-kill-ring)
       (define-key browse-kill-ring-mode-map (kbd "C-c C-k") 'browse-kill-ring-quit)
       (define-key browse-kill-ring-mode-map (kbd "C-x C-k") 'browse-kill-ring-quit)
       (define-key browse-kill-ring-mode-map (kbd "C-x k") 'browse-kill-ring-quit)
       (setq browse-kill-ring-quit-action 'save-and-restore))))

;; paredit
(eval-after-load "paredit-autoloads"
  '(progn
     (when (require 'paredit nil t)
       (global-set-key (kbd "C-S-h") 'paredit-splice-sexp))))

;; wgrep
(eval-after-load "wgrep-autoloads"
  '(progn
     (when (require 'wgrep nil t)
       (setq wgrep-enable-key "\C-x\C-q")
       (add-hook 'grep-mode-hook
                 '(lambda ()
                    (define-key grep-mode-map "\C-c\C-c"
                      'wgrep-save-all-buffers))))))

;; dos-mode
;; for editing Windows .bat-files
(eval-after-load "dos-autoloads"
  '(progn
     (when (require 'dos nil t)
       (setq auto-mode-alist
             (append '(("\\.cmd\\'" . dos-mode)
                       ("\\.bat\\'" . dos-mode))
                     auto-mode-alist)))))

(eval-after-load "yasnippet-autoloads"
  '(progn
     (if (require 'yasnippet nil t)
         (progn
           (let ((yas-dir "~/.yasnippets"))
             (when (file-exists-p yas-dir)
               (setq yas-snippet-dirs (list yas-dir))))
           (yas-global-mode 1))
       (warn "yasnippet not found"))))

;; ox-reveal
;; export .org files as reveal.js presentations (https://github.com/hakimel/reveal.js/)
(require 'ox-reveal nil t)

;; ------------------------------------------------------------
;; ADVICES

(defadvice insert-for-yank-1 (after indent-region activate)
  "Indent yanked region in certain modes, C-u prefix to disable"
  (if (and (not current-prefix-arg)
           (member major-mode '(sh-mode
                                emacs-lisp-mode lisp-mode
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
;; MATH DEFUNS

(defun deg2rad (x)
  "Converts degrees to radians"
  (/ (* x float-pi) 180.0))

(defun rad2deg (x)
  "Converts radians to degrees"
  (/ (* x 180.0) float-pi))

(defun fov2focal (fov)
  "Evaluates dimensionless focal length given fov in radians"
  (/ 1.0 (tan (/ fov 2.0))))

(defun focal2fov (fov)
  "Evaluates fov in radians given dimensionless focal length"
  (* 2.0 (atan (/ 1.0 fov))))

;; ------------------------------------------------------------
;; DEFUNS

(defun google-it ()
  "Google the selected region if any, display a query prompt
otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun lingvo-it ()
  "Translate the following region in lingvo, display a query
prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://lingvopro.abbyyonline.com/en/Translate/en-ru/"
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Lingvo: "))))))

(defmacro smart-isearch (direction)
  `(defun ,(intern (format "smart-isearch-%s" direction)) (&optional regexp-p no-recursive-edit)
     "If region is active and non empty, use it for searching and
make first jump. Otherwise, behave like original function."
     (interactive "P\np")
     (let ((smart-p (and
                     (region-active-p)
                     (< (region-beginning) (region-end))
                     (= (- (line-number-at-pos (region-end))
                           (line-number-at-pos (region-beginning))) 0)
                     )))
       (when smart-p
         (kill-ring-save (region-beginning) (region-end)))

       (,(intern (format "isearch-%s" direction)) regexp-p no-recursive-edit)

       (when smart-p
         (isearch-yank-kill)
         (,(intern (format "isearch-repeat-%s" direction)))))))
(define-key global-map [remap isearch-forward]  (smart-isearch forward))
(define-key global-map [remap isearch-backward] (smart-isearch backward))
(define-key global-map [remap occur]
  (defun smart-occur (arg)
    (interactive "P")
    (if (region-active-p)
        (occur (buffer-substring-no-properties (region-beginning) (region-end)) arg)
      (call-interactively 'occur))))

(define-key global-map [remap move-beginning-of-line]
  (defun smart-beginning-of-line ()
    "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
    (interactive)
    (let ((oldpos (point)))
      (back-to-indentation)
      (and (= oldpos (point))
           (beginning-of-line)))))

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

(defun increment-decimal-number-at-point (&optional arg)
  "Increment the number at point by `arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun parent-directory (dir)
  "Returns parent directory of dir"
  (when dir
    (file-name-directory (directory-file-name (expand-file-name dir)))))

(defun search-file-up (name &optional path)
  "Searches for file `name' in parent directories recursively"
  (let* ((file-name (concat path name))
         (parent (parent-directory path))
         (path (or path default-directory)))
    (cond
     ((file-exists-p file-name) file-name)
     ((string= parent path) nil)
     (t (search-file-up name parent)))))

(defun add-sudo-to-filename (filename)
  "Adds sudo proxy to filename for use with TRAMP.

Works for both local and remote hosts (>=23.4). The syntax used
for remote hosts follows the pattern
'/ssh:you@remotehost|sudo:remotehost:/path/to/file'. Some people
say, that you may need to call smth like
`(set-default 'tramp-default-proxies-alist (quote ((\".*\"
\"\\`root\\'\" \"/ssh:%u@%h:\"))))', but it works for me just fine
without it. "
  (with-temp-buffer
    (insert filename)
    (end-of-buffer)
    (if (re-search-backward "@\\(.*\\):" nil t)
        (let ((remote-name (buffer-substring (match-beginning 1) (match-end 1))))
          (goto-char (match-end 1))
          (insert (concat "|sudo:" remote-name))
          (beginning-of-buffer)
          (forward-char)
          (when (looking-at "scpc")
            (delete-char 4)
            (insert "ssh"))
          (buffer-string))
      (concat "/sudo::" filename))))

(defun update-tags-file (arg)
  "Suggests options to update the TAGS file via ctags.

With prefix arg - makes a call as sudo. Works for remote hosts
also (>=23.4)"
  (interactive "P")
  (let ((tags-file-name
         (read-file-name
          "TAGS file: " (let ((fn (search-file-up "TAGS" default-directory)))
                          (if fn
                              (parent-directory fn)
                            default-directory))
          nil nil "TAGS"))
        (ctags-command "")
        (languages (case major-mode
                     ((cc-mode c++-mode c-mode) "--languages=C,C++")
                     ((d-mode) "--languages=D")
                     (t ""))))
    (when tags-file-name
      (setq ctags-command (concat ctags-command "cd " (replace-regexp-in-string ".*:" "" (file-name-directory tags-file-name)) " && ")))

    (setq ctags-command (concat ctags-command "ctags -e " languages " -R . "))

    (with-temp-buffer
      (when arg
        (cd (add-sudo-to-filename (expand-file-name default-directory))))
      (shell-command (read-from-minibuffer "ctags command: "  ctags-command)))
    (visit-tags-table tags-file-name)))

(defun sudo-edit-current-file (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (let ((position (point)))
      (find-alternate-file (add-sudo-to-filename buffer-file-name))
      (goto-char position))))

(defun configure-theme ()
  "Make Emacs pretty"
  (load-theme 'tango-dark)
  (enable-theme 'tango-dark)
  ;; make background a little darker
  (set-background-color "#1d1f21")

  ;; require term mode after the theme is set
  (when (require 'term)
    (defcustom term-remote-hosts '()
      "List of remote hosts")

    (defcustom ssh-config-filename "~/.ssh/config"
      "ssh config filename")
    (defun term-parse-ssh-config ()
      "Parse `ssh-config-filename' to provide `remote-term'
completion capabilities."
      (interactive)
      (setq term-remote-hosts '())
      (if (file-exists-p ssh-config-filename)
          (with-temp-buffer
            (find-file ssh-config-filename)
            (goto-char (point-min))
            (while (re-search-forward "Host\\s-+\\([^\s]+\\)$" nil t)
              (let ((host (match-string-no-properties 1)))
                (add-to-list 'term-remote-hosts `(,host "ssh" ,host))))
            (kill-buffer))))
    (term-parse-ssh-config)

    (defun remote-term-do (new-buffer-name cmd &rest switches)
      "Fires a remote terminal"
      (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
      (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
      (setq term-ansi-buffer-name (apply 'term-ansi-make-term term-ansi-buffer-name cmd nil switches))
      (set-buffer term-ansi-buffer-name)
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x)
      (switch-to-buffer term-ansi-buffer-name))

    (defun remote-term (host-name)
      (interactive
       (list (completing-read "Remote host: " term-remote-hosts)))
      (dolist (known-host term-remote-hosts)
        (when (equal (car known-host) host-name)
          (apply 'remote-term-do known-host))))

    (add-hook 'term-mode-hook
              '(lambda ()
                 (yas-minor-mode -1)))

    (define-key term-mode-map "\C-x\C-j"   'dired-jump-universal-other)
    (define-key term-raw-escape-map "\C-j" 'dired-jump-universal-other)
    (define-key term-raw-escape-map "\C-l" 'term-line-mode)
    (define-key term-mode-map "\C-x\C-k"   'term-char-mode))

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
    (async-shell-command "explorer ."))
  (when (equal window-system 'x)
    (async-shell-command "nautilus .")))

(defun dired-goto-file-ido (file)
  "Use ido-read-file-name in dired-goto-file"
  (interactive
   (prog1                          ; let push-mark display its message
       (list (expand-file-name
          (ido-read-file-name "Goto file: " ; use ido-read-file-name
                  (dired-current-directory))))
     (push-mark)))
  (dired-goto-file file))

(define-key dired-mode-map (vector 'remap 'end-of-buffer)
  (defun dired-jump-to-bottom ()
    "Jumps to the last file"
    (interactive)
    (end-of-buffer)
    (dired-previous-line 1)))

(define-key dired-mode-map (vector 'remap 'beginning-of-buffer)
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
        (dired-next-line 1))))

(define-key global-map (vector 'remap 'dired-jump)
  (defun dired-jump-universal-other (arg)
    "Calls dired-jump. With prefix argument uses other window"
    (interactive "P")
    (dired-jump arg)))

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

(define-key global-map [remap open-line]
  (defun open-line-indent (arg)
    "Use newline-and-indent in open-line command if there are
non-whitespace characters after the point"
    (interactive "P")
    (save-excursion
      (if (looking-at-p "\\s-*$") ;; how in earth does this work?
          (newline arg)
        (newline-and-indent)))))

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

(defun grep-dispatch (arg)
  "With prefix calls `git-grep' and `find-grep' otherwise"
  (interactive "P")
  (if arg
      (call-interactively 'git-grep)
    (call-interactively 'find-grep)))

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

(defmacro action-dispatch (action)
  `(defun ,(intern (format "%s-dispatch" action)) (arg)
     "Perform action on word or region."
     (interactive "P")
     (if (region-active-p)
         (,(intern (format "%s-region" action)) (region-beginning) (region-end))
       (,(intern (format "%s-word" action)) (if arg arg 1)))))

(define-key global-map [remap upcase-word]     (action-dispatch upcase))
(define-key global-map [remap downcase-word]   (action-dispatch downcase))
(define-key global-map [remap capitalize-word] (action-dispatch capitalize))

(defun eval-dispatch (arg)
  "Evaluate previous sexp or region"
  (interactive "P")
  (if (region-active-p)
      (let ((edebug-all-forms arg))
        (eval-region (region-beginning) (region-end) t))
    (eval-and-replace)))

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

(define-key global-map [remap jump-to-register]
  (defun jump-to-register-with-save (register &optional delete)
    "Like jump-to-register, but saves current window configuration
to predefined register"
    (interactive "cJump to register: \nP")
    ;; autosave current window configuration unless we're jumping back
    (unless (equal register pop-predefined-register)
      (window-configuration-to-register pop-predefined-register))
    (jump-to-register register delete)))

(defun replace-path-with-truename ()
  "Replaces the region or the path around point with its true name.

To get the true name it follows the symbolic links and converts
relative paths to absolute."
  (interactive)
  (let (bds p1 p2 inputStr resultStr)
    ;; get current selection or filename
    (if (region-active-p)
        (setq bds (cons (region-beginning) (region-end) ))
      (setq bds (bounds-of-thing-at-point 'filename)))
    (setq p1 (car bds))
    (setq p2 (cdr bds))

    ;; grab the string
    (setq fn (buffer-substring-no-properties p1 p2)  )

    (if (file-exists-p fn)
        (progn
          (delete-region p1 p2 )
          (insert (file-truename fn)))
      (message "Path \"%s\" doesn't exist" fn))))

(defun find-function-push-tag (function)
  "This function is meant as a drop-in replacement for find-tag
in emacs-lisp-mode. It calls find-function and inserts current
position into find-tag-marker-ring."
  (require 'etags)
  (interactive (find-function-read))
  (ring-insert find-tag-marker-ring (point-marker))
  (find-function function))

;; ------------------------------------------------------------
;; CUSTOMIZED

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-buffer (quote new-buffer))
 '(c-basic-offset 4)
 '(c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (d-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(calendar-week-start-day 1)
 '(compilation-scroll-output (quote first-error))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(create-lockfiles nil)
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
 '(fill-column 80)
 '(frame-background-mode (quote dark))
 '(grep-find-command (quote ("find . -type f -exec grep -nHi -e  {} +" . 35)))
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill)))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(initial-major-mode (quote emacs-lisp-mode))
 '(initial-scratch-message nil)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-verbosity nil)
 '(org-agenda-files (quote ("~/Dropbox/Private/org/")))
 '(org-clock-mode-line-total (quote current))
 '(org-confirm-babel-evaluate nil)
 '(org-directory "~/Dropbox/Private/org")
 '(org-hide-leading-stars t)
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
 '(org-src-fontify-natively t)
 '(org-startup-indented t)
 '(org-support-shift-select (quote always))
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
 '(dired-async-in-process-face ((t (:background "cornflower blue"))) t)
 '(dired-async-message ((t (:inherit mode-line-emphasis))))
 '(dired-async-mode-message ((t (:inherit highlight))))
 '(ediff-current-diff-A ((t (:background "white" :foreground "black"))) t)
 '(ediff-current-diff-Ancestor ((t (:background "white" :foreground "black"))) t)
 '(ediff-current-diff-B ((t (:background "white" :foreground "black"))) t)
 '(ediff-current-diff-C ((t (:background "white" :foreground "black"))) t)
 '(ediff-even-diff-A ((t (:background "antique white" :foreground "Black"))) t)
 '(ediff-even-diff-Ancestor ((t (:background "antique white" :foreground "black"))) t)
 '(ediff-even-diff-B ((t (:background "antique white" :foreground "black"))) t)
 '(ediff-even-diff-C ((t (:background "antique white" :foreground "Black"))) t)
 '(ediff-fine-diff-A ((t (:background "gainsboro" :foreground "blue"))) t)
 '(ediff-fine-diff-Ancestor ((t (:background "gainsboro" :foreground "red"))) t)
 '(ediff-fine-diff-B ((t (:background "gainsboro" :foreground "forest green"))) t)
 '(ediff-fine-diff-C ((t (:background "gainsboro" :foreground "purple"))) t)
 '(ediff-odd-diff-A ((t (:background "antique white" :foreground "black"))) t)
 '(ediff-odd-diff-Ancestor ((t (:background "antique white" :foreground "black"))) t)
 '(ediff-odd-diff-B ((t (:background "antique white" :foreground "Black"))) t)
 '(ediff-odd-diff-C ((t (:background "antique white" :foreground "black"))) t)
 '(magit-item-highlight ((t (:background "black"))))
 '(term-color-black ((t (:background "#1d1f21" :foreground "#1d1f21"))))
 '(term-color-blue ((t (:background "#81a2be" :foreground "#81a2be"))))
 '(term-color-green ((t (:background "firebrick" :foreground "firebrick"))))
 '(term-color-magenta ((t (:background "#b294bb" :foreground "#b294bb"))))
 '(term-color-red ((t (:background "#cc6666" :foreground "#cc6666"))))
 '(term-color-white ((t (:background "#c5c8c6" :foreground "#c5c8c6"))))
 '(term-color-yellow ((t (:background "#f0c674" :foreground "#f0c674")))))

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
(global-set-key (kbd "M-P")         'scroll-down-line)
(global-set-key (kbd "M-N")         'scroll-up-line)
(global-set-key (kbd "\C-c m")      'magit-status)
(global-set-key (kbd "\C-c RET")    'magit-status)
(global-set-key (kbd "\C-c s")      'swap-buffers-in-windows)
(global-set-key (kbd "\C-c\C-s")    'swap-buffers-in-windows)
(global-set-key (kbd "M-\"")        'double-quote-word)
(global-set-key (kbd "\C-c w")      'show-file-name)
(global-set-key (kbd "\C-x v a")    'vc-annotate)
(global-set-key (kbd "\C-x v b")    'vc-annotate)
(global-set-key (kbd "<f5>")        'revert-buffer)
(global-set-key (kbd "\C-c f")      'toggle-window-split)
(global-set-key (kbd "\C-c\C-f")    'toggle-window-split)
(global-set-key [(control shift f)] 'grep-dispatch)
(global-set-key (kbd "\C-x\C-e")    'eval-dispatch)
(global-set-key (kbd "M-\\")        'fixup-whitespace)
(global-set-key (kbd "C-M-h")       'backward-kill-word)
(global-set-key (kbd "M-h")         'backward-kill-word)
(global-set-key (kbd "M-/")         'hippie-expand)
(global-set-key (kbd "\C-x k")      'kill-this-buffer)
(global-set-key (kbd "C-+")         'org-list-repair)
(global-set-key (kbd "M-+")         'org-list-repair)
(global-set-key (kbd "C-x w")       'webjump)
(global-set-key (kbd "\C-x\C-r")    'rename-buffer)
(global-set-key (kbd "C-x t")       'toggle-truncate-lines)
(global-set-key (kbd "M-j")         'join-following-line)
(global-set-key (kbd "M-Z")         'zap-up-to-char)
(global-set-key (kbd "\C-x!")       'sudo-edit-current-file)
(global-set-key (kbd "\C-cg")       'google-it)
(global-set-key (kbd "\C-cl")       'lingvo-it)
(global-set-key (kbd "\C-c\C-o")    'find-file-at-point)
(global-set-key (kbd "C-z")         'undo)
(global-set-key (kbd "C-x /")       'replace-path-with-truename)
(global-set-key [escape]            'keyboard-escape-quit)
(global-set-key "\C-x\C-u"          'update-tags-file)
(global-set-key "\C-x\C-v"          'visit-tags-table)
(global-set-key "\C-x\C-t"          'tags-reset-tags-tables)
(global-set-key "\C-x\C-l"          'tags-apropos)
(global-set-key "\C-c\C-c"          'compile)
(global-set-key "\C-c+"             'increment-decimal-number-at-point)

;; define translations
(define-key key-translation-map [?\C-h] [?\C-?]) ;; translate C-h to DEL

;; C-/ is not representable with an ASCII control code, so it cannot be sent to
;; terminals, but it is a convenient keybinding for undo. So mapping it to
;; "traditional" undo sequence C-_ is a cute way around
(define-key key-translation-map [?\C-/] [?\C-_]) ;; translate C-/ to C-_

;; convenient binding for C-x C-s in org-src-mode
(add-hook 'org-src-mode-hook
          '(lambda ()
             (define-key org-src-mode-map (kbd "C-x C-s") 'org-edit-src-exit)))

(add-hook 'shell-mode-hook
          '(lambda ()
             (define-key shell-mode-map (kbd "\C-c\C-o") nil)))

(add-hook 'org-mode-hook
          '(lambda ()
             ;; don't redefine some bindings
             (define-key org-mode-map [C-tab]
               nil)
             (define-key org-mode-map (kbd "M-h")
               nil)
             ;; swap active/inactive time-stamp bindings
             (define-key org-mode-map (kbd "C-c .")
               'org-time-stamp-inactive)
             (define-key org-mode-map (kbd "C-c !")
               'org-time-stamp)))

(add-hook 'find-file-hook 'append-tramp-host)

(add-hook 'dired-mode-hook
          '(lambda()
             (append-tramp-host)

             ;; keep default behavior in dired
             (define-key dired-mode-map (kbd "C-x C-q")
               'dired-toggle-read-only)
             (define-key dired-mode-map (kbd "E")
               'open-window-manager)
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

(add-hook 'conf-mode-hook
          '(lambda ()
             (define-key conf-mode-map "\C-c\C-c"
               nil)))

(add-hook 'occur-mode-hook
          '(lambda ()
             (define-key occur-mode-map "\C-x\C-q"
               'occur-edit-mode)))

(add-hook 'occur-edit-mode-hook
          '(lambda ()
             (define-key occur-edit-mode-map "\C-x\C-q"
               'occur-cease-edit)))

(add-hook 'sh-mode-hook
          '(lambda ()
             (define-key sh-mode-map "\C-c\C-c"
               nil)
             (define-key sh-mode-map "\C-c\C-o"
               nil)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (define-key emacs-lisp-mode-map (kbd "M-.")
               'find-function-push-tag)))

(add-hook 'tar-mode-hook
          '(lambda ()
             (define-key tar-mode-map (kbd "g")
               (defun revert-buffer-without-query ()
                 (interactive)
                 (revert-buffer nil t)))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-base-map "\C-c\C-o"
               'ff-find-other-file)

             (define-key c-mode-base-map "\C-c\C-c"    nil)
             (define-key c-mode-base-map (kbd "C-M-h") nil)
             (define-key c-mode-base-map (kbd "M-j")   nil)

             ;; hs-mode
             (hs-minor-mode t)
             (define-key c-mode-base-map "\C-ch"
               'hs-toggle-hideshow-all)
             ;; set //-style comments for c-mode
             (setq comment-start "//" comment-end "")))

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-to-list 'auto-mode-alist '("\\.abbrev_defs\\'" . emacs-lisp-mode))

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

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

;; ftp dumb hosts
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

;; Show keystrokes in progress
(setq echo-keystrokes 0.01)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Revolt, outrage, revolution! No double spaces in the end of sentences.
(set-default 'sentence-end-double-space nil)

;; make backspace to always delete chars
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
(define-key isearch-mode-map [escape] 'isearch-cancel)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; RecognizeCamelCaseSubwording
(global-subword-mode)
;; don't remap some commands
(define-key subword-mode-map (vector 'remap 'transpose-words) nil)
(define-key subword-mode-map (vector 'remap 'upcase-word) nil)
(define-key subword-mode-map (vector 'remap 'downcase-word) nil)

;; dired listing switches
(setq dired-listing-switches (concat "-alh"
                                     (when (not (equal window-system 'w32))
                                       " --group-directories-first")))

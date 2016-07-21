;;-*-coding: utf-8;-*-
(define-abbrev-table '2048-mode-abbrev-table '())

(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Custom-mode-abbrev-table '())

(define-abbrev-table 'Info-edit-mode-abbrev-table '())

(define-abbrev-table 'apropos-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'bibtex-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-edit-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'calc-trail-mode-abbrev-table '())

(define-abbrev-table 'calendar-mode-abbrev-table '())

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'cmake-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'conf-colon-mode-abbrev-table '())

(define-abbrev-table 'conf-javaprop-mode-abbrev-table '())

(define-abbrev-table 'conf-ppd-mode-abbrev-table '())

(define-abbrev-table 'conf-space-mode-abbrev-table '())

(define-abbrev-table 'conf-unix-mode-abbrev-table '())

(define-abbrev-table 'conf-windows-mode-abbrev-table '())

(define-abbrev-table 'conf-xdefaults-mode-abbrev-table '())

(define-abbrev-table 'css-mode-abbrev-table '())

(define-abbrev-table 'd-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'debugger-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'dos-mode-abbrev-table '())

(define-abbrev-table 'edebug-eval-mode-abbrev-table '())

(define-abbrev-table 'edit-abbrevs-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'ert-results-mode-abbrev-table '())

(define-abbrev-table 'ert-simple-view-mode-abbrev-table '())

(define-abbrev-table 'eshell-mode-abbrev-table '())

(define-abbrev-table 'eww-bookmark-mode-abbrev-table '())

(define-abbrev-table 'eww-history-mode-abbrev-table '())

(define-abbrev-table 'eww-mode-abbrev-table '())

(define-abbrev-table 'flycheck-error-list-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'gdb-breakpoints-mode-abbrev-table '())

(define-abbrev-table 'gdb-disassembly-mode-abbrev-table '())

(define-abbrev-table 'gdb-frames-mode-abbrev-table '())

(define-abbrev-table 'gdb-locals-mode-abbrev-table '())

(define-abbrev-table 'gdb-memory-mode-abbrev-table '())

(define-abbrev-table 'gdb-registers-mode-abbrev-table '())

(define-abbrev-table 'gdb-script-mode-abbrev-table '())

(define-abbrev-table 'gdb-threads-mode-abbrev-table '())

(define-abbrev-table 'gfm-mode-abbrev-table '())

(define-abbrev-table 'git-commit-mode-abbrev-table '())

(define-abbrev-table 'git-rebase-mode-abbrev-table '())

(define-abbrev-table 'gitconfig-mode-abbrev-table '())

(define-abbrev-table 'gitignore-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("char" "(char-to-string (+ ?x ))" nil 0)
    ("date" "(format-time-string \"%d, %b, %Y\")" nil 0)
    ("today" "(format-time-string \"%d, %b, %Y\")" nil 0)
    ("units" "(calc-eval (math-convert-units (calc-eval \"29ft + 8in\" 'raw) (calc-eval \"m\" 'raw)))" nil 0)
   ))

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'gud-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'howdoi-mode-abbrev-table '())

(define-abbrev-table 'html-mode-abbrev-table '())

(define-abbrev-table 'ibuffer-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'js-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'log4e-mode-abbrev-table '())

(define-abbrev-table 'magit-branch-manager-mode-abbrev-table '())

(define-abbrev-table 'magit-cherry-mode-abbrev-table '())

(define-abbrev-table 'magit-commit-mode-abbrev-table '())

(define-abbrev-table 'magit-diff-mode-abbrev-table '())

(define-abbrev-table 'magit-log-mode-abbrev-table '())

(define-abbrev-table 'magit-log-select-mode-abbrev-table '())

(define-abbrev-table 'magit-merge-preview-mode-abbrev-table '())

(define-abbrev-table 'magit-mode-abbrev-table '())

(define-abbrev-table 'magit-popup-mode-abbrev-table '())

(define-abbrev-table 'magit-popup-sequence-mode-abbrev-table '())

(define-abbrev-table 'magit-process-mode-abbrev-table '())

(define-abbrev-table 'magit-reflog-mode-abbrev-table '())

(define-abbrev-table 'magit-refs-mode-abbrev-table '())

(define-abbrev-table 'magit-revision-mode-abbrev-table '())

(define-abbrev-table 'magit-stash-mode-abbrev-table '())

(define-abbrev-table 'magit-stashes-mode-abbrev-table '())

(define-abbrev-table 'magit-status-mode-abbrev-table '())

(define-abbrev-table 'magit-wazzup-mode-abbrev-table '())

(define-abbrev-table 'makefile-automake-mode-abbrev-table '())

(define-abbrev-table 'makefile-bsdmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-gmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-imake-mode-abbrev-table '())

(define-abbrev-table 'makefile-makepp-mode-abbrev-table '())

(define-abbrev-table 'makefile-mode-abbrev-table '())

(define-abbrev-table 'markdown-mode-abbrev-table '())

(define-abbrev-table 'message-mode-abbrev-table '())

(define-abbrev-table 'messages-buffer-mode-abbrev-table '())

(define-abbrev-table 'nroff-mode-abbrev-table '())

(define-abbrev-table 'nxml-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'org-export-stack-mode-abbrev-table '())

(define-abbrev-table 'org-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'pike-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("class" "class" nil 0)
    ("def" "def" nil 0)
    ("for" "for" nil 0)
    ("if" "if" nil 0)
    ("try" "try" nil 0)
    ("while" "while" nil 0)
   ))

(define-abbrev-table 'python-mode-skeleton-abbrev-table
  '(
   ))

(define-abbrev-table 'reb-lisp-mode-abbrev-table '())

(define-abbrev-table 'reb-mode-abbrev-table '())

(define-abbrev-table 'rxt-help-mode-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sgml-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table
  '(
    ("ccache" "CC=/usr/lib/ccache/gcc CXX=/usr/lib/ccache/g++" nil 0)
    ("cmake" "cmake -GNinja .." nil 0)
    ("convert-scale" "convert in.jpg -scale 1280x720\! out.jpg" nil 0)
    ("convert-crop" "convert in.jpg -crop '745x745+0+70' out.jpg" nil 0)
    ("convert-gif" "convert -delay 50 *.jpg out.gif" nil 0)
    ("du" "du -sh * | sort -hr" nil 0)
    ("ffmpeg-add-audio" "ffmpeg -i video.avi -i audio.mp3 -codec copy -shortest output.avi" nil 0)
    ("ffmpeg-crop" "ffmpeg -i in.mp4 -b:v 10000k -r 24 -filter_complex \"[0:v:0]crop=w=100:h=100:x=12:y=34\" out.mp4" nil 0)
    ("ffmpeg-cut" "ffmpeg -i input.mkv -ss 00:00:30.0 -c copy -t 00:00:10.0 output.mkv" nil 0)
    ("ffmpeg-from-images" "ffmpeg -i %04d.jpg -b:v 10000k -r 24 -f mp4 out.mp4" nil 0)
    ("ffmpeg-grab" "ffmpeg -f alsa -ac 2 -i pulse -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv" nil 0)
    ("ffmpeg-scale" "ffmpeg -i in.mp4 -b:v 10000k -vf scale=320:240 out.mp4" nil 0)
    ("ffmpeg-side-by-side" "ffmpeg -i left.mp4 -i right.mp4 -b:v 10000k -r 24 -filter_complex \"[0:v:0]pad=iw*2:ih[bg]; [bg][1:v:0]overlay=w\" out.mp4" nil 0)
    ("ffmpeg-to-images" "ffmpeg -i in.mp4 -f image2 %04d.jpg" nil 0)
    ("ffmpeg-top-bottom" "ffmpeg -i top.mp4 -i bottom.mp4 -b:v 10000k -r 24 -filter_complex \"[0:v]pad=iw:ih*2[bg]; [bg][1:v]overlay=0:h\" out.mp4" nil 0)
    ("ln" "ln -s target link" nil 0)
    ("mencoder-concat" "mencoder -oac copy -ovc copy -idx -o output.mp4 *.mp4" nil 0)
    ("sshgen" "ssh-keygen -t rsa -C \"your_email@example.com\"" nil 0)
   ))

(define-abbrev-table 'snippet-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'speedbar-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'tar-mode-abbrev-table '())

(define-abbrev-table 'term-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'url-cookie-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-hg-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

(define-abbrev-table 'yaml-mode-abbrev-table '())

;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'bibtex-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-edit-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("tostring" "#include <sstream>
template <typename T>
std::string to_string(const T& obj)
{
    std::ostringstream oss;
    oss << obj;
    return oss.str();
}
" nil 0)
    ("savepbm" "std::vector<int> params;
params.push_back(CV_IMWRITE_PXM_BINARY);
params.push_back(0); // 1 for binary format, 0 for ascii format
cv::imwrite(\"image.pbm\", image, params); // the .pbm extension specifies the encoding format
" nil 0)
    ("cout" "std::cout << v << std::endl;" nil 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("timer" "TIMER(name, \"name\");
TIMERON(name);
TIMEROFF(name);
" nil 0)
   ))

(define-abbrev-table 'calendar-mode-abbrev-table '())

(define-abbrev-table 'change-log-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'conf-colon-mode-abbrev-table '())

(define-abbrev-table 'conf-javaprop-mode-abbrev-table '())

(define-abbrev-table 'conf-ppd-mode-abbrev-table '())

(define-abbrev-table 'conf-space-mode-abbrev-table '())

(define-abbrev-table 'conf-unix-mode-abbrev-table '())

(define-abbrev-table 'conf-windows-mode-abbrev-table '())

(define-abbrev-table 'conf-xdefaults-mode-abbrev-table '())

(define-abbrev-table 'd-mode-abbrev-table
  '(
    ("read" "T read(T)()
{
    T t;
    readf(\" %s\", &t);
    return t;
}" nil 0)
   ))

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'dos-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'gfm-mode-abbrev-table '())

(define-abbrev-table 'git-commit-mode-abbrev-table '())

(define-abbrev-table 'git-rebase-mode-abbrev-table '())

(define-abbrev-table 'gitconfig-mode-abbrev-table '())

(define-abbrev-table 'gitignore-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table
  '(
    ("ln" "ln -s target link" nil 0)
    ("avconvimages" "avconv -i %04d.jpg -b:v 10000k -r 24 -f mp4 out.mp4" nil 0)
    ("ccache" "CC=/usr/lib/ccache/gcc CXX=/usr/lib/ccache/g++" nil 0)
    ("cmake" "cmake -GNinja .." nil 0)
    ("run-at-time" "(run-at-time \"15 sec\" nil '(lambda () (call-process \"notify-send\" nil nil nil \"Time's up!\")))" nil 0)
   ))

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'howdoi-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'log-edit-mode-abbrev-table '())

(define-abbrev-table 'magit-branch-manager-mode-abbrev-table '())

(define-abbrev-table 'magit-cherry-mode-abbrev-table '())

(define-abbrev-table 'magit-commit-mode-abbrev-table '())

(define-abbrev-table 'magit-diff-mode-abbrev-table '())

(define-abbrev-table 'magit-log-mode-abbrev-table '())

(define-abbrev-table 'magit-mode-abbrev-table '())

(define-abbrev-table 'magit-process-mode-abbrev-table '())

(define-abbrev-table 'magit-reflog-mode-abbrev-table '())

(define-abbrev-table 'magit-status-mode-abbrev-table '())

(define-abbrev-table 'magit-wazzup-mode-abbrev-table '())

(define-abbrev-table 'markdown-mode-abbrev-table '())

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

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table
  '(
    ("bashdir" "DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" && pwd )\"" nil 0)
    ("bashroot" "if [[ $(id -u) -ne 0 ]] ; then echo \"Please, run as sudo\" ; exit 1 ; fi" nil 0)
   ))

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'term-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

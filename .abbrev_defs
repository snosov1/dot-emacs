;;-*-coding: utf-8;-*-
(define-abbrev-table '2048-mode-abbrev-table '())

(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Custom-mode-abbrev-table '())

(define-abbrev-table 'apropos-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'bibtex-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-edit-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("cout" "std::cout << v << std::endl;" nil 0)
    ("euleranglescpp" "
/** this conversion uses NASA standard aeroplane conventions as described on page:
 *   http://www.euclideanspace.com/maths/geometry/rotations/euler/index.htm
 *   Coordinate System: right hand
 *   Positive angle: right hand
 *   Order of euler angles: heading first, then attitude, then bank
 *   matrix row column ordering:
 *   [m00 m01 m02]
 *   [m10 m11 m12]
 *   [m20 m21 m22]*/
cv::Matx33f euler2mat(const cv::Vec3f &euler)
{
    double heading = euler[0];
    double attitude = euler[1];
    double bank = euler[2];

    // Assuming the angles are in radians.
    double ch = cos(heading);
    double sh = sin(heading);
    double ca = cos(attitude);
    double sa = sin(attitude);
    double cb = cos(bank);
    double sb = sin(bank);

    double m00 = ch * ca;
    double m01 = sh*sb - ch*sa*cb;
    double m02 = ch*sa*sb + sh*cb;
    double m10 = sa;
    double m11 = ca*cb;
    double m12 = -ca*sb;
    double m20 = -sh*ca;
    double m21 = sh*sa*cb + ch*sb;
    double m22 = -sh*sa*sb + ch*cb;

    return cv::Matx33f(m00, m01, m02,
                       m10, m11, m12,
                       m20, m21, m22);
}

/** this conversion uses conventions as described on page:
 *   http://www.euclideanspace.com/maths/geometry/rotations/euler/index.htm
 *   Coordinate System: right hand
 *   Positive angle: right hand
 *   Order of euler angles: heading first, then attitude, then bank
 *   matrix row column ordering:
 *   [m00 m01 m02]
 *   [m10 m11 m12]
 *   [m20 m21 m22]*/
cv::Vec3f mat2euler(const cv::Matx33f &m) {
    // Assuming the angles are in radians.
	if (m(1,0) > 0.998) { // singularity at north pole
		double heading = atan2(m(0,2), m(2,2));
		double attitude = M_PI/2;
		double bank = 0;
		return cv::Vec3f(heading, attitude, bank);
	}
	if (m(1,0) < -0.998) { // singularity at south pole
		double heading = atan2(m(0,2), m(2,2));
		double attitude = -M_PI/2;
		double bank = 0;
		return cv::Vec3f(heading, attitude, bank);
	}

	double heading  = atan2(-m(2,0), m(0,0));
	double attitude = asin ( m(1,0));
	double bank     = atan2(-m(1,2), m(1,1));

    return cv::Vec3f(heading, attitude, bank);
}
" nil 0)
    ("savepbm" "std::vector<int> params;
params.push_back(CV_IMWRITE_PXM_BINARY);
params.push_back(0); // 1 for binary format, 0 for ascii format
cv::imwrite(\"image.pbm\", image, params); // the .pbm extension specifies the encoding format
" nil 0)
    ("tostring" "#include <sstream>
template <typename T>
std::string to_string(const T& obj)
{
    std::ostringstream oss;
    oss << obj;
    return oss.str();
}
" nil 0)
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

(define-abbrev-table 'd-mode-abbrev-table '())

(define-abbrev-table 'diff-mode-abbrev-table '())

(define-abbrev-table 'dos-mode-abbrev-table '())

(define-abbrev-table 'edebug-eval-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'ert-results-mode-abbrev-table '())

(define-abbrev-table 'ert-simple-view-mode-abbrev-table '())

(define-abbrev-table 'eshell-mode-abbrev-table '())

(define-abbrev-table 'flycheck-error-list-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'gfm-mode-abbrev-table '())

(define-abbrev-table 'git-commit-mode-abbrev-table '())

(define-abbrev-table 'git-rebase-mode-abbrev-table '())

(define-abbrev-table 'gitconfig-mode-abbrev-table '())

(define-abbrev-table 'gitignore-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table '())

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'howdoi-mode-abbrev-table '())

(define-abbrev-table 'html-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'inferior-python-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table '())

(define-abbrev-table 'js-mode-abbrev-table '())

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

(define-abbrev-table 'nroff-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table '())

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'org-export-stack-mode-abbrev-table '())

(define-abbrev-table 'org-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'pike-mode-abbrev-table '())

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

(define-abbrev-table 'reb-lisp-mode-abbrev-table '())

(define-abbrev-table 'reb-mode-abbrev-table '())

(define-abbrev-table 'rxt-help-mode-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'sgml-mode-abbrev-table '())

(define-abbrev-table 'sh-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table
  '(
    ("ffmpeg-images" "ffmpeg -i %04d.jpg -b:v 10000k -r 24 -f mp4 out.mp4" nil 0)
    ("ffmpeg-side-by-side" "ffmpeg -i left.mp4 -i right.mp4 -b:v 10000k -r 24 -filter_complex \"[0:v:0]pad=iw*2:ih[bg]; [bg][1:v:0]overlay=w\" out.mp4")
    ("ffmpeg-top-bottom" "ffmpeg -i left.mp4 -i right.mp4 -b:v 10000k -r 24 -filter_complex \"[0:v]pad=iw:ih*2[bg]; [bg][1:v]overlay=0:h\" out.mp4")
    ("ccache" "CC=/usr/lib/ccache/gcc CXX=/usr/lib/ccache/g++" nil 0)
    ("cmake" "cmake -GNinja .." nil 0)
    ("du" "du -sh * | sort -hr" nil 0)
    ("ln" "ln -s target link" nil 0)
    ("convert-gif" "convert -delay 50 *.jpg out.gif" nil 0)
    ("mencoder-concat" "mencoder -oac copy -ovc copy -idx -o output.mp4 *.mp4" nil 0)
    ("notify-send" "notify-send \"HELLO FROM EMACS\"" nil 0)
    ("run-at-time" "(run-at-time \"15 sec\" nil '(lambda () (call-process \"notify-send\" nil nil nil \"Time's up!\")))" nil 0)
    ))

(define-abbrev-table 'snippet-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'tar-mode-abbrev-table '())

(define-abbrev-table 'term-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-hg-log-view-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

(define-abbrev-table 'yaml-mode-abbrev-table '())

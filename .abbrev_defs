;;-*-coding: utf-8;-*-
(define-abbrev-table 'global-abbrev-table
  '(
    ("char" "(char-to-string (+ ?x ))" nil :count 0)
    ("date" "(format-time-string \"%d, %b, %Y\")" nil :count 0)
    ("nocaps" "setxkbmap -option \"ctrl:nocaps\"" nil :count 0)
    ("today" "(format-time-string \"%d, %b, %Y\")" nil :count 0)
    ("units" "(calc-eval (math-convert-units (calc-eval \"29ft + 8in\" 'raw) (calc-eval \"m\" 'raw)))" nil :count 0)
   ))

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("class" "class" nil :count 0)
    ("def" "def" nil :count 0)
    ("for" "for" nil :count 0)
    ("if" "if" nil :count 0)
    ("try" "try" nil :count 0)
    ("while" "while" nil :count 0)
   ))

(define-abbrev-table 'shell-mode-abbrev-table
  '(
    ("ccache" "CC=/usr/lib/ccache/gcc CXX=/usr/lib/ccache/g++" nil :count 0)
    ("cmake" "cmake -GNinja .." nil :count 0)
    ("convert-crop" "convert in.jpg -crop '745x745+0+70' out.jpg" nil :count 0)
    ("convert-gif" "convert -delay 50 *.jpg out.gif" nil :count 0)
    ("convert-random" "convert -size 1280x720 xc: +noise Random random.png" nil :count 0)
    ("convert-scale" "convert in.jpg -scale 1280x720! out.jpg" nil :count 0)
    ("convert-side-by-side" "convert image1.png image2.png +append output.png" nil :count 0)
    ("du" "du -sh * | sort -hr" nil :count 0)
    ("ffmpeg-add-audio" "ffmpeg -i video.avi -i audio.mp3 -codec copy -shortest output.avi" nil :count 0)
    ("ffmpeg-crop" "ffmpeg -i in.mp4 -b:v 10000k -r 24 -filter_complex \"[0:v:0]crop=w=100:h=100:x=12:y=34\" out.mp4" nil :count 0)
    ("ffmpeg-cut" "ffmpeg -i input.mkv -ss 00:00:30.0 -c copy -t 00:00:10.0 output.mkv" nil :count 0)
    ("ffmpeg-from-images" "ffmpeg -i %04d.jpg -b:v 10000k -r 24 -f mp4 out.mp4" nil :count 0)
    ("ffmpeg-grab" "ffmpeg -f alsa -ac 2 -i pulse -f x11grab -r 30 -s 1920x1080 -i :0.0 -acodec pcm_s16le -vcodec libx264 -preset ultrafast -crf 0 -y output.mkv" nil :count 0)
    ("ffmpeg-scale" "ffmpeg -i in.mp4 -b:v 10000k -vf scale=320:240 out.mp4" nil :count 0)
    ("ffmpeg-side-by-side" "ffmpeg -i left.mp4 -i right.mp4 -b:v 10000k -r 24 -filter_complex \"[0:v:0]pad=iw*2:ih[bg]; [bg][1:v:0]overlay=w\" out.mp4" nil :count 0)
    ("ffmpeg-to-images" "ffmpeg -i in.mp4 -f image2 %04d.jpg" nil :count 0)
    ("ffmpeg-top-bottom" "ffmpeg -i top.mp4 -i bottom.mp4 -b:v 10000k -r 24 -filter_complex \"[0:v]pad=iw:ih*2[bg]; [bg][1:v]overlay=0:h\" out.mp4" nil :count 0)
    ("git-fetch-pr" "git fetch origin pull/ID/head:BRANCHNAME" nil :count 0)
    ("ln" "ln -s target link" nil :count 0)
    ("mencoder-concat" "mencoder -oac copy -ovc copy -idx -o output.mp4 *.mp4" nil :count 0)
    ("pip-install-recursive" "find . -name \"requirements.txt\" -type f -exec pip install -r '{}' ';'" nil :count 0)
    ("ssh-keygen" "ssh-keygen -t rsa -C \"your_email@example.com\"" nil :count 0)
   ))


set -e # exit on error
set -u # disallow unset variables usage

git_version=2.4.5
emacs_version=24.5
dir=~/.dev-setup

OPTIND=1
INSTALL=1
while getopts "n" opt; do
    case "$opt" in
    n)
            INSTALL=0
    esac
done

mkdir -p $dir
cd $dir

if [ $INSTALL -eq 1 ]; then
    echo "Install git $git_version"
    wget https://github.com/git/git/archive/v$git_version.tar.gz
    tar -xzvf v$git_version.tar.gz
    rm v$git_version.tar.gz
    cd git-$git_version
    make -j4
    sudo make install
    cd ..

    echo "Install Emacs $emacs_version"
    sudo apt-get -y install build-essential
    sudo apt-get -y build-dep emacs24
    wget http://ftp.gnu.org/gnu/emacs/emacs-$emacs_version.tar.gz
    tar -xzvf emacs-$emacs_version.tar.gz
    rm emacs-$emacs_version.tar.gz
    cd emacs-$emacs_version
    ./configure # --prefix=/opt/emacs
    make -j4
    sudo make install
    cd ..

    git clone https://github.com/snosov1/ctags-d.git
    cd ctags-d
    ./configure
    make -j4
    sudo make install
    cd ..

    # bind Caps Lock to ctrl
    echo 'remove Lock = Caps_Lock
remove Control = Control_L
keysym Caps_Lock = Control_L
add Control = Control_L' >/tmp/.xmodmap
    echo 'xmodmap ~/.xmodmap' >/tmp/.xsession
    cp -b /tmp/.xmodmap ~/.xmodmap
    cp -b /tmp/.xsession ~/.xsession
fi

# emacs config
if [ -d "dot-emacs" ]; then
    echo "Update Emacs config"
    cd dot-emacs
    git pull
    emacs --batch --script $dir/dot-emacs/create-links.el
    cd ..
else
    echo "Install Emacs config"
    git clone https://github.com/snosov1/dot-emacs.git
    emacs --batch --script $dir/dot-emacs/create-links.el
fi

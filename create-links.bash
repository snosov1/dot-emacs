DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ ! -f ~/.emacs ]; then
    ln -s $DIR/.emacs ~/.emacs
fi

if [ ! -f ~/emacs-init.org ]; then
    ln -s $DIR/emacs-init.org ~/emacs-init.org
fi

if [ ! -f ~/.mc-lists.el ]; then
    ln -s $DIR/.mc-lists.el ~/.mc-lists.el
fi

if [ ! -f ~/.abbrev_defs ]; then
    ln -s $DIR/.abbrev_defs ~/.abbrev_defs
fi

if [ ! -d ~/.yasnippets ]; then
    ln -s $DIR/.yasnippets ~/.yasnippets
fi

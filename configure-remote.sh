# add public key to authorized_keys
cat ~/.ssh/id_rsa.pub | ssh $1 'mkdir -p .ssh && cat - >>.ssh/authorized_keys'

# prepend .profile
cat .profile | ssh $1 'cp .profile .profile.sergei.bak && cat - .profile >.profile.sergei.emacs.dired && cp .profile.sergei.emacs.dired .profile'

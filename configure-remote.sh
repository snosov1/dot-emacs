# prepend .profile
cat .profile | ssh $1 'cp .profile .profile.sergei.bak && cat - .profile >.profile.sergei.emacs.dired && cp .profile.sergei.emacs.dired .profile'

# add public key to authorized_keys
ssh $1 mkdir .ssh
ssh $1 touch .ssh/authorized_keys
cat ~/.ssh/id_rsa.pub | ssh $1 'cat - >> .ssh/authorized_keys'

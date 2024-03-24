
#!/usr/bin/env bash

# set -e # quit after any error

echo "*** -> Resetting Docker State..."
./reset.sh # destory any existing container
echo "*** -> Booting and Building AC..."
./boot.sh # rebuild the container from cache (or nothing)
          # and start it as a daemon
echo "*** -> Starting a development session..."
#/emacs.sh # start an emacs session from within the container

docker exec -it aesthetic-container //usr/bin/fish -c "emacsclient -e '(kill-emacs)'"
docker exec -it aesthetic-container //usr/bin/fish -c "emacs --daemon"
docker exec -it aesthetic-container //usr/bin/fish -c "emacsclient -c --eval '(aesthetic-backend)'"

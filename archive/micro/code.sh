#!/usr/bin/env bash

# set -e # quit after any error

echo "*** -> Resetting Docker State..."

# destroy any existing container
./reset.sh

echo "*** -> Booting and Building AC..."
./boot.sh # rebuild the container from cache (or nothing)
# and start it as a daemon

echo "*** -> Starting a development session..."
# /emacs.sh # start an emacs session from within the container

# add ssl certificates to the host
# cd ..;
# npm run add:ssl;
# cd micro/machine;

if [ "$1" = "fish" ]; then
  docker exec --detach-keys='ctrl-z,z' -it aesthetic-container //usr/bin/fish
else
  docker exec -it aesthetic-container //usr/bin/fish -c "emacsclient -e '(kill-emacs)'"
  docker exec -it aesthetic-container //usr/bin/fish -c "emacs --daemon"
  docker exec --detach-keys='ctrl-z,z' -it aesthetic-container //usr/bin/fish -c "emacsclient -c --eval '(aesthetic-backend (quote \"$1\"))'"
  # Always jump back to the prompt after quitting emacsclient.
  # docker exec --detach-keys='ctrl-z,z' -it aesthetic-container //usr/bin/fish
fi

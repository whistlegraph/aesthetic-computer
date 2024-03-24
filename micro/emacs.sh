#!/usr/bin/env bash

echo "Emacs client here we go..."

docker exec -it aesthetic-container //usr/bin/fish -c "fish"

# docker exec -it aesthetic-container //usr/bin/fish -c "emacsclient -e '(kill-emacs)'; emacs --daemon; emacsclient -c"
# docker exec -it aesthetic-container //usr/bin/fish -c "fish /entry.fish"

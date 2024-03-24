#!/usr/bin/env bash

docker exec -it aesthetic-container //usr/bin/fish -c "fish /entry.fish; emacsclient -e '(kill-emacs)'; emacs --daemon; emacsclient -c"

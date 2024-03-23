#!/usr/bin/env bash

docker exec -it aesthetic-container //usr/bin/fish -c "emacsclient -e '(kill-emacs)'; emacs --daemon; emacsclient -c --eval '(aesthetic-backend)'"

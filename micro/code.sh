
#!/usr/bin/env bash

set -e # quit after any error

./reset.sh # destory any existing container
./boot.sh # rebuild the container from cache (or nothing)
          # and start it as a daemon
./emacs.sh # start an emacs session from within the container

#!/usr/bin/env bash

parentDir=$(dirname $(pwd))
cd machine
if [[ $parentDir =~ ^/[a-zA-Z]/ ]]; then
    parentDir=$(echo $parentDir | sed 's|^/\([a-zA-Z]\)/|\1:/|')
fi

if [ -z "$(docker ps -aq -f name=aesthetic-container)" ]; then
    npm run build && \
    docker run -d -p 8888:8888 -p 8889:8889 --hostname aesthetic \
    --env-file .env --env-file github.env --env-file netlify.env \
    --env-file vercel.env --env-file stripe.env \
    -e GIT_USER_EMAIL="$(git config --global user.email)" \
    -e GIT_USER_NAME="$(git config --global user.name)" \
    -v "${parentDir}/micro/machine/.emacs.d:/home/me/.emacs.d" \
    -v "${parentDir}:/home/me/aesthetic-computer" \
    --user me --platform linux/amd64 --name aesthetic-container \
    -it aesthetic-micro /usr/bin/fish entry.fish
else
    docker start aesthetic-container
fi

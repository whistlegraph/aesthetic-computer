#!/usr/bin/env bash

cd machine; docker ps -q -a --filter 'name=aesthetic-container' | xargs -r docker rm -f; docker rmi aesthetic-micro -f

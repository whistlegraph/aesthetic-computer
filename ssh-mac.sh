#!/usr/bin/env bash
# Quick SSH to Mac build machine with password already set
export SSHPASS='BuildM@chine'
exec sshpass -e ssh falsework@host.docker.internal "$@"

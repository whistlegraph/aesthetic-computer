#!/bin/bash
export PATH="/usr/sbin:$PATH"
exec chrome-devtools-mcp "$@"

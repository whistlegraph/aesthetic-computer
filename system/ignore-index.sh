#!/bin/bash

# Temporarily move index.html out of the public directory
if [ -f public/index.html ]; then
  mv public/index.html public/index.html.bak
fi

# Ensure the build proceeds
exit 1
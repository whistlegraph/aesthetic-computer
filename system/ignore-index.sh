#!/bin/bash

# Skip builds if only submodules/feral-file repos changed
if [ -n "${COMMIT_REF:-}" ] && [ -n "${CACHED_COMMIT_REF:-}" ]; then
  changed_files=$(git diff --name-only "$CACHED_COMMIT_REF" "$COMMIT_REF" || true)
  if [ -n "$changed_files" ]; then
    non_submodule_changes=$(echo "$changed_files" | grep -Ev '^(feed/dp1-feed|feral-file)(/|$)' || true)
    if [ -z "$non_submodule_changes" ]; then
      # Ignore build when only submodules or feral-file repos changed
      exit 0
    fi
  fi
fi

# Temporarily move index.html out of the public directory
if [ -f public/index.html ]; then
  mv public/index.html public/index.html.bak
fi

# Ensure the build proceeds
exit 1
#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p Resources/Session/easel
for tree in phone src context; do
  rsync -a --delete "../../easel/$tree/" "Resources/Session/easel/$tree/"
done

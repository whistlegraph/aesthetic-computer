#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p Resources/Session/easel
for tree in phone src context; do
  rsync -a --delete "../../easel/$tree/" "Resources/Session/easel/$tree/"
done
# Use the desktop's sanitized rich-text renderer on iPhone as well.
mkdir -p Resources/Session/easel/desktop/vendor
rsync -a --delete ../../easel/desktop/vendor/rich/ Resources/Session/easel/desktop/vendor/rich/
cp ../../easel/desktop/{transcript,reply-details}.js Resources/Session/easel/desktop/
cp ../../easel/desktop/rich.css Resources/Session/easel/desktop/

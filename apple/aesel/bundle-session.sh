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
cp ../../easel/desktop/{transcript,reply-details,notebook-donkey}.js Resources/Session/easel/desktop/
cp ../../easel/desktop/rich.css Resources/Session/easel/desktop/
# The desktop's pencil companion sheet, drawn natively by AeselDonkey.
mkdir -p Resources/Session/easel/desktop/assets
cp ../../easel/desktop/assets/donkey-pencil-run-v2.png Resources/Session/easel/desktop/assets/

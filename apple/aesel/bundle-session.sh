#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
rm -rf Resources/Session/easel/desktop
mkdir -p Resources/Session/easel
for tree in phone src context; do
  rsync -a --delete "../../easel/$tree/" "Resources/Session/easel/$tree/"
done
# Use the shared renderer's sanitized rich-text renderer on iPhone as well.
mkdir -p Resources/Session/easel/shared/vendor
rsync -a --delete ../../easel/shared/vendor/rich/ Resources/Session/easel/shared/vendor/rich/
cp ../../easel/shared/{transcript,reply-details,notebook-donkey}.js Resources/Session/easel/shared/
cp ../../easel/shared/rich.css Resources/Session/easel/shared/
# The shared renderer's pencil companion sheet, drawn natively by AeselDonkey.
mkdir -p Resources/Session/easel/shared/assets
cp ../../easel/shared/assets/donkey-pencil-run-v2.png Resources/Session/easel/shared/assets/
cp ../../easel/shared/assets/donkey-pencil-thinking-v1.png Resources/Session/easel/shared/assets/

#!/bin/sh
# Rebuild preview.css (fonts + refresh.css) and inject.js for headless preview.
# inject.js = preview-cv.js (the CV pages + counts, from build-cv.mjs) + refresh-data.js + refresh.js.
cd "$(dirname "$0")"
node build-cv.mjs --preview >/dev/null || exit 1
FONTS='https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,400..600;1,14..32,400..600&family=Newsreader:ital,opsz,wght@0,6..72,400..600;1,6..72,400..600&display=swap'
{ echo "@import url(\"$FONTS\");"; cat refresh.css; } > preview.css
node -e "
const fs=require('fs');const css=fs.readFileSync('preview.css','utf8');const js=fs.readFileSync('preview-cv.js','utf8')+fs.readFileSync('refresh-data.js','utf8')+fs.readFileSync('refresh.js','utf8');
fs.writeFileSync('inject.js', 'document.getElementById(\"tl-refresh-preview\")?.remove();var s=document.createElement(\"style\");s.id=\"tl-refresh-preview\";s.textContent='+JSON.stringify(css)+';document.body.appendChild(s);'+js);"

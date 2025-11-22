#!/usr/bin/env node

import { readFileSync, writeFileSync } from 'fs';
import { gzipSync } from 'zlib';

const html = readFileSync('keep-bundles/cow-minimal-nft.html', 'utf8');
const compressed = gzipSync(html, { level: 9 });
const base64 = compressed.toString('base64');

const selfContained = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>$cow • Aesthetic Computer</title>
  <style>
    body{margin:0;background:#000;overflow:hidden}
  </style>
</head>
<body>
  <script>
    fetch('data:application/gzip;base64,${base64}')
      .then(r=>r.blob())
      .then(b=>b.stream().pipeThrough(new DecompressionStream('gzip')))
      .then(s=>new Response(s).text())
      .then(h=>{document.open();document.write(h);document.close();});
  </script>
</body>
</html>`;

console.log('Original HTML:', html.length, 'bytes');
console.log('Compressed:   ', compressed.length, 'bytes');
console.log('Base64:       ', base64.length, 'bytes');
console.log('Self-contained:', selfContained.length, 'bytes =', Math.round(selfContained.length/1024), 'KB');

if (selfContained.length <= 256000) {
  console.log('\n✅ Fits in 256 KB!');
} else {
  console.log('\n❌ Does not fit in 256 KB. Size:', Math.round(selfContained.length/1024), 'KB');
  console.log('   Overage:', Math.round((selfContained.length - 256000)/1024), 'KB');
}

writeFileSync('keep-bundles/cow-self-contained.html', selfContained);
console.log('\n📝 Written to: keep-bundles/cow-self-contained.html');

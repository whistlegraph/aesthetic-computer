import {readFile, writeFile} from 'node:fs/promises';
import assert from 'node:assert/strict';

// Patch a fresh production backup: the October 5 checkout predates live swipe,
// animation, automatic rotation and caption changes and must not replace them.
const [input, output] = process.argv.slice(2);
assert.ok(input && output && input !== output, 'Usage: node build.mjs live-backup.php output.php');
const source = await readFile(input, 'utf8');
const css = await readFile(new URL('./stable-controls.css', import.meta.url), 'utf8');
const fit = `    function fit() {
      const available=carousel.parentElement.getBoundingClientRect().width;
      const header=document.querySelector('#tl-site-header')?.getBoundingClientRect().height||90;
      // Reserve caption space without measuring the current slide. Both axes
      // depend only on the viewport, so portrait and landscape works share it.
      const height=Math.max(200,Math.floor(innerHeight-header-160));
      figure.style.width=Math.floor(Math.min(available,height*1.5))+'px';
      link.style.height=height+'px';
    }
`;
const oldFit = /    function fit\(\) \{[\s\S]*?\n    \}\n(?=    async function show\()/g;
assert.equal([...source.matchAll(oldFit)].length, 1, 'Expected one current slideshow fit function');
assert.ok(!source.includes('/* Stable carousel controls. */'), 'Already patched');
const oldStyle = /(<style id="tl-quality-css">)([\s\S]*?)(<\/style>)/;
assert.ok(oldStyle.test(source), 'Expected quality stylesheet');
const result = source.replace(oldFit, () => fit).replace(oldStyle,
  (_, open, body, close) => open + body + '\n/* Stable carousel controls. */\n' + css + close);
await writeFile(output, result);

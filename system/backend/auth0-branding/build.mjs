// Inline local assets: the hosted login never needs a development server.
import { readFile, writeFile } from 'node:fs/promises';
const here = new URL('./', import.meta.url);
const template = new URL('template.html', here);
const css = await readFile(new URL('login.css', here), 'utf8');
const fit = await readFile(new URL('fit.js', here), 'utf8');
const html = (await readFile(template, 'utf8'))
  .replace(/(<style id="ac-login-style">)[\s\S]*?(<\/style>)/, (_, a, b) => `${a}\n${css}  ${b}`)
  .replace(/(<script id="ac-login-fit-script">)[\s\S]*?(<\/script>)/, (_, a, b) => `${a}\n${fit}  ${b}`);
await writeFile(template, html);

import { readFile, writeFile } from "node:fs/promises";

const here = new URL("./", import.meta.url);
await import('./build-tracks.mjs');
const module = await readFile(new URL("notepat.mjs", here), "utf8");
const example = await readFile(new URL("example.strudel", here), "utf8");
const standalone = "(() => {\n" + module.replace(/^export /gm, "")
  + "\n})()\n\n" + example.replace(/^await import\([^\n]+\)\n/m, "");
await writeFile(new URL("notepat-paste.strudel", here), standalone);
for (const [name, source] of [["example", example], ["standalone", standalone]]) {
  const url = "https://strudel.cc/#" + Buffer.from(source).toString("base64");
  await writeFile(new URL(`${name}.url`, here), url + "\n");
}
console.log("Built standalone source and Strudel example links.");
for (const name of ['marimbaba', 'marimbaba-orbit', 'stone']) {
  const source = await readFile(new URL(name + '.strudel', here), 'utf8');
  const paste = '(() => {\n' + module.replace(/^export /gm, '')
    + '\n})()\n\n' + source.replace(/^await import\([^\n]+\)\n/m, '');
  await writeFile(new URL(name + '-paste.strudel', here), paste);
  for (const [suffix, code] of [['', source], ['-paste', paste]]) {
    await writeFile(new URL(name + suffix + '.url', here), 'https://strudel.cc/#' + Buffer.from(code).toString('base64') + '\n');
  }
}

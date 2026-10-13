#!/usr/bin/env node
import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { resolve } from 'node:path';
import { readAlongBundle, readAlongAssets } from '../lib/read-along-pack.mjs';
const [jsonFile, audioFile, fontFile, destination] = process.argv.slice(2);
if (!destination) throw new Error('usage: read-along-pack.mjs episode.json master.mp3 unifont.hex.gz output-directory');
const episode = JSON.parse(readFileSync(jsonFile)), audio = readFileSync(audioFile);
const assets = readAlongAssets(episode, audio);
const { html, source, glyphs } = await readAlongBundle(episode, { fontFile });
mkdirSync(destination, { recursive: true });
writeFileSync(resolve(destination, 'index.html'), html);
writeFileSync(resolve(destination, 'source.lisp'), source);
for (const file of assets) writeFileSync(resolve(destination, file.name), file.content);
console.log(JSON.stringify({ directory: resolve(destination), glyphs, words: episode.words.length, bytes: Buffer.byteLength(html) }));

#!/usr/bin/env node
// Build readalong data from an existing master and measured Whisper word JSON.
// node read-along.mjs daily.md words.json 12384 episode.mp3 episode.json
import { readFileSync, writeFileSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { alignWords } from '../lib/read-along.mjs';
import { richDailyPiece, readRichDailySource } from '../lib/daily-richtext.mjs';
import { KidLisp } from '../../../system/public/aesthetic.computer/lib/kidlisp.mjs';
import { richNode, richLink } from '../../../system/public/aesthetic.computer/lib/rich-text.mjs';
const [markdown, timingFile, offset, audioFile, output] = process.argv.slice(2);
if (!output || !Number.isFinite(Number(offset))) throw new Error('usage: read-along.mjs daily.md whisper-words.json offsetMs audio.mp3 output.json');
const md = readFileSync(markdown, 'utf8');
const title = md.match(/^title: (.+)$/m)?.[1];
const body = md.split(/\n---\n/).slice(1).join('\n---\n').trim();
if (!title || !body) throw new Error('Expected daily title and prose');
const source = richDailyPiece({ title, body });
if (readRichDailySource(source).body !== body) throw new Error('Daily prose did not round-trip');
const ast = new KidLisp().parse(source).find(form => form[0] === 'flow');
const literal = value => Array.isArray(value) ? richLink(literal(value[1]), literal(value[2])) : value.slice(1,-1);
const blocks = ast.slice(2).map(form => richNode('paragraph', form.slice(1).map(literal)));
const result = alignWords(body, JSON.parse(readFileSync(timingFile)).transcription, Number(offset));
const audio = { url: 'episode.mp3', sha256: createHash('sha256').update(readFileSync(audioFile)).digest('hex') };
writeFileSync(output, JSON.stringify({ version: 1, title, body, blocks, audio, ...result }, null, 2) + '\n');
console.log(JSON.stringify(result.alignment));

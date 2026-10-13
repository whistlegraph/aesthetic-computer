#!/usr/bin/env node
// Build readalong data from an existing master and measured Whisper word JSON.
// node read-along.mjs script.md full-master-words.json 0 master.mp3 episode.json
import { readFileSync, writeFileSync, existsSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { alignWhisperWords, sentenceCues, spokenTranscript } from '../lib/read-along.mjs';
import { richDailyPiece, readRichDailySource } from '../lib/daily-richtext.mjs';
import { KidLisp } from '../../../system/public/aesthetic.computer/lib/kidlisp.mjs';
import { richNode, richLink } from '../../../system/public/aesthetic.computer/lib/rich-text.mjs';
import { readAlongThemes } from '../lib/read-along-palette.mjs';
const [markdown, timingFile, offset, audioFile, output, timelineFile] = process.argv.slice(2);
if (!output || !Number.isFinite(Number(offset))) throw new Error('usage: read-along.mjs script.md whisper-words.json offsetMs audio.mp3 output.json [episode.spoken.json|body-only.srt]');
const md = readFileSync(markdown, 'utf8');
const title = md.match(/^title: (.+)$/m)?.[1];
let body = md.split(/\n---\n/).slice(1).join('\n---\n').trim();
if (!title || !body) throw new Error('Expected daily title and prose');
const timeline = timelineFile || audioFile.replace(/\.[^.]+$/, '.spoken.json');
if (!existsSync(timeline)) throw new Error('Expected the complete .spoken.json timeline beside the master (or pass its path)');
const narration = timeline.endsWith('.json') ? JSON.parse(readFileSync(timeline, 'utf8')) : null;
if (narration && narration.title !== title) throw new Error('Spoken timeline belongs to another episode');
const full = narration ? spokenTranscript(body, narration) : null;
if (full) body = full.body;
const source = richDailyPiece({ title, body });
if (readRichDailySource(source).body !== body) throw new Error('Daily prose did not round-trip');
const ast = new KidLisp().parse(source).find(form => form[0] === 'flow');
const literal = value => Array.isArray(value) ? richLink(literal(value[1]), literal(value[2])) : value.slice(1,-1);
const blocks = ast.slice(2).map(form => richNode('paragraph', form.slice(1).map(literal)));
const result = alignWhisperWords(body, JSON.parse(readFileSync(timingFile)).transcription, Number(offset), full?.sentences || sentenceCues(readFileSync(timeline, 'utf8')));
const audio = { url: 'episode.mp3', sha256: createHash('sha256').update(readFileSync(audioFile)).digest('hex') };
if (narration?.audioSha256 && narration.audioSha256 !== audio.sha256) throw new Error('Spoken timeline differs from the supplied master');
writeFileSync(output, JSON.stringify({ version: 1, title, show: narration?.show, body, blocks, sections: full?.sections, themes: readAlongThemes(body, title), durationMs: narration?.durationMs, audio, ...result }, null, 2) + '\n');
console.log(JSON.stringify(result.alignment));

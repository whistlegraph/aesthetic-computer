#!/usr/bin/env node
// learning-to-look.mjs — Robin Cameron and Sam Orpol's critique loop, run
// through Aesel's relay so several models can be compared on one reference.
//
//   node aesel/bin/learning-to-look.mjs --reference <image>
//     [--models anthropic/claude-opus-5-5,anthropic/claude-sonnet-5,...]
//     [--conditions W,W+P,W+P+O] [--rounds 3] [--critique notes.md] [--out dir]
//
// The three conditions are the revised study in "An Instrument in Hand":
//   W      the critique in words before each redraw (the essay's loop)
//   W+P    the same words plus the model's own render, with the Aesel rule:
//          inspect before claiming a fix
//   W+P+O  as W+P with <circle> and <ellipse> removed from the instrument
// Every SVG (element order is the stroke order), every render, every
// critique and the model's own one-line statement about the band are kept,
// so "said open" and "drew open" are two columns. Rounds are stateless
// relay turns: each redraw receives the previous SVG source and the
// critiques so far, not a running conversation.
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { resolve, join, extname } from 'node:path';
import { ACSession } from '../src/ac-session.mjs';
import { runPersonalTurn } from '../src/personal-relay.mjs';

const run = promisify(execFile);
const args = process.argv.slice(2);
const opt = (name, fallback) => {
  const i = args.indexOf(`--${name}`);
  return i >= 0 ? args[i + 1] : fallback;
};
const reference = opt('reference');
if (!reference) {
  console.error('usage: learning-to-look.mjs --reference <image> [--models a,b] [--conditions W,W+P,W+P+O] [--rounds 3]');
  process.exit(1);
}
// Ids the relay's Claude provider accepts (2026-11-03); its codex provider
// answered 401 for openai/gpt-5.4 and needs its own login first.
const models = opt('models', 'anthropic/claude-opus-5-5,anthropic/claude-sonnet-5,anthropic/claude-opus-5,anthropic/claude-haiku-4-5').split(',');
const conditions = opt('conditions', 'W,W+P,W+P+O').split(',');
const rounds = Number(opt('rounds', '3'));
const stamp = new Date().toISOString().slice(0, 16).replace(/[:T]/g, '-');
const out = resolve(opt('out', `tmp/learning-to-look/${stamp}`));

// The critique a person gives after each round. The default is the four
// observations recorded in the paper, in the order a teacher would say them;
// pass --critique with one paragraph per round to use other words.
const DEFAULT_CRITIQUE = [
  `The two long outer wires come out open; good, you can draw a curve that ends. The copper band where a jaw would be is closed in your drawing. In the sculpture it is open at the top. The two free rods are mirrored in yours; in the photograph the left rod stands nearly upright and the right one leans. Do it again.`,
  `Look at the band again before you change anything, and say whether it is open at the top in what you drew. Then redraw: the band open at the top, the rods unequal, and no oval around the head that is not in the sculpture.`,
];
const critiques = opt('critique')
  ? (await readFile(opt('critique'), 'utf8')).split(/\n\s*\n/).map((s) => s.trim()).filter(Boolean)
  : DEFAULT_CRITIQUE;

const DRAW = `Draw this sculpture as an SVG: viewBox 0 0 400 600, white background, black strokes, stroke-width 2, no fills. Draw what you see, not what you know a thing like this looks like. Element order is your stroke order.
First line of your reply: "BAND: open" or "BAND: closed", describing the copper band at the jaw in the drawing you are about to write. Then the SVG in a \`\`\`svg fence. Nothing else.`;

const INSTRUCTIONS = {
  W: 'You draw by writing SVG. Return only what is asked.',
  'W+P': 'You draw by writing SVG. Return only what is asked. When your previous render is shown to you, inspect it before claiming any fix: name what is wrong in that picture before you redraw.',
  'W+P+O': 'You draw by writing SVG. Return only what is asked. When your previous render is shown to you, inspect it before claiming any fix: name what is wrong in that picture before you redraw. The <circle> and <ellipse> elements are not available to you; every closed shape must be written as a <path>.',
};

const image = async (file) => ({
  type: 'image',
  source: { type: 'base64', media_type: extname(file) === '.jpg' ? 'image/jpeg' : 'image/png', data: (await readFile(file)).toString('base64') },
});
const svgOf = (text) => {
  const fence = text.match(/```(?:svg|xml|html)?\s*([\s\S]*?)```/);
  const body = fence ? fence[1] : text;
  const m = body.match(/<svg[\s\S]*<\/svg>/);
  return m ? m[0] : null;
};
const bandOf = (text) => (text.match(/BAND:\s*(open|closed)/i)?.[1] || '').toLowerCase() || null;
// Closures a script can count: a proxy beside the human's look, never in place of it.
const closures = (svg) => ({
  circles: (svg.match(/<circle\b/g) || []).length,
  ellipses: (svg.match(/<ellipse\b/g) || []).length,
  closedPaths: (svg.match(/<path\b[^>]*\bd="[^"]*[zZ]\s*"/g) || []).length,
  elements: (svg.match(/<(path|line|polyline|polygon|rect|circle|ellipse)\b/g) || []).length,
});

const session = new ACSession();
const token = () => session.token();
async function turn(model, instructions, content) {
  let text = '';
  const result = await runPersonalTurn({
    token,
    model,
    instructions,
    tools: [],
    content,
    onEvent: ({ method, params }) => {
      if (method === 'item/agentMessage/delta') text += params.delta || '';
    },
  });
  return String(result.text || text);
}

// Downscale the reference once so every turn carries the same pixels.
await mkdir(out, { recursive: true });
const ref = join(out, 'reference.png');
await run('magick', [reference, '-resize', '600x600>', ref]);
const refImage = await image(ref);
const summary = { reference, models, conditions, rounds, critiques, started: new Date().toISOString(), runs: [] };

for (const model of models) {
  for (const condition of conditions) {
    const dir = join(out, model.replace('/', '_'), condition.replace(/\+/g, 'p'));
    await mkdir(dir, { recursive: true });
    const record = { model, condition, rounds: [] };
    summary.runs.push(record);
    let previousSvg = null, previousPng = null;
    const said = [];
    for (let r = 1; r <= rounds; r++) {
      const parts = [refImage];
      let text = DRAW;
      if (r > 1) {
        const words = critiques[Math.min(r - 2, critiques.length - 1)];
        text = `Your previous SVG:\n\`\`\`svg\n${previousSvg}\n\`\`\`\n\nCritique: ${words}\n\n${condition !== 'W' ? 'Your previous render is attached after the reference. Inspect it before claiming any fix; one line naming what is wrong in it, then the BAND line, then the SVG.\n\n' : ''}${DRAW}`;
        if (condition !== 'W' && previousPng) parts.push(await image(previousPng));
      }
      parts.push({ type: 'text', text });
      const started = Date.now();
      let reply = '', svg = null, error = null;
      try {
        for (let attempt = 0; attempt < 3; attempt++) {
          reply = await turn(model, INSTRUCTIONS[condition], parts);
          svg = svgOf(reply);
          if (!svg) { error = 'no svg in reply'; continue; }
          if (condition === 'W+P+O' && /<(circle|ellipse)\b/.test(svg)) {
            error = 'used circle or ellipse';
            parts.splice(parts.length - 1, 1, { type: 'text', text: `${text}\n\nYour last reply used <circle> or <ellipse>, which are not available. Write every closed shape as a <path>.` });
            continue;
          }
          error = null;
          break;
        }
      } catch (e) {
        error = e.message;
      }
      const base = join(dir, `round${r}`);
      await writeFile(`${base}.reply.md`, reply);
      const entry = { round: r, seconds: Math.round((Date.now() - started) / 100) / 10, band: bandOf(reply), error };
      if (svg) {
        await writeFile(`${base}.svg`, svg);
        try {
          await run('rsvg-convert', ['-w', '400', '-h', '600', '--background-color', 'white', `${base}.svg`, '-o', `${base}.png`]);
          previousPng = `${base}.png`;
        } catch (e) {
          entry.error = `render: ${e.message.split('\n')[0]}`;
        }
        previousSvg = svg;
        entry.closures = closures(svg);
      }
      said.push(entry.band);
      record.rounds.push(entry);
      console.log(`${model} ${condition} round ${r}: band ${entry.band ?? '?'}${entry.closures ? ` · circles ${entry.closures.circles} ellipses ${entry.closures.ellipses} closed paths ${entry.closures.closedPaths}` : ''}${entry.error ? ` · ${entry.error}` : ''} · ${entry.seconds}s`);
      await writeFile(join(out, 'summary.json'), JSON.stringify(summary, null, 2));
    }
    // A contact sheet: the reference, then each round, to look at side by side.
    const tiles = [ref, ...record.rounds.map((e) => join(dir, `round${e.round}.png`))].filter(Boolean);
    try {
      await run('magick', ['montage', ...tiles, '-tile', `${tiles.length}x1`, '-geometry', '300x450+6+6', '-background', '#ddd', join(dir, 'sheet.png')]);
    } catch {}
  }
}
summary.finished = new Date().toISOString();
await writeFile(join(out, 'summary.json'), JSON.stringify(summary, null, 2));
console.log(`kept in ${out}`);

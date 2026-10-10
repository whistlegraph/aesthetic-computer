#!/usr/bin/env node
// A census of what Whistlegraph pieces actually use of the AC API, read off
// the real corpus (every version of every thread on the knot), so a piece
// plan format for consoles is drawn from the pieces the model writes and
// not from any one experiment. Static: acorn parses each source, a walk
// collects the API names a hook destructures, the members read off them,
// the primitives called, the event kinds asked for, loop depth in paint,
// and every browser global a plan could not provide.
//
//   MONGODB_CONNECTION_STRING=… MONGODB_NAME=… node apple/whistlegraph/tools/piece-api-census.mjs [out-dir]
//
// Writes piece-api-census.json (per-piece profiles + totals) and
// piece-api-census.md (the readable report) into out-dir
// (default apple/whistlegraph/reports/).
import {parse} from 'acorn';
import {writeFileSync, mkdirSync} from 'node:fs';
import {join} from 'node:path';
import {MongoClient} from 'mongodb';

const HOOKS = ['boot', 'paint', 'sim', 'act', 'leave', 'beat', 'meta', 'preview', 'icon', 'brush', 'lift', 'filter'];
const BROWSER_GLOBALS = ['fetch', 'window', 'document', 'localStorage', 'sessionStorage', 'navigator', 'setTimeout', 'setInterval', 'requestAnimationFrame', 'XMLHttpRequest', 'WebSocket', 'Worker', 'Image', 'Audio', 'AudioContext', 'OffscreenCanvas', 'WebAssembly', 'eval', 'Function', 'globalThis', 'self', 'location', 'alert', 'console'];
const LEVEL1 = new Set(['wipe', 'ink', 'box', 'line', 'circle', 'plot', 'point', 'write', 'screen', 'pen', 'event', 'num', 'help', 'paintCount', 'clock', 'api', 'line3d', 'poly', 'shape', 'lineAngle', 'oval', 'tri', 'flood', 'page', 'pixel', 'blend', 'resolution', 'ui', 'typeface', 'text', 'geo', 'needsPaint', 'noise16', 'noise', 'blur', 'zoom', 'spin', 'scroll', 'skip', 'paste', 'stamp', 'painting', 'pens', 'pointer', 'hud', 'colon', 'params', 'store', 'system', 'jump', 'send', 'cursor', 'shear', 'contrast', 'steal', 'copy', 'delay', 'seconds', 'simCount', 'sound', 'beep', 'speak', 'synth', 'play', 'bgm', 'preload', 'download', 'leaving', 'piece', 'slug', 'dark', 'hand', 'motion', 'cam', 'video', 'rec', 'tape', 'gamepad', 'keyboard']);
const PRIMITIVES = ['wipe', 'ink', 'box', 'line', 'circle', 'plot', 'point', 'write', 'poly', 'shape', 'oval', 'tri', 'lineAngle', 'flood', 'paste', 'stamp', 'blur', 'zoom', 'spin', 'scroll', 'noise16', 'form', 'line3d', 'page', 'pixel', 'blend', 'resolution', 'shear', 'contrast', 'steal', 'copy'];
const THREE_D = ['form', 'Form', 'CUBE', 'QUAD', 'TRI', 'TRIANGLE', 'ORIGIN', 'Camera', 'Dolly', 'line3d'];
// Hand-rolled 3D: a projection written in 2D calls. Counted, not a level of its own.
const SOFT_3D = /\b(fov|focal|project(?:ed|ion)?|zbuffer|zBuffer|depthSort|cameraZ|camZ|\bcam\b)/;

export function walk(node, visit, parent = null, depth = 0) {
  if (!node || typeof node.type !== 'string') return;
  if (visit(node, parent, depth) === false) return;
  for (const key of Object.keys(node)) {
    if (key === 'type' || key === 'loc' || key === 'start' || key === 'end') continue;
    const value = node[key];
    if (Array.isArray(value)) value.forEach(child => child && typeof child.type === 'string' && walk(child, visit, node, depth + 1));
    else if (value && typeof value.type === 'string') walk(value, visit, node, depth + 1);
  }
}

// What one source uses. Parse failures are a finding of their own.
export function profile(source) {
  let ast;
  try { ast = parse(source, {ecmaVersion: 'latest', sourceType: 'module'}); }
  catch (error) { return {parses: false, error: String(error.message || error).slice(0, 120), chars: source.length, lines: source.split('\n').length}; }
  const out = {parses: true, chars: source.length, lines: source.split('\n').length, hooks: [], api: new Set(), members: new Map(), calls: new Map(), events: new Set(), globals: new Set(), threeD: new Set(),
    imports: [], soft3d: SOFT_3D.test(source), paintLoopDepth: 0, paintLoops: 0, topLevelState: 0, mathRandom: 0, dateNow: 0, dynamicImport: 0, asyncHooks: 0, formCalls: 0, maxLoopBound: 0};
  const apiParams = new Map();   // function node → the identifier name holding the whole api, if any
  for (const node of ast.body) {
    if (node.type === 'ImportDeclaration') out.imports.push(node.source.value);
    if (node.type === 'VariableDeclaration') out.topLevelState += node.declarations.length;
    const fn = node.type === 'ExportNamedDeclaration' ? node.declaration : null;
    if (fn?.type === 'FunctionDeclaration' && HOOKS.includes(fn.id.name)) {
      out.hooks.push(fn.id.name); if (fn.async) out.asyncHooks++;
      const param = fn.params[0];
      if (param?.type === 'ObjectPattern') for (const p of param.properties) { if (p.type === 'Property') out.api.add(p.key.name || p.key.value); else if (p.type === 'RestElement') out.api.add('...rest'); }
      else if (param?.type === 'Identifier') apiParams.set(fn, param.name);
      if (fn.id.name === 'paint') {
        let depth = 0;
        walk(fn.body, (n, parent, d) => {
          if (/^(For|While|DoWhile|ForOf|ForIn)Statement$/.test(n.type)) {
            out.paintLoops++;
            let inside = 0; for (let p = n; p; p = p.__parent) inside++;
            n.__loop = (parent?.__loop || 0) + 1; depth = Math.max(depth, n.__loop);
            const bound = n.test?.right?.value ?? n.test?.left?.value; if (typeof bound === 'number') out.maxLoopBound = Math.max(out.maxLoopBound, bound);
          } else n.__loop = parent?.__loop || 0;
        });
        out.paintLoopDepth = depth;
      }
    }
    if (fn?.type === 'VariableDeclaration') for (const d of fn.declarations) if (HOOKS.includes(d.id?.name)) out.hooks.push(d.id.name);
  }
  const apiNames = new Set([...out.api, ...apiParams.values()]);
  const bump = (map, key) => map.set(key, (map.get(key) || 0) + 1);
  walk(ast, (n, parent) => {
    if (n.type === 'MemberExpression' && n.object.type === 'Identifier' && apiNames.has(n.object.name) && !n.computed) {
      const name = n.property.name; bump(out.members, n.object.name + '.' + name);
      if (apiParams.size && [...apiParams.values()].includes(n.object.name)) out.api.add(name);
    }
    if (n.type === 'CallExpression') {
      const callee = n.callee;
      if (callee.type === 'Identifier') { bump(out.calls, callee.name); if (callee.name === 'form') out.formCalls++; }
      else if (callee.type === 'MemberExpression' && !callee.computed) {
        const obj = callee.object.type === 'Identifier' ? callee.object.name : callee.object.type === 'MemberExpression' && callee.object.property?.name ? callee.object.property.name : '';
        const name = callee.property.name;
        if (obj) bump(out.calls, obj + '.' + name);
        if (name === 'is' && n.arguments[0]?.type === 'Literal') out.events.add(String(n.arguments[0].value));
        if (obj === 'Math' && name === 'random') out.mathRandom++;
        if (obj === 'Date' && name === 'now') out.dateNow++;
      } else if (callee.type === 'Import') out.dynamicImport++;
    }
    if (n.type === 'Identifier' && BROWSER_GLOBALS.includes(n.name) && !(parent?.type === 'MemberExpression' && parent.property === n && !parent.computed) && !(parent?.type === 'Property' && parent.key === n)) out.globals.add(n.name);
    if (n.type === 'Identifier' && THREE_D.includes(n.name) && !(parent?.type === 'Property' && parent.key === n && parent.value !== n)) out.threeD.add(n.name);
  });
  return {...out, api: [...out.api].sort(), members: Object.fromEntries([...out.members].sort((a, b) => b[1] - a[1])), calls: Object.fromEntries([...out.calls].sort((a, b) => b[1] - a[1])), events: [...out.events].sort(), globals: [...out.globals].sort(), threeD: [...out.threeD].sort()};
}

// Which plan level a piece would need, from what it uses.
export function level(p) {
  if (!p.parses) return 'unparsed';
  if (p.imports.length || p.dynamicImport || p.globals.some(g => !['console'].includes(g))) return 'unbounded';
  if (p.threeD.length || p.formCalls) return 'L3-3d';
  const outside = p.api.filter(name => !LEVEL1.has(name));
  if (outside.length) return 'L2-wide:' + outside.slice(0, 3).join(',');
  return 'L1-2d';
}

function table(rows, header) {
  return [`| ${header.join(' | ')} |`, `| ${header.map(() => '---').join(' | ')} |`, ...rows.map(r => `| ${r.join(' | ')} |`)].join('\n');
}

async function main() {
  const outDir = process.argv[2] || join(process.cwd(), 'apple/whistlegraph/reports');
  mkdirSync(outDir, {recursive: true});
  const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING); await client.connect();
  const db = client.db(process.env.MONGODB_NAME);
  const threads = await db.collection('walkieware-threads').find({}, {projection: {code: 1, owner: 1, ledger: 1, updatedAt: 1}}).toArray();
  const handles = new Map((await db.collection('@handles').find({_id: {$in: [...new Set(threads.map(t => t.owner))]}}, {projection: {handle: 1}}).toArray()).map(h => [h._id, h.handle]));
  await client.close();
  const pieces = [], heads = [];
  for (const t of threads) {
    const versions = (t.ledger?.versions || []).filter(v => v.id > 0 && typeof v.source === 'string' && v.source.trim());
    for (const v of versions) {
      const p = {code: t.code, owner: handles.get(t.owner) || 'other', version: v.id, head: v.id === t.ledger.head, request: String(v.request || '').split('\n')[0].slice(0, 80), ...profile(v.source)};
      p.level = level(p); pieces.push(p); if (p.head) heads.push(p);
    }
  }
  const count = (list, pick) => { const m = new Map(); for (const p of list) for (const k of pick(p)) m.set(k, (m.get(k) || 0) + 1); return [...m].sort((a, b) => b[1] - a[1]); };
  const pct = (n, of) => `${n} (${Math.round(100 * n / Math.max(1, of))}%)`;
  const apiHeads = count(heads, p => p.api || []), apiAll = count(pieces, p => p.api || []);
  const members = count(heads, p => Object.keys(p.members || {}));
  const calls = count(heads, p => Object.keys(p.calls || {}).filter(c => PRIMITIVES.includes(c) || c.includes('.')));
  const events = count(heads, p => p.events || []);
  const globals = count(heads, p => p.globals || []);
  const levels = count(heads, p => [p.level.split(':')[0]]);
  const hooks = count(heads, p => p.hooks || []);
  const threeD = count(heads, p => p.threeD || []);
  const sizes = heads.map(p => p.chars).sort((a, b) => a - b);
  const q = f => sizes[Math.min(sizes.length - 1, Math.floor(f * sizes.length))] || 0;
  const loops = count(heads, p => [String(p.paintLoopDepth ?? 0)]);
  const md = `# Whistlegraph piece API census

Read off the knot on ${new Date().toISOString().slice(0, 10)}: ${threads.length} threads by ${handles.size} handles, ${pieces.length} versions, ${heads.length} heads (the version each piece is at now). Heads are what a console would play; all versions show what the model reaches for while working. Made by \`apple/whistlegraph/tools/piece-api-census.mjs\`.

## Shape

| | min | p25 | median | p75 | max |
| --- | --- | --- | --- | --- | --- |
| head source, characters | ${q(0)} | ${q(.25)} | ${q(.5)} | ${q(.75)} | ${sizes.at(-1) || 0} |

Parses: ${pct(heads.filter(p => p.parses).length, heads.length)} of heads. Hand-rolled 3D in 2D calls: ${heads.filter(p => p.soft3d).length}. Async hooks: ${heads.filter(p => p.asyncHooks).length}. Imports: ${heads.filter(p => p.imports?.length).length}. Dynamic import: ${heads.filter(p => p.dynamicImport).length}. Math.random: ${heads.filter(p => p.mathRandom).length}. Date.now: ${heads.filter(p => p.dateNow).length}.

## Plan level a head would need

${table(levels.map(([k, n]) => [k, pct(n, heads.length)]), ['level', 'heads'])}

L1-2d: only 2D primitives, screen, pen, events, num/help. L2-wide: 2D plus API names outside that set (named). L3-3d: forms, cubes, a camera. unbounded: browser globals, imports, or dynamic import, which no plan can provide.

## Hooks exported

${table(hooks.map(([k, n]) => [k, pct(n, heads.length)]), ['hook', 'heads'])}

## API names destructured (heads)

${table(apiHeads.map(([k, n]) => [k, pct(n, heads.length), pct(apiAll.find(([a]) => a === k)?.[1] || 0, pieces.length)]), ['name', 'heads', 'all versions'])}

## Members read off API objects (heads)

${table(members.slice(0, 60).map(([k, n]) => [k, pct(n, heads.length)]), ['member', 'heads'])}

## Primitives and method calls (heads)

${table(calls.slice(0, 60).map(([k, n]) => [k, pct(n, heads.length)]), ['call', 'heads'])}

## Event kinds asked for

${table(events.map(([k, n]) => [k, pct(n, heads.length)]), ['event', 'heads'])}

## 3D identifiers

${table(threeD.map(([k, n]) => [k, pct(n, heads.length)]), ['identifier', 'heads'])}

## Browser globals (a plan cannot provide these)

${table(globals.length ? globals.map(([k, n]) => [k, pct(n, heads.length)]) : [['none', '0']], ['global', 'heads'])}

## Loop depth inside paint (heads)

${table(loops.sort((a, b) => Number(a[0]) - Number(b[0])).map(([k, n]) => [k, pct(n, heads.length)]), ['nesting', 'heads'])}

Largest numeric loop bound seen in a head paint: ${Math.max(0, ...heads.map(p => p.maxLoopBound || 0))}.

## Every head

${table(heads.sort((a, b) => b.chars - a.chars).map(p => [p.code, '@' + p.owner, 'v' + p.version, p.chars, p.level, (p.api || []).join(' '), p.request.replace(/\|/g, '/')]), ['code', 'owner', 'head', 'chars', 'level', 'api', 'last request'])}
`;
  writeFileSync(join(outDir, 'piece-api-census.md'), md);
  writeFileSync(join(outDir, 'piece-api-census.json'), JSON.stringify({threads: threads.length, versions: pieces.length, heads: heads.length, pieces}, null, 1));
  console.log(`${threads.length} threads, ${pieces.length} versions, ${heads.length} heads → ${outDir}`);
}
if (import.meta.url === `file://${process.argv[1]}`) main().catch(error => { console.error(error); process.exit(1); });

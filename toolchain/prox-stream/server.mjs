// prox-stream — forward a live prompt rock to a spectator as "vibes".
//
// Watches one Claude session transcript and streams a heavily filtered view of
// it over SSE. The spectator sees the shape of the work — prompts, replies, and
// which tools are firing — and nothing else.
//
//   node server.mjs --session <uuid> [--port 8791] [--path <secret>]
//
// SAFETY: this is the whole point of the file. A raw transcript carries memory
// injections, tool output, file contents, and whatever the operator happened to
// grep today. Everything here is deny-by-default:
//   · only `user` text and `assistant` text/tool_use survive the filter
//   · system-reminders and tool_result blocks are dropped whole
//   · tool_use emits the tool NAME only, never its input
//   · surviving text still goes through redact() before it leaves the process
//   · the tail starts at EOF, so prior history is never sent
//
// Stop the process and the stream is gone; there is no replay buffer.

import { createServer } from 'node:http';
import { createReadStream, statSync, watch } from 'node:fs';
import { randomBytes } from 'node:crypto';
import { createInterface } from 'node:readline';

const arg = (flag, fallback) => {
  const i = process.argv.indexOf(flag);
  return i > -1 ? process.argv[i + 1] : fallback;
};

const SESSION = arg('--session');
const PORT = Number(arg('--port', 8791));
const SECRET = arg('--path', randomBytes(9).toString('base64url'));
const PROJECT = arg('--project', '-Users-jas-aesthetic-computer');
const FILE = `${process.env.HOME}/.claude/projects/${PROJECT}/${SESSION}.jsonl`;

if (!SESSION) {
  console.error('usage: node server.mjs --session <uuid> [--port N] [--path secret]');
  process.exit(1);
}

// ── Redaction ──────────────────────────────────────────────────────────────
// Belt and braces. The block filter should already have dropped anything
// dangerous; this catches secrets that appear inside otherwise-fine prose.
const RULES = [
  [/\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b/g, '‹email›'],
  [/\b\+?\d{10,15}\b/g, '‹number›'],
  [/\b\d{6}\b/g, '‹code›'],
  [/\b(gh[pousr]_|sk-|xox[baprs]-|AKIA)[A-Za-z0-9_-]{10,}/g, '‹secret›'],
  [/\/Users\/[^\s"']+/g, '‹path›'],
  [/\b[0-9a-f]{32,}\b/gi, '‹hash›'],
];
const redact = (s) => RULES.reduce((acc, [re, to]) => acc.replace(re, to), s);

// ── Transcript → events ────────────────────────────────────────────────────
const textOf = (content) => {
  if (typeof content === 'string') return content;
  if (!Array.isArray(content)) return '';
  return content.filter((b) => b?.type === 'text').map((b) => b.text ?? '').join('\n');
};

const hasToolResult = (content) =>
  Array.isArray(content) && content.some((b) => b?.type === 'tool_result');

function toEvents(line) {
  let msg;
  try { msg = JSON.parse(line); } catch { return []; }
  const m = msg.message;
  if (!m) return [];

  if (msg.type === 'user') {
    // Tool results and harness injections are never the user talking.
    if (hasToolResult(m.content)) return [];
    const raw = textOf(m.content).trim();
    if (!raw || raw.includes('<system-reminder>') || raw.includes('<local-command')) return [];
    return [{ kind: 'prompt', text: redact(raw).slice(0, 2000) }];
  }

  if (msg.type === 'assistant') {
    const out = [];
    const say = textOf(m.content).trim();
    if (say) out.push({ kind: 'say', text: redact(say).slice(0, 4000) });
    for (const b of Array.isArray(m.content) ? m.content : []) {
      // Name only. The input can hold file contents, paths, or credentials.
      if (b?.type === 'tool_use') out.push({ kind: 'tool', text: b.name });
    }
    return out;
  }
  return [];
}

// `--dump` replays the whole transcript through the filter and prints what
// WOULD be sent, without serving anything. Use it to audit the filter against
// real history before pointing a spectator at a live session.
if (process.argv.includes('--dump')) {
  const rl = createInterface({ input: createReadStream(FILE, { encoding: 'utf8' }), crlfDelay: Infinity });
  let n = 0;
  rl.on('line', (line) => {
    for (const ev of toEvents(line)) {
      n++;
      console.log(`[${ev.kind}] ${ev.text.replace(/\n/g, ' ⏎ ').slice(0, 160)}`);
    }
  });
  rl.on('close', () => console.error(`\n-- ${n} events would be emitted --`));
} else {

// ── Fan-out ────────────────────────────────────────────────────────────────
const clients = new Set();
const send = (ev) => {
  const frame = `data: ${JSON.stringify({ ...ev, t: Date.now() })}\n\n`;
  for (const res of clients) res.write(frame);
};

let offset = 0;
try { offset = statSync(FILE).size; } catch { /* not created yet */ }

function drain() {
  let size;
  try { size = statSync(FILE).size; } catch { return; }
  if (size <= offset) { offset = Math.min(offset, size); return; }
  const stream = createReadStream(FILE, { start: offset, end: size - 1, encoding: 'utf8' });
  offset = size;
  const rl = createInterface({ input: stream, crlfDelay: Infinity });
  rl.on('line', (line) => { for (const ev of toEvents(line)) send(ev); });
}

try {
  watch(FILE, { persistent: true }, drain);
} catch {
  console.error(`waiting for ${FILE} to appear…`);
}
setInterval(drain, 1000).unref?.();

// ── Viewer ─────────────────────────────────────────────────────────────────
const PAGE = `<!doctype html><meta charset=utf-8>
<meta name=viewport content="width=device-width,initial-scale=1">
<title>prox · live</title>
<style>
 :root{color-scheme:dark}
 *{box-sizing:border-box}
 body{margin:0;background:#0b0d0a;color:#d8e0d2;
      font:14px/1.55 ui-monospace,SFMono-Regular,Menlo,monospace}
 header{position:sticky;top:0;display:flex;gap:.6rem;align-items:center;
        padding:.7rem 1rem;background:#0b0d0ae6;backdrop-filter:blur(6px);
        border-bottom:1px solid #1e2a18}
 .dot{width:9px;height:9px;border-radius:50%;background:#81bb1a;
      box-shadow:0 0 10px #81bb1a;animation:p 1.6s infinite}
 @keyframes p{50%{opacity:.35}}
 h1{font-size:13px;margin:0;letter-spacing:.18em;text-transform:uppercase;
    font-weight:500;color:#9fb28e}
 main{padding:1rem;max-width:56rem;margin:0 auto}
 .e{margin:0 0 .85rem;white-space:pre-wrap;word-break:break-word}
 .prompt{color:#e8f0e0;border-left:2px solid #81bb1a;padding-left:.7rem}
 .say{color:#94a888}
 .tool{color:#5d6b53;font-size:12px;letter-spacing:.06em}
 .tool::before{content:"⚙ "}
 footer{padding:2rem 1rem;text-align:center;color:#3f4a38;font-size:12px}
</style>
<header><span class=dot></span><h1>prox · live</h1></header>
<main id=log></main>
<footer>view-only · ends when the prox does</footer>
<script>
 const log = document.getElementById('log');
 new EventSource(location.pathname.replace(/\\/$/,'') + '/events').onmessage = (m) => {
   const d = JSON.parse(m.data);
   const el = document.createElement('div');
   el.className = 'e ' + d.kind;
   el.textContent = d.text;
   log.append(el);
   const near = innerHeight + scrollY >= document.body.scrollHeight - 160;
   if (near) scrollTo(0, document.body.scrollHeight);
 };
</script>`;

createServer((req, res) => {
  const url = new URL(req.url, 'http://x');
  if (!url.pathname.startsWith(`/${SECRET}`)) {
    res.writeHead(404).end('not found');
    return;
  }
  if (url.pathname.endsWith('/events')) {
    res.writeHead(200, {
      'Content-Type': 'text/event-stream',
      'Cache-Control': 'no-cache, no-transform',
      Connection: 'keep-alive',
      'X-Accel-Buffering': 'no',
    });
    res.write(': connected\n\n');
    clients.add(res);
    const beat = setInterval(() => res.write(': ping\n\n'), 15000);
    req.on('close', () => { clearInterval(beat); clients.delete(res); });
    return;
  }
  res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' }).end(PAGE);
}).listen(PORT, '127.0.0.1', () => {
  console.log(`prox-stream  session=${SESSION}`);
  console.log(`local  http://127.0.0.1:${PORT}/${SECRET}`);
  console.log(`path   /${SECRET}`);
});

}

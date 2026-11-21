#!/usr/bin/env node
// route-judge — play a trial's pieces blind and rate them.
//
//   node bin/route-judge.mjs [--runs ../tmp/route-trial/full] [--port 7790]
//
// Each task shows its request and its arms as letters, shuffled per task and
// fixed for the trial (the order is kept in ratings.json). Choosing a letter
// pushes that run's final piece onto a private code channel, and the frame
// below plays it the way a phone on that channel would. Rate each 1–5; the
// models show once every letter of the task has a rating. Ratings save as you go.
import "../src/env.mjs";
import { createServer } from "node:http";
import { existsSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";
import { ACSession, USER_AGENT } from "../src/ac-session.mjs";
import { randomChannel } from "../src/names.mjs";

const aesel = resolve(fileURLToPath(import.meta.url), "../..");
const { values } = parseArgs({ options: {
  runs: { type: "string", default: join(aesel, "../tmp/route-trial/full") },
  port: { type: "string", default: "7790" },
} });
const runs = resolve(values.runs);
const ratingsFile = join(runs, "ratings.json");
const SITE = "https://aesthetic.computer";
const session = new ACSession();
const channel = randomChannel();

// Every task with at least one run, its requests, and each arm's last source.
function load() {
  const tasks = [];
  for (const dir of readdirSync(runs, { withFileTypes: true }).filter((d) => d.isDirectory()).map((d) => d.name).sort()) {
    const arms = {};
    let task = null;
    for (const arm of readdirSync(join(runs, dir))) {
      const base = join(runs, dir, arm);
      if (!existsSync(join(base, "result.json"))) continue;
      const result = JSON.parse(readFileSync(join(base, "result.json"), "utf8"));
      task ??= existsSync(join(base, "task.json")) ? JSON.parse(readFileSync(join(base, "task.json"), "utf8")) : null;
      const sources = existsSync(join(base, "source")) ? readdirSync(join(base, "source")).filter((f) => /^turn-\d+\.mjs$/.test(f)).sort() : [];
      arms[arm] = { delivered: !!result.delivered, source: sources.length ? readFileSync(join(base, "source", sources.at(-1)), "utf8") : "" };
    }
    if (Object.keys(arms).length) tasks.push({ id: dir, requests: task?.requests || [], arms });
  }
  return tasks;
}

const tasks = load();
const saved = existsSync(ratingsFile) ? JSON.parse(readFileSync(ratingsFile, "utf8")) : {};
for (const task of tasks) {
  saved[task.id] ??= { order: Object.keys(task.arms).sort(() => Math.random() - 0.5), scores: {}, notes: "" };
}
const save = () => writeFileSync(ratingsFile, JSON.stringify(saved, null, 2) + "\n");
save();

// What the page may see: letters, never arm names until a task is fully rated.
function view() {
  return tasks.map((task) => {
    const r = saved[task.id], letters = r.order.map((_, i) => "ABCDEFGH"[i]);
    const done = letters.every((l) => r.scores[l]);
    return { id: task.id, requests: task.requests, letters, scores: r.scores, notes: r.notes,
      reveal: done ? Object.fromEntries(letters.map((l, i) => [l, { arm: r.order[i], delivered: task.arms[r.order[i]].delivered }])) : null,
      blank: Object.fromEntries(letters.map((l, i) => [l, task.arms[r.order[i]].source.split("\n").length < 12])) };
  });
}

async function push(taskId, letter) {
  const task = tasks.find((t) => t.id === taskId), r = saved[taskId];
  const arm = r.order["ABCDEFGH".indexOf(letter)];
  const source = task?.arms[arm]?.source;
  if (!source) throw Error("no source for that letter");
  const token = await session.token();
  const response = await fetch(`${SITE}/run`, {
    method: "POST",
    headers: { "Content-Type": "application/json", "User-Agent": USER_AGENT, ...(token ? { Authorization: `Bearer ${token}` } : {}) },
    body: JSON.stringify({ piece: `judge-${letter.toLowerCase()}`, source, codeChannel: channel }),
  });
  if (!response.ok) throw Error(`push failed (HTTP ${response.status})`);
}

const body = (req) => new Promise((done) => { let s = ""; req.on("data", (d) => (s += d)); req.on("end", () => done(s ? JSON.parse(s) : {})); });
const json = (res, value, code = 200) => { res.writeHead(code, { "Content-Type": "application/json" }); res.end(JSON.stringify(value)); };

createServer(async (req, res) => {
  try {
    if (req.method === "GET" && req.url === "/") { res.writeHead(200, { "Content-Type": "text/html; charset=utf-8" }); return res.end(PAGE); }
    if (req.method === "GET" && req.url === "/tasks") return json(res, { channel, tasks: view() });
    if (req.method === "POST" && req.url === "/play") { const { task, letter } = await body(req); await push(task, letter); return json(res, { ok: true }); }
    if (req.method === "POST" && req.url === "/rate") {
      const { task, letter, score, notes } = await body(req);
      if (!saved[task]) return json(res, { error: "unknown task" }, 404);
      if (letter) saved[task].scores[letter] = Math.max(1, Math.min(5, Number(score) || 0)) || undefined;
      if (typeof notes === "string") saved[task].notes = notes;
      save();
      return json(res, { tasks: view() });
    }
    json(res, { error: "not found" }, 404);
  } catch (error) { json(res, { error: error.message }, 500); }
}).listen(Number(values.port), "127.0.0.1", () => {
  if (!session.signedIn) console.error("not signed in: pieces cannot be pushed — run `ac login` first");
  console.log(`http://localhost:${values.port}  ·  ${tasks.length} tasks  ·  ratings → ${ratingsFile}`);
});

const PAGE = `<!doctype html><meta charset=utf-8><title>route judge</title>
<style>
body{margin:0;font:15px/1.4 ui-monospace,Menlo,monospace;background:#111;color:#ddd;display:grid;grid-template-columns:260px 1fr;height:100vh}
nav{overflow:auto;border-right:1px solid #333}
nav div{padding:6px 10px;cursor:pointer;white-space:nowrap}
nav div:hover{background:#222} nav div.on{background:#2a2a40} nav .done{color:#7c7}
main{display:grid;grid-template-rows:auto 1fr auto;min-height:0}
header{padding:10px 14px;border-bottom:1px solid #333}
.req{color:#fff;font-size:17px;margin:2px 0}
.letters{display:flex;gap:10px;margin-top:8px;flex-wrap:wrap}
.l{border:1px solid #444;padding:6px 8px;border-radius:6px}
.l.on{border-color:#99f;background:#1d1d33}
.l b{font-size:18px;cursor:pointer;padding:0 6px}
.l button{background:#222;color:#ccc;border:1px solid #444;border-radius:4px;width:26px;cursor:pointer}
.l button.s{background:#6a6;color:#000}
.arm{color:#fc6;font-size:12px}
iframe{width:100%;height:100%;border:0;background:#000}
footer{padding:6px 14px;border-top:1px solid #333}
textarea{width:100%;height:40px;background:#1a1a1a;color:#ddd;border:1px solid #333;font:inherit}
</style>
<nav id=nav></nav>
<main><header id=head></header><iframe id=frame allow="autoplay; microphone; accelerometer; gyroscope"></iframe>
<footer><textarea id=notes placeholder="notes on this task"></textarea></footer></main>
<script>
let state,cur,playing;
const $=id=>document.getElementById(id);
async function load(){state=await (await fetch('/tasks')).json();cur??=state.tasks[0]?.id;
 $('frame').src='https://aesthetic.computer/prompt~channel%20'+state.channel+'~!autorun?nogap=true';draw();}
function draw(){
 $('nav').innerHTML=state.tasks.map(t=>'<div class="'+(t.id===cur?'on ':'')+(t.reveal?'done':'')+'" data-id="'+t.id+'">'+(t.reveal?'✓ ':'· ')+t.id.replace(/^piece-/,'')+'</div>').join('');
 const t=state.tasks.find(t=>t.id===cur);if(!t)return;
 $('head').innerHTML=t.requests.map((r,i)=>'<div class=req>'+(t.requests.length>1?(i+1)+'. ':'')+r.replace(/</g,'&lt;')+'</div>').join('')+
  '<div class=letters>'+t.letters.map(l=>'<div class="l'+(playing===l?' on':'')+'"><b data-play="'+l+'">'+l+'</b>'+
  [1,2,3,4,5].map(n=>'<button data-l="'+l+'" data-n="'+n+'" class="'+(t.scores[l]==n?'s':'')+'">'+n+'</button>').join('')+
  (t.reveal?' <span class=arm>'+t.reveal[l].arm+(t.reveal[l].delivered?'':' ✕')+'</span>':'')+'</div>').join('')+'</div>';
 $('notes').value=t.notes||'';}
document.addEventListener('click',async e=>{
 const d=e.target.dataset;
 if(e.target.closest('nav div')){cur=e.target.closest('nav div').dataset.id;playing=null;draw();}
 if(d.play){playing=d.play;draw();const r=await fetch('/play',{method:'POST',body:JSON.stringify({task:cur,letter:d.play})});if(!r.ok)alert((await r.json()).error);}
 if(d.n){state.tasks=(await (await fetch('/rate',{method:'POST',body:JSON.stringify({task:cur,letter:d.l,score:d.n})})).json()).tasks;draw();}
});
$('notes').addEventListener('change',async e=>{state.tasks=(await (await fetch('/rate',{method:'POST',body:JSON.stringify({task:cur,notes:e.target.value})})).json()).tasks;});
document.addEventListener('keydown',e=>{if(e.target.tagName==='TEXTAREA')return;const t=state.tasks.find(t=>t.id===cur);
 const i=state.tasks.indexOf(t);if(e.key==='j'||e.key==='ArrowDown'){cur=state.tasks[Math.min(i+1,state.tasks.length-1)].id;playing=null;draw();}
 if(e.key==='k'||e.key==='ArrowUp'){cur=state.tasks[Math.max(i-1,0)].id;playing=null;draw();}});
load();
</script>`;

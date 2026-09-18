import { controlPlan, controlsAt } from './model.mjs';
const $ = id => document.getElementById(id);
const empty = () => ({ scene: null, at: 0, plan: null, issuedAt: 0, count: 0, cost: 0, input: 0, output: 0, latency: 0, move: 'Waiting', failures: 0 });
let fighters = [empty(), empty()], running = false, ticket = '', endAt = 0, pending = 0;
let frame = null;
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
globalThis.__jevArena = {
  pad(seat, scene) {
    const f = fighters[seat];
    if (!f) return [];
    f.scene = scene; f.at = performance.now();
    if (!running || !scene.alive) return [];
    return controlsAt(f.plan, performance.now() - f.issuedAt);
  },
  frame(value) { frame = value; },
  inspect() { return { running, frame, fighters: fighters.map(f => ({ ...f, scene: f.scene })) }; },
};
function render() {
  fighters.forEach((f,i) => {
    $('cost'+i).textContent = '$'+f.cost.toFixed(6);
    $('tokens'+i).textContent = f.input.toLocaleString()+' / '+f.output.toLocaleString();
    $('latency'+i).textContent = f.latency ? f.latency+' ms' : '—';
    $('count'+i).textContent = f.count;
    $('move'+i).textContent = f.move;
  });
  $('start').disabled = running || pending > 0;
  $('stop').disabled = !running;
}
function stop(message = 'Stopped') {
  running = false;
  for (const f of fighters) f.plan = null;
  $('status').textContent = message;
  render();
}
async function post(body) {
  const response = await fetch('/api/oskiewar-jev', { method: 'POST',
    headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body), signal: AbortSignal.timeout(6000) });
  const result = await response.json();
  if (!response.ok) { const error = new Error(result.error || `Demo returned ${response.status}`); error.status = response.status; throw error; }
  return result;
}
async function drive(seat, session) {
  while (running && ticket === session) {
    const f = fighters[seat];
    if (!f.scene?.alive || performance.now()-f.at > 250 || frame?.phase !== 'fight') { await sleep(100); continue; }
    const started = performance.now(), round = f.scene.round;
    let releaseMs = 210;
    pending++;
    try {
      const scene = { ...f.scene, strikes: f.scene.strikeOptions?.() };
      const result = await post({ ticket: session, seat, scene });
      f.failures = 0;
      // Account for successful requests even when Stop was pressed in flight.
      f.count++; f.cost += result.usage.costUsd; f.input += result.usage.inputTokens; f.output += result.usage.outputTokens;
      f.latency = Math.round(performance.now() - started); f.move = result.motion+' + '+result.action;
      if (running && ticket === session && round === f.scene?.round && f.scene.alive &&
          frame?.phase === 'fight' && performance.now() - started < 1500) {
        f.plan = controlPlan(result.motion, result.action); f.issuedAt = performance.now();
        releaseMs = f.plan.releaseMs;
      }
    } catch (error) {
      f.plan = null; f.failures++; f.move = 'Waiting for Jev';
      if (error.status !== 503 || f.failures >= 3) stop(error.message);
    }
    finally { pending--; render(); }
    // Let a button release before its next press; all chosen holds are bounded.
    await sleep(releaseMs);
  }
}
$('start').addEventListener('click', async () => {
  if (running || pending) return;
  pending++; render(); $('status').textContent = 'Starting…';
  try {
    const session = await post({ op: 'start' });
    ticket = session.ticket; fighters = [empty(), empty()]; frame = null;
    $('arena').src = '/mac-test.html?self-play&jev-vs-jev&voice=off&run='+encodeURIComponent(ticket);
    running = true; endAt = performance.now() + session.durationMs;
    drive(0, ticket); drive(1, ticket);
  } catch (error) { stop(error.message); }
  finally { pending--; render(); }
});
$('stop').addEventListener('click', () => stop());
document.addEventListener('visibilitychange', () => { if (document.hidden) stop('Paused while this tab is hidden'); });
setInterval(() => {
  if (!running) return;
  const remaining = Math.ceil((endAt-performance.now())/1000);
  if (remaining <= 0) stop('Match complete');
  else $('status').textContent = remaining+' seconds remaining';
}, 200);

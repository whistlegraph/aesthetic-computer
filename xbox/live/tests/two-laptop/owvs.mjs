// owvs.mjs — two-laptop oskiewar versus driver (neo + blueberry over CDP).
// Phases: matchmake, fight (latency), leave/join. Uses xbox/live/bot-harness.mjs.
import { BotHarness } from "../../bot-harness.mjs";

export const NEO = "http://127.0.0.1:9227";
export const BB = "http://127.0.0.1:9225";
export const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
export const now = () => Date.now();
export const ts = (t0) => ((now() - t0) / 1000).toFixed(2) + "s";

export async function tabs(cdp) {
  return (await (await fetch(cdp + "/json/list")).json()).filter((t) => t.type === "page");
}
export async function openTab(cdp, url) {
  const r = await fetch(cdp + "/json/new?" + encodeURIComponent(url), { method: "PUT" });
  return r.json(); // { id, webSocketDebuggerUrl, url }
}
export async function closeTab(cdp, id) {
  await fetch(cdp + "/json/close/" + id);
}
export async function closeOskiewarTabs(cdp) {
  for (const t of await tabs(cdp)) if (/oskiewar/i.test(t.url)) await closeTab(cdp, t.id);
}

const PROBE = `(() => {
  const b = globalThis.__oskiewarRoundBridge;
  const n = globalThis.__oskiewarNetStats;
  return {
    path: location.pathname, room: globalThis.__oskiewarVersusRoom || "",
    seat: b ? (b.seat || "") : "", role: b ? (b.role || "") : "", live: b ? !!b.live : false,
    phase: b && b.lastState ? b.lastState.phase : "", seq: b && b.lastState ? b.lastState.seq : 0,
    fighters: b && b.lastState ? (b.lastState.fighters || []).map(f => f.name + (f.alive ? "" : "†")) : [],
    net: n ? { seat: n.seat, frame: n.frame, names: n.names, ping: n.pingMs, pings: n.pings,
      rb: n.rollbacks, rolled: n.rolledFrames, maxRb: n.maxRollback, stalls: n.stalls, waits: n.waits,
      behind: n.behind, desyncs: n.desyncs, hash: n.hash, hashFrame: n.hashFrame, sent: n.sent, recv: n.received } : null,
    remotePadAge: globalThis.__oskiewarRemotePad ? Date.now() - globalThis.__oskiewarRemotePad.at : null,
    localVersus: !!globalThis.__oskiewarLocalVersus, hidden: document.hidden, screen: globalThis.__oskiewarTouch ? globalThis.__oskiewarTouch.screen : '',
  };
})()`;

export class Seat {
  constructor(name, cdp) { this.name = name; this.cdp = cdp; this.bot = null; this.tab = null; }
  async open(url) {
    this.tab = await openTab(this.cdp, url);
    await sleep(1500);
    await this.attach();
    return this.tab;
  }
  async attach(tries = 20) {
    for (let i = 0; i < tries; i++) {
      try {
        this.bot = new BotHarness({ cdp: this.cdp, tab: this.tab.id });
        await this.bot.connect();
        return;
      } catch (e) { await sleep(500); if (i === tries - 1) throw e; }
    }
  }
  probe() { return this.bot.cdp.evaluate(PROBE); }
  eval(expr) { return this.bot.cdp.evaluate(expr); }
  async close() { try { this.bot?.cdp.close(); } catch {} if (this.tab) await closeTab(this.cdp, this.tab.id); this.tab = null; }
  // Wait until predicate(probe) true; returns [seconds, probe].
  async until(pred, { timeoutMs = 20000, every = 150, label = "" } = {}) {
    const t0 = now();
    let last = null;
    let sig = "";
    while (now() - t0 < timeoutMs) {
      try {
        last = await this.probe();
        const k = JSON.stringify({ room: last.room, seat: last.seat, live: last.live, phase: last.phase, net: !!last.net, screen: last.screen, f: last.fighters });
        if (k !== sig) { sig = k; log(`  [${this.name} +${((now() - t0) / 1000).toFixed(2)}s] ${k}`); }
        if (pred(last)) return [(now() - t0) / 1000, last];
      } catch {}
      await sleep(every);
    }
    throw new Error(`${this.name}: timeout waiting ${label}: ` + JSON.stringify(last));
  }
}

export function log(...a) { console.log(new Date().toISOString().slice(11, 23), ...a); }
export const fmtNet = (n) => n ? `seat=${n.seat} f=${n.frame} ping=${n.ping}ms(${n.pings}) rb=${n.rb}/${n.rolled} max=${n.maxRb} stalls=${n.stalls} waits=${n.waits} behind=${n.behind} desync=${n.desyncs} hash=${n.hash}@${n.hashFrame} s/r=${n.sent}/${n.recv} names=${(n.names||[]).join("v")}` : "net=null";

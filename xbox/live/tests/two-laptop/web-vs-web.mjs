// p4: both seats fight (host bot cross-fed from the challenger's bridge), sample net stats.
// usage: node p4.mjs <base> <room|bare> [seconds]
import { Seat, NEO, BB, closeOskiewarTabs, sleep, log, fmtNet, now } from "./owvs.mjs";
import { KEY } from "../../bot-harness.mjs";
const [base, roomArg, secArg] = process.argv.slice(2);
const bare = roomArg === "bare";
const ROOM = bare ? null : roomArg;
const URL = base + (bare ? "/" : "/" + ROOM);
const SECONDS = Number(secArg) || 20;
log("URL", URL);
await closeOskiewarTabs(NEO); await closeOskiewarTabs(BB); await sleep(1500);
const neo = new Seat("neo", NEO), bb = new Seat("blueberry", BB);
const kick = async (s) => { const scr = await s.eval("globalThis.__oskiewarTouch&&globalThis.__oskiewarTouch.screen"); if (scr === "title") { await s.bot.press("KeyD", 220); await sleep(400); } };
// Closed-loop aggressor with an explicit state source and seat index.
async function aggress(bot, stateFn, myIndex, seconds) {
  const rival = myIndex ? 0 : 1, end = now() + seconds * 1000; let held = null, lastAttack = 0;
  while (now() < end) {
    let s = null; try { s = await stateFn(); } catch {}
    const me = s?.fighters?.[myIndex], r = s?.fighters?.[rival];
    if (me && r) {
      const dx = r.x - me.x, near = Math.abs(dx) < 320, want = near ? null : (dx > 0 ? KEY.right : KEY.left);
      if (want !== held) { if (held) await bot.release(held); if (want) await bot.hold(want); held = want; }
      if (near && now() - lastAttack > 380) { await bot.press(KEY.attack, 170); lastAttack = now(); }
    }
    await sleep(120);
  }
  await bot.releaseAll();
}
let s, p;
await neo.open(URL);
[s, p] = await neo.until((p) => p.room || p.seat === "challenger", { label: "neo lands", timeoutMs: 20000 });
log(`neo: room=${p.room} seat=${p.seat} path=${p.path} in ${s.toFixed(2)}s (${p.room ? "HOST" : "CHALLENGER of someone"})`);
await kick(neo);
await bb.open(URL);
[s, p] = await bb.until((p) => p.room || p.seat === "challenger", { label: "bb lands", timeoutMs: 20000 });
log(`bb: room=${p.room} seat=${p.seat} path=${p.path} in ${s.toFixed(2)}s`);
await kick(bb);
const host = p.room ? bb : neo, guest = p.room ? neo : bb;
[s, p] = await guest.until((p) => p.net, { label: "guest net lane", timeoutMs: 20000 }); log(`${guest.name} net lane +${s.toFixed(2)}s`);
[s, p] = await host.until((p) => p.net, { label: "host net lane", timeoutMs: 10000 }); log(`${host.name} net lane +${s.toFixed(2)}s`);
await host.bot.refreshInfo(); await guest.bot.refreshInfo();
const guestState = () => guest.bot.state();
const fight = Promise.all([aggress(host.bot, guestState, 0, SECONDS), aggress(guest.bot, guestState, 1, SECONDS)]);
const t0 = now(); const rows = [];
while (now() - t0 < SECONDS * 1000) { const [a, b] = await Promise.all([host.probe().catch(() => null), guest.probe().catch(() => null)]); rows.push({ t: (now() - t0) / 1000, h: a?.net, g: b?.net, gf: b?.fighters }); await sleep(1000); }
await fight;
for (const r of rows) log(`t=${r.t.toFixed(1)} HOST(${host.name}) ${fmtNet(r.h)} | GUEST(${guest.name}) ${fmtNet(r.g)} | ${JSON.stringify(r.gf)}`);
const f = rows.filter((r) => r.h && r.g), a = f[1], z = f.at(-1), dt = z.t - a.t;
log(`SUMMARY host fps=${((z.h.frame - a.h.frame) / dt).toFixed(1)} guest fps=${((z.g.frame - a.g.frame) / dt).toFixed(1)} over ${dt.toFixed(1)}s | stalls host=${z.h.stalls - a.h.stalls} guest=${z.g.stalls - a.g.stalls} | rb host=${z.h.rb - a.h.rb}/${z.h.rolled - a.h.rolled} guest=${z.g.rb - a.g.rb}/${z.g.rolled - a.g.rolled} | ping host=${z.h.ping.toFixed(0)} guest=${z.g.ping.toFixed(0)} | desyncs=${z.h.desyncs + z.g.desyncs} | waits host=${z.h.waits - a.h.waits} guest=${z.g.waits - a.g.waits}`);
process.exit(0);

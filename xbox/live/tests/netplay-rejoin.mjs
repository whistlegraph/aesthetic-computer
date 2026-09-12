// oskiewar rejoin, browser end to end, 26.09.11
// @jeffrey: "when a player leaves then rejoins they cant rejoin as themselves /
// the identities just get confused".
//
// Two real browsers, the real relay, the real shell (netplay-stack.mjs). The
// guest's tab is killed the way a tab is really killed — no close frame, no
// `bye` — and then comes straight back to the same address. What has to hold:
//
//   1. the chair is theirs again, not a spectator seat behind their own ghost
//   2. the host's rollback session lets go instead of being held open by the
//      very hellos the returning seat is sending
//   3. both seats open a fresh session and agree on the fight
//   4. nobody on either screen is wearing a name that was never theirs
//
//   node xbox/live/tests/netplay-rejoin.mjs [--headful] [--audio]
//
// Exits non-zero with a reason. Diagnosis: xbox/live/oskiewar-multiplayer.md
import { localStack, openSeat, readStats, play, wait, launchBrowser }
  from "./netplay-stack.mjs";

const args = process.argv.slice(2);
const headful = args.includes("--headful");
// Both doors or neither: see launchBrowser in netplay-stack.mjs.
const audible = args.includes("--audio");
const room = "rejoi" + String(Math.floor(Math.random() * 90) + 10);
const fail = (reason) => { console.log(`\nFAILED: ${reason}`); process.exitCode = 1; };

// What each screen believes about who is in the room — the deal's names while
// a rollback fight is on, the wire's while it is not. The bug was never
// visible in a hash: both machines agreed perfectly about a fight between two
// people who were not there.
const readNames = (page) => page.evaluate(() =>
  globalThis.__oskiewarNetStats?.names ||
  (globalThis.__oskiewarRoundBridge?.lastState?.fighters || [])
    .map((one) => one.name));

const stack = await localStack();
console.log(`local stack on ${stack.origin}, room ${room}`);
const browsers = [await launchBrowser(40, { headful, audible }),
  await launchBrowser(980, { headful, audible })];
const closeAll = () => Promise.all(browsers.map((one) => one.close()));

try {
  // Seat one arrives at an empty address, waits out the claim, and hosts.
  const host = await openSeat(browsers[0], stack.origin, stack.wsOrigin, "/" + room, "host", { audible });
  await wait(1500);
  await play(host, [["Enter"]]);
  await wait(5000);
  if (!(await readStats(host)).room) fail("seat one never claimed the room");

  // Seat two takes the chair and the two fall into a rollback fight.
  let guest = await openSeat(browsers[1], stack.origin, stack.wsOrigin, "/" + room, "guest", { audible });
  await wait(1500);
  await play(guest, [["Enter"]]);
  await wait(3000);
  const first = { host: (await readStats(host)).net, guest: (await readStats(guest)).net };
  if (!first.host || !first.guest) fail("the first fight never opened a rollback session");
  else console.log(`first fight: host seat ${first.host.seat}, guest seat ${first.guest.seat}`);
  await play(guest, [["KeyD"], ["Space"]], 150);
  await wait(500);
  // Where the old fight had got to. A session that lets go restarts its frame
  // count from zero, so a later reading BELOW this one is the proof — and one
  // that only ever climbs past it is the bug: the host still simulating
  // against a rival who is doing nothing but trying to come back.
  const atKill = (await readStats(host)).net?.frame ?? 0;

  // The tab is killed. Not closed — killed: no close frame reaches the relay,
  // so the chair stays technically occupied and the host hears nothing but
  // silence. This is the case the 15 s ping sweep used to take up to half a
  // minute to clear, and the case a `bye` never covers.
  console.log("killing the guest's tab (no close frame, no bye)");
  await guest.evaluate(() => { globalThis.__oskiewarRoundBridge = null; })
    .catch(() => {});
  await browsers[1].close();

  // And straight back to the same address, well inside every old timeout.
  browsers[1] = await launchBrowser(980, { headful, audible });
  guest = await openSeat(browsers[1], stack.origin, stack.wsOrigin, "/" + room, "guest", { audible });
  await wait(1500);
  await play(guest, [["Enter"]]);

  // Watch from the moment they are back, not after it. The old session ends
  // within a beat or two of the first hello and the new one climbs at 60 Hz,
  // so a single reading taken later sees only a healthy fight and cannot tell
  // which one it is.
  let released = false;
  for (const deadline = Date.now() + 10000; Date.now() < deadline; ) {
    const net = (await readStats(host)).net;
    if (!net || net.frame < atKill) { released = true; break; }
    await wait(100);
  }
  if (!released)
    fail(`the host never let go of the fight it was in at frame ${atKill} — ` +
      "the rejoiner's hellos held the dead session open");
  else console.log(`the host let the old fight go (it was at frame ${atKill})`);

  await wait(4000);
  const back = await readStats(guest);
  if (back.seat !== "challenger")
    fail(`the returning player got "${back.seat || "no seat"}" instead of the chair`);
  else console.log("the chair is theirs again");

  const second = { host: (await readStats(host)).net, guest: (await readStats(guest)).net };
  if (!second.host) fail("the host never dealt a new fight");
  if (!second.guest) fail("the returning seat never opened a rollback session");
  if (second.host && second.guest) {
    if (second.host.seat !== 0 || second.guest.seat !== 1)
      fail("the seats came back wrong way round");
    if (second.host.desyncs || second.guest.desyncs)
      fail(`the rejoined fight desynced: ${second.host.desyncs} + ${second.guest.desyncs}`);
  }

  // Nobody wears a borrowed name. Both browsers are signed out, so every
  // fighter on both screens must be nameless — this is where "@JEFFREY vs
  // @OSKIE" used to appear, invented out of the local roster.
  for (const [label, page] of [["host", host], ["guest", guest]]) {
    const shown = await readNames(page);
    if (shown?.some((name) => /^@(JEFFREY|OSKIE|FIFI|SAT)$/.test(name || "")))
      fail(`${label} is showing a roster handle nobody claimed: ${shown.join(" vs ")}`);
    else console.log(`  ${label} names: ${(shown || []).map((n) => n || "(nobody)").join(" vs ")}`);
  }

  await play(guest, [["KeyA"], ["Space"]], 150);
  await play(host, [["KeyD"], ["Space"]], 150);
  await wait(800);
  const settled = { host: (await readStats(host)).net, guest: (await readStats(guest)).net };
  if (!settled.host || !settled.guest) fail("the rejoined fight did not survive play");
  else if (settled.host.desyncs || settled.guest.desyncs) fail("the rejoined fight desynced under play");
  else console.log(`both seats playing again: host frame ${settled.host.frame}, ` +
    `guest frame ${settled.guest.frame}, desyncs 0`);

  if (!process.exitCode) console.log("\nPASSED: they left, they came back, they are themselves.");
  // Whatever the fight managed to file, and anything the store would have
  // refused. A round only ends on a knockout, so a short movement-only run may
  // file nothing — but nothing it DOES file may be malformed.
  if (stack.filed.rejected.length)
    fail(`the store refused ${stack.filed.rejected.length} round(s): ` +
      stack.filed.rejected.map((one) => `${one.id} ${one.reason}`).join("; "));
  if (stack.filed.rounds.length)
    console.log(`filed ${stack.filed.rounds.length} round(s): ` +
      stack.filed.rounds.map((one) =>
        `${one.roundId} in ${one.roomId || "no room"}`).join(", "));
} catch (error) {
  // Without this the `process.exit` below swallows the throw whole: the run
  // stopped mid-script, printed nothing, and exited zero.
  fail(`${error.message}\n${error.stack}`);
} finally {
  await closeAll();
  stack.server.close();
  process.exit(process.exitCode || 0);
}

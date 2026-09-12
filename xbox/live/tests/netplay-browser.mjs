// oskiewar rollback netplay, browser end to end, 26.09.09
// Two real browsers on one room, joined by a local instance of the real relay
// and served the real shell (netplay-stack.mjs). Proves what the unit tests
// cannot: that the handshake happens through the actual bridge, that both
// browsers open a rollback session, and that after a fight their state hashes
// agree. The leave-and-come-back half lives in netplay-rejoin.mjs.
//
//   node xbox/live/tests/netplay-browser.mjs [--seconds 20] [--headful] [--audio]
//
// Exits non-zero with a reason if either seat fails to open a session or the
// two seats disagree. Findings and the streamed-lane analysis it replaces:
// xbox/live/oskiewar-multiplayer.md
import { localStack, openSeat, readStats, play, wait, launchBrowser }
  from "./netplay-stack.mjs";

const args = process.argv.slice(2);
const seconds = Number(args[args.indexOf("--seconds") + 1]) || 20;
const headful = args.includes("--headful");
// Both doors or neither: see launchBrowser in netplay-stack.mjs.
const audible = args.includes("--audio");
// Where to leave a frame from each seat, taken mid-fight. Looking at the two
// pictures is the only way to check the half of this that no hash covers:
// that both seats are drawing the same fight, with a HUD.
const shots = args.includes("--shots")
  ? (args[args.indexOf("--shots") + 1] || "").replace(/^-.*/, "") || "." : "";
const room = "netpl" + String(Math.floor(Math.random() * 90) + 10);

// WASD, not the arrows. A rollback seat reads `gamepad(0)` whichever chair it
// sits in — the rival's word is written into pad 1 — so arrow keys drive a pad
// that netplay immediately overwrites, and the fighter never hears them. The
// arrow version of these scripts still produced rollbacks, but only as a side
// effect of its Space/Enter presses, which is a much thinner test than it looks.
//
// Note also that these cannot reach a round rollover on their own: versus
// rounds are untimed (`roundIsTimed()` is false under `versusLane()`), so a
// movement-only fight runs one endless round. Landing hits is what ends one.
const hostScript = [["KeyD"], ["Space"], ["KeyD", "KeyW"],
  ["Enter"], ["KeyA"], ["Space"], ["KeyD"], ["Enter"]];
const guestScript = [["KeyA"], ["Enter"], ["KeyW"], ["KeyA"],
  ["Space"], ["KeyD"], ["Enter"], ["KeyA"]];

const fail = (reason) => { console.log(`\nFAILED: ${reason}`); process.exitCode = 1; };

const stack = await localStack();
console.log(`local stack on ${stack.origin}, room ${room}`);
const browsers = [await launchBrowser(40, { headful, audible }),
  await launchBrowser(980, { headful, audible })];
const browser = { close: () => Promise.all(browsers.map((one) => one.close())) };
try {
  // Seat one arrives at an empty address, waits out the claim, and hosts.
  const host = await openSeat(browsers[0], stack.origin, stack.wsOrigin, "/" + room, "host", { audible });
  await wait(1500);
  await play(host, [["Enter"]]);
  await wait(5000);
  const hosting = await readStats(host);
  console.log(`host claimed room ${hosting.room || "(none)"}`);
  if (!hosting.room) fail("seat one never claimed the room");

  // Seat two arrives at the same address and takes the chair.
  const guest = await openSeat(browsers[1], stack.origin, stack.wsOrigin, "/" + room, "guest", { audible });
  await wait(1500);
  await play(guest, [["Enter"]]);
  await wait(3000);

  let hostStats = (await readStats(host)).net;
  let guestStats = (await readStats(guest)).net;
  if (!hostStats) fail("the host never opened a rollback session");
  if (!guestStats) fail("the guest never opened a rollback session");
  if (hostStats && guestStats) {
    console.log(`sessions open: host seat ${hostStats.seat}, guest seat ${guestStats.seat}`);
    if (hostStats.seat !== 0 || guestStats.seat !== 1) fail("the seats are not host and challenger");
  }

  // Fight. Both hands run their own script at the same time.
  const until = Date.now() + seconds * 1000;
  let shot = false;
  while (Date.now() < until) {
    await Promise.all([play(host, hostScript, 150), play(guest, guestScript, 150)]);
    if (shots && !shot && Date.now() > until - seconds * 500) {
      shot = true;
      await host.screenshot({ path: `${shots}/netplay-host.png` });
      await guest.screenshot({ path: `${shots}/netplay-guest.png` });
      console.log(`frames written to ${shots}/netplay-{host,guest}.png`);
    }
  }
  await wait(1200);

  hostStats = (await readStats(host)).net;
  guestStats = (await readStats(guest)).net;
  if (!hostStats || !guestStats) fail("a session ended before the fight did");
  else {
    const row = (label, stats) => console.log(
      `  ${label.padEnd(6)} frame ${String(stats.frame).padStart(5)} ` +
      `confirmed ${String(stats.confirmed).padStart(5)} ` +
      `rollbacks ${String(stats.rollbacks).padStart(4)} (max ${stats.maxRollback}) ` +
      `stalls ${String(stats.stalls).padStart(4)} waits ${String(stats.waits).padStart(4)} ` +
      `desyncs ${stats.desyncs} sent ${stats.sent} recv ${stats.received} ` +
      `snapshot ${(stats.snapshotMs / Math.max(1, stats.frame)).toFixed(3)}ms/f ` +
      `resim ${(stats.resimMs / Math.max(1, stats.frame)).toFixed(3)}ms/f`);
    console.log(`\nafter ${seconds}s of two-handed play:`);
    row("host", hostStats);
    row("guest", guestStats);
    if (hostStats.desyncs || guestStats.desyncs)
      fail(`the seats disagreed: ${hostStats.desyncs} + ${guestStats.desyncs} desyncs`);
    if (hostStats.frame < 300) fail(`the fight barely ran (${hostStats.frame} frames)`);
    if (!hostStats.rollbacks && !guestStats.rollbacks)
      console.log("  note: no rollbacks happened — a local wire is nearly free");
    // The strongest check available from outside: each seat's own hash of a
    // frame both pads are known for, compared where the frames coincide.
    const shared = Math.min(hostStats.hashFrame, guestStats.hashFrame);
    if (hostStats.hashFrame === guestStats.hashFrame &&
        hostStats.hash !== guestStats.hash)
      fail(`frame ${shared} hashes differ: ${hostStats.hash} vs ${guestStats.hash}`);
    else console.log(`  hashes agree at the frames they share (last checked ${shared})`);
    if (!process.exitCode) console.log("\nPASSED: two browsers, one fight.");
  }
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
  await browser.close();
  stack.server.close();
  process.exit(process.exitCode || 0);
}

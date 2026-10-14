// mac-vs-mac.mjs — the Mac app on one machine hosts a room, the Mac app on
// another takes the chair, both seats are pressed by native keys, and the
// fight is read from the relay's spectator stream plus each app's net log.
//   node mac-vs-mac.mjs --host blueberry --guest poorslice [--seconds 45] [--matches 5] [--room name]
// A machine is "neo" (this Mac) or an ssh alias with a logged-in desktop,
// the app in /Applications or ~/Applications, and `keybot` at
// ~/.local/bin/keybot (built from keybot.swift). A suite is fresh apps and a
// fresh room per match. 26.10.14
import { spawn, execSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { sleep, log, now } from "./owvs.mjs";
import { createBrain } from "./bot-brain.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const arg = (name, fallback) => { const i = process.argv.indexOf("--" + name); return i > 0 ? process.argv[i + 1] : fallback; };
const HOST = arg("host", "blueberry"), GUEST = arg("guest", "neo");
const SECONDS = Number(arg("seconds", 20)), MATCHES = Number(arg("matches", 1));
const ROOM_ASKED = arg("room", "");
// --relay nj|sfo pins both apps to one relay; leave it off to test the
// apps' own choice (the room's relay if live, else the nearest).
const RELAY = arg("relay", "");
// Key codes the app maps for player one: A/D walk, Q punches, D also leaves the title.
const K = { left: 0, right: 2, attack: 14, start: 2 }; // E shoots (the item button)
const NETLOG = "Library/Containers/computer.aesthetic.oskiewar/Data/tmp/net.log";
const results = [];

// One machine's seat: how to launch the app there, press its keys, and read
// its net log. neo is this Mac; anything else is reached over ssh.
function seatOn(machine) {
  const remote = machine !== "neo";
  // Single-quoted whole, so nothing in the command — a $(…), a ~ — is
  // expanded on this Mac before it reaches the other one.
  const quote = (text) => "'" + text.replace(/'/g, "'\\''") + "'";
  // And run by bash whatever the login shell is: blueberry's is fish.
  const sh = (cmd) => remote ? execSync(`ssh ${machine} ${quote("bash -c " + quote(cmd))}`, { stdio: ["ignore", "pipe", "pipe"] }).toString()
    : execSync(cmd, { shell: "/bin/zsh", stdio: ["ignore", "pipe", "pipe"] }).toString();
  // Never `open -a Oskiewar`: LaunchServices picks whichever copy it likes,
  // and on poorslice that was a stale build in an old checkout. The app is
  // named by path, ~/Applications first, and the running process is checked.
  const app = sh(`[ -d ~/Applications/Oskiewar.app ] && echo ~/Applications/Oskiewar.app || echo /Applications/Oskiewar.app`).trim();
  return {
    machine, app,
    // What this endpoint will run: the bundle's build and source hashes.
    identity: () => {
      const manifest = JSON.parse(sh(`/bin/cat '${app}/Contents/Resources/live/oskiewar-release.json'`));
      const build = Number((sh(`grep -o 'const buildVersion = [0-9]*' '${app}/Contents/Resources/live/oskiewar.js'`).match(/\d+/) || [])[0]);
      return { build, game: manifest.game, release: manifest.release };
    },
    running: () => { try { return sh(`P=$(pgrep -x oskiewar | head -1); [ -n "$P" ] && ps -o command= -p $P || true`).trim(); } catch { return ""; } },
    kill: () => { try { sh(`pkill -x oskiewar; rm -f ~/${NETLOG}; true`); } catch {} },
    launch: (room) => sh(`open -n '${app}' --env OSKIEWAR_ROOM=${room} --env OSKIEWAR_NETLOG=net.log --env OSKIEWAR_HASHTRACE=1 --env OSKIEWAR_BOT=1${RELAY ? ` --env OSKIEWAR_RELAY=${RELAY}` : ""}`),
    netlog: () => { try { return remote ? sh(`/bin/cat ~/${NETLOG} 2>/dev/null`) : readFileSync(join(process.env.HOME, NETLOG), "utf8"); } catch { return ""; } },
    keybot: () => {
      const kb = remote ? spawn("ssh", [machine, "~/.local/bin/keybot"], { stdio: ["pipe", "pipe", "inherit"] })
        : spawn(join(HERE, "keybot"), [], { stdio: ["pipe", "pipe", "inherit"] });
      const ready = new Promise((res) => kb.stdout.on("data", (d) => {
        if (/ready/.test(d.toString())) { log(`${machine} keybot:`, d.toString().trim()); res(); }
      }));
      return {
        send: (line) => { try { kb.stdin.write(line + "\n"); } catch {} },
        ready,
        end: () => { try { kb.stdin.write("quit\n"); kb.stdin.end(); } catch {} },
      };
    },
  };
}

const lastNet = (text) => {
  const line = text.trim().split("\n").filter((l) => /"net":\{/.test(l)).at(-1);
  const m = line && line.match(/\{"net":.*\}$/);
  try { return m ? JSON.parse(m[0]).net : null; } catch { return null; }
};
const events = (text) => text.split("\n")
  .filter((l) => /jsexception|CLIENT_ERROR|NET_END|NET_REFUSED|VERSUS_/.test(l)).map((l) => l.replace(/^\S+ \S+ \S+ /, "").slice(0, 140));
const fmt = (n) => n ? `f=${n.frame} rb=${n.rollbacks}/${n.rolledFrames} max=${n.maxRollback} stalls=${n.stalls} waits=${n.waits} desync=${n.desyncs} ping=${Math.round(n.pingMs)} names=${JSON.stringify(n.names)}` : "no net";

async function match(index) {
  const ROOM = ROOM_ASKED && MATCHES === 1 ? ROOM_ASKED : "jevmac" + Math.floor(Math.random() * 90 + 10);
  log(`=== MATCH ${index + 1}/${MATCHES} ROOM ${ROOM} host=${HOST} guest=${GUEST}`);
  const host = seatOn(HOST), guest = seatOn(GUEST);
  // Preflight: both endpoints must be about to run the same program.
  const hostId = host.identity(), guestId = guest.identity();
  log(`preflight ${HOST} v${hostId.build} game ${hostId.game.slice(0, 12)} (${host.app})`);
  log(`preflight ${GUEST} v${guestId.build} game ${guestId.game.slice(0, 12)} (${guest.app})`);
  if (hostId.build !== guestId.build || hostId.game !== guestId.game)
    return void results.push({ index, room: ROOM, fail: `preflight: builds differ (${HOST} v${hostId.build}/${hostId.game.slice(0, 8)} vs ${GUEST} v${guestId.build}/${guestId.game.slice(0, 8)})` });
  host.kill(); guest.kill();
  const hk = host.keybot(), gk = guest.keybot();
  await Promise.all([hk.ready, gk.ready]);
  // Watch the room on whichever relay it lands on: open a viewer on every
  // relay and keep the frames from the one that goes live.
  const RELAY_HOSTS = { nj: "session-server.aesthetic.computer", sfo: "relay-sfo.aesthetic.computer" };
  const watchHosts = RELAY ? [RELAY_HOSTS[RELAY]] : Object.values(RELAY_HOSTS);
  let relayUsed = "";
  const sockets = watchHosts.map((h) => {
    const sock = new WebSocket(`wss://${h}/oskiewar-live?match=ow-${ROOM}&role=viewer&surface=web`);
    sock.onmessage = (e) => { const m = JSON.parse(e.data); if (m.type === "oskiewar:status" && m.content.live && !relayUsed) relayUsed = h; if (!relayUsed || relayUsed === h) onFrame(m); };
    return sock;
  });
  const ws = { close: () => sockets.forEach((sock) => { try { sock.close(); } catch {} }) };
  let status = null, state = null, seqs = 0, liveAt = 0, seatedAt = 0, tGuest = 0;
  function onFrame(m) {
    if (m.type === "oskiewar:status") {
      status = m.content;
      if (status.live && !liveAt) liveAt = now();
      // The chair counts once the guest has launched; before that the host's
      // own bridge holds it for a moment while it claims the room.
      if (status.challenger && tGuest && !seatedAt) seatedAt = now();
    }
    if (m.type === "oskiewar:state") { state = m.content; seqs++; }
  }
  const until = async (pred, ms, label) => {
    const t0 = now();
    while (now() - t0 < ms) { if (pred()) return true; await sleep(150); }
    log("TIMEOUT", label, JSON.stringify(status)); return false;
  };
  const bail = (why) => { results.push({ index, room: ROOM, fail: why }); hk.end(); gk.end(); ws.close(); };

  host.launch(ROOM);
  const tHost = now(); log(`${HOST} app launched (host)`);
  const runningAt = async (seat) => { for (let i = 0; i < 20; i++) { const cmd = seat.running(); if (cmd) return cmd; await sleep(250); } return ""; };
  const hostCmd = await runningAt(host);
  if (!hostCmd.toLowerCase().startsWith(host.app.toLowerCase())) return bail(`${HOST} is running ${hostCmd || "nothing"}, not ${host.app}`);
  await sleep(6000);
  const hostPress = setInterval(() => hk.send(`press ${K.start} 220`), 3000); hk.send(`press ${K.start} 220`);
  const hostUp = await until(() => liveAt, 40000, "host live");
  clearInterval(hostPress);
  if (!hostUp) return bail("host never live");
  log(`host live ${((liveAt - tHost) / 1000).toFixed(1)}s after launch`);

  guest.launch(ROOM);
  tGuest = now(); log(`${GUEST} app launched (challenger)`);
  const guestCmd = await runningAt(guest);
  if (!guestCmd.toLowerCase().startsWith(guest.app.toLowerCase())) return bail(`${GUEST} is running ${guestCmd || "nothing"}, not ${guest.app}`);
  await sleep(6000);
  const guestPress = setInterval(() => gk.send(`press ${K.start} 220`), 3000); gk.send(`press ${K.start} 220`);
  const seated = await until(() => seatedAt, 40000, "challenger seated");
  clearInterval(guestPress);
  if (!seated) return bail("challenger never seated");
  log(`challenger seated ${((seatedAt - tGuest) / 1000).toFixed(1)}s after launch`);
  await sleep(4000);

  // Each seat's bot reads its status off the stream and picks a goal from it
  // (bot-brain.mjs); here its keys become native key events.
  const goalTime = [{}, {}];
  async function bot(kb, me, seconds) {
    const think = createBrain(me, { log });
    const end = now() + seconds * 1000, held = new Set();
    let last = now(), goal = "";
    while (now() < end) {
      const t = now();
      if (goal) goalTime[me][goal] = (goalTime[me][goal] || 0) + (t - last);
      last = t;
      const plan = think(state, t);
      goal = plan.goal;
      for (const code of held) if (!plan.hold.has(code)) { kb.send(`release ${code}`); held.delete(code); }
      for (const code of plan.hold) if (!held.has(code)) { kb.send(`hold ${code}`); held.add(code); }
      for (const code of plan.tap) kb.send(`press ${code} 200`);
      await sleep(100);
    }
    kb.send("releaseall");
  }
  const deaths = [0, 0]; let wasAlive = [true, true];
  const watch = setInterval(() => {
    const f = state?.fighters || [];
    for (const i of [0, 1]) { const alive = f[i]?.alive !== false; if (wasAlive[i] && !alive) deaths[i]++; wasAlive[i] = alive; }
  }, 100);
  const t0 = now(), seq0 = seqs;
  await Promise.all([bot(hk, 0, SECONDS), bot(gk, 1, SECONDS)]);
  clearInterval(watch);
  const streamFps = (seqs - seq0) / ((now() - t0) / 1000);
  const wins = (state?.fighters || []).map((f) => f.roundWins);
  const names = (state?.fighters || []).map((f) => f.name);
  const share = (g) => Object.entries(g).sort((x, y) => y[1] - x[1]).map(([k, v]) => k + " " + Math.round(v / 10 / SECONDS) + "%").join(", ");
  log(`goals host: ${share(goalTime[0])} | guest: ${share(goalTime[1])}`);
  log(`stream ${streamFps.toFixed(1)} f/s; phase=${state?.phase} names=${JSON.stringify(names)} KOs host=${deaths[0]} guest=${deaths[1]} wins=${JSON.stringify(wins)}`);
  ws.close(); hk.end(); gk.end();
  await sleep(1500);
  const hostText = host.netlog(), guestText = guest.netlog();
  const hostNet = lastNet(hostText), guestNet = lastNet(guestText);
  const hostEv = events(hostText), guestEv = events(guestText);
  // Post-check: both seats logged one handshake, and it is the same one.
  const handshake = (text) => (text.match(/NET_BEGIN seat \d+ delay \d+ (build .*)$/m) || [])[1] || "";
  const hostShake = handshake(hostText), guestShake = handshake(guestText);
  const refused = /NET_REFUSED/.test(hostText + guestText);
  const agreed = !!hostShake && hostShake === guestShake && !refused;
  log(`handshake ${agreed ? "AGREED" : "FAILED"} host="${hostShake}" guest="${guestShake}"${refused ? " (refused)" : ""}`);
  log(`host(${HOST})   ${fmt(hostNet)}`);
  log(`guest(${GUEST}) ${fmt(guestNet)}`);
  if (hostEv.length || guestEv.length) log("events:", JSON.stringify({ host: hostEv, guest: guestEv }));
  results.push({ index, room: ROOM, hostLive: (liveAt - tHost) / 1000, seated: (seatedAt - tGuest) / 1000,
    streamFps, deaths, wins, names, hostNet, guestNet, errors: hostEv.length + guestEv.length, agreed, relay: relayUsed, map: hostShake.match(/map (\S+)/)?.[1] });
}

for (let i = 0; i < MATCHES; i++) {
  try { await match(i); }
  catch (e) { log("MATCH ERROR", e.message); results.push({ index: i, fail: e.message }); }
  await sleep(1500);
}
log("=== SUITE");
for (const r of results) {
  if (r.fail) { log(`#${r.index + 1} ${r.room || ""} FAIL ${r.fail}`); continue; }
  const n = (x) => x ? `f=${x.frame} rb=${x.rollbacks} stalls=${x.stalls} desync=${x.desyncs} ping=${Math.round(x.pingMs)}` : "no net";
  log(`#${r.index + 1} ${r.room} live ${r.hostLive.toFixed(1)}s seat ${r.seated.toFixed(1)}s | host ${n(r.hostNet)} | guest ${n(r.guestNet)} | relay ${r.relay} | handshake ${r.agreed ? "ok" : "FAILED"} map ${r.map} | names ${JSON.stringify(r.names)} | KOs host=${r.deaths[0]} guest=${r.deaths[1]} wins=${JSON.stringify(r.wins)} | events ${r.errors}`);
}
process.exit(0);

#!/usr/bin/env node
// oskiewar telemetry agent, 26.08.18
// Attaches to a running game by its session name — the one printed beside the
// debug bug — or by any round name, and reads the live frames back out. The
// relay counts a `role=agent` socket apart from phone spectators, so attaching
// lights the antenna mark on the game's own debug read-out.
//
//   node xbox/tools/oskiewar-agent.mjs <session-or-round-name> [--json] [--once] [--reload]
//
// A session room hands off to each round with one nextRoundId frame and takes
// the socket back when the scoring stops, so this follows: it walks forward to
// every announced round and falls back to the session room whenever a round
// room goes quiet. --json streams raw frames one per line; --once prints the
// first frame and exits. --reload asks the publishing game to reload itself —
// it obliges immediately on the title screen and at the next title otherwise —
// then waits to see the publisher drop before exiting.

const RELAY = process.env.OSKIEWAR_RELAY ||
  "wss://session-server.aesthetic.computer/oskiewar-live";
const NAME = /^(?:ow-)?((?:[a-z]{4,7}[0-9]{1,3})|(?:(?:[bdfgklmnprstvz][aeiou]){3}-(?:[bdfgklmnprstvz][aeiou]){3}-(?:[bdfgklmnprstvz][aeiou]){3}))$/;

const args = process.argv.slice(2);
const json = args.includes("--json");
const once = args.includes("--once");
const reload = args.includes("--reload");
const target = args.find((value) => !value.startsWith("--")) || "";
const match = target.toLowerCase().match(NAME);
if (!match) {
  console.error("usage: oskiewar-agent.mjs <session-or-round-name> [--json] [--once]");
  process.exit(1);
}

let room = "ow-" + match[1];
// The session room, learned from the frames themselves, is where this agent
// retreats when a round room stops answering.
let sessionRoom = "";
let socket = null;
let generation = 0;
let quietTimer = null;
// --reload only reports success once a live publisher actually went away.
let sawLive = false;

function line(frame) {
  const perf = frame.perf || {};
  const fighters = (frame.fighters || [])
    .map((f) => `${f.name} ${f.score ?? 0}`).join(" vs ");
  const timing = [
    perf.fps !== undefined ? `${perf.fps} fps` : "",
    perf.frameMs !== undefined ? `frame ${perf.frameMs}ms` : "",
    perf.renderMs !== undefined ? `render ${perf.renderMs}ms` : "",
    perf.hz !== undefined ? `@ ${perf.hz}Hz` : "",
  ].filter(Boolean).join(" ");
  return `[${room}] seq=${frame.seq} ${frame.phase}` +
    (timing ? `  ${timing}` : "") +
    (fighters ? `  ${fighters}` : "") +
    (frame.roundId && frame.roundId !== room ? `  round=${frame.roundId}` : "") +
    (frame.sessionId && frame.sessionId !== room
      ? `  session=${frame.sessionId}` : "");
}

function follow(next, why) {
  if (!next || next === room) return;
  console.error(`-> ${next} (${why})`);
  room = next;
  open();
}

// A round room that stops talking for this long is over; go home to the
// session room, which is the only address that outlives a round.
function armQuietFallback() {
  clearTimeout(quietTimer);
  if (!sessionRoom || sessionRoom === room) return;
  quietTimer = setTimeout(() => follow(sessionRoom, "room went quiet"), 8000);
}

function open() {
  const mine = ++generation;
  clearTimeout(quietTimer);
  try { socket?.close(); } catch {}
  socket = new WebSocket(`${RELAY}?match=${encodeURIComponent(room)}&role=agent`);
  socket.addEventListener("open", () => {
    console.error(`agent attached to ${room}`);
    if (reload) {
      socket.send(JSON.stringify({ type: "oskiewar:reload" }));
      console.error("reload requested — waiting for the publisher to drop " +
        "(a game mid-fight reloads at its next title screen)");
      setTimeout(() => {
        console.error("still live after 60s; the reload is armed and will " +
          "land at the next title screen");
        process.exit(0);
      }, 60000);
    }
  });
  socket.addEventListener("message", (event) => {
    if (mine !== generation) return;
    let message;
    try { message = JSON.parse(event.data); } catch { return; }
    if (message.type === "oskiewar:error") {
      console.error(`relay: ${message.content?.message}`);
      return;
    }
    if (message.type === "oskiewar:status") {
      if (!json) console.error(`status: live=${message.content?.live} ` +
        `viewers=${message.content?.viewers} agents=${message.content?.agents}`);
      if (message.content?.live === true) sawLive = true;
      else if (reload && sawLive) {
        console.error("publisher dropped — the page is reloading");
        process.exit(0);
      }
      return;
    }
    if (message.type !== "oskiewar:state") return;
    const frame = message.content;
    if (frame?.sessionId) sessionRoom = frame.sessionId;
    console.log(json ? JSON.stringify(frame) : line(frame));
    if (once) process.exit(0);
    armQuietFallback();
    if (frame?.nextRoundId && frame.nextRoundId !== room)
      follow(frame.nextRoundId, "hand-off");
  });
  socket.addEventListener("close", () => {
    if (mine !== generation) return;
    // A closed round room means the fight moved on; the session room is the
    // forwarding address. A closed session room just gets retried.
    setTimeout(() => {
      if (mine !== generation) return;
      if (sessionRoom && sessionRoom !== room) follow(sessionRoom, "room closed");
      else open();
    }, 2000);
  });
  socket.addEventListener("error", () => {});
}

open();

// lane-probe.mjs — prove a relay's datagram lane end to end: take a seat on
// its socket, read the UDP token it offers, send a stamped keepalive to the
// UDP port and wait for the echo. Prints the round trip; exit 1 on no answer.
//   node session-server/relay/lane-probe.mjs relay-sfo.aesthetic.computer
// Uses Node's own WebSocket (Node 22+). 26.10.15
import dgram from "node:dgram";

const host = process.argv[2];
if (!host) { console.error("usage: lane-probe.mjs <relay host>"); process.exit(2); }
const room = "ow-lanepr" + Math.floor(Math.random() * 900 + 100);
const fail = (why) => { console.log(`✗ ${host} lane: ${why}`); process.exit(1); };
const timer = setTimeout(() => fail("no answer in 6 s"), 6000);

const socket = new WebSocket(`wss://${host}/oskiewar-live?match=${room}&role=publisher&surface=probe`);
socket.addEventListener("error", () => fail("socket error"));
socket.addEventListener("message", (event) => {
  let message;
  try { message = JSON.parse(event.data); } catch { return; }
  if (message.type !== "oskiewar:udp") return;
  const { token, port } = message.content || {};
  if (!token || !port) fail("offer without token or port");
  const udp = dgram.createSocket("udp4");
  const trips = [];
  udp.on("message", (buffer) => {
    const reply = JSON.parse(buffer.toString());
    if (Number.isFinite(reply.t)) trips.push(Date.now() - reply.t);
    if (trips.length < 5) return;
    clearTimeout(timer);
    trips.sort((a, b) => a - b);
    console.log(`✓ ${host} lane: udp :${port} answers, round trip ${trips[0]}–${trips.at(-1)} ms` +
      (message.content.geckos ? ", geckos on" : ", geckos OFF"));
    udp.close(); socket.close(); process.exit(0);
  });
  let sent = 0;
  const ping = setInterval(() => {
    if (sent++ > 20) return clearInterval(ping);
    udp.send(JSON.stringify({ k: token, t: Date.now() }), port, host);
  }, 100);
});

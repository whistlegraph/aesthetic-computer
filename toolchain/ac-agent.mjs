#!/usr/bin/env node
// AC Agent, 26.09.03
// "Claude in" to aesthetic.computer: attach to a handle's presence room so
// every AC surface signed in as that handle shows the linked-agent mark in
// its bottom-right corner. Stays attached until killed.
//
//   node toolchain/ac-agent.mjs @jeffrey
//   node toolchain/ac-agent.mjs @jeffrey --label fable
//   AC_PRESENCE_RELAY=ws://localhost:8889 node toolchain/ac-agent.mjs @jeffrey

const RELAY =
  process.env.AC_PRESENCE_RELAY || "wss://session-server.aesthetic.computer";
const RETRY_MS = 5000;

const args = process.argv.slice(2);
const target = args.find((value) => !value.startsWith("--")) || "";
const labelIndex = args.indexOf("--label");
const label = labelIndex !== -1 ? args[labelIndex + 1] : "claude";
const room = target.toLowerCase().replace(/^@/, "");

if (!/^[a-z0-9_-]{1,32}$/.test(room)) {
  console.error("usage: ac-agent.mjs <@handle> [--label name]");
  process.exit(1);
}

let attempts = 0;

function connect() {
  const url = `${RELAY}/agent-presence?room=${room}&role=agent&label=${
    encodeURIComponent(label)}`;
  const socket = new WebSocket(url);

  socket.onopen = () => {
    attempts = 0;
    console.log(`🛰️ linked into @${room} as "${label}"`);
  };

  socket.onmessage = (event) => {
    let message;
    try { message = JSON.parse(event.data); } catch { return; }
    if (message.type === "agent-presence:status") {
      const { agents, surfaces } = message.content;
      console.log(
        `[@${room}] surfaces=${surfaces} agents=${agents.join(", ") || "none"}`,
      );
    } else if (message.type === "agent-presence:error") {
      console.error(`❌ ${message.content.message}`);
    }
  };

  socket.onclose = () => {
    attempts += 1;
    if (attempts > 60) {
      console.error("❌ relay unreachable, giving up");
      process.exit(1);
    }
    console.log(`… reconnecting (${attempts})`);
    setTimeout(connect, RETRY_MS);
  };
  socket.onerror = () => socket.close();
}

connect();

// 🪵 Logger, 24.05.17.23.10
// Writes system logs to a database and publishes them through
// redis so they can appear across aesthetic.

//import * as db from "./database.mjs";
//import * as KeyValue from "./kv.mjs";

let database; //, KeyValue;
const dev = process.env.CONTEXT === "dev";
let got;

// Link to a preconnected database and redis instance.
async function link(db /*, kv*/) {
  database = db;
  try {
    got = (await import("got")).got;
  } catch (err) {
    console.log(err);
  }
  // KeyValue = kv;
}

// 🚩
// Using the "action" field on logs.
// The action can be "handle:created" or "handle:changed"
// * This will be stored in the database so it can later
// be queried.
// * And read
// Actions should essentially follow the create / update / destroy / CRUD model
// with a colon separating the object for proper parsing on the other end.

// Every AC chat lives in the session server, which takes logs at /chat/log
// and picks the room from `x-chat-instance` (session-server/session.mjs).
// Handle and mute changes go to every AC chat; other logs to chat-system.
// Sotce Net's chat is excluded: AC handle updates don't apply there.
const CHAT_LOG_URL = dev
  ? "https://localhost:8889/chat/log"
  : "https://session-server.aesthetic.computer/chat/log";

function chatTargets(multi) {
  const system = { name: "chat-system", room: "chat-system.aesthetic.computer" };
  const clock = { name: "chat-clock", room: "chat-clock.aesthetic.computer" };
  return multi ? [system, clock] : [system];
}

async function log(text, data, from = "log") {
  if (!database) {
    console.error("⚠️🪵 Could not log:", from, text);
    return;
  }

  console.log("🪵 Logging...", from, text);
  // const database = await db.connect(); // 📕 Database
  // await KeyValue.connect();

  const msg = { from, text, when: new Date() };

  // Associate userID with a log if specified.
  if (data.user) {
    msg.users = [data.user];
  } else {
    msg.users = [];
  }

  if (data.action) msg.action = data.action;
  if (data.value) msg.value = data.value;

  // Save the `log` to the database.
  const logs = database.db.collection("logs");
  await logs.createIndex({ when: 1 }); // Index for `when`.
  await logs.insertOne({ ...msg }); // Add to database,

  // Alert all chat instances directly through HTTP calls with `LOGGER_KEY`.
  // For mute/unmute and handle actions (AC users only), we need to notify all AC chat servers
  // Note: Sotce users' handle changes are never logged (filtered in handle.mjs), so they won't reach here
  const isMultiChatAction = msg.action && (
    msg.action.match(/chat-system:(mute|unmute)/) ||
    msg.action.match(/handle:(update|strip|colors)/)
  );
  
  const chatServers = chatTargets(isMultiChatAction);

  for (const server of chatServers) {
    try {
      const response = await got.post(CHAT_LOG_URL, {
        json: msg,
        headers: {
          Authorization: `Bearer ${process.env.LOGGER_KEY}`,
          "x-chat-instance": server.room,
        },
        https: { rejectUnauthorized: !dev },
        timeout: { request: 10000 }, // 10 seconds
      });
      console.log(`Log to \`${server.name}\` successful:`, response.body);
    } catch (error) {
      console.error(`Log to \`${server.name}\` failed:`, error.message);
    }
  }

  // await KeyValue.pub("log", JSON.stringify(msg)); // & send to subs.

  // await KeyValue.disconnect();
  // await database.disconnect();
}

// Broadcast-only: HTTP POST to chat servers without writing to the database.
async function broadcast(text, data, from = "log") {
  if (!got) {
    try {
      got = (await import("got")).got;
    } catch (err) {
      console.error("⚠️📡 Could not load `got` for broadcast:", err);
      return;
    }
  }

  console.log("📡 Broadcasting...", from, text);

  const msg = { from, text, when: new Date() };

  if (data.user) {
    msg.users = [data.user];
  } else {
    msg.users = [];
  }

  if (data.action) msg.action = data.action;
  if (data.value) msg.value = data.value;

  const isMultiChatAction = msg.action && (
    msg.action.match(/chat-system:(mute|unmute)/) ||
    msg.action.match(/handle:(update|strip|colors)/)
  );

  const chatServers = chatTargets(isMultiChatAction);

  for (const server of chatServers) {
    try {
      const response = await got.post(CHAT_LOG_URL, {
        json: msg,
        headers: {
          Authorization: `Bearer ${process.env.LOGGER_KEY}`,
          "x-chat-instance": server.room,
        },
        https: { rejectUnauthorized: !dev },
        timeout: { request: 10000 },
      });
      console.log(`Broadcast to \`${server.name}\` successful:`, response.body);
    } catch (error) {
      console.error(`Broadcast to \`${server.name}\` failed:`, error.message);
    }
  }
}

export { link, log, broadcast };

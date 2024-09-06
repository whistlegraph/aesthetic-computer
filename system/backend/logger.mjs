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
// TODO: Add an "action" field to logs.
// The action can be "handle:created" or "handle:changed"
// * This will be stored in the database so it can later
// be queried.
// * And read
// Actions should essentially follow the create / update / destroy / CRUD model
// with a colon separating the object for proper parsing on the other end.

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

  const logs = database.db.collection("logs");
  await logs.createIndex({ when: 1 }); // Index for `when`.
  await logs.insertOne({ ...msg }); // Add to database,

  // TODO: Make a post request to https://localhost:8083 if dev is true
  //       otherwise make a post request to https://chat-system.aesthetic.computer
  // This request should include the msg object.
  const url = dev
    ? "https://localhost:8083/log"
    : "https://chat-system.aesthetic.computer/log";

  try {
    const response = await got.post(url, {
      json: msg,
      headers: {
        Authorization: `Bearer ${process.env.LOGGER_KEY}`,
      },
      https: { rejectUnauthorized: !dev },
    });
    console.log("Log to `chat-system` successful:", response.body);
  } catch (error) {
    console.error("Log to `chat-system` failed:", error.message);
  }

  // await KeyValue.pub("log", JSON.stringify(msg)); // & send to subs.

  // await KeyValue.disconnect();
  // await database.disconnect();
}

export { link, log };

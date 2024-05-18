// 🪵 Logger, 24.05.17.23.10
// Writes system logs to a database and publishes them through
// redis so they can appear across aesthetic.

//import * as db from "./database.mjs";
//import * as KeyValue from "./kv.mjs";

let database, KeyValue;

// Link to a preconnected database and redis instance.
function link(db, kv) {
  database = db;
  KeyValue = kv;
}

async function log(text, from = "system") {
  if (!database || !KeyValue) {
    console.erroer("⚠️🪵 Could not log:", from, text);
    return;
  }

  console.log("🪵 Logging...", from, text);
  // const database = await db.connect(); // 📕 Database
  // await KeyValue.connect();

  const msg = { from, text, when: new Date() };
  const logs = database.db.collection("logs");
  await logs.createIndex({ when: 1 }); // Index for `when`.
  await logs.insertOne({ ...msg }); // Add to database,
  await KeyValue.pub("log", JSON.stringify(msg)); // & send to subs.

  // await KeyValue.disconnect();
  // await database.disconnect();
}

export { link, log };
